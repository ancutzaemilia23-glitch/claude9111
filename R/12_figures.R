# =============================================================================
# 12_figures.R  -- corrected Part 8 table + the paper's figures
#
# THE PART 8 BUG
#   RUN_ANALYSIS printed a `gap` column reading 3160.08 for Austria. Inside
#   the mutate, b_chal was overwritten with its rounded percentage-point
#   value BEFORE gap was computed, so the line differenced 31.92 against
#   0.3192. Same class of error as the earlier summarise() name-masking bug:
#   a column referenced after being redefined in the same call. The b_prr and
#   b_chal columns themselves were correct; only gap was wrong. Fixed below
#   by computing gap on the raw proportions first. (Now also fixed in
#   RUN_ANALYSIS Part 8.)
#
# FIGURES (base R, no ggplot2 dependency)
#   Figure 1  country slope against informality, marker area proportional to
#             precision, region distinguished, random-effects fit with 95%
#             band. This is the paper's main figure.
#   Figure 2  the same with the challenger-vote slope overlaid, which is what
#             rules out the party-supply explanation.
#
# INPUTS   output/10_moderator.csv, output/11_challenger_slopes.csv
# OUTPUTS  output/12_challenger_gap.csv
#          figures/fig1_informality_slope.{png,pdf}
#          figures/fig2_prr_vs_challenger.{png,pdf}
#
# Run AFTER RUN_ANALYSIS.R, in the same session or a fresh one.
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - paths are defined unconditionally. The old `if (!exists("paths"))`
#     test picked up whatever `paths` another script had left in the session
#     (e.g. script 13's, which has no $figures) -> session-dependent results.
#   - PDFs via cairo_pdf(): pdf() cannot encode the en dash (\u2013) in the
#     default Windows encoding and prints dots/warnings instead.
#   - legend label "Post-2004 accession" -> "Post-communist member states":
#     the `cee` dummy excludes Cyprus (a 2004 entrant), so the old label was
#     factually wrong.
#   - POS lookup no longer passes NA to text(pos=) for an unlisted country.
# =============================================================================

PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")   # FIX: unconditional
paths <- list(output  = file.path(PROJ, "output"),
              figures = file.path(PROJ, "figures"))
if (!dir.exists(paths$figures)) dir.create(paths$figures, recursive = TRUE)
suppressPackageStartupMessages({library(dplyr); library(readr)})

cmp <- readr::read_csv(file.path(paths$output, "11_challenger_slopes.csv"),
                       show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(c(b, se, b_chal, se_chal, informality,
                                incumbent, cee, n_extra),
                              ~ suppressWarnings(as.numeric(.x))))

est <- cmp |> dplyr::filter(incumbent == 0)

## ---- random-effects meta-regression (as in RUN_ANALYSIS) -------------------
metareg <- function(y, se, x) {
  ok <- stats::complete.cases(y, se, x)
  y <- y[ok]; v <- se[ok]^2; X <- cbind(1, x[ok]); k <- length(y); p <- 2
  W <- diag(1/v); XtWX <- t(X) %*% W %*% X
  b <- solve(XtWX, t(X) %*% W %*% y); r <- y - X %*% b
  Qe <- as.numeric(t(r) %*% W %*% r)
  P  <- W - W %*% X %*% solve(XtWX, t(X) %*% W)
  tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
  W2 <- diag(1/(v + tau2)); XtW2X <- t(X) %*% W2 %*% X
  b2 <- solve(XtW2X, t(X) %*% W2 %*% y); V <- solve(XtW2X)
  list(b = as.numeric(b2), V = V, se = sqrt(diag(V)), tau = sqrt(tau2),
       k = k, df = k - p,
       p = as.numeric(2 * stats::pt(-abs(b2 / sqrt(diag(V))), k - p)))
}

m_prr  <- metareg(est$b,      est$se,      est$informality)
m_chal <- metareg(est$b_chal, est$se_chal, est$informality)

## ---- 1. CORRECTED GAP TABLE -------------------------------------------------
cat("\n", strrep("=", 74), "\n CORRECTED: PRR vs CHALLENGER SLOPES\n",
    strrep("=", 74), "\n", sep = "")

gap_tab <- est |>
  dplyr::mutate(gap_pp = 100 * (b_chal - b),          # computed BEFORE rounding
                b_prr_pp  = round(100 * b, 2),
                b_chal_pp = round(100 * b_chal, 2),
                gap_pp    = round(gap_pp, 2),
                informality = round(informality, 1)) |>
  dplyr::select(cty, informality, n_extra, b_prr_pp, b_chal_pp, gap_pp) |>
  dplyr::arrange(informality)
print(as.data.frame(gap_tab), row.names = FALSE)

cat("\nCountries with n_extra = 0 must show gap exactly 0 (the challenger\n")
cat("outcome equals the PRR outcome there by construction):\n")
print(as.data.frame(gap_tab |> dplyr::filter(n_extra == 0) |>
                      dplyr::select(cty, gap_pp)), row.names = FALSE)
stopifnot(all(gap_tab$gap_pp[gap_tab$n_extra == 0] == 0))
cat("check passed.\n")

readr::write_csv(gap_tab, file.path(paths$output, "12_challenger_gap.csv"), na = "")

cat(sprintf("\nPRR        gamma1 = %.4f (se %.4f, p = %.4f)\n",
            m_prr$b[2],  m_prr$se[2],  m_prr$p[2]))
cat(sprintf("CHALLENGER gamma1 = %.4f (se %.4f, p = %.4f)\n",
            m_chal$b[2], m_chal$se[2], m_chal$p[2]))
cat("Both negative: dissatisfaction converts into anti-establishment voting\n")
cat("of ANY kind at a lower rate where informality is higher. The party-\n")
cat("supply explanation does not account for the result.\n")

## ---- 2. SHARED PLOTTING HELPERS --------------------------------------------
COL_OLD <- "#D1651B"   # older member states
COL_CEE <- "#2E7D5B"   # post-communist member states
COL_FIT <- "#3C6E9F"
COL_ALT <- "#8E5AA8"   # challenger series

# label position per country: 1 below, 2 left, 3 above, 4 right
POS <- c(Austria = 3, Netherlands = 1, France = 3, Germany = 4, Denmark = 4,
         Sweden = 4, Belgium = 2, Spain = 4, Portugal = 3, Cyprus = 1,
         Greece = 4, `Czech Republic` = 1, Poland = 3, Slovenia = 1,
         Latvia = 1, Romania = 4, Estonia = 3, Lithuania = 1, Bulgaria = 3)
pos_of <- function(ct) { p <- unname(POS[ct]); p[is.na(p)] <- 4; p }  # FIX: no NA to text()

# CHANGE: journal convention -- never print "p = 0.000"
fmt_p <- function(p) if (p < .001) "p < 0.001" else sprintf("p = %.3f", p)

band <- function(m, xs) {
  X <- cbind(1, xs)
  fit <- as.vector(X %*% m$b)
  sef <- sqrt(rowSums((X %*% m$V) * X))
  tc  <- stats::qt(.975, m$df)
  list(fit = fit, lo = fit - tc * sef, hi = fit + tc * sef)
}

open_dev <- function(stub, w = 7.2, h = 5.2) {
  grDevices::png(file.path(paths$figures, paste0(stub, ".png")),
                 width = w, height = h, units = "in", res = 300)
}
# FIX: cairo_pdf embeds the en dash; pdf() cannot in the default encoding
open_pdf <- function(stub, w = 7.2, h = 5.2)
  grDevices::cairo_pdf(file.path(paths$figures, paste0(stub, ".pdf")),
                       width = w, height = h)

## ---- 3. FIGURE 1 ------------------------------------------------------------
draw_fig1 <- function() {
  x <- est$informality; y <- 100 * est$b
  prec <- 1 / est$se^2
  cex <- 0.9 + 2.1 * sqrt(prec / max(prec))
  xs <- seq(8, 30.5, length.out = 200)
  bd <- band(m_prr, xs)

  par(mar = c(4.4, 4.6, 3.6, 1.2), mgp = c(2.7, .7, 0), las = 1,
      family = "sans", cex.axis = .85)
  plot(NA, xlim = c(8, 30.5), ylim = c(-16, 40), xlab = "", ylab = "",
       axes = FALSE)
  polygon(c(xs, rev(xs)), c(100 * bd$lo, rev(100 * bd$hi)),
          col = grDevices::adjustcolor(COL_FIT, .13), border = NA)
  abline(h = 0, col = "#999999", lty = 2, lwd = .9)
  lines(xs, 100 * bd$fit, col = COL_FIT, lwd = 2.2)

  pch <- ifelse(est$cee == 1, 22, 21)
  bg  <- ifelse(est$cee == 1, COL_CEE, COL_OLD)
  points(x, y, pch = pch, bg = bg, col = "white", lwd = 1.1, cex = cex)
  text(x, y, est$cty, pos = pos_of(est$cty), offset = .55, cex = .62,
       col = "#333333")

  axis(1, at = seq(10, 30, 5), lwd = 0, lwd.ticks = .8)
  axis(2, at = seq(-10, 40, 10), lwd = 0, lwd.ticks = .8)
  box(bty = "l", col = "#444444")
  mtext("Informal output, % of official GDP (World Bank DGE, 2010\u20132020 mean)",
        side = 1, line = 2.6, cex = .88)
  mtext("Dissatisfaction\u2013PRR gap, percentage points",
        side = 2, line = 3.1, cex = .88, las = 0)
  mtext("Institutional dissatisfaction translates less into radical-right voting",
        side = 3, line = 1.9, adj = 0, cex = 1.02, font = 2)
  mtext("where informality is higher \u2014 but the pattern also tracks region",
        side = 3, line = .7, adj = 0, cex = .92, col = "#444444")

  legend("topright", legend = c("Older member states", "Post-communist member states"),
         pch = c(21, 22), pt.bg = c(COL_OLD, COL_CEE), col = "white",
         pt.cex = 1.5, bty = "n", cex = .78, y.intersp = 1.3)
  legend("bottomleft",
         legend = c(sprintf("Random-effects meta-regression, k = %d", m_prr$k),
                    sprintf("gamma1 = %.3f (se %.3f), %s",
                            m_prr$b[2], m_prr$se[2], fmt_p(m_prr$p[2])),
                    "Marker area proportional to precision"),
         bty = "n", cex = .68, text.col = "#555555", y.intersp = 1.25)
}

open_dev("fig1_informality_slope"); draw_fig1(); grDevices::dev.off()
open_pdf("fig1_informality_slope"); draw_fig1(); grDevices::dev.off()

## ---- 4. FIGURE 2: PRR AGAINST CHALLENGER -----------------------------------
draw_fig2 <- function() {
  x <- est$informality
  y1 <- 100 * est$b; y2 <- 100 * est$b_chal
  xs <- seq(8, 30.5, length.out = 200)
  b1 <- band(m_prr, xs); b2 <- band(m_chal, xs)

  par(mar = c(4.4, 4.6, 3.6, 1.2), mgp = c(2.7, .7, 0), las = 1,
      family = "sans", cex.axis = .85)
  plot(NA, xlim = c(8, 30.5), ylim = c(-16, 50), xlab = "", ylab = "",
       axes = FALSE)
  abline(h = 0, col = "#999999", lty = 2, lwd = .9)
  segments(x, y1, x, y2, col = "#BBBBBB", lwd = .8)
  lines(xs, 100 * b1$fit, col = COL_FIT, lwd = 2.2)
  lines(xs, 100 * b2$fit, col = COL_ALT, lwd = 2.2, lty = 5)
  points(x, y1, pch = 21, bg = COL_FIT, col = "white", lwd = 1, cex = 1.25)
  points(x, y2, pch = 24, bg = COL_ALT, col = "white", lwd = 1, cex = 1.15)

  axis(1, at = seq(10, 30, 5), lwd = 0, lwd.ticks = .8)
  axis(2, at = seq(-10, 50, 10), lwd = 0, lwd.ticks = .8)
  box(bty = "l", col = "#444444")
  mtext("Informal output, % of official GDP", side = 1, line = 2.6, cex = .88)
  mtext("Dissatisfaction\u2013vote gap, percentage points",
        side = 2, line = 3.1, cex = .88, las = 0)
  mtext("The decline is not about which parties are on offer",
        side = 3, line = 1.9, adj = 0, cex = 1.02, font = 2)
  mtext("Dissatisfaction converts into anti-establishment voting of any kind at a lower rate",
        side = 3, line = .7, adj = 0, cex = .82, col = "#444444")

  legend("topright",
         legend = c(sprintf("Radical right   gamma1 = %.3f (%s)",
                            m_prr$b[2], fmt_p(m_prr$p[2])),
                    sprintf("Any challenger  gamma1 = %.3f (%s)",
                            m_chal$b[2], fmt_p(m_chal$p[2]))),
         pch = c(21, 24), pt.bg = c(COL_FIT, COL_ALT), col = "white",
         lty = c(1, 5), lwd = 2, pt.cex = 1.3, bty = "n", cex = .74,
         y.intersp = 1.4, seg.len = 2.4)
  legend("bottomleft",
         legend = c("Vertical lines join the two outcomes for one country",
                    "Countries with no additional challenger party show no line"),
         bty = "n", cex = .66, text.col = "#555555", y.intersp = 1.25)
}

open_dev("fig2_prr_vs_challenger"); draw_fig2(); grDevices::dev.off()
open_pdf("fig2_prr_vs_challenger"); draw_fig2(); grDevices::dev.off()

cat("\nWritten to", paths$figures, ":\n")
cat("  fig1_informality_slope.png / .pdf\n")
cat("  fig2_prr_vs_challenger.png / .pdf\n")
cat("\nPNG at 300 dpi for the Word template; PDF for a later journal version.\n")
