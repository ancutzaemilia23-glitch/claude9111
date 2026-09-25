# =============================================================================
# 23_scale_check.R
# Purpose : Stage-1 scale robustness (audit step A3). First gate of the
#           post-GEBA workflow.
#
# Question, stated without the method's name:
#   Is gamma_1 a fact about how dissatisfaction converts into a PRR vote, or a
#   mechanical consequence of the LPM risk difference being bounded by the PRR
#   base rate, which may itself vary with informality?
#
# Stage 1 re-estimated on three scales, same sample and controls as script 18
# (corrected ITN classification), each fed into the same REML + Knapp-Hartung
# stage 2:
#   (1) LPM risk difference  - must reproduce 18_country_slopes_corrected.csv
#   (2) logit coefficient    - log-odds scale, not bounded by the base rate
#   (3) semi-elasticity b/p  - risk difference relative to the PRR share
#
# DECISION RULE (declared before running):
#   PASS : gamma_1 negative on the logit scale, with KH p in the same region as
#          the LPM result, in BOTH the EU-19 and older-11 panels -> script 24.
#   FAIL : logit gamma_1 ~ 0 or positive while the LPM version is significant
#          -> base-rate compression; skip the horse race; reframe.
#   Magnitudes are not comparable across scales; only sign and p are.
#
# Inputs : derived/analysis_individual.rds, output/05b_prr_linkage.csv,
#          output/10_moderator.csv, output/18_country_slopes_corrected.csv
# Outputs: output/23_slopes_three_scales.csv, output/23_scale_check.csv,
#          output/23_bubble_scales.pdf, logs/23_console.txt
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - Anchor check: if no script-18 slope matched (wrong file, renamed
#     column), max(abs(...), na.rm = TRUE) was -Inf and the check PASSED.
#     It now requires every country to have a reference slope.
#   - Records which warning sent a country to Firth (was discarded).
#   - paths overridable by environment variables; CSVs written with na = "".
# =============================================================================

while (sink.number() > 0) sink()
PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
paths <- list(derived = file.path(PROJ, "derived"),
              output  = file.path(PROJ, "output"),
              logs    = file.path(PROJ, "logs"))

pkgs <- c("dplyr", "purrr", "tibble", "readr", "metafor", "logistf")
miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss)) stop("install.packages(c(",
                       paste(sprintf('"%s"', miss), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
options(width = 150, max.print = 100000)
set.seed(20260925)

con <- file(file.path(paths$logs, "23_console.txt"), open = "wt")
sink(con, split = TRUE)
hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")

# na = "" keeps Latvia's party abbreviation "NA" from becoming missing
NUMCOLS <- c("q6_code", "prr", "informality", "incumbent", "cee", "recent_gov", "b", "se")
read_keepNA <- function(f) readr::read_csv(f, show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(dplyr::any_of(NUMCOLS), ~ suppressWarnings(as.numeric(.x))))

INCUMB <- c("Hungary", "Italy", "Slovakia", "Finland", "Croatia")
CEE    <- c("Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia",
            "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
EST19  <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark",
            "Estonia", "France", "Germany", "Greece", "Latvia", "Lithuania",
            "Netherlands", "Poland", "Portugal", "Romania", "Slovenia", "Spain", "Sweden")
ALL24  <- c(EST19, INCUMB)
OLDER  <- setdiff(EST19, CEE)          # 11: the split is post-communist, CY included
CTRL   <- c("age", "female", "educ", "urban")


# ========================= 1. DATA ===========================================

hdr("1. DATA (corrected classification, as script 18)")
link_b    <- read_keepNA(file.path(paths$output, "05b_prr_linkage.csv"))
new_codes <- link_b$q6_code[link_b$prr == 1 & !is.na(link_b$q6_code)]
stopifnot(10001 %in% new_codes)        # ITN present

d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
  dplyr::mutate(
    dissatisfied = dplyr::case_when(dissat_nat %in% c(3, 4) ~ 1L,
                                    dissat_nat %in% c(1, 2) ~ 0L,
                                    TRUE ~ NA_integer_),
    prr_corr = ifelse(classified, as.integer(q6_raw %in% new_codes), NA_integer_))

cat("OLDER panel (", length(OLDER), "):", paste(OLDER, collapse = ", "), "\n")


# ========================= 2. STAGE ONE, THREE SCALES ========================

hdr("2. STAGE ONE ON THREE SCALES")
f1 <- y ~ dissatisfied + age + female + educ + urban

fit_country <- function(k) {
  dd <- d |> dplyr::filter(classified, cty == k) |>
    dplyr::select(y = prr_corr, dissatisfied, dplyr::all_of(CTRL)) |>
    stats::na.omit()
  if (nrow(dd) < 100 || stats::var(dd$y) == 0) return(NULL)

  ct <- summary(stats::lm(f1, data = dd))$coefficients
  b_lpm <- ct["dissatisfied", 1]; se_lpm <- ct["dissatisfied", 2]

  # Logit; any warning (separation, fitted 0/1) sends the country to Firth
  why <- ""
  g <- tryCatch(stats::glm(f1, family = binomial(), data = dd),
                warning = function(w) { why <<- conditionMessage(w); NULL },
                error   = function(e) { why <<- conditionMessage(e); NULL })
  if (!is.null(g) && g$converged) {
    b_log  <- unname(coef(g)["dissatisfied"])
    se_log <- sqrt(vcov(g)["dissatisfied", "dissatisfied"])
    meth   <- "glm"
  } else {
    lf <- logistf::logistf(f1, data = dd)
    j  <- which(names(coef(lf)) == "dissatisfied")
    b_log <- unname(coef(lf)[j]); se_log <- sqrt(diag(lf$var))[j]
    meth  <- "firth"
  }

  p <- mean(dd$y)
  tibble::tibble(cty = k, n = nrow(dd), p_prr = p,
                 b_lpm = b_lpm, se_lpm = se_lpm,
                 b_log = b_log, se_log = se_log, logit_method = meth,
                 firth_reason = why,                       # CHANGE: new column
                 # delta-method SE treating p as known; state this in the note
                 b_sem = b_lpm / p, se_sem = se_lpm / p)
}

st1 <- purrr::map_dfr(ALL24, fit_country)
mod <- read_keepNA(file.path(paths$output, "10_moderator.csv")) |>
  dplyr::select(cty, informality)
st1 <- st1 |> dplyr::left_join(mod, by = "cty") |>
  dplyr::mutate(incumbent = as.integer(cty %in% INCUMB),
                cee       = as.integer(cty %in% CEE))

# ---- Anchor: LPM must reproduce script 18 -----------------------------------
ref <- read_keepNA(file.path(paths$output, "18_country_slopes_corrected.csv")) |>
  dplyr::filter(classification == "corr") |> dplyr::select(cty, b_ref = b)
chk <- st1 |> dplyr::left_join(ref, by = "cty")
if (anyNA(chk$b_ref))                                           # FIX: see header
  stop("no script-18 reference slope for: ",
       paste(chk$cty[is.na(chk$b_ref)], collapse = ", "))
dev <- max(abs(chk$b_lpm - chk$b_ref))
cat("max |LPM slope - script 18 slope|:", signif(dev, 3), "(must be ~0)\n")
if (dev > 1e-8) stop("Stage 1 does not reproduce script 18. Stop and return this ",
                     "output: the sample or coding differs, and nothing below is valid.")

print(as.data.frame(st1 |> dplyr::arrange(informality) |>
                      dplyr::transmute(cty, n, prr_share = round(100 * p_prr, 1),
                                       lpm_pp = round(100 * b_lpm, 2), logit = round(b_log, 3),
                                       semi = round(b_sem, 3), logit_method,
                                       informality = round(informality, 1), incumbent, cee)),
      row.names = FALSE)
cat("\nFirth fallback used in:",
    if (any(st1$logit_method == "firth"))
      paste(sprintf("%s (%s)", st1$cty[st1$logit_method == "firth"],
                    st1$firth_reason[st1$logit_method == "firth"]), collapse = "; ")
    else "none", "\n")
readr::write_csv(st1, file.path(paths$output, "23_slopes_three_scales.csv"), na = "")


# ========================= 3. STAGE TWO ON EACH SCALE ========================

hdr("3. STAGE TWO: REML + KNAPP-HARTUNG, EACH SCALE x EACH PANEL")
panels <- list(
  EU19         = EST19,
  older11      = OLDER,
  older10_noCY = setdiff(OLDER, "Cyprus"),
  cee8         = intersect(EST19, CEE))
scales <- list(LPM = c("b_lpm", "se_lpm"), logit = c("b_log", "se_log"),
               semi = c("b_sem", "se_sem"))

run_meta <- function(dat, yi, sei) {
  r <- metafor::rma(yi = dat[[yi]], sei = dat[[sei]], mods = ~ informality,
                    data = dat, method = "REML", test = "knha",
                    control = list(stepadj = 0.5, maxiter = 1000))
  pt <- metafor::permutest(r, iter = 9999, progbar = FALSE)
  tibble::tibble(k = r$k, gamma1 = r$beta[2], se = r$se[2],
                 ci_lb = r$ci.lb[2], ci_ub = r$ci.ub[2],
                 p_KH = r$pval[2], p_perm = pt$pval[2], tau = sqrt(r$tau2))
}

res <- purrr::imap_dfr(panels, function(ctys, pn) {
  dat <- st1 |> dplyr::filter(cty %in% ctys)
  purrr::imap_dfr(scales, function(v, sn)
    run_meta(dat, v[1], v[2]) |> dplyr::mutate(panel = pn, scale = sn, .before = 1))
})
print(as.data.frame(res |> dplyr::mutate(dplyr::across(where(is.double), ~ signif(.x, 4)))),
      row.names = FALSE)
readr::write_csv(res, file.path(paths$output, "23_scale_check.csv"), na = "")

cat("\nDECISION VIEW (sign and p only; magnitudes not comparable across scales)\n")
print(as.data.frame(res |> dplyr::filter(panel %in% c("EU19", "older11")) |>
                      dplyr::transmute(panel, scale, sign = ifelse(gamma1 < 0, "neg", "POS"),
                                       p_KH = round(p_KH, 4), p_perm = round(p_perm, 4))), row.names = FALSE)


# ========================= 4. BASE-RATE DIAGNOSTICS ==========================

hdr("4. BASE-RATE DIAGNOSTICS")
for (pn in c("EU19", "older11")) {
  dat <- st1 |> dplyr::filter(cty %in% panels[[pn]])
  cat(sprintf("%-8s cor(informality, PRR share) = %6.3f\n", pn,
              cor(dat$informality, dat$p_prr)))
}
# One added covariate; at k = 11 KH df = 8 -- read as descriptive
for (pn in c("EU19", "older11")) {
  dat <- st1 |> dplyr::filter(cty %in% panels[[pn]])
  r <- metafor::rma(yi = b_lpm, sei = se_lpm, mods = ~ informality + p_prr,
                    data = dat, method = "REML", test = "knha",
                    control = list(stepadj = 0.5, maxiter = 1000))
  cat("\n---", pn, ": LPM slope on informality + PRR share ---\n")
  print(round(coef(summary(r)), 4))
}


# ========================= 5. FIGURE =========================================

pdf(file.path(paths$output, "23_bubble_scales.pdf"), width = 10.5, height = 3.8)
op <- par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.2, 1))
dat <- st1 |> dplyr::filter(cty %in% EST19)
for (s in list(c("b_lpm", "Risk difference"), c("b_log", "Log-odds"),
               c("b_sem", "Semi-elasticity"))) {
  plot(dat$informality, dat[[s[1]]], pch = ifelse(dat$cee == 1, 1, 16),
       xlab = "Informal output (% of GDP)", ylab = s[2], main = s[2])
  abline(lm(dat[[s[1]]] ~ dat$informality), lty = 2)
  abline(h = 0, col = "grey60")
}
par(op); dev.off()

hdr("DONE -- return the full console output (logs/23_console.txt)")
sink(); close(con)
