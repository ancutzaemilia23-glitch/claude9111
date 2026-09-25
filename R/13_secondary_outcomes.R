# =============================================================================
# 13_secondary_outcomes.R
# Purpose : The two pre-specified secondary outcomes, and the paper's tables.
#
# WHY THESE TWO AND NOTHING ELSE
#   PTV       Script 06 showed the PRR vote outcome over-reports by +6.2 pp in
#             older member states against -0.5 pp in post-2004 ones. That is a
#             REGIONAL gradient in measurement error, running along the same
#             axis as the moderator, and it is the main threat to gamma1. The
#             q9_ propensity-to-vote battery is asked of EVERYONE regardless of
#             turnout or vote recall, so it does not share that failure mode.
#             If gamma1 survives here, the objection is answered with data.
#
#   ABSTAIN   The remaining piece of the theory. Party supply is already ruled
#             out by the challenger result; abstention asks whether
#             dissatisfaction goes to non-voting instead. Under an exit
#             reading gamma1 on abstention should be POSITIVE.
#             CAVEAT: reported turnout is ~75% against ~51% actual, so this
#             measures reported abstention. It can support an interpretation,
#             not carry one.
#
#   Sample differs by outcome and this is deliberate:
#     prr, challenger : classified voters only
#     prr_ptv         : ALL respondents (no turnout filter) - the point of it
#     abstain         : all respondents with q5 in {1,2}; q5 == 98 dropped
#
# Run AFTER RUN_ANALYSIS.R.
# Output  : output/13_secondary_slopes.csv
#           output/TABLE1_countries.csv, TABLE2_stagetwo.csv, TABLE3_robust.csv
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - paths defined unconditionally (the `exists("paths")` test inherited
#     script 12's `paths`, which has no $derived -> readRDS(character(0))).
#   - TABLE 3 is now BUILT from output/10_robustness.csv (written by
#     RUN_ANALYSIS) instead of being transcribed by hand. Hand-typed numbers
#     are the most common way a replication package disagrees with its paper.
#   - Table 1 column "Post-2004" renamed "Post-communist" (the dummy excludes
#     Cyprus, a 2004 entrant).
# =============================================================================

PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")   # FIX: unconditional
paths <- list(derived = file.path(PROJ, "derived"),
              output  = file.path(PROJ, "output"))
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(tibble)})

hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")
write_out <- function(x, f) readr::write_csv(x, file.path(paths$output, f), na = "")

d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
  dplyr::mutate(dissatisfied = dplyr::case_when(
    dissat_nat %in% c(3,4) ~ 1L, dissat_nat %in% c(1,2) ~ 0L,
    TRUE ~ NA_integer_))

mod <- readr::read_csv(file.path(paths$output, "10_moderator.csv"),
                       show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(c(informality, incumbent, cee, b, se),
                              ~ suppressWarnings(as.numeric(.x)))) |>
  dplyr::select(cty, informality, incumbent, cee, b_prr = b, se_prr = se)

metareg <- function(y, se, x, label, show = TRUE) {
  ok <- stats::complete.cases(y, se, x)
  y <- y[ok]; v <- se[ok]^2; X <- cbind(1, x[ok]); k <- length(y); p <- 2
  W <- diag(1/v); XtWX <- t(X) %*% W %*% X
  b <- solve(XtWX, t(X) %*% W %*% y); r <- y - X %*% b
  Qe <- as.numeric(t(r) %*% W %*% r)
  P  <- W - W %*% X %*% solve(XtWX, t(X) %*% W)
  tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
  W2 <- diag(1/(v + tau2)); XtW2X <- t(X) %*% W2 %*% X
  b2 <- solve(XtW2X, t(X) %*% W2 %*% y); V <- solve(XtW2X)
  sb <- sqrt(diag(V)); tv <- b2[2]/sb[2]
  pv <- 2*stats::pt(-abs(tv), k-p)
  ci <- b2[2] + c(-1,1)*stats::qt(.975, k-p)*sb[2]
  if (show)
    cat(sprintf("%-32s k=%2d  gamma1 = %8.4f  se %6.4f  t %6.2f  p %.4f  CI [%7.4f,%7.4f]  tau %5.2f\n",
                label, k, b2[2], sb[2], tv, pv, ci[1], ci[2], sqrt(tau2)))
  invisible(list(g1 = b2[2], se = sb[2], p = pv, k = k, ci = ci))
}

# stage one for an arbitrary outcome, on an arbitrary respondent subset
slope_by_cty <- function(dat, yvar) {
  ctys <- sort(unique(dat$cty))
  purrr::map_dfr(ctys, function(k) {
    dd <- dat |> dplyr::filter(cty == k) |>
      dplyr::select(y = dplyr::all_of(yvar), dissatisfied, age, female,
                    educ, urban) |> stats::na.omit()
    if (nrow(dd) < 100 || stats::var(dd$y) == 0)
      return(tibble::tibble(cty = k, n = nrow(dd), b = NA_real_, se = NA_real_))
    ct <- summary(stats::lm(y ~ dissatisfied + age + female + educ + urban,
                            data = dd))$coefficients
    tibble::tibble(cty = k, n = nrow(dd), b = unname(ct["dissatisfied",1]),
                   se = unname(ct["dissatisfied",2]))
  })
}

## ---- 1. PTV -----------------------------------------------------------------
hdr("1. PROPENSITY TO VOTE FOR THE PRR PARTY (0-10)")
cat("Sample: ALL respondents, not just voters. Units are PTV points, so the\n")
cat("coefficient is NOT comparable in size to the vote-share slopes.\n\n")

ptv <- slope_by_cty(d |> dplyr::filter(!is.na(prr_ptv)), "prr_ptv") |>
  dplyr::rename(b_ptv = b, se_ptv = se, n_ptv = n)
cat("countries with a PTV outcome:", sum(!is.na(ptv$b_ptv)),
    "(Lithuania has none: its only PRR party is unrated)\n\n")

p1 <- mod |> dplyr::left_join(ptv, by = "cty") |> dplyr::filter(incumbent == 0)
print(as.data.frame(p1 |> dplyr::mutate(
  b_prr_pp = round(100*b_prr, 2), b_ptv_pts = round(b_ptv, 3),
  informality = round(informality, 1)) |>
    dplyr::select(cty, informality, n_ptv, b_prr_pp, b_ptv_pts) |>
    dplyr::arrange(informality)), row.names = FALSE)

cat("\n")
metareg(p1$b_prr, p1$se_prr, p1$informality, "PRR vote (reference)")
metareg(p1$b_ptv, p1$se_ptv, p1$informality, "PRR propensity to vote")
cat("\nIf the PTV estimate is also negative, the regional measurement-error\n")
cat("gradient documented in script 06 does NOT generate the result, because\n")
cat("PTV depends on neither recalled turnout nor recalled vote.\n")

## ---- 2. ABSTENTION ----------------------------------------------------------
hdr("2. ABSTENTION")
cat("Sample: respondents answering q5 yes or no. q5 == 98 dropped.\n")
cat("Coefficient = pp difference in reported abstention, dissatisfied vs not.\n")
cat("Exit reading predicts gamma1 > 0.\n\n")

abst <- slope_by_cty(d |> dplyr::filter(!is.na(abstain)), "abstain") |>
  dplyr::rename(b_abs = b, se_abs = se, n_abs = n)
p2 <- mod |> dplyr::left_join(abst, by = "cty") |> dplyr::filter(incumbent == 0)
print(as.data.frame(p2 |> dplyr::mutate(
  b_abs_pp = round(100*b_abs, 2), informality = round(informality, 1)) |>
    dplyr::select(cty, informality, n_abs, b_abs_pp) |>
    dplyr::arrange(informality)), row.names = FALSE)

cat("\n")
metareg(p2$b_abs, p2$se_abs, p2$informality, "reported abstention")
cat("\nCAVEAT: reported turnout is ~75% against ~51% actual. A positive\n")
cat("coefficient is consistent with exit but does not establish it; a null\n")
cat("does not rule it out either, because the measure is compromised.\n")

sec <- mod |> dplyr::left_join(ptv, by = "cty") |> dplyr::left_join(abst, by = "cty")
write_out(sec, "13_secondary_slopes.csv")

## ---- 3. PAPER TABLES --------------------------------------------------------
hdr("3. TABLES FOR THE MANUSCRIPT")

cmp <- readr::read_csv(file.path(paths$output, "11_challenger_slopes.csv"),
                       show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(c(b, se, b_chal, se_chal, informality,
                                incumbent, cee, n, n_extra),
                              ~ suppressWarnings(as.numeric(.x))))

t1 <- cmp |>
  dplyr::left_join(sec |> dplyr::select(cty, b_ptv, b_abs), by = "cty") |>
  dplyr::transmute(
    Country = cty, N = n,
    `Dissat-PRR gap (pp)` = round(100*b, 2), SE = round(100*se, 2),
    `Challenger gap (pp)` = round(100*b_chal, 2),
    `PRR PTV (pts)` = round(b_ptv, 2),
    `Informality (% GDP)` = round(informality, 1),
    `PRR incumbent` = ifelse(incumbent == 1, "yes", ""),
    `Post-communist` = ifelse(cee == 1, "yes", "")) |>     # CHANGE: was "Post-2004"
  dplyr::arrange(dplyr::desc(`Informality (% GDP)`))
write_out(t1, "TABLE1_countries.csv")
print(as.data.frame(t1), row.names = FALSE)

est <- cmp |> dplyr::filter(incumbent == 0)
g <- function(m) sprintf("%.4f (%.4f)%s", m$g1, m$se,
                         ifelse(m$p < .01, "**", ifelse(m$p < .05, "*", "")))
cat("\n--- Table 2: stage two ---\n")
r_prr  <- metareg(est$b, est$se, est$informality, sprintf("(1) PRR vote, k=%d", nrow(est)))
r_chal <- metareg(est$b_chal, est$se_chal, est$informality, "(2) any challenger")
r_ptv  <- metareg(p1$b_ptv, p1$se_ptv, p1$informality, "(3) PRR propensity to vote")
r_abs  <- metareg(p2$b_abs, p2$se_abs, p2$informality, "(4) reported abstention")

t2 <- tibble::tibble(
  Specification = c("PRR vote (primary)", "Any challenger vote",
                    "PRR propensity to vote", "Reported abstention"),
  gamma1 = c(r_prr$g1, r_chal$g1, r_ptv$g1, r_abs$g1),
  SE     = c(r_prr$se, r_chal$se, r_ptv$se, r_abs$se),
  p      = c(r_prr$p,  r_chal$p,  r_ptv$p,  r_abs$p),
  k      = c(r_prr$k,  r_chal$k,  r_ptv$k,  r_abs$k)) |>
  dplyr::mutate(dplyr::across(c(gamma1, SE), ~ round(.x, 4)), p = round(p, 4))
write_out(t2, "TABLE2_stagetwo.csv")

# CHANGE: Table 3 computed from RUN_ANALYSIS output, not typed by hand.
cat("\n--- Table 3: robustness (from output/10_robustness.csv) ---\n")
rob_f <- file.path(paths$output, "10_robustness.csv")
if (!file.exists(rob_f))
  stop("output/10_robustness.csv not found: re-run RUN_ANALYSIS.R (this version ",
       "writes it) before script 13.")
rob <- readr::read_csv(rob_f, show_col_types = FALSE, na = "")
fmt <- function(r) {
  if (is.na(r$g1) || r$test == "Leave-one-country-out") return(r$note)
  s <- sprintf("gamma1 = %+.4f, p = %.4f, k = %d", r$g1, r$p, r$k)
  if (!is.na(r$note) && nzchar(r$note)) s <- paste0(s, "; ", r$note)
  s
}
t3 <- tibble::tibble(Test = rob$test,
                     Result = purrr::map_chr(seq_len(nrow(rob)), \(i) fmt(rob[i, ])))
t3$Result[t3$Test == "Permutation test"] <-
  sprintf("p = %.4f, %s", rob$p[rob$test == "Permutation test"],
          rob$note[rob$test == "Permutation test"])
write_out(t3, "TABLE3_robust.csv")
print(as.data.frame(t3), row.names = FALSE)

hdr("DONE")
cat("Written: 13_secondary_slopes.csv, TABLE1_countries.csv,\n")
cat("         TABLE2_stagetwo.csv, TABLE3_robust.csv\n")
cat("\nTable 3 is recomputed from RUN_ANALYSIS output on every run, so it\n")
cat("cannot drift out of agreement with Table 2.\n")
