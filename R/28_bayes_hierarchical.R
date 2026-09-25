# =============================================================================
# 28_bayes_hierarchical.R
# Purpose : Bayesian hierarchical estimation for the small number of countries
#           (supervisor's point: k is small, so treat tau as uncertain rather
#           than as known). Reports posterior means, 95% credible intervals
#           and P(effect < 0) instead of p-values.
#
# Two estimators, both on the LOGIT scale (the scale that passed/failed the
# declared test in script 23), frequentist REML+KH printed alongside:
#   (A) Bayesian meta-regression of the stage-1 logit slopes (script 23),
#       measurement SEs known, between-country SD tau given a half-normal
#       prior. Fast (seconds per model).
#   (B) One-stage multilevel logit: random intercept and random slope on
#       dissatisfaction by country, cross-level interactions with
#       informality and PRR incumbency. Slow (tens of minutes to hours).
#
# Moderators:
#   informality : DGE 2010-2020 mean, centred, per 10 percentage points
#   cab_main    : main PRR party in cabinet on 6 June 2024 (script 26 rule)
#   cab_paper   : conference-paper incumbency list (Slovakia = 1) -- sensitivity
#
# STATUS: EXPLORATORY. The cabinet-status moderator was introduced after the
# informality result failed the declared logit-scale test (script 23); it
# must be reported as such, and confirmed on EES 2019 (scripts 26-27).
#
# Inputs : output/23_slopes_three_scales.csv, output/26_prr_position_coding.csv,
#          derived/analysis_individual.rds, output/05b_prr_linkage.csv
# Outputs: output/28_bayes_meta.csv, output/28_prior_sensitivity.csv,
#          output/28_influence_checks.csv, output/28_bayes_onestage.csv,
#          output/28_fits/*.rds, logs/28_console.txt
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - Section 3b (influence checks) runs inside a guard: an error in the
#     Student-t model no longer kills Part B, which is the slow step.
#   - Before fitting the Student-t model the script prints brms's own prior
#     table for class "df", so the constant(4) prior is shown to attach to an
#     actual parameter (if not, brms stops; the guard reports it).
#   - stops with a clear message if script 23's file does not have 24 rows or
#     script 26's coding is missing for a country (was a bare stopifnot).
#   - paths overridable by environment variables; CSVs written with na = "".
#   - the console snippets that followed this script are now 28b (one-stage
#     diagnostics); the influence snippet was already merged here as 3b.
# =============================================================================

while (sink.number() > 0) sink()
closeAllConnections()
options(error = NULL)   # a plain stop on error, not the debugger
PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
paths <- list(derived = file.path(PROJ, "derived"),
              output  = file.path(PROJ, "output"),
              fits    = file.path(PROJ, "output", "28_fits"),
              logs    = file.path(PROJ, "logs"))
dir.create(paths$fits, showWarnings = FALSE, recursive = TRUE)

pkgs <- c("dplyr", "purrr", "tibble", "readr", "tidyr", "metafor", "brms", "posterior")
miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss)) stop("install.packages(c(",
                       paste(sprintf('"%s"', miss), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
options(width = 150, max.print = 100000, mc.cores = max(1, parallel::detectCores() - 1))
# Backend: use cmdstanr only if CmdStan itself is installed and its path set;
# the R package alone is not enough. Otherwise fall back to rstan.
BACKEND <- "rstan"
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  cs_ok <- tryCatch({ cmdstanr::cmdstan_version(); TRUE }, error = function(e) FALSE)
  if (cs_ok) BACKEND <- "cmdstanr"
}
if (BACKEND == "rstan" && !requireNamespace("rstan", quietly = TRUE))
  stop("No working Stan backend. Either install CmdStan:\n",
       "  cmdstanr::check_cmdstan_toolchain(fix = TRUE); cmdstanr::install_cmdstan(cores = 4)\n",
       "or install rstan: install.packages('rstan'). Both need Rtools on Windows.")
SEED <- 20260926
RUN_ONESTAGE <- TRUE     # part A is clean (checked); now fit the one-stage model

con <- file(file.path(paths$logs, "28_console.txt"), open = "wt")
sink(con, split = TRUE)
hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")
guard <- function(label, expr) tryCatch(expr, error = function(e)
  cat("\n*** PART", label, "FAILED:", conditionMessage(e), "***\n"))
cat("backend:", BACKEND, "| brms", as.character(packageVersion("brms")),
    "| metafor", as.character(packageVersion("metafor")), "\n")

NUMCOLS <- c("q6_code", "prr", "informality", "incumbent", "cee", "b_log", "se_log",
             "b_lpm", "se_lpm", "in_cabinet", "wave")
read_keepNA <- function(f) readr::read_csv(f, show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(dplyr::any_of(NUMCOLS), ~ suppressWarnings(as.numeric(.x))))

CEE   <- c("Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia",
           "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
OLDER <- c("Austria", "Belgium", "Cyprus", "Denmark", "France", "Germany", "Greece",
           "Netherlands", "Portugal", "Spain", "Sweden")


# ========================= 1. COUNTRY-LEVEL DATA =============================

hdr("1. COUNTRY-LEVEL DATA")
pos24 <- read_keepNA(file.path(paths$output, "26_prr_position_coding.csv")) |>
  dplyr::filter(wave == 2024) |>
  dplyr::select(cty, cab_main = in_cabinet, position)

st <- read_keepNA(file.path(paths$output, "23_slopes_three_scales.csv")) |>
  dplyr::left_join(pos24, by = "cty") |>
  dplyr::mutate(cab_paper = incumbent,
                inf10 = (informality - mean(informality)) / 10)
# FIX: informative stops instead of a bare stopifnot
if (nrow(st) != 24) stop("23_slopes_three_scales.csv has ", nrow(st),
                         " rows, expected 24: re-run script 23.")
if (anyNA(st$cab_main)) stop("no 2024 cabinet coding (script 26) for: ",
                             paste(st$cty[is.na(st$cab_main)], collapse = ", "))
print(as.data.frame(st |> dplyr::arrange(b_log) |>
                      dplyr::transmute(cty, logit = round(b_log, 2), se = round(se_log, 2),
                                       informality = round(informality, 1), cab_main, cab_paper, position)),
      row.names = FALSE)
cat("\nCodings differ for:", paste(st$cty[st$cab_main != st$cab_paper], collapse = ", "), "\n")

samples <- list(older11 = st |> dplyr::filter(cty %in% OLDER),
                EU19    = st |> dplyr::filter(cab_paper == 0),
                all24   = st)


# ========================= 2. PART A: BAYESIAN META-REGRESSION ===============

hdr("2. PART A: BAYESIAN META-REGRESSION OF STAGE-1 LOGIT SLOPES")

fit_meta <- function(dat, rhs, y = "b_log", se = "se_log",
                     sd_b = 1, sd_tau = 1, tag) {
  f <- brms::bf(as.formula(sprintf("%s | se(%s) ~ %s + (1 | cty)", y, se, rhs)))
  pr <- c(brms::set_prior("normal(0, 2)", class = "Intercept"),
          brms::set_prior(sprintf("normal(0, %s)", sd_tau), class = "sd"))
  if (rhs != "1") pr <- c(pr, brms::set_prior(sprintf("normal(0, %s)", sd_b), class = "b"))
  brms::brm(f, data = dat, prior = pr, chains = 4, iter = 6000, warmup = 2000,
            seed = SEED, control = list(adapt_delta = 0.995, max_treedepth = 12),
            backend = BACKEND, refresh = 0, silent = 2,
            file = file.path(paths$fits, tag), file_refit = "on_change")
}

summ <- function(fit, vars, tag) {
  dr <- posterior::as_draws_df(fit)
  np <- brms::nuts_params(fit)
  diag <- tibble::tibble(max_rhat = max(brms::rhat(fit), na.rm = TRUE),
                         divergences = sum(np$Value[np$Parameter == "divergent__"]))
  purrr::map_dfr(vars, function(v) {
    x <- dr[[paste0("b_", v)]]
    tibble::tibble(model = tag, term = v, post_mean = mean(x),
                   cri_lb = unname(quantile(x, .025)), cri_ub = unname(quantile(x, .975)),
                   p_neg = mean(x < 0))
  }) |> dplyr::mutate(tau_median = median(dr$sd_cty__Intercept),
                      tau_cri_ub = unname(quantile(dr$sd_cty__Intercept, .975)),
                      k = stats::nobs(fit)) |>
    dplyr::bind_cols(diag)
}

freq <- function(dat, rhs, y = "b_log", se = "se_log") {
  r <- metafor::rma(yi = dat[[y]], sei = dat[[se]], mods = as.formula(paste("~", rhs)),
                    data = dat, method = "REML", test = "knha",
                    control = list(stepadj = 0.5, maxiter = 1000))
  tibble::tibble(term = rownames(r$beta)[-1], freq_est = r$beta[-1],
                 freq_p_KH = r$pval[-1])
}

specs <- tibble::tribble(
  ~tag,                 ~sample,   ~rhs,               ~y,      ~se,
  "A1_older11_inf",     "older11", "inf10",            "b_log", "se_log",
  "A2_EU19_inf",        "EU19",    "inf10",            "b_log", "se_log",
  "A3_all24_cab",       "all24",   "cab_main",         "b_log", "se_log",
  "A4_all24_cab_inf",   "all24",   "cab_main + inf10", "b_log", "se_log",
  "A5_all24_cabpaper",  "all24",   "cab_paper",        "b_log", "se_log",
  "A6_EU19_inf_LPM",    "EU19",    "inf10",            "b_lpm", "se_lpm")

resA <- purrr::pmap_dfr(specs, function(tag, sample, rhs, y, se) {
  dat <- samples[[sample]]
  fit <- fit_meta(dat, rhs, y, se, tag = tag)
  vars <- strsplit(gsub(" ", "", rhs), "\\+")[[1]]
  summ(fit, vars, tag) |>
    dplyr::left_join(freq(dat, rhs, y, se), by = "term") |>
    dplyr::mutate(sample = sample, scale = ifelse(y == "b_log", "logit", "LPM"),
                  .after = model)
})
print(as.data.frame(resA |> dplyr::mutate(dplyr::across(where(is.double), ~ signif(.x, 3)))),
      row.names = FALSE)
readr::write_csv(resA, file.path(paths$output, "28_bayes_meta.csv"), na = "")
cat("\nReading: p_neg is the posterior probability that the effect is negative.",
    "\nA6 is on the LPM scale, shown only to make the scale dependence explicit.\n")


# ========================= 3. PRIOR SENSITIVITY ==============================

hdr("3. PRIOR SENSITIVITY (A1 and A3)")
grid <- tidyr::expand_grid(tag0 = c("A1_older11_inf", "A3_all24_cab"),
                           sd_b = c(0.5, 1, 2), sd_tau = c(0.5, 1, 2))
resP <- purrr::pmap_dfr(grid, function(tag0, sd_b, sd_tau) {
  sp <- specs |> dplyr::filter(tag == tag0)
  tag <- sprintf("%s_b%s_t%s", tag0, sd_b, sd_tau)
  fit <- fit_meta(samples[[sp$sample]], sp$rhs, sp$y, sp$se, sd_b, sd_tau, tag)
  summ(fit, sp$rhs, tag) |> dplyr::mutate(sd_b = sd_b, sd_tau = sd_tau)
})
print(as.data.frame(resP |> dplyr::select(model, sd_b, sd_tau, post_mean, cri_lb, cri_ub,
                                          p_neg, tau_median, max_rhat, divergences) |>
                      dplyr::mutate(dplyr::across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
readr::write_csv(resP, file.path(paths$output, "28_prior_sensitivity.csv"), na = "")


# ========================= 3b. INFLUENCE AND HETEROGENEITY CHECKS ============

guard("3b", {                                   # FIX: failure here must not stop Part B
  hdr("3b. VARIANCE EXPLAINED, HUNGARY, HEAVY-TAILED COUNTRY EFFECTS")
  # (i) Share of between-country variance explained by cabinet status
  #     (ratio of posterior medians of tau^2 -- an approximation, not a posterior)
  f0 <- fit_meta(samples$all24, "1", tag = "A0_all24_null")
  f3 <- fit_meta(samples$all24, "cab_main", tag = "A3_all24_cab")
  t0 <- median(posterior::as_draws_df(f0)$sd_cty__Intercept)
  t3 <- median(posterior::as_draws_df(f3)$sd_cty__Intercept)
  cat(sprintf("tau null %.3f | tau with cabinet %.3f | approx. share of tau^2 explained %.2f\n",
              t0, t3, 1 - t3^2 / t0^2))

  # (ii) Without Hungary
  chkH <- summ(fit_meta(dplyr::filter(samples$all24, cty != "Hungary"), "cab_main",
                        tag = "A3_noHU"), "cab_main", "A3_noHU")

  # (iii) Student-t country effects with df fixed at 4 (estimating df at k = 24
  #       produced divergences and low E-BFMI; fixing it is the standard remedy)
  f_t <- brms::bf(b_log | se(se_log) ~ cab_main + (1 | gr(cty, dist = "student")))
  gp  <- brms::get_prior(f_t, data = samples$all24)
  cat("\nbrms prior slots of class 'df' (constant(4) must attach to one):\n")
  print(as.data.frame(gp[gp$class == "df", c("prior", "class", "group")]), row.names = FALSE)
  fit_t <- brms::brm(f_t, data = samples$all24,
                     prior = c(brms::set_prior("normal(0, 2)", class = "Intercept"),
                               brms::set_prior("normal(0, 1)", class = "b"),
                               brms::set_prior("normal(0, 1)", class = "sd"),
                               brms::set_prior("constant(4)", class = "df", group = "cty")),
                     chains = 4, iter = 6000, warmup = 2000, seed = SEED,
                     control = list(adapt_delta = 0.999, max_treedepth = 12),
                     backend = BACKEND, refresh = 0, silent = 2,
                     file = file.path(paths$fits, "A3_student_df4"), file_refit = "on_change")
  chkT <- summ(fit_t, "cab_main", "A3_student_df4")
  chk <- dplyr::bind_rows(chkH, chkT)
  print(as.data.frame(chk |> dplyr::mutate(dplyr::across(where(is.double), ~ signif(.x, 3)))),
        row.names = FALSE)
  readr::write_csv(chk, file.path(paths$output, "28_influence_checks.csv"), na = "")
})


# ========================= 4. PART B: ONE-STAGE MULTILEVEL LOGIT =============

if (RUN_ONESTAGE) {
  hdr("4. PART B: ONE-STAGE MULTILEVEL LOGIT (all 24 countries)")
  link_b    <- read_keepNA(file.path(paths$output, "05b_prr_linkage.csv"))
  new_codes <- link_b$q6_code[link_b$prr == 1 & !is.na(link_b$q6_code)]
  zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

  ind <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
    dplyr::filter(classified, cty %in% st$cty) |>
    dplyr::mutate(dissatisfied = dplyr::case_when(dissat_nat %in% c(3, 4) ~ 1L,
                                                  dissat_nat %in% c(1, 2) ~ 0L),
                  y = as.integer(q6_raw %in% new_codes)) |>
    dplyr::select(cty, y, dissatisfied, age, female, educ, urban) |>
    stats::na.omit() |>
    dplyr::left_join(st |> dplyr::select(cty, inf10, cab_main), by = "cty") |>
    dplyr::mutate(dplyr::across(c(age, female, educ, urban), zs))
  cat("respondents:", nrow(ind), " countries:", dplyr::n_distinct(ind$cty), "\n")

  pr1 <- c(brms::set_prior("normal(0, 2)", class = "Intercept"),
           brms::set_prior("normal(0, 1)", class = "b"),
           brms::set_prior("normal(0, 1)", class = "sd"),
           brms::set_prior("lkj(2)", class = "cor"))
  f1 <- brms::bf(y ~ dissatisfied * (inf10 + cab_main) + age + female + educ + urban +
                   (1 + dissatisfied | cty))
  fitB <- brms::brm(f1, data = ind, family = brms::bernoulli(), prior = pr1,
                    chains = 4, iter = 3000, warmup = 1000, seed = SEED,
                    control = list(adapt_delta = 0.95), backend = BACKEND,
                    refresh = 250, file = file.path(paths$fits, "B1_onestage"),
                    file_refit = "on_change")
  dr <- posterior::as_draws_df(fitB)
  resB <- purrr::map_dfr(c("dissatisfied", "dissatisfied:inf10", "dissatisfied:cab_main"),
                         function(v) { x <- dr[[paste0("b_", v)]]
                         tibble::tibble(term = v, post_mean = mean(x),
                                        cri_lb = unname(quantile(x, .025)), cri_ub = unname(quantile(x, .975)),
                                        p_neg = mean(x < 0)) }) |>
    dplyr::mutate(tau_slope_median = median(dr$sd_cty__dissatisfied),
                  max_rhat = max(brms::rhat(fitB), na.rm = TRUE))
  print(as.data.frame(resB |> dplyr::mutate(dplyr::across(where(is.double), ~ signif(.x, 3)))),
        row.names = FALSE)
  readr::write_csv(resB, file.path(paths$output, "28_bayes_onestage.csv"), na = "")
  cat("\nNote: controls enter with common coefficients here, whereas stage 1 in the",
      "two-stage design lets them vary by country; differences in the dissatisfaction",
      "terms between A and B partly reflect that.\n")
}

hdr("DONE -- return the full console output (logs/28_console.txt); then run 28b")
sink(); close(con)
