# =============================================================================
# 28b_onestage_diagnostics.R
# Purpose : MCMC diagnostics for the one-stage model of script 28 (Part B),
#           and the dissatisfaction slope in PRR-cabinet countries.
#           Saved from the console snippet, which was never written to a file.
#
# FIXES relative to the console snippet
#   - round() was applied to the tibble returned by summarise_draws(); its
#     first column (`variable`) is character, so round() stopped with
#     "non-numeric argument to mathematical function".
#   - ESS was computed for the fixed effects only; the parameters that
#     usually mix worst in this model are the country SDs and the slope-
#     intercept correlation. All b_, sd_ and cor_ parameters are now checked,
#     with R-hat, and the ones below threshold are listed.
#   - The cabinet-country slope is at inf10 = 0, i.e. at the 24-country MEAN
#     informality; that is now stated in the output. The non-cabinet slope and
#     the difference are printed too, on both the logit and the odds-ratio scale.
#
# Input : output/28_fits/B1_onestage.rds (script 28, Part B)
# Output: output/28b_onestage_diagnostics.csv, logs/28b_console.txt
# =============================================================================

while (sink.number() > 0) sink()
PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
suppressPackageStartupMessages({ library(brms); library(posterior); library(dplyr) })
con <- file(file.path(PROJ, "logs", "28b_console.txt"), open = "wt"); sink(con, split = TRUE)

f <- file.path(PROJ, "output", "28_fits", "B1_onestage.rds")
if (!file.exists(f)) stop(f, " not found: run script 28 with RUN_ONESTAGE <- TRUE first.")
fitB <- readRDS(f)
dr   <- posterior::as_draws_df(fitB)

# 1. Divergences (want 0)
np <- brms::nuts_params(fitB)
cat("divergences:", sum(np$Value[np$Parameter == "divergent__"]), "\n")
cat("max tree depth reached:", max(np$Value[np$Parameter == "treedepth__"]),
    "(brms default limit 10; script 28 does not change it for Part B)\n")

# 2. R-hat and effective sample sizes for every population-level, SD and
#    correlation parameter (want R-hat < 1.01, ESS > 400)
sm <- posterior::summarise_draws(
  posterior::subset_draws(dr, variable = "^(b_|sd_|cor_)", regex = TRUE),
  "mean", "rhat", "ess_bulk", "ess_tail")
print(as.data.frame(sm |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)                                              # FIX: numeric cols only
bad <- sm |> dplyr::filter(rhat > 1.01 | ess_bulk < 400 | ess_tail < 400)
cat("\nparameters failing R-hat < 1.01 or ESS > 400:",
    if (nrow(bad)) paste(bad$variable, collapse = ", ") else "none", "\n")

# 3. The dissatisfaction slope by cabinet status, at MEAN informality (inf10 = 0)
s_opp <- dr$b_dissatisfied
s_cab <- dr$b_dissatisfied + dr$`b_dissatisfied:cab_main`
rep1 <- function(x, lab)
  cat(sprintf("%-32s logit %.2f [%.2f, %.2f]  OR %.2f  P(< 0) = %.3f\n", lab,
              mean(x), quantile(x, .025), quantile(x, .975), exp(mean(x)), mean(x < 0)))
cat("\nslopes at the 24-country mean informality:\n")
rep1(s_opp, "PRR not in cabinet")
rep1(s_cab, "PRR in cabinet")
rep1(s_cab - s_opp, "difference (cabinet - not)")

readr::write_csv(sm, file.path(PROJ, "output", "28b_onestage_diagnostics.csv"), na = "")
sink(); close(con)
