# =============================================================================
# 23b_logit_heterogeneity.R
# Purpose : Between-country heterogeneity of the stage-1 LOGIT slopes
#           (intercept-only REML + Knapp-Hartung), EU-19 and older-11.
#           Saved from the console snippet run after script 23, which was
#           never written to a file.
#
# CHANGES relative to the console snippet
#   - the loop variable lost the panel names (`for (sub in list(...))`), so
#     the two printed lines were unlabelled; now labelled and saved.
#   - `EU19` was defined as incumbent == 0 among the 24 = the 19, as intended;
#     made explicit.
#   - reads with na = "" and coerces, like the rest of the pipeline.
#
# Input : output/23_slopes_three_scales.csv
# Output: output/23b_logit_heterogeneity.csv
# =============================================================================

PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
suppressPackageStartupMessages({ library(metafor); library(readr); library(dplyr) })

s <- readr::read_csv(file.path(PROJ, "output", "23_slopes_three_scales.csv"),
                     show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(c(b_log, se_log, incumbent), ~ suppressWarnings(as.numeric(.x))))
older <- c("Austria","Belgium","Cyprus","Denmark","France","Germany",
           "Greece","Netherlands","Portugal","Spain","Sweden")
subsets <- list(EU19 = s$incumbent == 0, older11 = s$cty %in% older)

out <- do.call(rbind, lapply(names(subsets), function(nm) {
  r <- metafor::rma(yi = b_log, sei = se_log, data = s[subsets[[nm]], ],
                    method = "REML", test = "knha")
  cat(sprintf("%-8s k=%d  Q=%.1f (p=%.4f)  I2=%.1f%%  tau=%.3f  mean=%.3f\n",
              nm, r$k, r$QE, r$QEp, r$I2, sqrt(r$tau2), r$beta[1]))
  data.frame(panel = nm, k = r$k, Q = r$QE, Q_p = r$QEp, I2 = r$I2,
             tau = sqrt(r$tau2), mean_logit = r$beta[1], ci_lb = r$ci.lb, ci_ub = r$ci.ub)
}))
readr::write_csv(out, file.path(PROJ, "output", "23b_logit_heterogeneity.csv"), na = "")
