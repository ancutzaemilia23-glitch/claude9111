# reproduce.R -- runs the analysis pipeline from scratch, in order, each
# script in a FRESH R process (so no script can depend on another's session).
# Usage (Windows or any OS):  Rscript reproduce.R        [all confirmatory + exploratory]
#                             Rscript reproduce.R core   [RUN_ANALYSIS -> 14 only]
# Stops at the first failing script and prints the tail of its log.
args  <- commandArgs(trailingOnly = TRUE)
core  <- c("RUN_ANALYSIS", "12_figures", "13_secondary_outcomes", "14_robustness")
later <- c("18_itn_correction_and_KH", "20_regional_build", "21_core_regional_model",
           "22_opposition_prr", "23_scale_check", "23b_logit_heterogeneity",
           "26_build_position_table", "28_bayes_hierarchical", "28b_onestage_diagnostics")
steps <- if (length(args) && args[1] == "core") core else c(core, later)
# Before this: scripts 01-06 (not in this repository yet) must have produced
# output/02_party_frame.csv and output/05_prr_linkage.csv.
rs <- file.path(R.home("bin"), "Rscript")
dir.create("run_logs", showWarnings = FALSE)
for (s in steps) {
  t0 <- Sys.time(); log <- file.path("run_logs", paste0(s, ".log"))
  cat(sprintf("%-28s ... ", s))
  st <- system2(rs, file.path("R", paste0(s, ".R")), stdout = log, stderr = log)
  cat(sprintf("%s (%.1f min)\n", if (st == 0) "ok" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  if (st != 0) { cat(tail(readLines(log), 25), sep = "\n"); quit(status = 1) }
}
writeLines(capture.output(sessionInfo()), file.path("run_logs", "sessionInfo.txt"))
cat("done; logs in run_logs/, sessionInfo in run_logs/sessionInfo.txt\n")
