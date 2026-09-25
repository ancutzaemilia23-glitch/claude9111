# 00_install_packages.R -- one-time setup. R >= 4.3 recommended (native pipe
# `|>` and `\(x)` need 4.1; UTF-8 source files on Windows need 4.2).
# Versions below are those the code was checked against in this repository's
# smoke test; your own exact versions must be frozen with renv (step 2).
pkgs <- c(
  haven = "2.5.4", dplyr = "1.1.4", tidyr = "1.3.1", readr = "2.1.5",
  purrr = "1.0.2", tibble = "3.2.1", stringr = "1.5.1", readxl = "1.4.3",
  metafor = "4.4.0", clubSandwich = "0.5.10",
  logistf = NA, brms = NA, posterior = NA)   # NA = not tested here (no Stan in sandbox)
miss <- setdiff(names(pkgs), rownames(installed.packages()))
if (length(miss)) install.packages(miss)
# Stan backend for script 28 (needs Rtools on Windows):
#   install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
#   cmdstanr::check_cmdstan_toolchain(fix = TRUE); cmdstanr::install_cmdstan(cores = 4)
# Step 2 -- freeze the exact environment used for the paper:
#   install.packages("renv"); renv::init(bare = TRUE); renv::snapshot()
#   commit renv.lock; a replicator then runs renv::restore().
for (p in names(pkgs)) cat(sprintf("%-13s installed %-9s tested %s\n", p,
  tryCatch(as.character(packageVersion(p)), error = function(e) "MISSING"),
  ifelse(is.na(pkgs[[p]]), "-", pkgs[[p]])))
