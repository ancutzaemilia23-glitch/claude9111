# =============================================================================
# ALL_IN_ONE.R -- the whole pipeline in ONE file.
#
# HOW TO RUN (do NOT paste this into the console -- pasting long code is what
# dropped lines before):
#   RStudio: open this file, press Ctrl+Shift+S (Source).
#   or:      source("D:/Cluj/informality-prr/ALL_IN_ONE.R")
#
# Each script runs inside STEP(), in its own fresh environment: no object
# from one step can leak into another, a failure closes the log sinks and
# stops with the name of the failing step. Every step writes its own log in
# logs/ exactly as the separate scripts do.
#
# To re-run only some steps, edit RUN below, e.g. RUN <- c("23_scale_check").
# Steps read each other's OUTPUT FILES, not objects, so any step can be
# re-run alone once the steps it depends on have run once.
#
# Not included: scripts 01-06 (not provided yet; they create
# output/02_party_frame.csv and output/05_prr_linkage.csv) and 27 (gated by
# FREEZE_2019.txt; run it on its own).
# =============================================================================

RUN <- c("RUN_ANALYSIS",
         "12_figures",
         "13_secondary",
         "14_robustness",
         "18_itn_KH",
         "20_regional_build",
         "21_regional_model",
         "22_opposition",
         "23_scale_check",
         "23b_heterogeneity",
         "26_position_table",
         "28_bayes",
         "28b_diagnostics")
# optional discovery audits, off by default:
# RUN <- c(RUN, "15b_audit", "16_audit", "17_audit")

# ---- 0. packages (installs only what is missing) ----------------------------
if (nzchar(Sys.getenv("INFPRR_STEPS")))            # optional override, e.g. for testing
  RUN <- strsplit(Sys.getenv("INFPRR_STEPS"), ",")[[1]]
need <- c("haven","dplyr","tidyr","readr","purrr","tibble","stringr","readxl",
          "metafor","clubSandwich",
          if ("23_scale_check" %in% RUN) "logistf",
          if (any(c("28_bayes", "28b_diagnostics") %in% RUN)) c("brms", "posterior"))
miss <- setdiff(need, rownames(installed.packages()))
if (length(miss)) install.packages(miss)

STEP <- function(name, code) {
  if (!name %in% RUN) { cat("-- skip", name, "\n"); return(invisible(FALSE)) }
  cat("\n############ STEP", name, "############\n")
  t0 <- Sys.time()
  ok <- tryCatch({ eval(substitute(code), envir = new.env(parent = globalenv())); TRUE },
                 error = function(e) {
                   while (sink.number() > 0) sink()
                   cat("\n*** STEP", name, "FAILED:", conditionMessage(e), "***\n"); FALSE })
  while (sink.number() > 0) sink()
  cat(sprintf("############ %s %s (%.1f min)\n", name, if (ok) "done" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  if (!ok) stop("pipeline stopped at step ", name,
                " -- send me the lines above and the step's log in logs/", call. = FALSE)
  invisible(TRUE)
}


# =============================================================================
# STEP RUN_ANALYSIS  <-  R/RUN_ANALYSIS.R
# needs: ZA8868 .sav, World Bank xlsx, output/02 + 05 CSVs
# =============================================================================
STEP("RUN_ANALYSIS", {
  # =============================================================================
  # RUN_ANALYSIS.R  -- complete, self-contained analysis pipeline
  #
  # Informality and the political translation of institutional dissatisfaction
  # EES 2024 (ZA8868) + CHES 2024 + World Bank Informal Economy Database
  #
  # RUN THIS FILE TOP TO BOTTOM IN A CLEAN R SESSION. It calls no source(),
  # depends on no console state, and rebuilds the individual dataset from raw
  # if it is missing.
  #
  # FIXES RELATIVE TO THE PIECEMEAL SCRIPTS
  #   (1) The World Bank workbook is located from paths$raw_informal. Script 10
  #       scanned PROJ/data-raw instead and stopped, which is why `est` was
  #       never created and every later block failed.
  #   (2) Stage two now uses RANDOM-EFFECTS meta-regression, weights
  #       1/(se^2 + tau^2). Script 10 used 1/se^2, i.e. fixed-effect weighting,
  #       which assumes the moderator explains all between-country
  #       heterogeneity. With I2 ~ 92% that is wrong and understates the
  #       standard error on gamma1.
  #   (3) readr's default na = c("","NA") destroys Latvia's National Alliance,
  #       whose CHES abbreviation is literally "NA". Handled once, centrally.
  #
  # FIXES IN THE REPLICATION-PACKAGE VERSION (each marked # FIX: / # CHANGE:)
  #   (4) on.exit() at top level does not do what it looks like: under source()
  #       it fires immediately (the log file stayed EMPTY); under Rscript it is
  #       ignored. The sink is now closed explicitly at the end.
  #   (5) `classified` was TRUE for respondents whose q6 is NA. haven's
  #       zap_labels() turns SPSS user-missing codes into NA, and
  #       !NA %in% SPECIAL is TRUE, so such respondents were counted as
  #       classified NON-PRR voters. Now excluded; the count is printed.
  #       If the count is 0, no result changes.
  #   (6) Part 2 printed t = b/(100*se) after b and se had already been
  #       multiplied by 100: t was shown 100x too small (print only).
  #   (7) Part 8 printed `gap` after b_chal had been overwritten with its
  #       rounded pp value (the bug documented in script 12). Print only.
  #   (8) Robustness results (7a-7c and the secondary specifications) are now
  #       written to output/10_robustness.csv so that Table 3 (script 13) is
  #       computed, not transcribed by hand.
  #   (9) CSVs are written with na = "" so they round-trip with na = "" reads.
  #  (10) Paths can be overridden with environment variables (defaults are the
  #       original author paths).
  #
  # PREREQUISITES ON DISK
  #   D:/Cluj/ZA8868_v1-0-0.sav                          raw EES 2024
  #   D:/Cluj/1999-2024_CHES_dataset_meansV2.csv         CHES trend file
  #   C:/Users/Lenovo/Downloads/informal-economy-database (1).xlsx
  #   output/02_party_frame.csv, output/05_prr_linkage.csv  (from scripts 02/05)
  # =============================================================================


  # ============================== PART 0: SETUP ================================

  while (sink.number() > 0) sink()   # FIX: clear sinks left open by an earlier run

  # CHANGE: paths overridable via environment variables; defaults unchanged.
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")

  paths <- list(
    raw_ees      = file.path(RAW, "ZA8868_v1-0-0.sav"),
    raw_ches     = file.path(RAW, "1999-2024_CHES_dataset_meansV2.csv"),
    raw_informal = Sys.getenv("INFPRR_WB",
                              "C:/Users/Lenovo/Downloads/informal-economy-database (1).xlsx"),
    derived      = file.path(PROJ, "derived"),
    output       = file.path(PROJ, "output"),
    logs         = file.path(PROJ, "logs")
  )
  for (dd in paths[c("derived", "output", "logs")])
    if (!dir.exists(dd)) dir.create(dd, recursive = TRUE)

  pkgs <- c("haven", "dplyr", "tidyr", "stringr", "purrr", "tibble",
            "readr", "readxl")
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss))
    stop("install.packages(c(", paste(sprintf('"%s"', miss), collapse = ", "), "))")
  invisible(lapply(pkgs, library, character.only = TRUE))

  options(stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE,
          width = 120, max.print = 100000)
  set.seed(20260914)

  log_file <- file.path(paths$logs, "RUN_ANALYSIS_console.txt")
  con <- file(log_file, open = "wt"); sink(con, split = TRUE)
  # FIX: removed top-level on.exit(); the sink is closed explicitly in PART 9.

  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")

  # read_csv keeping the literal string "NA" (Latvia's National Alliance) while
  # still parsing numeric columns as numbers
  NUMCOLS <- c("q6_code","modal_slot","n_voters","vote_share_sample","iso_num",
               "prr","b","se","b_gd","se_gd","b_eu","se_eu","incumbent",
               "recent_gov","informality")
  read_keepNA <- function(f)
    readr::read_csv(f, show_col_types = FALSE, na = "") |>
    dplyr::mutate(dplyr::across(dplyr::any_of(NUMCOLS),
                                ~ suppressWarnings(as.numeric(.x))))
  # CHANGE: write missing as "" so files round-trip through read_keepNA()
  write_out <- function(x, f) readr::write_csv(x, file.path(paths$output, f), na = "")

  SPECIAL <- c(89, 90, 91, 96, 98)   # non-substantive q6 codes
  DK      <- 98

  # PRR in national government at the 2024 EP election.
  #   Croatia  : DP joined the HDZ cabinet on 17 May 2024, three weeks before
  #              the vote (verified against press and cabinet records).
  #   Netherlands: coalition agreement signed 16 May 2024 but the Schoof cabinet
  #              was sworn in on 2 July, AFTER the election. Treated as
  #              NON-incumbent in the baseline; flipped in robustness.
  #   Latvia   : National Alliance left government in 2023; kept separate.
  PRR_INCUMBENT  <- c("Hungary", "Italy", "Slovakia", "Finland", "Croatia")
  PRR_RECENT_GOV <- c("Latvia")
  # NOTE: `cee` = the 11 post-communist member states. Cyprus and Malta (2004
  # entrants) are NOT in it, so the dummy is "post-communist", not "post-2004".
  CEE <- c("Bulgaria","Croatia","Czech Republic","Estonia","Hungary","Latvia",
           "Lithuania","Poland","Romania","Slovakia","Slovenia")

  cat("Setup complete.\n")
  for (f in c("raw_ees","raw_ches","raw_informal"))
    cat(sprintf("  %-13s %s\n", f,
                if (file.exists(paths[[f]])) "[found]" else "[NOT FOUND]"))


  # ================ PART 1: INDIVIDUAL DATA (rebuild if missing) ===============

  hdr("PART 1. INDIVIDUAL-LEVEL DATA")

  rds <- file.path(paths$derived, "analysis_individual.rds")
  frame <- read_keepNA(file.path(paths$output, "02_party_frame.csv"))
  link  <- read_keepNA(file.path(paths$output, "05_prr_linkage.csv"))
  prr_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
  cat("PRR ballot codes:", length(prr_codes), "\n")

  if (file.exists(rds)) {
    d <- readRDS(rds)
    cat("loaded existing", rds, "-", nrow(d), "rows\n")
  } else {
    cat("rebuilding from raw EES (this takes a minute)...\n")
    ees <- haven::read_sav(paths$raw_ees, user_na = TRUE)
    # NOTE: zap_labels() converts SPSS user-missing codes to NA (haven >= 2.2).
    num <- function(v) as.numeric(haven::zap_labels(ees[[v]]))
    miss_dk <- function(x, dk = DK) ifelse(x %in% dk, NA_real_, x)

    # FIX: report declared user-missing codes so the NA handling is visible
    for (v in c("q6", "d4_age"))
      cat(sprintf("declared SPSS user-missing values in %s: %s\n", v,
                  paste(c(attr(ees[[v]], "na_values"), attr(ees[[v]], "na_range")),
                        collapse = ", ")))

    d <- tibble::tibble(
      resp_id = ees$resp_id,
      cty     = as.character(haven::as_factor(ees$country)),
      w1 = as.numeric(ees$Weight1), w2 = as.numeric(ees$Weight2),
      q5_raw = num("q5"), q6_raw = num("q6"), q6n_raw = num("q6n")) |>
      dplyr::mutate(
        # FIX: an NA q6 is not a classified vote (was TRUE -> coded prr = 0)
        classified  = !is.na(q6_raw) & !q6_raw %in% SPECIAL,
        prr         = ifelse(classified, as.integer(q6_raw %in% prr_codes), NA_integer_),
        abstain     = dplyr::case_when(q5_raw == 2 ~ 1L, q5_raw == 1 ~ 0L,
                                       TRUE ~ NA_integer_),
        dissat_nat  = miss_dk(num("q2")),
        dissat_eu   = miss_dk(num("q3")),
        govt_disapp = dplyr::case_when(num("q4") == 2 ~ 1L, num("q4") == 1 ~ 0L,
                                       TRUE ~ NA_integer_),
        # CHANGE: was as.numeric(ees$d4_age), which keeps user-missing codes as
        # numbers (e.g. a refusal code would enter the regression as an age).
        # num() maps them to NA; values are identical if none are declared.
        age    = num("d4_age"),
        female = dplyr::case_when(num("d3") == 2 ~ 1L, num("d3") == 1 ~ 0L,
                                  TRUE ~ NA_integer_),
        educ   = miss_dk(num("Edu_rec")),
        urban  = miss_dk(num("d8")),
        lr     = miss_dk(num("q10")))

    q9 <- as.data.frame(lapply(paste0("q9_", 1:8), \(v) miss_dk(num(v))))
    names(q9) <- paste0("q9_", 1:8)
    slot_map <- frame |> dplyr::filter(q6_code %in% prr_codes, !is.na(modal_slot)) |>
      dplyr::group_by(cty) |> dplyr::summarise(slots = list(unique(modal_slot)),
                                               .groups = "drop")
    d$prr_ptv <- NA_real_
    for (i in seq_len(nrow(slot_map))) {
      rows <- which(d$cty == slot_map$cty[i])
      vals <- as.matrix(q9[rows, paste0("q9_", slot_map$slots[[i]]), drop = FALSE])
      d$prr_ptv[rows] <- suppressWarnings(apply(vals, 1, max, na.rm = TRUE))
    }
    d$prr_ptv[is.infinite(d$prr_ptv)] <- NA_real_
    saveRDS(d, rds); cat("written", rds, "\n")
  }

  # FIX: apply the q6-NA correction to an rds built by an earlier version too
  n_q6na <- sum(d$classified & is.na(d$q6_raw))
  cat(sprintf("respondents with q6 = NA previously coded classified: %d\n", n_q6na))
  if (n_q6na > 0) {
    cat("  -> CHANGE: recoded classified = FALSE, prr = NA, and rds re-saved.\n",
        "    Every downstream result changes; report this as a correction.\n")
    print(table(d$cty[d$classified & is.na(d$q6_raw)]))
    d <- d |> dplyr::mutate(classified = classified & !is.na(q6_raw),
                            prr = ifelse(classified, prr, NA_integer_))
    saveRDS(d, rds)
  }
  cat("age range:", paste(range(d$age, na.rm = TRUE), collapse = " to "),
      "(check: no missing-value codes such as 98/99/999)\n")

  # binary dissatisfaction contrast (selected by the functional-form test)
  d <- d |> dplyr::mutate(
    dissatisfied  = dplyr::case_when(dissat_nat %in% c(3,4) ~ 1L,
                                     dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
    dissat_eu_bin = dplyr::case_when(dissat_eu  %in% c(3,4) ~ 1L,
                                     dissat_eu  %in% c(1,2) ~ 0L, TRUE ~ NA_integer_))

  prr_ctys <- d |> dplyr::filter(classified) |> dplyr::group_by(cty) |>
    dplyr::summarise(any_prr = sum(prr) > 0, .groups = "drop") |>
    dplyr::filter(any_prr) |> dplyr::pull(cty)
  dv <- d |> dplyr::filter(classified, cty %in% prr_ctys)

  cat(sprintf("classified voters: %d | PRR voters: %d (%.1f%%) | countries with a PRR option: %d\n",
              sum(d$classified), sum(d$prr, na.rm = TRUE),
              100*mean(d$prr[d$classified]), length(prr_ctys)))


  # ========================= PART 2: STAGE ONE =================================

  hdr("PART 2. STAGE ONE - COUNTRY SLOPES")
  cat("prr ~ dissatisfied + age + female + educ + urban, OLS within country.\n")
  cat("Coefficient = pp difference in PRR voting, dissatisfied vs satisfied.\n\n")

  fit_one <- function(k, yvar = "prr", extra = NULL) {
    vars <- c("dissatisfied","age","female","educ","urban", extra)
    dd <- dv |> dplyr::filter(cty == k) |>
      dplyr::select(y = dplyr::all_of(yvar), dplyr::all_of(vars)) |> stats::na.omit()
    if (nrow(dd) < 100 || stats::var(dd$y) == 0)
      return(tibble::tibble(cty = k, n = nrow(dd), b = NA_real_, se = NA_real_))
    f <- stats::as.formula(paste("y ~", paste(vars, collapse = " + ")))
    ct <- summary(stats::lm(f, data = dd))$coefficients
    tibble::tibble(cty = k, n = nrow(dd),
                   b  = unname(ct["dissatisfied",1]),
                   se = unname(ct["dissatisfied",2]))
  }

  slopes <- purrr::map_dfr(prr_ctys, fit_one)
  gd     <- purrr::map_dfr(prr_ctys, \(k) fit_one(k, "prr", "govt_disapp")) |>
    dplyr::select(cty, b_gd = b, se_gd = se)
  eu     <- purrr::map_dfr(prr_ctys, function(k) {
    dd <- dv |> dplyr::filter(cty == k) |>
      dplyr::select(prr, dissat_eu_bin, age, female, educ, urban) |> stats::na.omit()
    if (nrow(dd) < 100 || stats::var(dd$prr) == 0)
      return(tibble::tibble(cty = k, b_eu = NA_real_, se_eu = NA_real_))
    ct <- summary(stats::lm(prr ~ dissat_eu_bin + age + female + educ + urban,
                            data = dd))$coefficients
    tibble::tibble(cty = k, b_eu = unname(ct["dissat_eu_bin",1]),
                   se_eu = unname(ct["dissat_eu_bin",2]))
  })

  slopes <- slopes |>
    dplyr::left_join(gd, by = "cty") |> dplyr::left_join(eu, by = "cty") |>
    dplyr::mutate(incumbent  = as.integer(cty %in% PRR_INCUMBENT),
                  recent_gov = as.integer(cty %in% PRR_RECENT_GOV),
                  cee        = as.integer(cty %in% CEE)) |>
    dplyr::arrange(b)

  print(as.data.frame(slopes |> dplyr::mutate(
    t = round(b / se, 2),                        # FIX: computed before rescaling
    dplyr::across(c(b, se, b_gd, b_eu), ~ round(100*.x, 2))) |>
      dplyr::select(cty, n, b, se, t, b_gd, b_eu, incumbent, cee)),
    row.names = FALSE)

  write_out(slopes, "09_country_slopes_binary.csv")


  # ===================== PART 3: HETEROGENEITY =================================

  hdr("PART 3. IS THERE HETEROGENEITY FOR A MODERATOR TO EXPLAIN?")

  het <- function(b, se, label) {
    ok <- !is.na(b) & !is.na(se); b <- b[ok]; se <- se[ok]
    w <- 1/se^2; mu <- sum(w*b)/sum(w)
    Q <- sum(w*(b-mu)^2); df <- length(b)-1
    tau2 <- max(0, (Q-df)/(sum(w) - sum(w^2)/sum(w)))
    cat(sprintf("%-28s k=%2d  mean %6.2f pp  Q=%7.1f (p=%.4f)  tau=%5.2f pp  I2=%4.1f%%\n",
                label, length(b), 100*mu, Q, stats::pchisq(Q, df, lower.tail = FALSE),
                100*sqrt(tau2), max(0, 100*(Q-df)/Q)))
  }
  het(slopes$b, slopes$se, "all countries")
  s19 <- slopes |> dplyr::filter(incumbent == 0)
  het(s19$b, s19$se, "excluding PRR incumbents")
  s18 <- slopes |> dplyr::filter(incumbent == 0, recent_gov == 0)
  het(s18$b, s18$se, "also excluding Latvia")


  # ======================= PART 4: THE MODERATOR ===============================

  hdr("PART 4. WORLD BANK INFORMALITY (DGE)")

  WB <- paths$raw_informal
  if (!file.exists(WB)) {
    alt <- list.files(c(file.path(PROJ, "data-raw"), "C:/Users/Lenovo/Downloads"),
                      pattern = "informal.*\\.xlsx?$", full.names = TRUE,
                      ignore.case = TRUE)
    if (!length(alt)) stop("World Bank workbook not found. Set paths$raw_informal.")
    WB <- alt[1]
  }
  cat("using:", WB, "\n")

  sheets <- readxl::excel_sheets(WB)
  cat("sheets:", paste(sheets, collapse = " | "), "\n")
  dge_sheet <- sheets[grepl("^DGE", sheets, ignore.case = TRUE)][1]
  if (is.na(dge_sheet)) stop("No DGE sheet. Sheets present: ",
                             paste(sheets, collapse = ", "))
  cat("DGE sheet:", dge_sheet,
      "  (informal output, % of official GDP; Elgin, Kose, Ohnsorge & Yu 2021)\n")

  raw <- suppressMessages(readxl::read_excel(WB, sheet = dge_sheet))
  name_col  <- names(raw)[grepl("economy|country", names(raw), ignore.case = TRUE)][1]
  year_cols <- names(raw)[grepl("^(19|20)[0-9]{2}$", names(raw))]
  keep_years <- intersect(as.character(2010:2020), year_cols)
  cat(sprintf("name column: %s | year columns: %d | years used: %s-%s\n",
              name_col, length(year_cols), min(keep_years), max(keep_years)))

  RENAME <- c("Slovak Republic" = "Slovakia", "Czechia" = "Czech Republic")

  mod <- raw |>
    dplyr::select(dplyr::all_of(c(name_col, keep_years))) |>
    dplyr::rename(wb_name = dplyr::all_of(name_col)) |>
    tidyr::pivot_longer(-wb_name, names_to = "year", values_to = "informal") |>
    dplyr::mutate(informal = suppressWarnings(as.numeric(informal)),
                  cty = dplyr::recode(wb_name, !!!RENAME)) |>
    dplyr::group_by(cty) |>
    dplyr::summarise(informality = mean(informal, na.rm = TRUE),
                     n_years     = sum(!is.na(informal)),
                     sd_within   = stats::sd(informal, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(!is.nan(informality))


  # ======================= PART 5: MERGE AUDIT =================================

  hdr("PART 5. MERGE AUDIT")
  cat("stage-one countries      :", nrow(slopes), "\n")
  cat("unmatched on slope side  :",
      paste(setdiff(slopes$cty, mod$cty), collapse = ", "), "\n")
  cat("  (empty line above = every country matched)\n")
  cat("duplicate country rows   :", sum(duplicated(mod$cty)), "\n")
  cat("name transformations     :",
      paste(names(RENAME), "->", RENAME, collapse = "; "), "\n")  # CHANGE: was hard-coded text

  dat <- slopes |> dplyr::inner_join(mod, by = "cty")
  cat(sprintf("merged                   : %d countries\n\n", nrow(dat)))
  print(as.data.frame(dat |> dplyr::select(cty, informality, n_years, sd_within,
                                           b, se, incumbent, cee) |>
                        dplyr::mutate(dplyr::across(c(informality, sd_within), ~round(.x,2)),
                                      dplyr::across(c(b, se), ~round(100*.x,2))) |>
                        dplyr::arrange(dplyr::desc(informality))), row.names = FALSE)
  write_out(dat, "10_moderator.csv")

  cat(sprintf("\nR2 of informality on the post-communist dummy: %.3f\n",
              summary(stats::lm(informality ~ cee, dat))$r.squared))
  print(as.data.frame(dat |> dplyr::group_by(cee) |>
                        dplyr::summarise(n = dplyr::n(), min = round(min(informality),1),
                                         median = round(stats::median(informality),1),
                                         max = round(max(informality),1), .groups = "drop")),
        row.names = FALSE)


  # ==================== PART 6: STAGE TWO (THE TEST) ===========================

  hdr("PART 6. STAGE TWO - RANDOM-EFFECTS META-REGRESSION")
  cat("b_j = g0 + g1*Informality_j + u_j, weights 1/(se_j^2 + tau^2).\n")
  cat("tau^2 is residual between-country heterogeneity (DerSimonian-Laird).\n")
  cat("NOT 1/se^2: that would assume informality explains all heterogeneity.\n")

  metareg <- function(y, se, X, label, show = TRUE) {
    ok <- stats::complete.cases(cbind(y, se, X))
    y <- y[ok]; v <- se[ok]^2; X <- cbind(1, as.matrix(X)[ok, , drop = FALSE])
    k <- length(y); p <- ncol(X)
    W <- diag(1/v); XtWX <- t(X) %*% W %*% X
    b <- solve(XtWX, t(X) %*% W %*% y); r <- y - X %*% b
    Qe <- as.numeric(t(r) %*% W %*% r)
    P  <- W - W %*% X %*% solve(XtWX, t(X) %*% W)
    tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
    W2 <- diag(1/(v + tau2)); XtW2X <- t(X) %*% W2 %*% X
    b2 <- solve(XtW2X, t(X) %*% W2 %*% y); V <- solve(XtW2X)
    sb <- sqrt(diag(V)); tv <- b2/sb
    pv <- 2*stats::pt(-abs(tv), k - p)
    ci <- cbind(b2 - stats::qt(.975, k-p)*sb, b2 + stats::qt(.975, k-p)*sb)
    if (show) {
      cat(sprintf("\n--- %s ---  k = %d\n", label, k))
      cat(sprintf("  residual Q = %.1f on %d df (p = %.4f) | residual tau = %.2f pp\n",
                  Qe, k-p, stats::pchisq(Qe, k-p, lower.tail = FALSE), 100*sqrt(tau2)))
      nm <- c("gamma0 (intercept)", colnames(X)[-1])
      for (j in seq_len(p))
        cat(sprintf("  %-22s %8.4f  se %6.4f  t %6.2f  p %.4f  95%% CI [%7.4f, %7.4f]\n",
                    nm[j], b2[j], sb[j], tv[j], pv[j], ci[j,1], ci[j,2]))
    }
    invisible(list(b = b2, se = sb, p = pv, tau = sqrt(tau2), k = k))
  }

  # CHANGE: collect robustness results for Table 3 (script 13) instead of
  # transcribing them from this console by hand.
  rob <- list()
  add_rob <- function(test, m, j = 2, note = "")
    rob[[test]] <<- tibble::tibble(test = test, g1 = as.numeric(m$b[j]),
                                   se = as.numeric(m$se[j]), p = as.numeric(m$p[j]),
                                   k = m$k, note = note)

  est <- dat |> dplyr::filter(incumbent == 0)          # PRIMARY SAMPLE, k = 19
  cat(sprintf("\nPRIMARY SAMPLE: %d non-incumbent countries\n", nrow(est)))
  cat(paste(sort(est$cty), collapse = ", "), "\n")

  m_main <- metareg(est$b, est$se, est[, "informality"], "PRIMARY: PRR vote")
  add_rob("Primary (DL + t)", m_main)
  add_rob("All countries incl. incumbents",
          metareg(dat$b, dat$se, dat[, "informality"], "all countries incl. incumbents"))
  add_rob("Net of govt disapproval",
          metareg(est$b_gd, est$se_gd, est[, "informality"], "net of government disapproval"))
  add_rob("EU dissatisfaction (H2 placebo)",
          metareg(est$b_eu, est$se_eu, est[, "informality"], "EU dissatisfaction (H2 placebo)"))

  cat(sprintf("\nsimple correlation slope~informality: %.3f\n",
              stats::cor(est$b, est$informality)))
  cat(sprintf("detectable at 80%% power with k=%d: r ~ %.2f\n",
              nrow(est), tanh(2.80/sqrt(nrow(est)-3))))
  cat("gamma1 is in percentage points of the dissatisfaction-PRR gap per one\n")
  cat("point of informal output as a share of GDP.\n")


  # ======================= PART 7: ROBUSTNESS ==================================

  hdr("PART 7a. LEAVE ONE COUNTRY OUT")
  loo <- purrr::map_dfr(est$cty, function(k) {
    dd <- est |> dplyr::filter(cty != k)
    m <- metareg(dd$b, dd$se, dd[, "informality"], "", show = FALSE)
    tibble::tibble(dropped = k, g1 = m$b[2], se = m$se[2], p = m$p[2])
  }) |> dplyr::arrange(g1)
  print(as.data.frame(loo |> dplyr::mutate(dplyr::across(c(g1, se), ~round(.x,4)),
                                           p = round(p,4),
                                           flag = ifelse(p >= .05, "loses p<.05", ""))),
        row.names = FALSE)
  cat(sprintf("\nfull g1 = %.4f | LOO range %.4f to %.4f | sign stable: %s\n",
              m_main$b[2], min(loo$g1), max(loo$g1),
              all(sign(loo$g1) == sign(m_main$b[2]))))
  write_out(loo, "10_loo.csv")                          # CHANGE: new output
  worst <- loo$dropped[which.max(loo$p)]
  rob[["Leave-one-country-out"]] <- tibble::tibble(
    test = "Leave-one-country-out", g1 = NA_real_, se = NA_real_, p = max(loo$p),
    k = nrow(est) - 1L,
    note = sprintf("sign %s in all %d; range %.4f to %.4f; p<.05 in %d of %d (max p: %s, %.4f)",
                   ifelse(m_main$b[2] < 0, "negative", "positive"), nrow(loo),
                   min(loo$g1), max(loo$g1), sum(loo$p < .05), nrow(loo), worst, max(loo$p)))

  hdr("PART 7b. REGION - THE PRESPECIFIED H4 TEST")
  m_reg <- metareg(est$b, est$se, est[, c("informality","cee")], "informality + region")
  add_rob("Region control (H4)", m_reg,
          note = sprintf("informality-region correlation %.3f",
                         stats::cor(est$informality, est$cee)))
  cat("\nleave-one-region-out:\n")
  for (g in c(0,1)) {
    dd <- est |> dplyr::filter(cee == g)
    lab <- if (g == 0) "older member states only" else "post-communist only"
    if (nrow(dd) >= 6) {
      add_rob(lab, metareg(dd$b, dd$se, dd[, "informality"], lab),
              note = sprintf("informality spans %.1f-%.1f",
                             min(dd$informality), max(dd$informality)))
    } else cat(sprintf("  %s: k = %d, too few\n", lab, nrow(dd)))
  }
  cat(sprintf("\ncorrelation informality with post-communist dummy: %.3f\n",
              stats::cor(est$informality, est$cee)))
  cat("If gamma1 loses significance with the region dummy, H4 FAILS and the\n")
  cat("result must be reported as inseparable from region.\n")

  hdr("PART 7c. PERMUTATION TEST")
  cat("Moderator reassigned at random across countries, 9999 draws.\n")
  obs <- abs(m_main$b[2])
  perm <- replicate(9999, {
    xx <- sample(est$informality)
    abs(metareg(est$b, est$se, data.frame(informality = xx), "", show = FALSE)$b[2])
  })
  p_perm <- (sum(perm >= obs)+1)/(length(perm)+1)
  cat(sprintf("observed |g1| = %.4f | permutation p = %.4f\n", obs, p_perm))
  cat("This is an exact test, not equivalent to the regression p-value.\n")
  rob[["Permutation test"]] <- tibble::tibble(
    test = "Permutation test", g1 = m_main$b[2], se = NA_real_, p = p_perm,
    k = m_main$k, note = "9999 reassignments of the moderator")

  write_out(est, "10_stage_two.csv")
  write_out(dplyr::bind_rows(rob), "10_robustness.csv")    # CHANGE: new output


  # ================== PART 8: CHALLENGER-VOTE COMPARISON =======================

  hdr("PART 8. CHALLENGER OUTCOME (attenuation vs party supply)")
  cat("challenger = PRR set + CHES 2024 family 6 (radical left) and family 9\n")
  cat("(populist/unaffiliated). COVERAGE CAVEAT: CHES codes few family 6/9\n")
  cat("parties in CEE, so the extra parties sit mostly in LOW-informality\n")
  cat("countries. That biases this test toward a flat challenger slope.\n")

  EXTRA <- tibble::tribble(
    ~q6_code, ~cty,           ~ches_party, ~family,
    56104L,"Belgium","PVDA-PTB",6L,      56204L,"Belgium","PVDA-PTB",6L,
    10001L,"Bulgaria","ITN",1L,          19601L,"Cyprus","AKEL",6L,
    20304L,"Czech Republic","ANO2011",9L,20302L,"Czech Republic","Prisaha",9L,
    20308L,"Czech Republic","KSCM",6L,   20810L,"Denmark","EL",6L,
    24605L,"Finland","VAS",6L,           25002L,"France","FI",6L,
    25011L,"France","PCF",6L,            27605L,"Germany","LINKE",6L,
    27616L,"Germany","BSW",6L,           27608L,"Germany","FW",9L,
    30004L,"Greece","SYRIZA",6L,         30006L,"Greece","KKE",6L,
    34807L,"Hungary","MKKP",9L,          37206L,"Ireland","S-PBP",6L,
    38008L,"Italy","MS5",9L,             42804L,"Latvia","LPV",9L,
    52815L,"Netherlands","SP",6L,        52813L,"Netherlands","BBB",9L,
    62002L,"Portugal","BE",6L,           62014L,"Portugal","CDU",6L,
    72407L,"Spain","Sumar",6L,           72401L,"Spain","Podemos",6L,
    75207L,"Sweden","V",6L
  )

  chk <- EXTRA |> dplyr::left_join(
    frame |> dplyr::select(q6_code, cty_data = cty, party_label, vote_share_sample),
    by = "q6_code")
  cat("\ncodes absent from the EES ballot frame:\n")
  bad <- chk |> dplyr::filter(is.na(party_label))
  if (!nrow(bad)) cat("  none\n") else
    print(as.data.frame(bad |> dplyr::select(cty, q6_code, ches_party)), row.names = FALSE)
  cat("country mismatches:", sum(!is.na(chk$cty_data) & chk$cty != chk$cty_data), "\n")
  cat("overlap with the PRR set (should be 0):",
      length(intersect(EXTRA$q6_code, prr_codes)), "\n\n")
  print(as.data.frame(chk |> dplyr::select(cty, q6_code, party_label, ches_party,
                                           family, share = vote_share_sample) |>
                        dplyr::arrange(cty)), row.names = FALSE)

  chal_codes <- unique(c(prr_codes, EXTRA$q6_code))
  dv <- dv |> dplyr::mutate(challenger = as.integer(q6_raw %in% chal_codes))
  cat(sprintf("\npooled challenger share: %.1f%% (PRR was %.1f%%)\n",
              100*mean(dv$challenger), 100*mean(dv$prr)))

  ch <- purrr::map_dfr(prr_ctys, \(k) fit_one(k, "challenger")) |>
    dplyr::select(cty, b_chal = b, se_chal = se)
  n_extra <- EXTRA |> dplyr::count(cty, name = "n_extra")

  cmp <- dat |> dplyr::left_join(ch, by = "cty") |>
    dplyr::left_join(n_extra, by = "cty") |>
    dplyr::mutate(n_extra = tidyr::replace_na(n_extra, 0L))

  cat("\ncountry slopes, PRR vs challenger (pp):\n")
  print(as.data.frame(cmp |> dplyr::filter(incumbent == 0) |>
                        dplyr::mutate(gap = round(100*(b_chal-b),2),   # FIX: before b_chal is rounded
                                      b_prr = round(100*b,2), b_chal = round(100*b_chal,2)) |>
                        dplyr::select(cty, informality, n_extra, b_prr, b_chal, gap) |>
                        dplyr::arrange(informality)), row.names = FALSE)

  cat("\nCountries gaining NO extra party have challenger slope identical to\n")
  cat("the PRR slope by construction; the test has no bite there.\n")

  e <- cmp |> dplyr::filter(incumbent == 0)
  metareg(e$b,      e$se,      e[, "informality"], "PRR outcome")
  metareg(e$b_chal, e$se_chal, e[, "informality"], "CHALLENGER outcome")
  e2 <- e |> dplyr::filter(n_extra > 0)
  if (nrow(e2) >= 6) {
    cat("\nrestricted to countries with a genuinely broader challenger set:\n")
    metareg(e2$b,      e2$se,      e2[, "informality"], "  PRR, extended countries")
    metareg(e2$b_chal, e2$se_chal, e2[, "informality"], "  CHALLENGER, same countries")
  }

  cat("\nREADING: challenger gamma1 near zero -> party supply (weak evidence,\n")
  cat("given the coverage bias). Challenger gamma1 also negative -> party\n")
  cat("supply does not explain it (strong, because the bias runs against it).\n")

  write_out(cmp, "11_challenger_slopes.csv")


  # ============================ PART 9: DONE ===================================

  hdr("DONE")
  cat("Written:\n")
  for (f in c("09_country_slopes_binary.csv","10_moderator.csv","10_loo.csv",
              "10_robustness.csv","10_stage_two.csv","11_challenger_slopes.csv"))
    cat("  ", file.path(paths$output, f), "\n")
  cat("  ", log_file, "\n")
  cat("\nReport in this order: PART 6 primary estimate, PART 7a leave-one-out,\n")
  cat("PART 7b region test, PART 7c permutation. A result failing 7a or 7c is\n")
  cat("not a finding; one failing 7b is a finding that cannot be separated\n")
  cat("from region and must be reported that way.\n")
  sink(); close(con)   # FIX: explicit close (replaces the top-level on.exit)
})


# =============================================================================
# STEP 12_figures  <-  R/12_figures.R
# needs: step RUN_ANALYSIS
# =============================================================================
STEP("12_figures", {
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
})


# =============================================================================
# STEP 13_secondary  <-  R/13_secondary_outcomes.R
# needs: step RUN_ANALYSIS
# =============================================================================
STEP("13_secondary", {
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
})


# =============================================================================
# STEP 14_robustness  <-  R/14_robustness.R
# needs: step RUN_ANALYSIS
# =============================================================================
STEP("14_robustness", {
  # =============================================================================
  # 14_robustness.R -- the specifications Section 3 promises
  #
  # Section 3 makes four forward references that are not yet estimated:
  #   3.2.2 -> 4.4  sensitivity to the eleven radical-right parties absent from
  #                 CHES 2024 and consequently coded zero
  #   3.2.4 -> 4.4  specifications adding left-right self-placement, immigration
  #                 attitudes and economic perceptions to the first stage
  #   3.3   -> supp pooled cross-level interaction, as a specification check
  #   (new)         alternative informality series from the same database
  #
  # Run AFTER RUN_ANALYSIS.R.
  # Output: output/TABLE4_robustness.csv, 14_robustness_slopes.csv
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - paths defined unconditionally. With the old `exists("paths")` test, a
  #     session that had run script 13 first had no paths$raw_informal and
  #     section 4 failed in read_excel(NULL).
  #   - Section 2: `imm_restr` and `econ_retro` are NOT created by RUN_ANALYSIS
  #     (its rds has only lr among the three). The script now stops with a
  #     clear message instead of a dplyr "column doesn't exist" error. I have
  #     not invented the EES item numbers: tell me which q-variables they are.
  #   - Section 4: "Czechia" -> "Czech Republic" added to the rename (RUN_ANALYSIS
  #     had it, this script did not, so the Czech Republic could silently drop
  #     from the MIMIC and SEMP series). Sheet names are checked before use.
  #   - baseline first-stage n computed once, not re-estimated in every loop.
  # =============================================================================

  PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")   # FIX: unconditional
  paths <- list(derived = file.path(PROJ, "derived"),
                output  = file.path(PROJ, "output"),
                raw_informal = Sys.getenv("INFPRR_WB",
                  "C:/Users/Lenovo/Downloads/informal-economy-database (1).xlsx"))
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(purrr); library(tibble)
    library(tidyr); library(readxl)})
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

  d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
    dplyr::mutate(dissatisfied = dplyr::case_when(
      dissat_nat %in% c(3,4) ~ 1L, dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_))
  mod <- readr::read_csv(file.path(paths$output, "10_moderator.csv"),
                         show_col_types = FALSE, na = "") |>
    dplyr::mutate(dplyr::across(c(informality, incumbent, cee, b, se),
                                ~ suppressWarnings(as.numeric(.x)))) |>
    dplyr::select(cty, informality, incumbent, cee, b_base = b, se_base = se)

  metareg <- function(y, se, X, label, show = TRUE) {
    ok <- stats::complete.cases(cbind(y, se, X))
    y <- y[ok]; v <- se[ok]^2; X <- cbind(1, as.matrix(X)[ok, , drop = FALSE])
    k <- length(y); p <- ncol(X)
    W <- diag(1/v); A <- t(X) %*% W %*% X
    b <- solve(A, t(X) %*% W %*% y); r <- y - X %*% b
    Qe <- as.numeric(t(r) %*% W %*% r)
    P <- W - W %*% X %*% solve(A, t(X) %*% W)
    tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
    W2 <- diag(1/(v + tau2)); A2 <- t(X) %*% W2 %*% X
    b2 <- solve(A2, t(X) %*% W2 %*% y); V <- solve(A2); sb <- sqrt(diag(V))
    tv <- b2[2]/sb[2]; pv <- 2*stats::pt(-abs(tv), k-p)
    if (show) cat(sprintf("%-38s k=%2d  g1 = %8.4f  se %6.4f  t %6.2f  p %.4f  tau %5.2f\n",
                          label, k, b2[2], sb[2], tv, pv, 100*sqrt(tau2)))
    invisible(tibble::tibble(spec = label, g1 = b2[2], se = sb[2],
                             p = pv, k = k, tau = 100*sqrt(tau2)))
  }

  prr_ctys <- d |> dplyr::filter(classified) |> dplyr::group_by(cty) |>
    dplyr::summarise(a = sum(prr) > 0, .groups="drop") |>
    dplyr::filter(a) |> dplyr::pull(cty)
  dv <- d |> dplyr::filter(classified, cty %in% prr_ctys)

  stage1 <- function(dat, yvar, extra = NULL) {
    vars <- c("dissatisfied","age","female","educ","urban", extra)
    purrr::map_dfr(sort(unique(dat$cty)), function(k) {
      dd <- dat |> dplyr::filter(cty == k) |>
        dplyr::select(y = dplyr::all_of(yvar), dplyr::all_of(vars)) |> stats::na.omit()
      if (nrow(dd) < 100 || stats::var(dd$y) == 0)
        return(tibble::tibble(cty = k, n = nrow(dd), b = NA_real_, se = NA_real_))
      ct <- summary(stats::lm(stats::as.formula(
        paste("y ~", paste(vars, collapse=" + "))), data = dd))$coefficients
      tibble::tibble(cty = k, n = nrow(dd), b = unname(ct["dissatisfied",1]),
                     se = unname(ct["dissatisfied",2]))
    })
  }

  R <- list()

  ## ---- 1. Alternative radical-right classification (Section 4.4) -------------
  hdr("1. SENSITIVITY TO THE ELEVEN PARTIES ABSENT FROM CHES 2024")
  cat("Section 3.2.2 codes these zero because CHES 2024 does not classify them.\n")
  cat("Here they are coded as radical right and the analysis is re-run.\n\n")

  EXTRA_PRR <- c(70306L,  # Kotlebovci-LSNS, Slovakia
                 27614L,  # Die Heimat, Germany
                 25009L,  # Les Patriotes, France
                 52809L, 52818L,  # JA21, BVNL, Netherlands
                 75209L,  # Alternativ for Sverige
                 62008L, 62012L,  # ADN, Ergue-te, Portugal
                 10008L,  # VMRO, Bulgaria
                 44209L,  # ADR, Luxembourg
                 47003L)  # Imperium Europa, Malta

  link <- readr::read_csv(file.path(paths$output, "05_prr_linkage.csv"),
                          show_col_types = FALSE, na = "") |>
    dplyr::mutate(q6_code = suppressWarnings(as.numeric(q6_code)),
                  prr = suppressWarnings(as.numeric(prr)))
  base_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
  alt_codes  <- unique(c(base_codes, EXTRA_PRR))

  dv <- dv |> dplyr::mutate(prr_alt = as.integer(q6_raw %in% alt_codes))
  cat(sprintf("radical-right share: baseline %.1f%%, extended %.1f%%\n",
              100*mean(dv$prr), 100*mean(dv$prr_alt)))
  cat("Luxembourg and Malta remain excluded: they gain a radical-right option\n")
  cat("under this coding but were not in the estimation sample.\n\n")

  s_alt <- stage1(dv, "prr_alt") |> dplyr::rename(b_alt = b, se_alt = se)
  e_alt <- mod |> dplyr::left_join(s_alt, by="cty") |> dplyr::filter(incumbent == 0)
  R$base <- metareg(e_alt$b_base, e_alt$se_base, e_alt[,"informality"], "baseline classification")
  R$alt  <- metareg(e_alt$b_alt,  e_alt$se_alt,  e_alt[,"informality"], "extended classification (+11 parties)")

  ## ---- 2. Extended control sets (Section 4.4) --------------------------------
  hdr("2. ADDING THE EXCLUDED INDIVIDUAL CONTROLS")
  cat("Section 3.2.4 excludes these from the baseline and promises them here.\n")
  cat("The point is to observe what they absorb, and at what cost in sample.\n\n")

  specs <- list("+ left-right"          = "lr",
                "+ immigration"         = "imm_restr",
                "+ economic perception"  = "econ_retro",
                "+ all three"            = c("lr","imm_restr","econ_retro"))
  # FIX: fail with an actionable message if a control is not in the rds
  absent <- setdiff(unique(unlist(specs)), names(dv))
  if (length(absent)) {
    cat("NOT IN analysis_individual.rds:", paste(absent, collapse = ", "), "\n",
        "RUN_ANALYSIS creates only `lr` (q10). Add the immigration and economic-\n",
        "perception items to the rds build (names from the ZA8868 codebook), then\n",
        "re-run. The specifications needing them are SKIPPED in this run.\n")
    specs <- specs[vapply(specs, \(v) all(v %in% names(dv)), logical(1))]
  }
  n_base <- sum(stage1(dv, "prr")$n, na.rm = TRUE)       # CHANGE: computed once
  for (nm in names(specs)) {
    s <- stage1(dv, "prr", specs[[nm]])
    e <- mod |> dplyr::left_join(s, by="cty") |> dplyr::filter(incumbent == 0)
    cat(sprintf("   [retains %.1f%% of the baseline first-stage sample]\n",
                100*sum(s$n, na.rm=TRUE)/n_base))
    R[[nm]] <- metareg(e$b, e$se, e[,"informality"], nm)
  }

  ## ---- 3. Pooled cross-level interaction (supplementary) ---------------------
  hdr("3. POOLED CROSS-LEVEL INTERACTION")

  pooled <- dv |> dplyr::inner_join(mod, by="cty") |>
    dplyr::filter(incumbent == 0) |>
    dplyr::select(prr, dissatisfied, informality, age, female, educ, urban, cty) |>
    stats::na.omit() |>
    dplyr::mutate(inf_c = informality - mean(informality))

  m_pool <- stats::lm(prr ~ dissatisfied * inf_c + age + female + educ + urban +
                        factor(cty), data = pooled)
  ct  <- summary(m_pool)$coefficients
  key <- grep("dissatisfied:inf_c", rownames(ct), value = TRUE)
  cat(sprintf("N = %d individuals, %d countries\n", nrow(pooled),
              dplyr::n_distinct(pooled$cty)))
  cat(sprintf("interaction = %9.5f  conventional se %8.5f  p %.4f\n",
              ct[key,1], ct[key,2], ct[key,4]))

  # inf_c is collinear with the country dummies and is aliased away by lm();
  # model.matrix() still returns its column, so drop the aliased ones first
  keep <- !is.na(stats::coef(m_pool))
  X <- stats::model.matrix(m_pool)[, keep, drop = FALSE]
  u <- stats::resid(m_pool)
  XtXi <- solve(crossprod(X))
  meat <- matrix(0, ncol(X), ncol(X))
  for (g in unique(pooled$cty)) {
    i <- pooled$cty == g
    meat <- meat + tcrossprod(crossprod(X[i, , drop = FALSE], u[i]))
  }
  G <- dplyr::n_distinct(pooled$cty); n <- nrow(X); kk <- ncol(X)
  Vc   <- XtXi %*% meat %*% XtXi * (G/(G-1)) * ((n-1)/(n-kk))
  se_c <- sqrt(diag(Vc))[match(key, colnames(X))]
  cat(sprintf("%12s  country-clustered se %8.5f  p %.4f\n", "",
              se_c, 2*stats::pt(-abs(ct[key,1]/se_c), G-1)))
  cat("NOTE: CR1 with G = 19 clusters over-rejects; script 21 uses CR2 +\n",
      "Satterthwaite (clubSandwich), which is the defensible small-G choice.\n")
  cat("\nTwo-stage estimate (RUN_ANALYSIS Part 6): see output/10_robustness.csv.\n")


  ## ---- 4. Alternative informality series (Section 4.4) -----------------------
  hdr("4. ALTERNATIVE INFORMALITY SERIES FROM THE SAME DATABASE")

  WB <- paths$raw_informal
  wb_sheets <- readxl::excel_sheets(WB)
  grab <- function(sheet) {
    # FIX: say which sheets exist instead of a bare readxl error
    if (!sheet %in% wb_sheets)
      stop("sheet '", sheet, "' not in workbook. Sheets: ", paste(wb_sheets, collapse = ", "))
    raw <- suppressMessages(readxl::read_excel(WB, sheet = sheet))
    nc <- names(raw)[grepl("economy|country", names(raw), ignore.case=TRUE)][1]
    yc <- intersect(as.character(2010:2020),
                    names(raw)[grepl("^(19|20)[0-9]{2}$", names(raw))])
    raw |> dplyr::select(dplyr::all_of(c(nc, yc))) |>
      dplyr::rename(wb = dplyr::all_of(nc)) |>
      tidyr::pivot_longer(-wb, names_to="y", values_to="v") |>
      dplyr::mutate(v = suppressWarnings(as.numeric(v)),
                    # FIX: Czechia added, as in RUN_ANALYSIS
                    cty = dplyr::recode(wb, "Slovak Republic" = "Slovakia",
                                        "Czechia" = "Czech Republic")) |>
      dplyr::group_by(cty) |> dplyr::summarise(x = mean(v, na.rm=TRUE), .groups="drop") |>
      dplyr::filter(!is.nan(x))
  }
  alt_mod <- mod |>
    dplyr::left_join(grab("MIMIC_p") |> dplyr::rename(mimic = x), by="cty") |>
    dplyr::left_join(grab("SEMP_p")  |> dplyr::rename(semp  = x), by="cty")

  cat("countries missing an alternative series:",
      paste(alt_mod$cty[is.na(alt_mod$mimic) | is.na(alt_mod$semp)], collapse = ", "), "\n")
  cat("correlations between the three series, estimation sample:\n")
  ee <- alt_mod |> dplyr::filter(incumbent == 0)
  print(round(stats::cor(ee[,c("informality","mimic","semp")],
                         use="complete.obs"), 3))
  cat("\n")
  R$dge   <- metareg(ee$b_base, ee$se_base, ee[,"informality"], "DGE (baseline)")
  R$mimic <- metareg(ee$b_base, ee$se_base, ee[,"mimic"], "MIMIC series")
  R$semp  <- metareg(ee$b_base, ee$se_base, ee[,"semp"],  "self-employment share")
  cat("\nCoefficients are not comparable in size across series: the three have\n")
  cat("different scales. Sign and significance are what is comparable.\n")

  ## ---- 5. Table --------------------------------------------------------------
  hdr("5. TABLE 4")
  tab <- dplyr::bind_rows(R) |>
    dplyr::mutate(dplyr::across(c(g1, se), ~ round(.x, 4)),
                  p = round(p, 4), tau = round(tau, 2))
  print(as.data.frame(tab), row.names = FALSE)
  readr::write_csv(tab, file.path(paths$output, "TABLE4_robustness.csv"), na = "")
  readr::write_csv(alt_mod |> dplyr::left_join(s_alt, by="cty"),
                   file.path(paths$output, "14_robustness_slopes.csv"), na = "")
  cat("\nWritten: TABLE4_robustness.csv, 14_robustness_slopes.csv\n")
})


# =============================================================================
# STEP 18_itn_KH  <-  R/18_itn_correction_and_KH.R
# needs: step RUN_ANALYSIS
# =============================================================================
STEP("18_itn_KH", {
  # =============================================================================
  # 18_itn_correction_and_KH.R
  #
  # DECLARED CHANGES (both decided before seeing any result from this script):
  #   (1) CLASSIFICATION CORRECTION. ITN (Bulgaria, EES code 10001) is CHES 2024
  #       family 1 and on the ballot, but script 05's matcher never linked it.
  #       Under the paper's stated rule it is PRR. Added here. POT (Romania)
  #       and SALF (Spain) are family 1 but have no EES 2024 ballot code:
  #       documented, not recoded.
  #   (2) INFERENCE UPGRADE. Stage two re-estimated by REML with the
  #       Knapp-Hartung adjustment (metafor, test = "knha"), the current
  #       small-k standard, alongside the conference DL + t estimator.
  #   Every result is printed for BOTH classifications x BOTH estimators so the
  #   effect of each change is separately visible.
  #
  # What ITN touches: Bulgaria's PRR vote slope, its govt-disapproval and
  # EU-dissatisfaction variants, and its PTV outcome. The challenger outcome
  # already contained ITN (script RUN_ANALYSIS Part 8) and is unchanged.
  #
  # Inputs : derived/analysis_individual.rds, output/05_prr_linkage.csv,
  #          output/02_party_frame.csv, output/10_moderator.csv, raw ZA8868
  # Outputs: output/05b_prr_linkage.csv
  #          output/18_country_slopes_corrected.csv
  #          output/18_stage_two_comparison.csv
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - NUMCOLS now includes b/se: 10_moderator.csv was written with NA as the
  #     string "NA", which a na = "" read turns into a CHARACTER column, so
  #     `chk$b - chk$b_old` could fail with "non-numeric argument".
  #   - The reproduction check compared metafor with the transcribed, rounded
  #     console value (-0.0099, 0.0036) at 5e-5 tolerance, which can fail or
  #     pass by rounding luck. It now compares with the hand-coded DL estimator
  #     recomputed on the same data (tolerance 1e-8); the transcribed target is
  #     still printed for reference.
  #   - The "slopes that differ" table divided the PTV slope by 100 while
  #     labelling it "PTV points" (print only).
  #   - stops if ITN has more than one row in the ballot frame (tibble() would
  #     silently recycle and add two ITN linkage rows).
  #   - RUN THIS FILE WHOLE: source("R/18_itn_correction_and_KH.R", echo = TRUE)
  #     or Rscript. Pasting it into the console can drop lines (it did: section 2
  #     and the slope() definition were lost, and a stale `d` from another
  #     script was used). Guards before sections 3 and 4 now stop in that case.
  # =============================================================================

  while (sink.number() > 0) sink()
  closeAllConnections()   # CHANGE: close connections left open by an aborted script (e.g. 15b)
  rm(list = ls())   # CHANGE: no stale objects (e.g. `d`) from other scripts in this session
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  paths <- list(raw_ees24 = file.path(RAW, "ZA8868_v1-0-0.sav"),
                derived = file.path(PROJ, "derived"),
                output  = file.path(PROJ, "output"),
                logs    = file.path(PROJ, "logs"))

  pkgs <- c("haven","dplyr","tidyr","purrr","tibble","readr","metafor")
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss)) stop("install.packages(c(",
                         paste(sprintf('"%s"', miss), collapse = ", "), "))")
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
  options(width = 150, max.print = 100000)
  set.seed(20260914)                                   # same seed as RUN_ANALYSIS

  con <- file(file.path(paths$logs, "18_console.txt"), open = "wt")
  sink(con, split = TRUE)
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")
  cat("metafor", as.character(packageVersion("metafor")), "\n")

  # FIX: b/se added -- otherwise they can arrive as character (see header)
  NUMCOLS <- c("q6_code","modal_slot","n_voters","vote_share_sample","prr",
               "informality","incumbent","cee","recent_gov","b","se")
  read_keepNA <- function(f) readr::read_csv(f, show_col_types = FALSE, na = "") |>
    dplyr::mutate(dplyr::across(dplyr::any_of(NUMCOLS), ~ suppressWarnings(as.numeric(.x))))
  DK <- 98
  miss_dk <- function(x) ifelse(x %in% DK, NA_real_, x)


  # ========================= 1. CORRECTED LINKAGE ==============================

  hdr("1. CORRECTED PRR LINKAGE (declared)")
  link  <- read_keepNA(file.path(paths$output, "05_prr_linkage.csv"))
  frame <- read_keepNA(file.path(paths$output, "02_party_frame.csv"))
  old_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
  stopifnot(!10001 %in% old_codes, 10001 %in% frame$q6_code)

  itn_frame <- frame |> dplyr::filter(q6_code == 10001)
  stopifnot(nrow(itn_frame) == 1)                      # FIX: see header
  link_b <- dplyr::bind_rows(
    link,
    tibble::tibble(q6_code = 10001, cty = "Bulgaria", ches_party = "ITN", prr = 1,
                   note = "CHES 2024 family 1; on ballot; missed by script-05 matcher. Declared correction, script 18.",
                   party_label = itn_frame$party_label, n_voters = itn_frame$n_voters,
                   vote_share_sample = itn_frame$vote_share_sample),
    tibble::tibble(q6_code = NA_real_, cty = "Romania", ches_party = "POT", prr = 0,
                   note = "CHES 2024 family 1 (Dec 2024 national vote); not on EES 2024 Romanian ballot."))
  if (!any(link_b$ches_party == "Salf" & link_b$cty == "Spain", na.rm = TRUE))
    cat("NOTE: no Salf row found; expected one from script 05.\n")
  new_codes <- link_b$q6_code[link_b$prr == 1 & !is.na(link_b$q6_code)]
  cat("PRR ballot codes: conference", length(old_codes), "| corrected", length(new_codes), "\n")
  cat("ITN modal PTV slot:", itn_frame$modal_slot, "\n")
  readr::write_csv(link_b, file.path(paths$output, "05b_prr_linkage.csv"), na = "")


  # ========================= 2. INDIVIDUAL DATA ================================

  hdr("2. OUTCOMES UNDER BOTH CLASSIFICATIONS")
  d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
    dplyr::mutate(
      dissatisfied  = dplyr::case_when(dissat_nat %in% c(3,4) ~ 1L,
                                       dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
      dissat_eu_bin = dplyr::case_when(dissat_eu %in% c(3,4) ~ 1L,
                                       dissat_eu %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
      prr_conf = ifelse(classified, as.integer(q6_raw %in% old_codes), NA_integer_),
      prr_corr = ifelse(classified, as.integer(q6_raw %in% new_codes), NA_integer_))
  # CHANGE: compare values, not storage type (an older rds may hold prr as double)
  stopifnot(identical(as.integer(d$prr_conf), as.integer(d$prr)))
  cat("respondents whose PRR coding changes:",
      sum(d$prr_conf != d$prr_corr, na.rm = TRUE), "(all should be Bulgaria)\n")
  print(table(d$cty[which(d$prr_conf != d$prr_corr)]))

  # PTV: Bulgaria's max over PRR slots now includes ITN's slot
  d$ptv_conf <- d$prr_ptv
  d$ptv_corr <- d$prr_ptv
  slots_bg <- frame |> dplyr::filter(cty == "Bulgaria", q6_code %in% new_codes,
                                     !is.na(modal_slot)) |> dplyr::pull(modal_slot) |> unique()
  cat("Bulgarian PRR PTV slots, corrected:", paste(slots_bg, collapse = ", "), "\n")
  raw_q9 <- haven::read_sav(paths$raw_ees24, user_na = TRUE,
                            col_select = c("resp_id", paste0("q9_", 1:8)))
  q9 <- raw_q9 |> dplyr::mutate(dplyr::across(-resp_id,
                                              \(x) miss_dk(as.numeric(haven::zap_labels(x)))))
  bg <- which(d$cty == "Bulgaria")
  vals <- as.matrix(q9[match(d$resp_id[bg], q9$resp_id), paste0("q9_", slots_bg), drop = FALSE])
  newp <- suppressWarnings(apply(vals, 1, max, na.rm = TRUE)); newp[is.infinite(newp)] <- NA
  d$ptv_corr[bg] <- newp
  cat("Bulgarian PTV changed for", sum(d$ptv_conf[bg] != d$ptv_corr[bg], na.rm = TRUE),
      "respondents\n")


  # ========================= 3. STAGE ONE ======================================

  hdr("3. STAGE ONE")
  prr_ctys <- d |> dplyr::filter(classified) |> dplyr::group_by(cty) |>
    dplyr::summarise(a = sum(prr_conf) > 0, .groups = "drop") |>
    dplyr::filter(a) |> dplyr::pull(cty)

  slope <- function(dat, yvar, xvar = "dissatisfied", extra = NULL) {
    vars <- c(xvar, "age","female","educ","urban", extra)
    purrr::map_dfr(prr_ctys, function(k) {
      dd <- dat |> dplyr::filter(cty == k) |>
        dplyr::select(y = dplyr::all_of(yvar), dplyr::all_of(vars)) |> stats::na.omit()
      if (nrow(dd) < 100 || stats::var(dd$y) == 0)
        return(tibble::tibble(cty = k, b = NA_real_, se = NA_real_))
      ct <- summary(stats::lm(stats::reformulate(vars, "y"), data = dd))$coefficients
      tibble::tibble(cty = k, b = ct[xvar, 1], se = ct[xvar, 2])
    })
  }
  # FIX: stop if section 2 or the slope() definition did not run in this session
  stopifnot("section 2 did not run: `d` lacks prr_corr/ptv_corr" =
              all(c("prr_conf", "prr_corr", "ptv_corr") %in% names(d)),
            "slope() is not defined: run the whole file" = exists("slope", mode = "function"))
  vot <- d |> dplyr::filter(classified, cty %in% prr_ctys)
  ptv <- d |> dplyr::filter(cty %in% prr_ctys)

  S <- list()
  for (cl in c("conf","corr")) {
    y <- paste0("prr_", cl)
    S[[cl]] <- slope(vot, y) |>
      dplyr::left_join(slope(vot, y, extra = "govt_disapp") |>
                         dplyr::rename(b_gd = b, se_gd = se), by = "cty") |>
      dplyr::left_join(slope(vot, y, xvar = "dissat_eu_bin") |>
                         dplyr::rename(b_eu = b, se_eu = se), by = "cty") |>
      dplyr::left_join(slope(ptv |> dplyr::filter(!is.na(.data[[paste0("ptv_", cl)]])),
                             paste0("ptv_", cl)) |>
                         dplyr::rename(b_ptv = b, se_ptv = se), by = "cty") |>
      dplyr::mutate(classification = cl)
  }
  mod <- read_keepNA(file.path(paths$output, "10_moderator.csv")) |>
    dplyr::select(cty, informality, incumbent, cee)
  slopes <- dplyr::bind_rows(S) |> dplyr::left_join(mod, by = "cty")
  stopifnot("stage one incomplete" = setequal(unique(slopes$classification), c("conf", "corr")))

  # check: conference slopes reproduce RUN_ANALYSIS Part 2
  old <- read_keepNA(file.path(paths$output, "10_moderator.csv")) |> dplyr::select(cty, b_old = b)
  chk <- slopes |> dplyr::filter(classification == "conf") |> dplyr::left_join(old, by = "cty")
  cat("max |conference slope - RUN_ANALYSIS slope|:",
      signif(max(abs(chk$b - chk$b_old), na.rm = TRUE), 3), "(should be ~0)\n")

  cat("\nslopes that differ between classifications (b in pp, b_ptv in PTV points):\n")
  print(as.data.frame(slopes |>
                        dplyr::select(cty, classification, b, b_gd, b_eu, b_ptv) |>
                        tidyr::pivot_wider(names_from = classification, values_from = c(b, b_gd, b_eu, b_ptv)) |>
                        dplyr::filter(abs(b_conf - b_corr) > 1e-10 | abs(b_ptv_conf - b_ptv_corr) > 1e-10) |>
                        dplyr::mutate(dplyr::across(dplyr::starts_with(c("b_conf","b_corr","b_gd","b_eu")),
                                                    ~ round(100 * .x, 2)),
                                      # FIX: was round(.x / 100, 3) -- PTV is already in points
                                      dplyr::across(dplyr::starts_with("b_ptv"), ~ round(.x, 3)))),
        row.names = FALSE)
  readr::write_csv(slopes, file.path(paths$output, "18_country_slopes_corrected.csv"), na = "")


  # ========================= 4. STAGE TWO ======================================

  hdr("4. STAGE TWO: DL + t (conference) vs REML + Knapp-Hartung (journal)")

  fit <- function(df, yv, sv, form, method, test) {
    df <- df |> dplyr::filter(!is.na(.data[[yv]]), !is.na(.data[[sv]]))
    m <- tryCatch(metafor::rma(yi = df[[yv]], sei = df[[sv]], mods = form, data = df,
                               method = method, test = test,
                               control = list(stepadj = 0.5, maxiter = 1000)),
                  error = function(e) NULL)
    if (is.null(m)) return(tibble::tibble(k = nrow(df), g1 = NA, se = NA, p = NA,
                                          lo = NA, hi = NA, tau = NA))
    tibble::tibble(k = m$k, g1 = unname(coef(m)["informality"]),
                   se = unname(m$se[names(coef(m)) == "informality"]),
                   p  = unname(m$pval[names(coef(m)) == "informality"]),
                   lo = unname(m$ci.lb[names(coef(m)) == "informality"]),
                   hi = unname(m$ci.ub[names(coef(m)) == "informality"]),
                   tau = 100 * sqrt(m$tau2))
  }

  # CHANGE: hand-coded DL estimator from RUN_ANALYSIS, used as an exact target
  dl_hand <- function(y, se, x) {
    v <- se^2; X <- cbind(1, x); k <- length(y); p <- 2
    W <- diag(1/v); A <- t(X) %*% W %*% X
    r <- y - X %*% solve(A, t(X) %*% W %*% y)
    Qe <- as.numeric(t(r) %*% W %*% r)
    P  <- W - W %*% X %*% solve(A, t(X) %*% W)
    tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
    W2 <- diag(1/(v + tau2)); A2 <- t(X) %*% W2 %*% X
    c(g1 = unname(solve(A2, t(X) %*% W2 %*% y)[2]), se = unname(sqrt(diag(solve(A2)))[2]))
  }

  # reproduction check against the conference hand-coded estimate
  e_conf <- slopes |> dplyr::filter(classification == "conf", incumbent == 0)
  r0 <- fit(e_conf, "b", "se", ~ informality, "DL", "t")
  h0 <- dl_hand(e_conf$b, e_conf$se, e_conf$informality)
  cat(sprintf("metafor DL+t: g1 = %.6f, se = %.6f | hand-coded: g1 = %.6f, se = %.6f\n",
              r0$g1, r0$se, h0["g1"], h0["se"]))
  cat("(conference console, rounded: -0.0099, 0.0036)\n")
  if (abs(r0$g1 - h0["g1"]) > 1e-8 || abs(r0$se - h0["se"]) > 1e-8)
    stop("metafor does not reproduce the hand-coded estimator; stop and report.")

  specs <- tibble::tribble(
    ~label,                       ~sample,  ~yv,     ~sv,      ~form,
    "PRR vote, primary",          "est19",  "b",     "se",     "~ informality",
    "PRR vote, all 24",           "all24",  "b",     "se",     "~ informality",
    "  + post-communist dummy (H4)", "est19", "b",   "se",     "~ informality + cee",
    "  older member states",      "old",    "b",     "se",     "~ informality",
    "  post-communist states",    "post",   "b",     "se",     "~ informality",
    "net of govt disapproval",    "est19",  "b_gd",  "se_gd",  "~ informality",
    "EU dissatisfaction (H2)",    "est19",  "b_eu",  "se_eu",  "~ informality",
    "PRR propensity to vote",     "est19",  "b_ptv", "se_ptv", "~ informality")

  pick <- function(df, s) switch(s,
                                 est19 = df |> dplyr::filter(incumbent == 0),
                                 all24 = df,
                                 old   = df |> dplyr::filter(incumbent == 0, cee == 0),
                                 post  = df |> dplyr::filter(incumbent == 0, cee == 1))

  res <- purrr::pmap_dfr(specs, function(label, sample, yv, sv, form) {
    purrr::map_dfr(c("conf","corr"), function(cl) {
      df <- pick(slopes |> dplyr::filter(classification == cl), sample)
      dplyr::bind_rows(
        fit(df, yv, sv, stats::as.formula(form), "DL",   "t")    |> dplyr::mutate(est = "DL+t"),
        fit(df, yv, sv, stats::as.formula(form), "REML", "knha") |> dplyr::mutate(est = "REML+KH")) |>
        dplyr::mutate(spec = label, classification = cl)
    })
  })
  out <- res |> dplyr::select(spec, classification, est, k, g1, se, lo, hi, p, tau) |>
    dplyr::mutate(dplyr::across(c(g1, se, lo, hi), ~ round(.x, 4)),
                  p = round(p, 4), tau = round(tau, 2))
  print(as.data.frame(out), row.names = FALSE)
  cat("\nPTV coefficients are in PTV points x informality, not pp.\n")
  readr::write_csv(out, file.path(paths$output, "18_stage_two_comparison.csv"), na = "")


  # ===================== 5. PERMUTATION AND LEAVE-ONE-OUT ======================

  hdr("5. PERMUTATION AND LOO, CORRECTED CLASSIFICATION")
  e_corr <- slopes |> dplyr::filter(classification == "corr", incumbent == 0)

  m_kh <- metafor::rma(yi = b, sei = se, mods = ~ informality, data = e_corr,
                       method = "REML", test = "knha",
                       control = list(stepadj = 0.5, maxiter = 1000))
  pt <- metafor::permutest(m_kh, iter = 9999, progbar = FALSE)
  cat(sprintf("REML+KH, corrected: g1 = %.4f | permutation p = %.4f (9999 draws)\n",
              coef(m_kh)["informality"], pt$pval[2]))

  m_dl <- metafor::rma(yi = b, sei = se, mods = ~ informality, data = e_corr,
                       method = "DL", test = "t")
  pt2 <- metafor::permutest(m_dl, iter = 9999, progbar = FALSE)
  cat(sprintf("DL+t,    corrected: g1 = %.4f | permutation p = %.4f (conference value 0.0152)\n",
              coef(m_dl)["informality"], pt2$pval[2]))

  loo <- purrr::map_dfr(e_corr$cty, function(k) {
    f <- fit(e_corr |> dplyr::filter(cty != k), "b", "se", ~ informality, "REML", "knha")
    f |> dplyr::mutate(dropped = k)
  }) |> dplyr::arrange(g1)
  cat("\nleave-one-out, REML+KH, corrected:\n")
  print(as.data.frame(loo |> dplyr::transmute(dropped, g1 = round(g1, 4), se = round(se, 4),
                                              p = round(p, 4), flag = ifelse(p >= .05, "p >= .05", ""))), row.names = FALSE)
  cat(sprintf("\nsign stable: %s | p < .05 in %d of %d\n",
              all(loo$g1 < 0), sum(loo$p < .05), nrow(loo)))

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 20_regional_build  <-  R/20_regional_build.R
# needs: EQI csv, Eurostat xlsx, OWID clientelism csv
# =============================================================================
STEP("20_regional_build", {
  # =============================================================================
  # 20_regional_build.R  -- DATA BUILD + AUDIT. No estimation.
  #
  # Decisions fixed BEFORE any regional estimate is seen (declared):
  #   Regional moderator  EQI, regional mean of rounds 2017, 2021, 2024
  #                       (test-retest of within-country deviations 0.64-0.72;
  #                       averaging raises reliability). Sensitivity: 2024 only.
  #                       Higher EQI = better government => theory predicts
  #                       gamma > 0 on dissatisfied x EQI.
  #   Units               EQI units: NUTS-2, except DE/BE at NUTS-1 and
  #                       CY, EE, LV, LU, MT as one national unit.
  #   Competitor          regional unemployment, mean 2017-2022, NUTS 2021 codes
  #                       (PT17 is discontinued after 2022 in the Eurostat table).
  #   GDP                 only if a Eurostat file WITH GEO CODES is found.
  #   Clientelism         V-Dem v2xnp_client, mean 2010-2020 (DGE window);
  #                       2023 as sensitivity. Country level only.
  #   Classification      corrected (05b, with ITN).
  #
  # Inputs  (D:/, its immediate subfolders, or the project folder):
  #   qog_eqi_long_24.csv, lfst_r_lfu3rt__custom_22894442_spreadsheet.xlsx,
  #   clientelism-index.csv (OWID download; unzip clientelism-index.zip first),
  #   [optional] nama_10r_2gdp ... with codes
  #   + derived/analysis_individual.rds, output/05b_prr_linkage.csv,
  #     output/10_moderator.csv, raw ZA8868
  # Outputs:
  #   derived/20_individual_regional.rds
  #   output/20_region_units.csv, output/20_country_clientelism.csv
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - Joins on `unit` now use na_matches = "never". dplyr's default matches
  #     NA to NA, so every respondent WITHOUT a region code would have been
  #     given the EQI/unemployment value of any EQI row whose region_code is NA
  #     (and duplicated if there were several). The stopifnot(nrow == 25904)
  #     guard catches duplication, not the single-match case.
  #   - Section F "PRIMARY (19)" now really is the 19: it also included the
  #     non-PRR countries (Ireland, Luxembourg, Malta), which have prr = 0,
  #     not NA, and so passed the filter.
  #   - `%||%` defined before the function that uses it (worked by accident of
  #     lazy evaluation; base R >= 4.4 also defines %||% without the NA case).
  #   - paths overridable by environment variables; CSVs written with na = "".
  # =============================================================================

  while (sink.number() > 0) sink()
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  SEARCH_ROOT <- Sys.getenv("INFPRR_SEARCH", "D:/")
  paths <- list(raw_ees24 = file.path(RAW, "ZA8868_v1-0-0.sav"),
                derived = file.path(PROJ, "derived"),
                output  = file.path(PROJ, "output"),
                logs    = file.path(PROJ, "logs"))
  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(tidyr); library(readr)
    library(readxl); library(stringr); library(purrr); library(tibble)
  })
  options(width = 150, max.print = 100000)
  con <- file(file.path(paths$logs, "20_console.txt"), open = "wt"); sink(con, split = TRUE)
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

  find_file <- function(pattern, required = TRUE) {
    # D:/ itself, its immediate subfolders (OWID downloads unzip into one), and
    # the project folders. Not recursive below that, to keep the search fast.
    dirs <- unique(c(SEARCH_ROOT, list.dirs(SEARCH_ROOT, recursive = FALSE),
                     PROJ, file.path(PROJ, "data-raw")))
    hits <- list.files(dirs, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (!length(hits)) {
      if (required) stop("not found: ", pattern, " (looked in ", SEARCH_ROOT,
                         ", its subfolders and ", PROJ, ")")
      return(NA_character_)
    }
    if (length(hits) > 1) cat("  note: several matches, using the first:\n   ",
                              paste(hits, collapse = "\n    "), "\n")
    cat("  using", hits[1], "\n"); hits[1]
  }
  num <- function(x) suppressWarnings(as.numeric(stringr::str_trim(as.character(x))))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a  # FIX: defined first

  ISO2 <- c(Austria="AT",Belgium="BE",Bulgaria="BG",Croatia="HR",Cyprus="CY",
            `Czech Republic`="CZ",Denmark="DK",Estonia="EE",Finland="FI",France="FR",
            Germany="DE",Greece="EL",Hungary="HU",Ireland="IE",Italy="IT",Latvia="LV",
            Lithuania="LT",Luxembourg="LU",Malta="MT",Netherlands="NL",Poland="PL",
            Portugal="PT",Romania="RO",Slovakia="SK",Slovenia="SI",Spain="ES",Sweden="SE")
  NUTS1_CTY <- c("Germany","Belgium")
  NAT_CTY   <- c("Cyprus","Estonia","Latvia","Luxembourg","Malta")
  INCUMB    <- c("Hungary","Italy","Slovakia","Finland","Croatia")
  EST19     <- c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark",
                 "Estonia","France","Germany","Greece","Latvia","Lithuania",
                 "Netherlands","Poland","Portugal","Romania","Slovenia","Spain","Sweden")

  hdr("0. FILES")
  f_eqi   <- find_file("^qog_eqi_long_24\\.csv$")
  f_unemp <- find_file("^lfst_r_lfu3rt.*\\.xlsx$")
  f_clien <- find_file("^clientelism-index.*\\.csv$")
  f_gdp   <- find_file("^nama_10r_2gdp.*\\.xlsx$", required = FALSE)


  # ============================== A. EQI =======================================

  hdr("A. EQI REGIONAL MODERATOR")
  eqi <- readr::read_csv(f_eqi, show_col_types = FALSE, na = "") |>
    dplyr::mutate(EQI = num(EQI), year = as.integer(year))
  cat("rounds:", paste(sort(unique(eqi$year)), collapse = ", "), "\n")
  cat("EQI rows with missing region_code:", sum(is.na(eqi$region_code)), "\n")  # CHANGE

  units <- eqi |> dplyr::filter(year %in% c(2017, 2021, 2024), !is.na(region_code)) |>
    dplyr::group_by(unit = region_code, cname, nuts_level) |>
    dplyr::summarise(eqi_avg = mean(EQI, na.rm = TRUE), n_rounds = sum(!is.na(EQI)),
                     eqi_2024 = EQI[year == 2024][1], .groups = "drop")
  cat("EQI units:", nrow(units), "| units with < 3 rounds:", sum(units$n_rounds < 3), "\n")
  cat("duplicated unit codes (cname/nuts_level differ across rounds):",
      sum(duplicated(units$unit)), "\n")
  print(as.data.frame(units |> dplyr::filter(n_rounds < 3)), row.names = FALSE)


  # ======================= B. EES 2024 -> EQI UNITS ============================

  hdr("B. EES 2024 REGIONS -> EQI UNITS")
  raw <- haven::read_sav(paths$raw_ees24, user_na = TRUE,
                         col_select = c(resp_id, country, d12_NUTSII))
  reg <- tibble::tibble(resp_id = raw$resp_id,
                        cty  = as.character(haven::as_factor(raw$country)),
                        code = stringr::str_trim(as.character(haven::as_factor(raw$d12_NUTSII)))) |>
    dplyr::mutate(code = ifelse(stringr::str_detect(code, "^[A-Z]{2}[0-9A-Z]{2}$"), code, NA),
                  unit = dplyr::case_when(is.na(code)       ~ NA_character_,
                                          cty %in% NUTS1_CTY ~ substr(code, 1, 3),
                                          cty %in% NAT_CTY   ~ unname(ISO2[cty]),
                                          TRUE               ~ code))
  aud <- reg |> dplyr::filter(!is.na(unit)) |> dplyr::distinct(cty, code, unit) |>
    dplyr::left_join(units |> dplyr::select(unit, eqi_avg), by = "unit")
  bad <- aud |> dplyr::filter(is.na(eqi_avg))
  cat("EES codes with no EQI unit:", nrow(bad), "\n")
  if (nrow(bad)) print(as.data.frame(bad), row.names = FALSE)

  # individual file, corrected classification
  link <- readr::read_csv(file.path(paths$output, "05b_prr_linkage.csv"),
                          show_col_types = FALSE, na = "") |>
    dplyr::mutate(q6_code = num(q6_code), prr = num(prr))
  codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
  stopifnot(10001 %in% codes)

  mod <- readr::read_csv(file.path(paths$output, "10_moderator.csv"),
                         show_col_types = FALSE, na = "") |>
    dplyr::transmute(cty, informality = num(informality), cee = num(cee))

  d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
    dplyr::mutate(dissatisfied = dplyr::case_when(dissat_nat %in% c(3,4) ~ 1L,
                                                  dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
                  prr = ifelse(classified, as.integer(q6_raw %in% codes), NA_integer_)) |>
    dplyr::left_join(reg |> dplyr::select(resp_id, code, unit), by = "resp_id") |>
    # FIX: na_matches = "never" -- a missing unit must not match anything
    dplyr::left_join(units |> dplyr::select(unit, eqi_avg, eqi_2024), by = "unit",
                     na_matches = "never") |>
    dplyr::left_join(mod, by = "cty") |>
    dplyr::mutate(incumbent = as.integer(cty %in% INCUMB))
  stopifnot(nrow(d) == 25904)
  cat("respondents with an EQI value:", sum(!is.na(d$eqi_avg)), "of", nrow(d), "\n")


  # ======================== C. UNEMPLOYMENT ====================================

  hdr("C. REGIONAL UNEMPLOYMENT, MEAN 2017-2022")
  u <- suppressMessages(readxl::read_excel(f_unemp, sheet = "Sheet 1", col_names = FALSE))
  trow <- which(u[[1]] == "TIME")[1]
  yrs  <- stringr::str_trim(as.character(unlist(u[trow, ])))
  ycol <- which(yrs %in% as.character(2017:2022))
  ud <- u[(trow + 2):nrow(u), ]
  un <- tibble::tibble(geo = stringr::str_trim(as.character(ud[[1]]))) |>
    dplyr::bind_cols(tibble::as_tibble(lapply(ud[, ycol], num), .name_repair = ~ yrs[ycol])) |>
    dplyr::filter(!is.na(geo)) |>
    tidyr::pivot_longer(-geo, names_to = "year", values_to = "u") |>
    dplyr::group_by(geo) |>
    dplyr::summarise(unemp = mean(u, na.rm = TRUE), n_yrs = sum(!is.na(u)), .groups = "drop") |>
    dplyr::filter(n_yrs > 0)
  cat("geo codes with data:", nrow(un), "\n")
  # national units appear as e.g. EE, EE0 or EE00 -- take the first that exists
  pick_geo <- function(x) purrr::map_chr(x, \(k) {
    cand <- c(k, paste0(k, "0"), paste0(k, "00")); cand[cand %in% un$geo][1] %||% NA_character_ })
  units <- units |> dplyr::mutate(geo = pick_geo(unit)) |>
    dplyr::left_join(un |> dplyr::select(geo, unemp, n_yrs), by = "geo",
                     na_matches = "never")                                   # FIX
  used <- units |> dplyr::filter(unit %in% d$unit)
  cat("EQI units used by EES respondents:", nrow(used),
      "| missing unemployment:", sum(is.na(used$unemp)), "\n")
  if (any(is.na(used$unemp)))
    print(as.data.frame(used |> dplyr::filter(is.na(unemp)) |> dplyr::select(cname, unit, geo)),
          row.names = FALSE)
  d <- d |> dplyr::left_join(units |> dplyr::select(unit, unemp), by = "unit",
                             na_matches = "never")                           # FIX
  stopifnot(nrow(d) == 25904)


  # ============================ D. GDP =========================================

  hdr("D. REGIONAL GDP PER HEAD")
  has_codes <- FALSE
  if (!is.na(f_gdp)) {
    g <- suppressMessages(readxl::read_excel(f_gdp, sheet = "Sheet 1", col_names = FALSE))
    has_codes <- any(g[[1]] == "GEO (Codes)", na.rm = TRUE)
  }
  if (!has_codes) {
    cat("SKIPPED: the GDP workbook has GEO labels only (duplicated labels make a\n")
    cat("name merge unsafe). Re-download nama_10r_2gdp with 'Codes and labels',\n")
    cat("unit PPS_EU27_2020_HAB, NUTS 1+2, 2015-2023. Script 21 runs without it.\n")
  } else cat("GDP file with codes found -- parse in script 21 (structure to be confirmed).\n")


  # ========================= E. CLIENTELISM ====================================

  hdr("E. V-DEM CLIENTELISM (country level)")
  cl_raw <- readr::read_csv(f_clien, show_col_types = FALSE, na = "")
  cat("clientelism file columns:", paste(names(cl_raw), collapse = ", "), "\n")
  cl <- cl_raw |>
    dplyr::rename(entity = Entity, year = Year, client = `Clientelism Index`) |>
    dplyr::mutate(cty = dplyr::recode(entity, "Czechia" = "Czech Republic"),
                  client = num(client)) |>
    dplyr::filter(cty %in% names(ISO2)) |>
    dplyr::group_by(cty) |>
    dplyr::summarise(client_1020 = mean(client[year %in% 2010:2020], na.rm = TRUE),
                     client_2023 = client[year == 2023][1], .groups = "drop")
  cm <- mod |> dplyr::left_join(cl, by = "cty") |>
    dplyr::mutate(incumbent = as.integer(cty %in% INCUMB))
  cat("countries missing clientelism:", paste(cm$cty[is.na(cm$client_1020)], collapse = ", "), "\n")
  e19 <- cm |> dplyr::filter(incumbent == 0)
  cat("\ncorrelations, k = 19:\n")
  print(round(stats::cor(e19[, c("informality","client_1020","client_2023","cee")],
                         use = "complete.obs"), 3))
  readr::write_csv(cm, file.path(paths$output, "20_country_clientelism.csv"), na = "")


  # ================= F. IDENTIFYING VARIATION FOR THE CORE MODEL ===============

  hdr("F. WITHIN-COUNTRY EQI VARIATION AMONG CLASSIFIED VOTERS")
  v <- d |> dplyr::filter(classified, !is.na(eqi_avg), !is.na(dissatisfied), !is.na(prr))
  tab <- v |> dplyr::group_by(cty) |>
    dplyr::summarise(voters = dplyr::n(), units = dplyr::n_distinct(unit),
                     units30 = sum(table(unit) >= 30),
                     sd_within = stats::sd(eqi_avg - mean(eqi_avg)),
                     incumbent = dplyr::first(incumbent), cee = dplyr::first(cee),
                     .groups = "drop") |>
    dplyr::mutate(sd_within = round(sd_within, 3),
                  share_of_ident = round(voters * sd_within^2 /
                                           sum(voters * sd_within^2, na.rm = TRUE), 3)) |>
    dplyr::arrange(dplyr::desc(share_of_ident))
  print(as.data.frame(tab), row.names = FALSE)
  cat("\nshare_of_ident = voters x within-country EQI variance, normalised: a rough\n")
  cat("guide to how much each country contributes to identifying gamma.\n")
  p19 <- tab |> dplyr::filter(cty %in% EST19)          # FIX: was incumbent == 0 (22 countries)
  cat(sprintf("\nPRIMARY (%d): identifying countries (units >= 3): %d | voters %d | units %d\n",
              nrow(p19), sum(p19$units >= 3), sum(p19$voters[p19$units >= 3]),
              sum(p19$units[p19$units >= 3])))
  cat(sprintf("post-communist share of identifying variance, primary: %.3f\n",
              sum(p19$share_of_ident[p19$cee == 1], na.rm = TRUE) /
                sum(p19$share_of_ident, na.rm = TRUE)))
  cat(sprintf("Italy share, all countries: %.3f\n", tab$share_of_ident[tab$cty == "Italy"]))

  saveRDS(d, file.path(paths$derived, "20_individual_regional.rds"))
  readr::write_csv(units, file.path(paths$output, "20_region_units.csv"), na = "")
  cat("\nwritten: derived/20_individual_regional.rds, output/20_region_units.csv,",
      "output/20_country_clientelism.csv\n")

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 21_regional_model  <-  R/21_core_regional_model.R
# needs: step 20
# =============================================================================
STEP("21_regional_model", {
  # =============================================================================
  # 21_core_regional_model.R  -- THE WITHIN-COUNTRY TEST
  #
  # Pre-declared (fixed before any regional estimate was seen):
  #
  #   prr_irc = a_r + b_c*D_i + gamma*D_i*(EQI_r - mean_c EQI) + X_i*d_c + e
  #     a_r   region (EQI unit) fixed effects -- absorb EQI's main effect
  #     b_c   country-specific dissatisfaction slopes -- absorb informality,
  #           incumbency and the entire East-West contrast
  #     d_c   country-specific control coefficients (age, female, educ, urban)
  #   gamma is identified ONLY from within-country regional variation.
  #   PREDICTION: gamma > 0 (dissatisfaction translates more where EQI is high).
  #
  #   Inference: CR2 clustered on EQI unit, Satterthwaite df (clubSandwich);
  #   randomisation inference permuting unit EQI values WITHIN country, 1999
  #   draws. RI is the design-based check and does not rely on cluster asymptotics.
  #
  #   DECLARED EXCLUSION: Belgium. The PRR party (VB, 562xx ballot) stands only
  #   on the Dutch-language college ballot, so the within-Belgium slope contrast
  #   is party supply, not institutional quality. M4 shows it; Part 1 documents it.
  #   Ceuta/Melilla (ES63/ES64) have no EQI unit and drop out.
  #
  #   M1 primary     19 non-incumbent minus Belgium, EQI mean 2017/2021/2024
  #   M2             M1 + D x regional unemployment (competitor)
  #   M3             M1, EQI 2024 round only
  #   M4 diagnostic  M1 + Belgium
  #   M5 extension   24 minus Belgium (incumbents included)
  #   M6             M5 minus Italy
  #   M7             post-communist only (primary countries)
  #   M8             older member states only (primary countries)
  #   + leave-one-country-out on M1; + missing-region slope selection test.
  #   Only M1 is confirmatory. Everything else is labelled as it is here.
  #
  # Input : derived/20_individual_regional.rds, output/20_region_units.csv
  # Output: output/21_core_results.csv, output/21_loo.csv
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - ri(): sample(m) replaced by m[sample.int(length(m))]. For a country with
  #     ONE unit, sample(x) with a single number x >= 1 samples from 1:x (the
  #     classic R trap). Today m = 0 there, so it was harmless, but any change
  #     of centring would have silently corrupted the permutation distribution.
  #   - ri(): stops if the FWL-reconstructed coefficient differs from lm()'s
  #     (guards against rows being dropped by lm() silently).
  #   - paths overridable by environment variables; CSVs written with na = "".
  # =============================================================================

  while (sink.number() > 0) sink()
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  paths <- list(derived = file.path(PROJ, "derived"), output = file.path(PROJ, "output"),
                logs = file.path(PROJ, "logs"))
  pkgs <- c("dplyr","tibble","purrr","readr","clubSandwich")
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss)) stop("install.packages(c(", paste(sprintf('"%s"', miss), collapse = ", "), "))")
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
  options(width = 150, max.print = 100000)
  set.seed(20260924)
  con <- file(file.path(paths$logs, "21_console.txt"), open = "wt"); sink(con, split = TRUE)
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
  cat("clubSandwich", as.character(packageVersion("clubSandwich")), "\n")

  INCUMB <- c("Hungary","Italy","Slovakia","Finland","Croatia")
  CEE    <- c("Bulgaria","Croatia","Czech Republic","Estonia","Hungary","Latvia",
              "Lithuania","Poland","Romania","Slovakia","Slovenia")
  EST19  <- c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark","Estonia",
              "France","Germany","Greece","Latvia","Lithuania","Netherlands","Poland",
              "Portugal","Romania","Slovenia","Spain","Sweden")
  ALL24  <- c(EST19, INCUMB)
  CTRL   <- c("age","female","educ","urban")

  d <- readRDS(file.path(paths$derived, "20_individual_regional.rds"))
  base <- d |> dplyr::filter(classified, cty %in% ALL24, !is.na(prr), !is.na(dissatisfied),
                             !is.na(unit), !is.na(eqi_avg)) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(CTRL), ~ !is.na(.x)))
  cat("analysis base (24 countries, classified voters with EQI and controls):",
      nrow(base), "\n")


  # ===================== 1. BELGIUM: DOCUMENT THE SUPPLY PROBLEM ===============

  hdr("1. BELGIUM -- PRR SUPPLY BY UNIT AND BALLOT SERIES")
  be <- d |> dplyr::filter(cty == "Belgium", classified, !is.na(unit)) |>
    dplyr::mutate(ballot = dplyr::case_when(floor(q6_raw / 100) == 561 ~ "561xx",
                                            floor(q6_raw / 100) == 562 ~ "562xx (VB here)",
                                            TRUE ~ "other"))
  print(as.data.frame(be |> dplyr::group_by(unit, ballot) |>
                        dplyr::summarise(n = dplyr::n(), prr_pct = round(100 * mean(prr), 1), .groups = "drop")),
        row.names = FALSE)
  cat("If the non-VB ballot series shows prr = 0, the within-Belgium contrast is\n")
  cat("party supply. That is why Belgium is excluded from M1 (declared).\n")


  # ====================== 2. MODEL MACHINERY ===================================

  prep <- function(dat, mvar, comp = NULL) {
    dat <- dat |> dplyr::filter(!is.na(.data[[mvar]]))
    if (!is.null(comp)) dat <- dat |> dplyr::filter(!is.na(.data[[comp]]))
    dat$m   <- dat[[mvar]] - stats::ave(dat[[mvar]], dat$cty)      # within-country
    dat$DxM <- dat$dissatisfied * dat$m
    if (!is.null(comp)) {
      dat$DxU <- dat$dissatisfied * (dat[[comp]] - stats::ave(dat[[comp]], dat$cty))
    }
    dat$cty <- factor(dat$cty); dat$unit <- factor(dat$unit)
    dat
  }
  form <- function(comp = NULL) stats::as.formula(paste(
    "prr ~ DxM", if (!is.null(comp)) "+ DxU",
    "+ dissatisfied:cty + (", paste(CTRL, collapse = " + "), "):cty + unit"))

  get_ct <- function(m, dat, coef) {
    vc <- clubSandwich::vcovCR(m, cluster = dat$unit, type = "CR2")
    ct <- as.data.frame(clubSandwich::coef_test(m, vcov = vc, test = "Satterthwaite",
                                                coefs = coef))
    pick <- function(pats) {                      # first pattern that matches wins
      for (p in pats) { j <- grep(p, names(ct), ignore.case = TRUE)
      if (length(j)) return(ct[[j[1]]]) }
      stop("coef_test column not found: ", paste(pats, collapse = "|"),
           " -- columns are: ", paste(names(ct), collapse = ", "))
    }
    list(b = pick(c("^beta$", "^estimate$")), se = pick("^SE$"),
         df = pick("^df"), p = pick("^p_"))
  }

  ri <- function(m, dat, B = 1999) {
    # FWL: residualise y and the interaction on everything else once, then only
    # the permuted interaction column needs re-residualising each draw.
    stopifnot(nrow(stats::model.matrix(m)) == nrow(dat))   # FIX: no silent row drops
    X  <- stats::model.matrix(m)
    X0 <- X[, colnames(X) != "DxM", drop = FALSE]
    X0 <- X0[, !is.na(stats::coef(m))[colnames(X0)], drop = FALSE]
    Q  <- qr(X0)
    ry <- qr.resid(Q, dat$prr)
    obs <- unname(stats::coef(m)["DxM"])
    rx0 <- qr.resid(Q, dat$DxM)
    if (abs(sum(rx0 * ry) / sum(rx0^2) - obs) > 1e-8)       # FIX: FWL sanity check
      stop("FWL reconstruction does not reproduce the lm() coefficient")
    uv  <- dat |> dplyr::distinct(cty, unit, m)             # unit-level values
    stopifnot(!anyDuplicated(uv$unit))
    draws <- replicate(B, {
      # FIX: m[sample.int(n)] -- sample(m) misbehaves when a country has one unit
      pv <- uv |> dplyr::group_by(cty) |> dplyr::mutate(m = m[sample.int(dplyr::n())]) |>
        dplyr::ungroup()
      x  <- dat$dissatisfied * pv$m[match(dat$unit, pv$unit)]
      rx <- qr.resid(Q, x)
      sum(rx * ry) / sum(rx^2)
    })
    (sum(abs(draws) >= abs(obs)) + 1) / (B + 1)
  }

  run <- function(label, countries, mvar = "eqi_avg", comp = NULL, do_ri = TRUE) {
    dat <- prep(base |> dplyr::filter(cty %in% countries), mvar, comp)
    m   <- stats::lm(form(comp), data = dat)
    g   <- get_ct(m, dat, "DxM")
    out <- tibble::tibble(spec = label, n = nrow(dat), countries = dplyr::n_distinct(dat$cty),
                          units = dplyr::n_distinct(dat$unit),
                          gamma = g$b, se = g$se, df = g$df, p_CR2 = g$p,
                          p_RI = if (do_ri) ri(m, dat) else NA_real_)
    if (!is.null(comp)) {
      u <- get_ct(m, dat, "DxU")
      out$comp_b <- u$b; out$comp_se <- u$se; out$comp_p <- u$p
    }
    cat(sprintf("%-44s n=%5d k=%2d units=%3d  gamma=%8.4f se=%7.4f df=%5.1f p_CR2=%.4f p_RI=%s\n",
                label, out$n, out$countries, out$units, out$gamma, out$se, out$df, out$p_CR2,
                ifelse(is.na(out$p_RI), "  -  ", sprintf("%.4f", out$p_RI))))
    invisible(list(res = out, m = m, dat = dat))
  }


  # ======================== 3. SPECIFICATIONS ==================================

  hdr("3. CORE ESTIMATES (M1 confirmatory; the rest as labelled)")
  P18   <- setdiff(EST19, "Belgium")
  A23   <- setdiff(ALL24, "Belgium")
  R <- list()
  R$M1 <- run("M1 PRIMARY: 18 countries, EQI avg",       P18)
  R$M2 <- run("M2 M1 + D x unemployment",                P18, comp = "unemp")
  R$M3 <- run("M3 M1, EQI 2024 only",                    P18, mvar = "eqi_2024")
  R$M4 <- run("M4 DIAGNOSTIC: M1 + Belgium",             EST19)
  R$M5 <- run("M5 EXTENSION: 23 incl. incumbents",       A23)
  R$M6 <- run("M6 M5 minus Italy",                       setdiff(A23, "Italy"))
  R$M7 <- run("M7 post-communist only (primary)",        intersect(P18, CEE))
  R$M8 <- run("M8 older member states (primary)",        setdiff(P18, CEE))
  if (!is.null(R$M2$res$comp_b))
    cat(sprintf("\nM2 competitor D x unemployment: b = %.4f se = %.4f p = %.4f\n",
                R$M2$res$comp_b, R$M2$res$comp_se, R$M2$res$comp_p))

  res <- dplyr::bind_rows(lapply(R, `[[`, "res"))
  readr::write_csv(res, file.path(paths$output, "21_core_results.csv"), na = "")


  # ===================== 4. MAGNITUDE ==========================================

  hdr("4. WHAT GAMMA MEANS")
  dm <- R$M1$dat
  q  <- stats::quantile(dm$m, c(.25, .75))
  g1 <- R$M1$res$gamma
  cat(sprintf("within-country EQI deviation among M1 voters: IQR %.3f to %.3f (width %.3f)\n",
              q[1], q[2], diff(q)))
  cat(sprintf("implied change in the dissatisfaction-PRR gap across that IQR: %.1f pp\n",
              100 * g1 * diff(q)))
  cat(sprintf("per 1 within-country SD of EQI (%.3f): %.1f pp\n",
              stats::sd(dm$m), 100 * g1 * stats::sd(dm$m)))
  cat("Measurement error in regional EQI attenuates gamma toward zero.\n")


  # ===================== 5. LEAVE ONE COUNTRY OUT (M1) =========================

  hdr("5. LEAVE ONE COUNTRY OUT, M1 (CR2 only)")
  ident <- dm |> dplyr::group_by(cty) |> dplyr::summarise(u = dplyr::n_distinct(unit)) |>
    dplyr::filter(u >= 2) |> dplyr::pull(cty) |> as.character()
  loo <- purrr::map_dfr(ident, function(drop_cty) {
    r <- run(paste("  drop", drop_cty), setdiff(P18, drop_cty), do_ri = FALSE)$res
    r |> dplyr::mutate(dropped = drop_cty)
  })
  cat(sprintf("\nsign stable: %s | p_CR2 < .05 in %d of %d | range %.4f to %.4f\n",
              all(sign(loo$gamma) == sign(g1)), sum(loo$p_CR2 < .05), nrow(loo),
              min(loo$gamma), max(loo$gamma)))
  readr::write_csv(loo, file.path(paths$output, "21_loo.csv"), na = "")


  # ============== 6. MISSING-REGION SELECTION ON THE SLOPE =====================

  hdr("6. DOES A MISSING REGION CODE SELECT ON THE SLOPE? (M1 countries)")
  ms <- d |> dplyr::filter(classified, cty %in% P18, !is.na(prr), !is.na(dissatisfied)) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(CTRL), ~ !is.na(.x))) |>
    dplyr::mutate(miss_reg = as.integer(is.na(unit)), cty = factor(cty))
  cat("missing-region share:", round(100 * mean(ms$miss_reg), 1), "%\n")
  m_sel <- stats::lm(stats::as.formula(paste(
    "prr ~ dissatisfied:cty + dissatisfied:miss_reg + miss_reg:cty + (",
    paste(CTRL, collapse = " + "), "):cty + cty")), data = ms)
  vc <- clubSandwich::vcovCR(m_sel, cluster = ms$cty, type = "CR2")
  print(clubSandwich::coef_test(m_sel, vcov = vc, test = "Satterthwaite",
                                coefs = "dissatisfied:miss_reg"))
  cat("A small, insignificant coefficient = the regional sample is not selected on\n")
  cat("the dissatisfaction-PRR slope.\n")

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 22_opposition  <-  R/22_opposition_prr.R
# needs: step 18
# =============================================================================
STEP("22_opposition", {
  # =============================================================================
  # 22_opposition_prr.R
  #
  # DECLARED CHANGE (outcome redefinition, decided after the k = 19 results):
  #   Outcome = vote for a PRR party that held NO CABINET SEAT on election day.
  #   Rule applied to CHES 2024 family-1 parties with an EES ballot code:
  #     governing, excluded : Fidesz-KDNP (HU), SNS (SK), FdI + Lega (IT),
  #                           PS (FI), DP (HR)
  #     opposition, kept    : all other family-1 parties, incl. Mi Hazank and
  #                           Jobbik (HU), Republika (SK)
  #   Countries with no opposition PRR on the ballot drop out: IT, FI, HR.
  #   => k = 21 (the 19 + Hungary + Slovakia).
  #   Written edge cases:
  #     Sweden SD   support party (Tido agreement), no cabinet seat -> opposition.
  #                 Sensitivity: support party counted as incumbent (SE drops).
  #     Netherlands Schoof cabinet sworn in after the election -> opposition.
  #   Caveat stated in advance: in Hungary the main anti-government vehicle in
  #   2024 was Tisza (not PRR), so HU's opposition-PRR slope is small by
  #   construction. Party supply re-enters there; read alongside the challenger
  #   outcome.
  #   Because this redefinition was chosen AFTER the k = 19 results, every
  #   estimate from this script is EXPLORATORY and must be reported as such.
  #
  # Inputs : derived/analysis_individual.rds, output/05b_prr_linkage.csv,
  #          output/10_moderator.csv
  # Output : output/22_opposition_slopes.csv, output/22_stage_two.csv
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - the "insufficient data" branch returned share = NA (logical); now
  #     NA_real_ so the column type does not depend on which country fails.
  #   - fit() returns an NA row instead of stopping the script when a subset
  #     is too small for REML (e.g. a post-communist subset of k < 4).
  #   - labels "post-2004" -> "post-communist" (the dummy excludes Cyprus).
  #   - paths overridable by environment variables; CSVs written with na = "".
  # =============================================================================

  while (sink.number() > 0) sink()
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  paths <- list(derived = file.path(PROJ, "derived"), output = file.path(PROJ, "output"),
                logs = file.path(PROJ, "logs"))
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(purrr); library(tibble); library(metafor)
  })
  options(width = 150)
  set.seed(20260914)
  con <- file(file.path(paths$logs, "22_console.txt"), open = "wt"); sink(con, split = TRUE)
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
  num <- function(x) suppressWarnings(as.numeric(x))

  GOVT_CODES <- c(34801,           # Fidesz-KDNP, Hungary
                  70304,           # SNS, Slovakia
                  38005, 38006,    # FdI, Lega, Italy
                  24604,           # PS, Finland
                  19101)           # DP, Croatia
  SD_CODE <- 75203                 # Sverigedemokraterna (support party)
  CEE <- c("Bulgaria","Croatia","Czech Republic","Estonia","Hungary","Latvia",
           "Lithuania","Poland","Romania","Slovakia","Slovenia")

  # ============================ 1. CODES =======================================
  hdr("1. OPPOSITION-PRR BALLOT CODES")
  link <- readr::read_csv(file.path(paths$output, "05b_prr_linkage.csv"),
                          show_col_types = FALSE, na = "") |>
    dplyr::mutate(q6_code = num(q6_code), prr = num(prr))
  prr_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
  stopifnot(all(GOVT_CODES %in% prr_codes), SD_CODE %in% prr_codes, 10001 %in% prr_codes)
  opp_codes <- setdiff(prr_codes, GOVT_CODES)
  print(as.data.frame(link |> dplyr::filter(q6_code %in% prr_codes) |>
                        dplyr::mutate(status = ifelse(q6_code %in% GOVT_CODES, "GOVERNING (excluded)", "opposition")) |>
                        dplyr::select(cty, q6_code, ches_party, status) |> dplyr::arrange(cty)), row.names = FALSE)

  d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
    dplyr::mutate(dissatisfied = dplyr::case_when(dissat_nat %in% c(3,4) ~ 1L,
                                                  dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
                  opp     = ifelse(classified, as.integer(q6_raw %in% opp_codes), NA_integer_),
                  opp_sd  = ifelse(classified, as.integer(q6_raw %in% setdiff(opp_codes, SD_CODE)),
                                   NA_integer_),
                  prr_corr = ifelse(classified, as.integer(q6_raw %in% prr_codes), NA_integer_))

  # ============================ 2. STAGE ONE ===================================
  hdr("2. STAGE ONE, OPPOSITION-PRR OUTCOME")
  slope <- function(yvar) {
    v <- d |> dplyr::filter(classified)
    ctys <- v |> dplyr::group_by(cty) |>
      dplyr::summarise(a = sum(.data[[yvar]]) > 0, .groups = "drop") |>
      dplyr::filter(a) |> dplyr::pull(cty)
    purrr::map_dfr(ctys, function(k) {
      dd <- v |> dplyr::filter(cty == k) |>
        dplyr::select(y = dplyr::all_of(yvar), dissatisfied, age, female, educ, urban) |>
        stats::na.omit()
      if (nrow(dd) < 100 || stats::var(dd$y) == 0)
        return(tibble::tibble(cty = k, n = nrow(dd), share = NA_real_,   # FIX: typed NA
                              b = NA_real_, se = NA_real_))
      ct <- summary(stats::lm(y ~ dissatisfied + age + female + educ + urban, dd))$coefficients
      tibble::tibble(cty = k, n = nrow(dd), share = round(100 * mean(dd$y), 1),
                     b = ct["dissatisfied", 1], se = ct["dissatisfied", 2])
    })
  }
  mod <- readr::read_csv(file.path(paths$output, "10_moderator.csv"),
                         show_col_types = FALSE, na = "") |>
    dplyr::transmute(cty, informality = num(informality), cee = as.integer(cty %in% CEE))

  s_opp  <- slope("opp")    |> dplyr::left_join(mod, by = "cty")
  s_sd   <- slope("opp_sd") |> dplyr::left_join(mod, by = "cty")
  s_corr <- slope("prr_corr") |> dplyr::left_join(mod, by = "cty") |>
    dplyr::filter(!cty %in% c("Hungary","Italy","Slovakia","Finland","Croatia"))

  cat("countries with an opposition-PRR option:", nrow(s_opp), "\n")
  cat("of which missing informality:", paste(s_opp$cty[is.na(s_opp$informality)], collapse = ", "), "\n")
  print(as.data.frame(s_opp |> dplyr::mutate(b_pp = round(100 * b, 2), se_pp = round(100 * se, 2),
                                             informality = round(informality, 1)) |>
                        dplyr::select(cty, n, share, b_pp, se_pp, informality, cee) |>
                        dplyr::arrange(informality)), row.names = FALSE)
  cat("\nHungary and Slovakia, conference outcome vs opposition outcome (pp):\n")
  old <- readr::read_csv(file.path(paths$output, "10_moderator.csv"), show_col_types = FALSE, na = "") |>
    dplyr::transmute(cty, b_conf = round(100 * num(b), 2))
  print(as.data.frame(s_opp |> dplyr::filter(cty %in% c("Hungary","Slovakia")) |>
                        dplyr::transmute(cty, b_opp = round(100 * b, 2)) |> dplyr::left_join(old, by = "cty")),
        row.names = FALSE)
  readr::write_csv(s_opp, file.path(paths$output, "22_opposition_slopes.csv"), na = "")

  # ============================ 3. STAGE TWO ===================================
  hdr("3. STAGE TWO")
  ctrl <- list(stepadj = 0.5, maxiter = 1000)
  fit <- function(df, form, method, test, label) {
    df <- df |> dplyr::filter(!is.na(b), !is.na(se), !is.na(informality))
    m <- tryCatch(metafor::rma(yi = b, sei = se, mods = form, data = df, method = method,
                               test = test, control = ctrl),
                  error = function(e) { cat("  [", label, method, "failed:",
                                            conditionMessage(e), "]\n"); NULL })
    if (is.null(m))                                                     # FIX: no hard stop
      return(tibble::tibble(spec = label, est = paste(method, test), k = nrow(df),
                            g1 = NA_real_, se = NA_real_, lo = NA_real_, hi = NA_real_,
                            p = NA_real_, tau = NA_real_))
    i <- which(names(coef(m)) == "informality")
    tibble::tibble(spec = label, est = paste(method, test), k = m$k,
                   g1 = unname(coef(m)[i]), se = m$se[i], lo = m$ci.lb[i], hi = m$ci.ub[i],
                   p = m$pval[i], tau = 100 * sqrt(m$tau2))
  }
  both <- function(df, form, label) dplyr::bind_rows(
    fit(df, form, "DL", "t", label), fit(df, form, "REML", "knha", label))
  f1 <- ~ informality; f2 <- ~ informality + cee
  res <- dplyr::bind_rows(
    both(s_corr, f1, "PRR, corrected, k=19 (reference)"),
    both(s_opp,  f1, sprintf("OPPOSITION PRR, k=%d", nrow(s_opp))),
    both(s_opp,  f2, "  + post-communist dummy"),
    both(dplyr::filter(s_opp, cee == 0), f1, "  older member states"),
    both(dplyr::filter(s_opp, cee == 1), f1, "  post-communist states"),
    both(s_sd,   f1, "  sensitivity: SD as incumbent"))
  print(as.data.frame(res |> dplyr::mutate(dplyr::across(c(g1, se, lo, hi), ~ round(.x, 4)),
                                           p = round(p, 4), tau = round(tau, 2))), row.names = FALSE)
  readr::write_csv(res, file.path(paths$output, "22_stage_two.csv"), na = "")

  # ======================= 4. PERMUTATION AND LOO ==============================
  hdr("4. PERMUTATION AND LEAVE-ONE-OUT, OPPOSITION PRR, REML+KH")
  m_kh <- metafor::rma(yi = b, sei = se, mods = ~ informality, data = s_opp,
                       method = "REML", test = "knha", control = ctrl)
  pt <- metafor::permutest(m_kh, iter = 9999, progbar = FALSE)
  cat(sprintf("g1 = %.4f | permutation p = %.4f\n", coef(m_kh)["informality"], pt$pval[2]))
  loo <- purrr::map_dfr(s_opp$cty, function(drop_cty) {
    fit(dplyr::filter(s_opp, cty != drop_cty), f1, "REML", "knha", drop_cty)
  }) |> dplyr::arrange(g1)
  print(as.data.frame(loo |> dplyr::transmute(dropped = spec, g1 = round(g1, 4),
                                              se = round(se, 4), p = round(p, 4), flag = ifelse(p >= .05, "p >= .05", ""))),
        row.names = FALSE)
  cat(sprintf("\nsign stable: %s | p < .05 in %d of %d\n",
              all(loo$g1 < 0, na.rm = TRUE), sum(loo$p < .05, na.rm = TRUE), nrow(loo)))

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 23_scale_check  <-  R/23_scale_check.R
# needs: step 18; package logistf
# =============================================================================
STEP("23_scale_check", {
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
})


# =============================================================================
# STEP 23b_heterogeneity  <-  R/23b_logit_heterogeneity.R
# needs: step 23
# =============================================================================
STEP("23b_heterogeneity", {
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
})


# =============================================================================
# STEP 26_position_table  <-  R/26_build_position_table.R
# needs: CHES csv; step 18
# =============================================================================
STEP("26_position_table", {
  # =============================================================================
  # 26_build_position_table.R
  # Purpose : Build the PRR-position coding table (27 states x 2 waves) for the
  #           v2 pre-registration, mechanically, from
  #             (a) the CHES trend file  -> which parties are PRR, main PRR party
  #             (b) a researched cabinet-events table (section 2, with sources)
  #           under BOTH classification schemes:
  #             FIXED : a party's CHES-2024 family applies to both waves
  #                     (parties absent from CHES 2024 keep their 2019 family)
  #             WAVE  : each wave uses its own CHES release
  #           The script DOES NOT touch ZA7581. It uses no outcome data.
  #
  # Discovery first: section 1 prints the CHES structure and stops on anything
  # unexpected. Return the full console output before the table is used.
  #
  # Inputs : D:/Cluj/1999-2024_CHES_dataset_meansV2.csv
  #          output/05b_prr_linkage.csv (cross-check of the 2024 PRR set)
  # Outputs: output/26_prr_position_coding.csv, logs/26_console.txt
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION (this is the LAST of the five
  # versions pasted; the third had a stray "1" after close(con), a parse error)
  #   - "^FP[O\u00d6]" written with a Unicode escape: a literal Ö in the source
  #     is mangled when the file is sourced under a non-UTF-8 locale (Windows
  #     R < 4.2, or RStudio with a non-UTF-8 default encoding), and the Austrian
  #     event then matches nothing.
  #   - stops if no vote-share column is found (was .data[[NA]] -> cryptic error).
  #   - NEW column `main_rule`: "largest EP share" or "ARBITRARY (no EP share)".
  #     which.max() on an all-missing vote vector returns the FIRST row, so the
  #     "main party" was silently arbitrary in those country-waves.
  #   - paths overridable by environment variables.
  # =============================================================================

  while (sink.number() > 0) sink()
  closeAllConnections()
  PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW   <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  paths <- list(ches   = file.path(RAW, "1999-2024_CHES_dataset_meansV2.csv"),
                output = file.path(PROJ, "output"),
                logs   = file.path(PROJ, "logs"))

  pkgs <- c("dplyr", "tibble", "readr", "stringr", "purrr", "tidyr")
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss)) stop("install.packages(c(",
                         paste(sprintf('"%s"', miss), collapse = ", "), "))")
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
  options(width = 160, max.print = 100000)

  con <- file(file.path(paths$logs, "26_console.txt"), open = "wt")
  sink(con, split = TRUE)
  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")
  num <- function(x) suppressWarnings(as.numeric(x))

  REF <- c(`2019` = "2019-05-23", `2024` = "2024-06-06")
  EU27 <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
            "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
            "Romania","Slovakia","Slovenia","Spain","Sweden")
  # CHES numeric country codes -> names. VERIFY against your script-05 mapping;
  # section 1 prints the resulting party counts so a wrong code is visible.
  CHES_CTY <- c(`1`="Belgium", `2`="Denmark", `3`="Germany", `4`="Greece", `5`="Spain",
                `6`="France", `7`="Ireland", `8`="Italy", `10`="Netherlands",
                `12`="Portugal", `13`="Austria", `14`="Finland", `16`="Sweden",
                `20`="Bulgaria", `21`="Czech Republic", `22`="Estonia", `23`="Hungary",
                `24`="Latvia", `25`="Lithuania", `26`="Poland", `27`="Romania",
                `28`="Slovakia", `29`="Slovenia", `31`="Croatia", `37`="Malta",
                `38`="Luxembourg", `40`="Cyprus")


  # ========================= 1. CHES: DISCOVERY ================================

  hdr("1. CHES TREND FILE: STRUCTURE")
  # Read everything as character with na = "" : Latvia's party "NA" must survive.
  ches_raw <- readr::read_csv(paths$ches, col_types = readr::cols(.default = "c"),
                              na = "", show_col_types = FALSE)
  cat("columns (", ncol(ches_raw), "):\n"); print(names(ches_raw))

  need <- c("country", "year", "party_id", "party", "family")
  if (length(setdiff(need, names(ches_raw))))
    stop("Missing expected CHES columns: ",
         paste(setdiff(need, names(ches_raw)), collapse = ", "),
         ". Return this output; the names need mapping.")
  vote_col <- intersect(c("epvote", "ep_vote", "vote_ep", "vote"), names(ches_raw))[1]
  if (is.na(vote_col)) stop("No vote-share column (epvote/ep_vote/vote_ep/vote) in CHES.")  # FIX
  cat("vote-share column used for the main-party rule:", vote_col,
      if (vote_col == "vote") "(NATIONAL election share -- EP share not found)" else "", "\n")

  ches <- ches_raw |>
    dplyr::transmute(cty = unname(CHES_CTY[country]), year = num(year),
                     party_id = num(party_id), party = party,
                     family = num(family), vote = num(.data[[vote_col]])) |>
    dplyr::filter(year %in% c(2019, 2024), cty %in% EU27)
  if (!nrow(ches)) stop("No CHES rows mapped to EU countries for 2019/2024. The `country` ",
                        "codes are not numeric as assumed: print unique(ches_raw$country) ",
                        "and return it.")

  cat("\nparties per country x wave (check codes: a country with 0 is a mapping error):\n")
  print(as.data.frame(table(ches$cty, ches$year)) |>
          tidyr::pivot_wider(names_from = Var2, values_from = Freq), row.names = FALSE)


  # ========================= 2. CABINET / SUPPORT EVENTS =======================
  # Party-level spells. end = NA means ongoing on 2024-06-06.
  # verified = TRUE : checked against the cited source in this project (25 Sep 2026)
  # verified = FALSE: from recollection -> CHECK against ParlGov before deposit.
  # party_rx matches the CHES `party` abbreviation (case-insensitive); section 3
  # prints every match so a wrong pattern is visible.

  events <- tibble::tribble(
    ~cty,          ~party_rx,                    ~type,     ~start,       ~end,         ~verified, ~source,
    "Austria",     "^FP[O\u00d6]",               "cabinet", "2017-12-18", "2019-05-22", TRUE,  "EJPR Political Data Yearbook 2019 (Austria): coalition ended 22 May 2019",
    "Finland",     "^PS$|^Finns|^Perus",          "cabinet", "2015-05-29", "2017-06-13", TRUE,  "Sipila cabinet: Finns Party expelled 13 June 2017",
    "Finland",     "^PS$|^Finns|^Perus",          "cabinet", "2023-06-20", NA,           FALSE, "Orpo cabinet (verify start date)",
    "Latvia",      "^NA$|TB/LNNK|Nacion",         "cabinet", "2011-10-25", "2023-09-15", FALSE, "End verified (Saeima, 15 Sep 2023, Silina cabinet); start: verify continuity",
    "Croatia",     "^DPS?$|Domovin",              "cabinet", "2024-05-17", NA,           TRUE,  "Plenkovic III confidence vote 17 May 2024 (OSW)",
    "Denmark",     "^DF$|^DFP$|^O$",              "support", "2015-06-28", "2019-06-27", FALSE, "Lokke II/III supported by DF; basis for formal_support = annual written, published budget agreements (finanslovsaftaler); end = Frederiksen I 27 Jun 2019",
    "Sweden",      "^SD$",                        "support", "2022-10-18", NA,           FALSE, "Tido agreement (14 Oct 2022); Kristersson cabinet 18 Oct 2022",
    "Estonia",     "^EKRE$",                      "cabinet", "2019-04-29", "2021-01-26", TRUE,  "Ratas II sworn in 29 Apr 2019 (ERR); end irrelevant to both reference dates",
    "Poland",      "^PiS$|Solidarn|Suweren",      "cabinet", "2015-11-16", "2023-12-13", FALSE, "Szydlo/Morawiecki; Tusk III sworn 13 Dec 2023",
    "Bulgaria",    "VMRO|IMRO|BMRO|NFSB",         "cabinet", "2017-05-04", "2021-05-12", FALSE, "Borisov III (United Patriots). CHES codes BMRO and NFSB as family 2: not PRR, cannot set position",
    "Hungary",     "Fidesz",                      "cabinet", "2010-05-29", NA,           FALSE, "Orban II-V",
    "Italy",       "^LN$|^Lega",                  "cabinet", "2018-06-01", "2019-09-05", FALSE, "Conte I",
    "Italy",       "^LN$|^Lega",                  "cabinet", "2021-02-13", NA,           FALSE, "Draghi, then Meloni",
    "Italy",       "FdI|Fratelli",                "cabinet", "2022-10-22", NA,           FALSE, "Meloni",
    "Slovakia",    "^SNS$",                       "cabinet", "2016-03-23", "2020-03-21", FALSE, "Fico III / Pellegrini",
    "Slovakia",    "^SNS$",                       "cabinet", "2023-10-25", NA,           FALSE, "Fico IV",
    "Greece",      "ANEL",                        "cabinet", "2015-01-27", "2019-01-13", FALSE, "Tsipras I/II. ANEL absent from CHES 2019 (collapsed; ~0.8% EP 2019): matches NONE by design, cannot be main PRR",
    "Slovenia",    "^SDS$",                       "cabinet", "2020-03-13", "2022-06-01", FALSE, "Jansa III (matters only if SDS is family 1)",
    "Netherlands", "^PVV$",                       "cabinet", "2024-07-02", NA,           FALSE, "Schoof cabinet -- AFTER the 2024 reference date"
  ) |> dplyr::mutate(start = as.Date(start), end = as.Date(end))

  cat("\nEvents needing ParlGov verification before deposit:",
      sum(!events$verified), "of", nrow(events), "\n")


  # ---- 2b. Manual EP vote shares (official results) ---------------------------
  # Fill ONLY for parties that Guard 1 lists AND that were on that EP ballot.
  # Source every entry (official national election commission results).
  manual_ep <- tibble::tribble(
    ~cty, ~year, ~party_rx, ~epvote, ~source
    # "Bulgaria", 2019, "IMRO|VMRO", 0.00, "CIK official EP 2019 results (enter value)"
  )
  if (nrow(manual_ep)) for (i in seq_len(nrow(manual_ep))) {
    m <- manual_ep[i, ]
    hit <- ches$cty == m$cty & ches$year == m$year &
      stringr::str_detect(ches$party, stringr::regex(m$party_rx, TRUE))
    if (sum(hit) != 1) stop("manual_ep row ", i, " matches ", sum(hit), " parties; must be 1")
    ches$vote[hit] <- m$epvote
    cat("manual EP share applied:", m$cty, m$year, ches$party[hit], "=", m$epvote, "\n")
  }


  # ========================= 3. PRR SETS UNDER BOTH SCHEMES ====================

  hdr("3. PRR PARTIES: FIXED vs WAVE-MATCHED CLASSIFICATION")
  fam24 <- ches |> dplyr::filter(year == 2024) |> dplyr::select(cty, party_id, fam24 = family)
  fam19 <- ches |> dplyr::filter(year == 2019) |> dplyr::select(cty, party_id, fam19 = family)

  cls <- ches |>
    dplyr::left_join(fam24, by = c("cty", "party_id")) |>
    dplyr::left_join(fam19, by = c("cty", "party_id")) |>
    dplyr::mutate(fam_fixed = dplyr::coalesce(fam24, fam19),
                  prr_wave  = family == 1,
                  prr_fixed = fam_fixed == 1)

  cat("Parties whose family CHANGES between CHES 2019 and 2024 (either wave = 1).\n",
      "These are the cells where the scheme choice changes the outcome definition:\n")
  print(as.data.frame(cls |>
    dplyr::filter(!is.na(fam19), !is.na(fam24), fam19 != fam24, fam19 == 1 | fam24 == 1) |>
    dplyr::distinct(cty, party_id, party, fam19, fam24)), row.names = FALSE)

  # Cross-check: every 2024 PRR line in the paper's linkage is family 1 here
  link <- readr::read_csv(file.path(paths$output, "05b_prr_linkage.csv"),
                          col_types = readr::cols(.default = "c"), na = "",
                          show_col_types = FALSE)
  p24 <- link |> dplyr::filter(num(prr) == 1) |> dplyr::distinct(cty, ches_party)
  chk <- p24 |> dplyr::left_join(cls |> dplyr::filter(year == 2024) |>
                                   dplyr::select(cty, ches_party = party, family),
                                 by = c("cty", "ches_party"))
  cat("\n2024 linkage PRR lines not found as family 1 in the trend file:\n")
  print(as.data.frame(chk |> dplyr::filter(is.na(family) | family != 1)), row.names = FALSE)
  cat("(empty = consistent; non-empty = abbreviation mismatch or a file-version difference)\n")


  # Every event must match a party in the CHES file; report what each matches
  # and whether that party is PRR. An event matching nothing is a pattern error
  # (this is how the Croatia 'DPS' abbreviation was caught).
  cat("\nEvent-to-CHES matches (fixed scheme):\n")
  ev_chk <- purrr::map_dfr(seq_len(nrow(events)), function(i) {
    e <- events[i, ]
    m <- cls |> dplyr::filter(cty == e$cty,
                              stringr::str_detect(party, stringr::regex(e$party_rx, TRUE))) |>
      dplyr::distinct(party, fam_fixed)
    tibble::tibble(cty = e$cty, party_rx = e$party_rx, type = e$type,
                   matched = if (nrow(m)) paste(unique(m$party), collapse = "/") else "NONE",
                   is_prr  = if (nrow(m)) any(m$fam_fixed == 1, na.rm = TRUE) else NA)
  })
  print(as.data.frame(ev_chk), row.names = FALSE)
  if (any(ev_chk$matched == "NONE" & ev_chk$cty != "Greece"))
    cat("WARNING: events matching no CHES party -- fix party_rx before using the table.\n")

  # ---- Guard 1: PRR parties with no EP vote share ------------------------------
  # which.max() ranks a missing share last, so a family-1 party with NA epvote
  # can never be selected as main party. List every such case: any row here in
  # a country-wave that had that party on the EP ballot is a potential miscoding.
  cat("\nFamily-1 parties (fixed scheme) with MISSING EP vote share:\n")
  print(as.data.frame(cls |> dplyr::filter(prr_fixed %in% TRUE, is.na(vote)) |>
                        dplyr::select(cty, year, party_id, party, fam19, fam24)), row.names = FALSE)
  cat("(each row: was this party on that year's EP ballot? if yes, enter its official\n",
      " EP share by hand in section 2b and re-run; if no, it is correctly ignored)\n")

  # ---- Guard 2: full party list wherever an event matched no PRR party ---------
  flag_cty <- unique(ev_chk$cty[is.na(ev_chk$is_prr) | !ev_chk$is_prr])
  for (k in flag_cty) {
    cat("\n--- all CHES parties,", k, "---\n")
    print(as.data.frame(cls |> dplyr::filter(cty == k) |>
                          dplyr::arrange(year, dplyr::desc(vote)) |>
                          dplyr::select(year, party_id, party, family, fam_fixed, epvote = vote)), row.names = FALSE)
  }


  # ========================= 4. POSITION ON THE REFERENCE DATES ================

  hdr("4. POSITION CODING")
  status_on <- function(k, pty, ref) {
    ev <- events |> dplyr::filter(cty == k,
                                  stringr::str_detect(pty, stringr::regex(party_rx, TRUE)))
    if (!nrow(ev)) return(c(cab = 0, sup = 0, left = 0, left_date = NA_real_))
    on   <- ev$start <= ref & (is.na(ev$end) | ev$end >= ref)
    cab  <- any(on & ev$type == "cabinet")
    sup  <- any(on & ev$type == "support")
    past <- ev$type == "cabinet" & !is.na(ev$end) & ev$end < ref & ev$end >= ref - 730
    c(cab = cab, sup = sup, left = any(past) & !cab,
      left_date = if (any(past)) as.numeric(max(ev$end[past])) else NA_real_)
  }

  code_scheme <- function(flag) {
    purrr::map_dfr(EU27, function(k) purrr::map_dfr(c(2019, 2024), function(w) {
      ref <- as.Date(REF[as.character(w)])
      p <- cls |> dplyr::filter(cty == k, year == w, .data[[flag]] %in% TRUE)
      if (!nrow(p))
        return(tibble::tibble(cty = k, wave = w, n_prr = 0L, prr_main_party = NA_character_,
                              prr_main_vote = NA_real_, main_rule = NA_character_,
                              in_cabinet = NA, formal_support = NA,
                              left_within_24m = NA, left_cabinet_date = NA_character_,
                              any_prr_in_cabinet = NA, position = "no PRR option"))
      st <- purrr::map(p$party, ~ status_on(k, .x, ref))
      main <- which.max(dplyr::coalesce(p$vote, -1))
      s <- st[[main]]
      pos <- dplyr::case_when(s["cab"] == 1 ~ "cabinet", s["sup"] == 1 ~ "formal_support",
                              s["left"] == 1 ~ "left_24m", TRUE ~ "opposition")
      tibble::tibble(cty = k, wave = w, n_prr = nrow(p),
                     prr_main_party = p$party[main], prr_main_vote = p$vote[main],
                     # CHANGE: make an arbitrary main-party pick visible
                     main_rule = if (all(is.na(p$vote))) "ARBITRARY (no EP share)" else "largest EP share",
                     in_cabinet = as.integer(s["cab"]), formal_support = as.integer(s["sup"]),
                     left_within_24m = as.integer(s["left"]),
                     left_cabinet_date = if (is.na(s["left_date"])) NA_character_ else
                       as.character(as.Date(s["left_date"], origin = "1970-01-01")),
                     any_prr_in_cabinet = as.integer(any(purrr::map_dbl(st, "cab") == 1)),
                     position = pos)
    }))
  }

  tab <- dplyr::left_join(
    code_scheme("prr_fixed"),
    code_scheme("prr_wave") |> dplyr::select(cty, wave, main_wave = prr_main_party,
                                             position_wave = position,
                                             cabinet_wave = in_cabinet),
    by = c("cty", "wave")) |>
    dplyr::mutate(ref_date = REF[as.character(wave)], .after = wave)

  print(as.data.frame(tab |> dplyr::select(cty, wave, prr_main_party, prr_main_vote, main_rule,
                                           position, any_prr_in_cabinet,
                                           main_wave, position_wave)), row.names = FALSE)
  if (any(tab$main_rule == "ARBITRARY (no EP share)", na.rm = TRUE))
    cat("\nWARNING: main PRR party chosen arbitrarily in the rows flagged above --",
        "enter EP shares in section 2b.\n")

  # ---- Switchers under each scheme (P1 identification) ------------------------
  sw <- function(v) tab |> dplyr::group_by(cty) |>
    dplyr::summarise(a = dplyr::first(.data[[v]][wave == 2019]),
                     b = dplyr::first(.data[[v]][wave == 2024]), .groups = "drop") |>
    dplyr::filter(!is.na(a), !is.na(b), a != b)
  cat("\nP1 switchers, FIXED scheme (main party in cabinet 2019 vs 2024):\n")
  print(as.data.frame(sw("in_cabinet")), row.names = FALSE)
  cat("\nP1 switchers, WAVE scheme:\n"); print(as.data.frame(sw("cabinet_wave")), row.names = FALSE)
  cat("\nP2 position changes, FIXED scheme:\n"); print(as.data.frame(sw("position")), row.names = FALSE)

  readr::write_csv(tab, file.path(paths$output, "26_prr_position_coding.csv"), na = "")
  hdr("DONE -- return the full console output (logs/26_console.txt)")
  sink(); close(con)
})


# =============================================================================
# STEP 28_bayes  <-  R/28_bayes_hierarchical.R
# needs: steps 23 + 26; brms + CmdStan or rstan
# =============================================================================
STEP("28_bayes", {
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
})


# =============================================================================
# STEP 28b_diagnostics  <-  R/28b_onestage_diagnostics.R
# needs: step 28
# =============================================================================
STEP("28b_diagnostics", {
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
})


# =============================================================================
# STEP 15b_audit  <-  R/15b_verify_extensions.R
# needs: discovery only
# =============================================================================
STEP("15b_audit", {
  # =============================================================================
  # 15b_verify_extensions.R  -- DISCOVERY ONLY. Supersedes 15. Run in a FRESH
  # R session (script 15 errored with its sink() still open).
  #
  # Changes relative to 15 (declared, none is a design change):
  #   B  "99. Missing" was counted as a region, inflating n_regions and creating
  #      spurious tiny min_cells. Only syntactically valid NUTS-2 codes now
  #      count. Adds: missing-region share, cells of CLASSIFIED VOTERS (the
  #      units the pooled model actually uses), code/country prefix mismatches,
  #      and the codes that identify the NUTS vintage. Fixes the sprintf bug.
  #   B0 NEW: q6 system-NA check. RUN_ANALYSIS defined
  #      classified = !q6_raw %in% SPECIAL, which is TRUE for NA; if any
  #      non-voter carries NA in q6, stage one miscoded them as non-PRR voters.
  #      (Now fixed in RUN_ANALYSIS; this check tells you whether it mattered.)
  #   C  ZA7581 read with encoding fallback (declared -> CP1252 -> latin1),
  #      plus a mojibake count so any label damage is visible, not silent.
  #   D  unchanged in substance; now reachable.
  #   Every part runs inside tryCatch so one failure no longer kills the rest.
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - paths overridable by environment variables.
  #   - B0 now also prints the DECLARED user-missing codes of q6: haven's
  #     zap_labels() turns them into NA, which is how they reach q6_is_NA.
  #   - prefix-mismatch counts use na.rm (a country absent from PREFIX gave NA).
  # =============================================================================

  while (sink.number() > 0) sink()          # clear any sink left open by 15

  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  paths <- list(
    raw_ees24 = file.path(RAW, "ZA8868_v1-0-0.sav"),
    raw_ees19 = file.path(RAW, "ZA7581_v2-0-1.sav"),
    raw_ches  = file.path(RAW, "1999-2024_CHES_dataset_meansV2.csv"),
    logs      = file.path(PROJ, "logs"))
  if (!dir.exists(paths$logs)) dir.create(paths$logs, recursive = TRUE)

  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(tibble); library(purrr)
    library(stringr); library(readr); library(tidyr)
  })
  options(width = 120, max.print = 100000)

  log_file <- file.path(paths$logs, "15b_verify_console.txt")
  con <- file(log_file, open = "wt"); sink(con, split = TRUE)

  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")
  guard <- function(label, expr) tryCatch(expr, error = function(e)
    cat("\n*** PART", label, "FAILED:", conditionMessage(e), "***\n"))

  inventory <- function(df) tibble::tibble(
    var   = names(df),
    label = purrr::map_chr(df, \(x) {
      l <- attr(x, "label", exact = TRUE); if (is.null(l)) "" else as.character(l)[1]}))
  show_matches <- function(inv, pattern, what) {
    m <- inv |> dplyr::filter(stringr::str_detect(tolower(var), pattern) |
                                stringr::str_detect(tolower(label), pattern))
    cat(sprintf("\n-- %s (pattern: %s) -- %d match(es)\n", what, pattern, nrow(m)))
    if (nrow(m)) print(as.data.frame(m), row.names = FALSE) else
      cat("   NONE FOUND\n")
    invisible(m)
  }
  show_vlabs <- function(df, v, n = 14) {
    vl <- attr(df[[v]], "labels", exact = TRUE)
    cat(sprintf("\nvalue labels for %s (%d declared):\n", v, length(vl)))
    if (is.null(vl)) cat("   none\n") else print(head(vl, n))
  }

  SPECIAL <- c(89, 90, 91, 96, 98)
  EST19 <- c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark",
             "Estonia","France","Germany","Greece","Latvia","Lithuania",
             "Netherlands","Poland","Portugal","Romania","Slovenia","Spain","Sweden")
  # NUTS country prefixes (note Greece = EL)
  PREFIX <- c(Austria="AT",Belgium="BE",Bulgaria="BG",Croatia="HR",Cyprus="CY",
              `Czech Republic`="CZ",Denmark="DK",Estonia="EE",Finland="FI",
              France="FR",Germany="DE",Greece="EL",Hungary="HU",Ireland="IE",
              Italy="IT",Latvia="LV",Lithuania="LT",Luxembourg="LU",Malta="MT",
              Netherlands="NL",Poland="PL",Portugal="PT",Romania="RO",
              Slovakia="SK",Slovenia="SI",Spain="ES",Sweden="SE")


  # ===================== B0 + B. EES 2024 REGION, CORRECTED ====================

  guard("B", {
    hdr("B0. q6 SYSTEM-NA CHECK (stage-one coding integrity)")
    e24 <- haven::read_sav(paths$raw_ees24, user_na = TRUE,
                           col_select = c(country, d12_NUTSII, q5, q6))
    cat("q6 declared user-missing codes:",                       # CHANGE: new line
        paste(c(attr(e24$q6, "na_values"), attr(e24$q6, "na_range")), collapse = ", "),
        "(zap_labels() turns these into NA)\n")
    q6  <- as.numeric(haven::zap_labels(e24$q6))
    q5  <- as.numeric(haven::zap_labels(e24$q5))
    cat("q6 system NA:", sum(is.na(q6)), "of", length(q6), "\n")
    print(table(q5_turnout = q5, q6_is_NA = is.na(q6), useNA = "ifany"))
    cat("If q6_is_NA = TRUE has ANY count, those respondents were coded\n")
    cat("classified = TRUE, prr = 0 in the conference RUN_ANALYSIS. The\n")
    cat("replication-package RUN_ANALYSIS excludes them and prints how many.\n")

    hdr("B. ZA8868 REGIONS (valid NUTS-2 codes only)")
    d <- tibble::tibble(
      cty  = as.character(haven::as_factor(e24$country)),
      code = as.character(haven::as_factor(e24$d12_NUTSII)),
      voter = !is.na(q6) & !q6 %in% SPECIAL) |>
      dplyr::mutate(code  = stringr::str_trim(code),
                    valid = stringr::str_detect(code, "^[A-Z]{2}[0-9A-Z]{2}$"),
                    code  = ifelse(valid, code, NA_character_),
                    mismatch = valid & substr(code, 1, 2) != PREFIX[cty])

    cat("non-code values treated as missing:\n")
    print(table(as.character(haven::as_factor(e24$d12_NUTSII))[!d$valid]))

    resp <- d |> dplyr::filter(valid) |> dplyr::count(cty, code, name = "n_resp")
    vote <- d |> dplyr::filter(valid, voter) |> dplyr::count(cty, code, name = "n_vot")
    cells <- resp |> dplyr::left_join(vote, by = c("cty","code")) |>
      dplyr::mutate(n_vot = tidyr::replace_na(n_vot, 0L))

    tab <- d |> dplyr::group_by(cty) |>
      dplyr::summarise(N = dplyr::n(), pct_miss = round(100*mean(!valid), 1),
                       mismatches = sum(mismatch, na.rm = TRUE),   # FIX: na.rm
                       .groups = "drop") |>
      dplyr::left_join(cells |> dplyr::group_by(cty) |>
                         dplyr::summarise(n_regions = dplyr::n(),
                                          min_resp = min(n_resp), med_resp = stats::median(n_resp),
                                          min_vot = min(n_vot), med_vot = stats::median(n_vot),
                                          reg_vot30 = sum(n_vot >= 30), .groups = "drop"),
                       by = "cty") |>
      dplyr::mutate(in_est = cty %in% EST19) |>
      dplyr::arrange(n_regions)
    print(as.data.frame(tab), row.names = FALSE)

    e <- tab |> dplyr::filter(in_est)
    cat(sprintf("\nESTIMATION SAMPLE (k = 19): single-region %d (%s); two-region %d (%s)\n",
                sum(e$n_regions == 1, na.rm = TRUE),
                paste(e$cty[e$n_regions %in% 1], collapse = ", "),
                sum(e$n_regions == 2, na.rm = TRUE),
                paste(e$cty[e$n_regions %in% 2], collapse = ", ")))
    cat(sprintf("regions with >= 30 classified voters: %d of %d\n",
                sum(e$reg_vot30, na.rm = TRUE), sum(e$n_regions, na.rm = TRUE)))
    cat("total prefix mismatches (misassigned codes):", sum(tab$mismatches), "\n")

    hdr("B2. NUTS VINTAGE MARKERS (compare against Eurostat NUTS history)")
    cat("Croatia HR02/HR05/HR06 => NUTS 2021+; HR04 => NUTS 2016 or earlier.\n")
    cat("Hungary HU11/HU12 => 2016+; HU10 => 2013. Poland PL91/PL92 => 2016+.\n")
    cat("Lithuania LT01/LT02 => 2016+.\n\n")
    for (k in c("Croatia","Hungary","Poland","Lithuania","Ireland","Germany","Belgium"))
      cat(sprintf("%-10s %s\n", k, paste(sort(unique(cells$code[cells$cty == k])),
                                         collapse = " ")))
    cat("\nGermany/Belgium codes printed because EQI reports them at NUTS-1:\n")
    cat("the first three characters give the NUTS-1 region to aggregate to.\n")
  })


  # ================= C. EES 2019 WITH ENCODING FALLBACK ========================

  guard("C", {
    hdr("C. ZA7581: READ WITH ENCODING FALLBACK")
    ees19 <- NULL; used <- NA
    for (enc in list(NULL, "CP1252", "latin1")) {
      nm <- if (is.null(enc)) "declared" else enc
      res <- tryCatch(haven::read_sav(paths$raw_ees19, user_na = TRUE, encoding = enc),
                      error = function(e) e)
      if (inherits(res, "error")) {
        cat("encoding", nm, "failed:", conditionMessage(res), "\n")
      } else { ees19 <- res; used <- nm; break }
    }
    if (is.null(ees19)) stop("all encodings failed; request the Stata (.dta) release")
    cat("read OK with encoding:", used, "| vars:", ncol(ees19),
        "| rows:", nrow(ees19), "\n")

    # label-damage audit: UTF-8 text decoded as single-byte shows these patterns
    all_labs <- unlist(c(
      purrr::map(ees19, \(x) attr(x, "label", exact = TRUE)),
      purrr::map(ees19, \(x) names(attr(x, "labels", exact = TRUE)))))
    moj <- sum(stringr::str_detect(all_labs, "\u00c3|\u00c2|\u00c5|\u00d0"), na.rm = TRUE)
    cat(sprintf("labels with mojibake signatures: %d of %d\n", moj, length(all_labs)))
    cat("Numeric codes are unaffected; only text labels would be damaged.\n")

    inv19 <- inventory(ees19)
    sat <- show_matches(inv19, "satisf",           "satisfaction items")
    vot <- show_matches(inv19, "vote|voted|party", "vote / turnout items")
    show_matches(inv19, "nuts|region",             "region")
    show_matches(inv19, "weight",                  "weights")
    show_matches(inv19, "age|birth",               "age")
    show_matches(inv19, "gender|sex",              "gender")
    show_matches(inv19, "educ",                    "education")
    show_matches(inv19, "urban|rural|community",   "urbanisation")
    show_matches(inv19, "left|right|placement",    "left-right")
    show_matches(inv19, "country",                 "country identifier")

    cat("\nVALUE LABELS (confirm DK/refusal codes; do not assume 2024's 98):\n")
    for (v in head(sat$var, 4)) show_vlabs(ees19, v)
    for (v in head(vot$var, 6)) show_vlabs(ees19, v, n = 10)
  })


  # ======================== D. CHES: ITN CODING ================================

  guard("D", {
    hdr("D. CHES TREND: ITN (BULGARIA) AND THE BULGARIAN PARTY LIST")
    ches <- readr::read_csv(paths$raw_ches, show_col_types = FALSE, na = "")
    cat("columns:", paste(head(names(ches), 25), collapse = ", "), "\n\n")
    pick <- intersect(c("year","country","party_id","party","family"), names(ches))
    itn <- ches |> dplyr::filter(dplyr::if_any(dplyr::where(is.character),
                                               \(x) stringr::str_detect(toupper(dplyr::coalesce(x, "")),
                                                                        "^ITN$|SUCH A PEOPLE")))
    cat("ITN rows:\n")
    print(as.data.frame(itn |> dplyr::select(dplyr::all_of(pick))), row.names = FALSE)
    if ("country" %in% names(ches) && "year" %in% names(ches)) {
      cat("\nall Bulgarian parties, 2019 and 2024 (country code 20 in CHES):\n")
      print(as.data.frame(ches |> dplyr::filter(country == 20, year %in% c(2019, 2024)) |>
                            dplyr::select(dplyr::all_of(pick)) |> dplyr::arrange(year, family)),
            row.names = FALSE)
    }
  })

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 16_audit  <-  R/16_audit_linkage_and_2019.R
# needs: discovery only
# =============================================================================
STEP("16_audit", {
  # =============================================================================
  # 16_audit_linkage_and_2019.R  -- DISCOVERY / AUDIT ONLY. No derived files.
  #
  #   A. 2024 linkage audit: every CHES 2024 family-1 party vs the PRR set in
  #      05_prr_linkage.csv. Triggered by ITN (family 1 in CHES, absent from the
  #      PRR set). Prints the Bulgarian linkage rows in full so you can see
  #      whether ITN's exclusion was a failed match or a coded decision.
  #   B. Region-missingness selection test (2024): is a missing NUTS code
  #      related to dissatisfaction or PRR voting within country?
  #   C. EES 2019: remaining variables (gender, urbanisation, government
  #      approval, age format, education), Q7 special codes against Q6 by
  #      country, country coverage, region_NUTS2 vintage and missingness.
  #      NOTE: region_NUTS2 is read through its LABELS here, which in ZA7581
  #      are region names; script 17 part B re-reads the stored codes.
  #   D. Label repair test for the CP1252-read file.
  #
  # Fresh session. Output: console + logs/16_audit_console.txt
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - paths overridable by environment variables; logs dir created if absent
  #     (file() failed on a fresh checkout).
  # =============================================================================

  while (sink.number() > 0) sink()
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  paths <- list(
    raw_ees24 = file.path(RAW, "ZA8868_v1-0-0.sav"),
    raw_ees19 = file.path(RAW, "ZA7581_v2-0-1.sav"),
    raw_ches  = file.path(RAW, "1999-2024_CHES_dataset_meansV2.csv"),
    derived   = file.path(PROJ, "derived"),
    output    = file.path(PROJ, "output"),
    logs      = file.path(PROJ, "logs"))
  if (!dir.exists(paths$logs)) dir.create(paths$logs, recursive = TRUE)   # FIX

  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(tibble); library(purrr)
    library(stringr); library(readr); library(tidyr)
  })
  options(width = 150, max.print = 100000)
  con <- file(file.path(paths$logs, "16_audit_console.txt"), open = "wt")
  sink(con, split = TRUE)

  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")
  guard <- function(label, expr) tryCatch(expr, error = function(e)
    cat("\n*** PART", label, "FAILED:", conditionMessage(e), "***\n"))

  # UTF-8 text that was decoded as CP1252: re-encode to CP1252 bytes and
  # reinterpret those bytes as UTF-8. Returns input unchanged where it fails.
  repair <- function(s) {
    out <- iconv(s, from = "UTF-8", to = "CP1252")
    Encoding(out) <- "UTF-8"
    ok <- !is.na(out) & validUTF8(out)
    ifelse(ok, out, s)
  }

  EST19 <- c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark",
             "Estonia","France","Germany","Greece","Latvia","Lithuania",
             "Netherlands","Poland","Portugal","Romania","Slovenia","Spain","Sweden")


  # ========================= A. 2024 LINKAGE AUDIT =============================

  guard("A", {
    hdr("A. CHES 2024 FAMILY-1 PARTIES vs THE BASELINE PRR SET")
    ches <- readr::read_csv(paths$raw_ches, show_col_types = FALSE, na = "")
    link <- readr::read_csv(file.path(paths$output, "05_prr_linkage.csv"),
                            show_col_types = FALSE, na = "")
    cat("05_prr_linkage.csv columns:", paste(names(link), collapse = ", "), "\n")
    cat("rows:", nrow(link), "\n\n")

    f1 <- ches |> dplyr::filter(year == 2024, family == 1) |>
      dplyr::select(country, party_id, party, vote) |> dplyr::arrange(country)
    cat("CHES 2024 family-1 parties:", nrow(f1), "\n")
    print(as.data.frame(f1), row.names = FALSE)

    # match on the CHES abbreviation if the linkage file carries one
    abbr_col <- intersect(c("ches_party","party","ches_abbrev","party_ches"),
                          names(link))[1]
    if (!is.na(abbr_col)) {
      in_link <- link |> dplyr::mutate(key = toupper(.data[[abbr_col]]))
      aud <- f1 |> dplyr::mutate(key = toupper(party)) |>
        dplyr::left_join(in_link |> dplyr::select(key, dplyr::any_of(
          c("cty","q6_code","prr","party_label"))), by = "key")
      cat("\nfamily-1 parties and their linkage status (prr NA = no ballot match):\n")
      print(as.data.frame(aud), row.names = FALSE)
      cat("\nFAMILY-1 PARTIES NOT CODED PRR = 1:\n")
      print(as.data.frame(aud |> dplyr::filter(is.na(prr) | prr != 1)),
            row.names = FALSE)
    } else {
      cat("\nno CHES abbreviation column found in the linkage file;",
          "compare the two lists above by eye.\n")
    }

    cat("\nBULGARIA, every linkage row:\n")
    cty_col <- intersect(c("cty","country"), names(link))[1]
    print(as.data.frame(link |> dplyr::filter(.data[[cty_col]] %in%
                                                c("Bulgaria", 20, "BG"))),
          row.names = FALSE)
    cat("\nEES ballot code 10001 (ITN) in linkage file:\n")
    print(as.data.frame(link |> dplyr::filter(q6_code == 10001)), row.names = FALSE)
  })


  # ================= B. REGION-MISSINGNESS SELECTION TEST ======================

  guard("B", {
    hdr("B. IS A MISSING NUTS CODE RELATED TO THE OUTCOME OR THE PREDICTOR?")
    d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
      dplyr::mutate(dissatisfied = dplyr::case_when(
        dissat_nat %in% c(3,4) ~ 1L, dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_))
    reg <- haven::read_sav(paths$raw_ees24, user_na = TRUE,
                           col_select = c(resp_id, d12_NUTSII))
    reg <- tibble::tibble(resp_id = reg$resp_id,
                          code = as.character(haven::as_factor(reg$d12_NUTSII))) |>
      dplyr::mutate(miss_reg = !stringr::str_detect(code, "^[A-Z]{2}[0-9A-Z]{2}$"))
    dd <- d |> dplyr::left_join(reg, by = "resp_id")
    cat("unmatched resp_id after join:", sum(is.na(dd$miss_reg)), "\n")

    v <- dd |> dplyr::filter(classified)
    bycty <- v |> dplyr::group_by(cty) |>
      dplyr::summarise(pct_miss = round(100*mean(miss_reg), 1),
                       prr_miss = round(100*mean(prr[miss_reg]), 1),
                       prr_obs  = round(100*mean(prr[!miss_reg]), 1),
                       dis_miss = round(100*mean(dissatisfied[miss_reg], na.rm = TRUE), 1),
                       dis_obs  = round(100*mean(dissatisfied[!miss_reg], na.rm = TRUE), 1),
                       .groups = "drop") |>
      dplyr::filter(pct_miss > 0) |> dplyr::arrange(dplyr::desc(pct_miss))
    cat("\nclassified voters, by country (PRR and dissatisfied shares, %):\n")
    print(as.data.frame(bycty), row.names = FALSE)

    m <- stats::lm(as.numeric(miss_reg) ~ dissatisfied + prr + factor(cty),
                   data = v |> dplyr::filter(!is.na(dissatisfied)))
    ct <- summary(m)$coefficients[c("dissatisfied","prr"), ]
    cat("\nLPM, missing region on dissatisfied + prr + country FE (voters):\n")
    print(round(ct, 4))
    cat("Small, insignificant coefficients = missingness ignorable for the\n")
    cat("regional design. Otherwise the regional sample is selected.\n")
  })


  # ===================== C. EES 2019 REMAINING VARIABLES =======================

  guard("C", {
    hdr("C. ZA7581: REMAINING VARIABLES AND CODES")
    e19 <- haven::read_sav(paths$raw_ees19, user_na = TRUE, encoding = "CP1252")
    inv <- tibble::tibble(var = names(e19),
                          label = purrr::map_chr(e19, \(x) {
                            l <- attr(x, "label", exact = TRUE); if (is.null(l)) "" else repair(l)[1]}))

    cat("\nALL D-, h- and demographic-block variables:\n")
    print(as.data.frame(inv |> dplyr::filter(
      stringr::str_detect(var, "^(D|d|h|H)[0-9A-Za-z_]") |
        stringr::str_detect(tolower(label), "govern|approv|disapprov|interest|trust"))),
      row.names = FALSE)

    vl <- function(v, n = 12) {
      if (!v %in% names(e19)) return(cat("  [", v, "absent ]\n"))
      l <- attr(e19[[v]], "labels", exact = TRUE)
      cat(sprintf("\n%s (%d labels):\n", v, length(l)))
      if (length(l)) { names(l) <- repair(names(l)); print(head(l, n)) }
      else print(summary(as.numeric(e19[[v]])))
    }
    for (v in c("hAge","EDU","D2_1","D3","D8","D12","hCountry","region_NUTS2"))
      vl(v)

    cty <- as.character(haven::as_factor(e19$hCountry))
    cat("\ncountries and N:\n"); print(table(cty))

    q6 <- as.numeric(haven::zap_labels(e19$Q6))
    q7 <- as.numeric(haven::zap_labels(e19$Q7))
    cat("\nQ7 special codes against Q6 turnout (all countries):\n")
    print(table(Q6 = q6, Q7_special = ifelse(q7 %in% c(0,90,96,98,99), q7, "party"),
                useNA = "ifany"))
    cat("\nany Q7 codes in 1..89 range that are NOT party codes? (party codes\n")
    cat("appear to start at 101) -- distinct Q7 values < 100:\n")
    print(sort(unique(q7[q7 < 100])))

    cat("\nQ3 (national SWD) distribution incl. 98/99:\n")
    print(table(as.numeric(haven::zap_labels(e19$Q3)), useNA = "ifany"))

    r2 <- as.character(haven::as_factor(e19$region_NUTS2))
    valid <- stringr::str_detect(r2, "^[A-Z]{2}[0-9A-Z]{2}$")
    cat("\nregion_NUTS2: valid share", round(100*mean(valid), 1),
        "% | non-code values:\n")
    print(head(sort(table(r2[!valid]), decreasing = TRUE), 10))
    cat("\nmissing share by country (%):\n")
    print(round(100*tapply(!valid, cty, mean), 1))
    cat("\nNUTS vintage markers, 2019 (HR04 => NUTS 2016; HR02/HR05/HR06 => 2021):\n")
    for (k in c("Croatia","Hungary","Poland","Lithuania","Ireland"))
      cat(sprintf("%-10s %s\n", k, paste(sort(unique(r2[valid & cty == k])),
                                         collapse = " ")))
  })


  # ========================== D. LABEL REPAIR TEST =============================

  guard("D", {
    hdr("D. LABEL REPAIR TEST (Q7 party labels)")
    e19 <- haven::read_sav(paths$raw_ees19, user_na = TRUE, encoding = "CP1252",
                           col_select = c(Q7))
    l <- attr(e19$Q7, "labels", exact = TRUE)
    bad <- stringr::str_detect(names(l), "\u00c3|\u00c2|\u00c5|\u00d0")
    cat("damaged labels before:", sum(bad), "| after repair:",
        sum(stringr::str_detect(repair(names(l)), "\u00c3|\u00c2|\u00c5|\u00d0")), "\n\n")
    print(tibble::tibble(code = unname(l[bad]), before = names(l)[bad],
                         after = repair(names(l)[bad])) |> head(20) |> as.data.frame(),
          row.names = FALSE)
  })

  hdr("DONE -- return the full console output")
  sink(); close(con)
})


# =============================================================================
# STEP 17_audit  <-  R/17_ballot_regions19_linkage19.R
# needs: discovery only
# =============================================================================
STEP("17_audit", {
  # =============================================================================
  # 17_ballot_regions19_linkage19.R  -- DISCOVERY + one TEMPLATE file.
  #
  #   A. 2024 ballot frame: where are ITN, POT, SALF and every other CHES 2024
  #      family-1 party? Decides the content of the declared correction to the
  #      baseline PRR set (script 18). Nothing is recoded here.
  #   B. 2019 regions, read from the STORED CODES (script 16 read the labels,
  #      which in ZA7581 are region names -- that result was an artefact).
  #   C. 2019 linkage candidates: Q7 ballot codes vs CHES 2019 parties.
  #      Writes output/17_linkage2019_TEMPLATE.csv with prr = NA for you to
  #      fill by hand (with notes), exactly as 05_prr_linkage.csv was built.
  #
  # Fresh session. Output: console + logs/17_console.txt
  #
  # CHANGES IN THE REPLICATION-PACKAGE VERSION
  #   - paths overridable by environment variables; logs dir created if absent.
  #   - Part C candidate matching: abbreviations of 1-2 characters (e.g. "V",
  #     "SD", "PS") matched as substrings of labels produce false candidates;
  #     a word-boundary match is now used for abbreviations under 4 characters.
  #     Discovery output only: nothing downstream reads the candidates.
  # =============================================================================

  while (sink.number() > 0) sink()
  PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
  RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
  paths <- list(
    raw_ees19 = file.path(RAW, "ZA7581_v2-0-1.sav"),
    raw_ches  = file.path(RAW, "1999-2024_CHES_dataset_meansV2.csv"),
    output    = file.path(PROJ, "output"),
    logs      = file.path(PROJ, "logs"))
  if (!dir.exists(paths$logs)) dir.create(paths$logs, recursive = TRUE)   # FIX

  suppressPackageStartupMessages({
    library(haven); library(dplyr); library(tibble); library(purrr)
    library(stringr); library(readr); library(tidyr)
  })
  options(width = 160, max.print = 100000)
  con <- file(file.path(paths$logs, "17_console.txt"), open = "wt")
  sink(con, split = TRUE)

  hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")
  guard <- function(label, expr) tryCatch(expr, error = function(e)
    cat("\n*** PART", label, "FAILED:", conditionMessage(e), "***\n"))
  repair <- function(s) {
    out <- iconv(s, from = "UTF-8", to = "CP1252"); Encoding(out) <- "UTF-8"
    ifelse(!is.na(out) & validUTF8(out), out, s)
  }
  norm <- function(s) toupper(stringr::str_replace_all(s, "[^[:alnum:]]", ""))
  # CHANGE: short abbreviations must match as a whole word, not a substring
  abbr_in_label <- function(abbr, label) {
    a <- norm(abbr)
    if (nchar(a) < 2) return(FALSE)
    if (nchar(a) >= 4) return(stringr::str_detect(norm(label), stringr::fixed(a)))
    words <- norm(unlist(strsplit(toupper(label), "[^[:alnum:]]+")))
    a %in% words
  }

  # CHES country codes: confirmed by the script-16 audit output except those
  # marked (*), which are from the CHES codebook and not yet checked here.
  CHES_CTY <- c(`1`="Belgium",`2`="Denmark",`3`="Germany",`4`="Greece",`5`="Spain",
                `6`="France",`7`="Ireland",`8`="Italy",`10`="Netherlands",`11`="UK",
                `12`="Portugal",`13`="Austria",`14`="Finland",`16`="Sweden",`20`="Bulgaria",
                `21`="Czech Republic",`22`="Estonia",`23`="Hungary",`24`="Latvia",
                `25`="Lithuania",`26`="Poland",`27`="Romania",`28`="Slovakia",
                `29`="Slovenia",`31`="Croatia",`37`="Malta",`38`="Luxembourg",`40`="Cyprus")
  cat("(*) Ireland 7, Malta 37, Luxembourg 38: codebook values, unverified.\n")

  ches <- readr::read_csv(paths$raw_ches, show_col_types = FALSE, na = "") |>
    dplyr::mutate(cty = CHES_CTY[as.character(country)])


  # ================= A. 2024 BALLOT FRAME: UNLINKED FAMILY-1 ===================

  guard("A", {
    hdr("A. 2024 BALLOT FRAME -- WHERE ARE THE UNLINKED FAMILY-1 PARTIES?")
    frame <- readr::read_csv(file.path(paths$output, "02_party_frame.csv"),
                             show_col_types = FALSE, na = "")
    cat("frame columns:", paste(names(frame), collapse = ", "), "\n\n")

    f1 <- ches |> dplyr::filter(year == 2024, family == 1)
    # candidate ballot rows: CHES abbreviation appears in the EES label
    hits <- purrr::map_dfr(seq_len(nrow(f1)), function(i) {
      ab <- norm(f1$party[i])
      fr <- frame |> dplyr::filter(cty == f1$cty[i])
      m  <- fr |> dplyr::filter(stringr::str_detect(norm(party_label),
                                                    stringr::fixed(ab)))
      tibble::tibble(cty = f1$cty[i], ches_party = f1$party[i],
                     n_ballot_hits = nrow(m),
                     q6_codes = paste(m$q6_code, collapse = ";"),
                     labels = paste(m$party_label, collapse = " | "))
    })
    print(as.data.frame(hits), row.names = FALSE)

    cat("\nFULL BALLOT for the countries with an unresolved party:\n")
    for (k in c("Bulgaria","Romania","Spain"))  {
      cat("\n--", k, "--\n")
      print(as.data.frame(frame |> dplyr::filter(cty == k) |>
                            dplyr::select(dplyr::any_of(c("q6_code","party_label",
                                                          "n_voters","vote_share_sample"))) |>
                            dplyr::arrange(dplyr::desc(vote_share_sample))), row.names = FALSE)
    }
    cat("\nRead: ITN should appear at 10001. POT / SALF either have a code here\n")
    cat("(=> linkage failure, correct in 18) or do not (=> write-in only,\n")
    cat("document as for MECh/Velichie).\n")
  })


  # ======================= B. 2019 REGIONS FROM CODES ==========================

  guard("B", {
    hdr("B. ZA7581 region_NUTS2 FROM STORED CODES")
    e19 <- haven::read_sav(paths$raw_ees19, user_na = TRUE, encoding = "CP1252",
                           col_select = c(hCountry, Q6, Q7, region_NUTS2, region))
    cty  <- as.character(haven::as_factor(e19$hCountry))
    code <- toupper(stringr::str_trim(as.character(haven::zap_labels(e19$region_NUTS2))))
    q6 <- as.numeric(haven::zap_labels(e19$Q6)); q7 <- as.numeric(haven::zap_labels(e19$Q7))
    voter <- q6 == 1 & !q7 %in% c(0, 90, 96, 98, 99)
    valid <- stringr::str_detect(code, "^[A-Z]{2}[0-9A-Z]{2}$")

    cat("stored values that are not 4-character codes:\n")
    print(sort(table(code[!valid], useNA = "ifany"), decreasing = TRUE))
    cat("\ncountries coded 96 (does not map to NUTS2):\n")
    print(table(cty[code == "96"]))

    d <- tibble::tibble(cty, code = ifelse(valid, code, NA), voter)
    tab <- d |> dplyr::group_by(cty) |>
      dplyr::summarise(N = dplyr::n(), pct_miss = round(100*mean(is.na(code)), 1),
                       n_regions = dplyr::n_distinct(code, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::left_join(d |> dplyr::filter(!is.na(code), voter) |>
                         dplyr::count(cty, code) |> dplyr::group_by(cty) |>
                         dplyr::summarise(med_vot = stats::median(n),
                                          reg_vot30 = sum(n >= 30), .groups = "drop"),
                       by = "cty") |>
      dplyr::arrange(n_regions)
    print(as.data.frame(tab), row.names = FALSE)

    cat("\nNUTS vintage markers (HU10 => 2013; HU11/HU12 => 2016+;",
        "HR04 => <=2016; PL12 => 2013; PL91/PL92 => 2016+):\n")
    for (k in c("Hungary","Croatia","Poland","Lithuania","Ireland","France","Germany"))
      cat(sprintf("%-10s %s\n", k, paste(sort(unique(code[valid & cty == k])),
                                         collapse = " ")))
    cat("\nfor the 96-coded countries, the raw 'region' variable (first values):\n")
    for (k in unique(cty[code == "96"])) {
      r <- as.character(haven::as_factor(e19$region))[cty == k]
      cat(k, ":", paste(head(names(sort(table(repair(r)), decreasing = TRUE)), 12),
                        collapse = " | "), "\n")
    }
  })


  # =================== C. 2019 LINKAGE CANDIDATE TEMPLATE ======================

  guard("C", {
    hdr("C. 2019 BALLOT (Q7) vs CHES 2019 -- CANDIDATE MATCHES")
    e19 <- haven::read_sav(paths$raw_ees19, user_na = TRUE, encoding = "CP1252",
                           col_select = c(hCountry, Q6, Q7))
    cty <- as.character(haven::as_factor(e19$hCountry))
    hc  <- as.numeric(haven::zap_labels(e19$hCountry))
    q6  <- as.numeric(haven::zap_labels(e19$Q6)); q7 <- as.numeric(haven::zap_labels(e19$Q7))
    voter <- q6 == 1 & !q7 %in% c(0, 90, 96, 98, 99)

    cat("check: is the Q7 country prefix floor(code/100) equal to hCountry?\n")
    cat("mismatches among voters:", sum(floor(q7[voter]/100) != hc[voter], na.rm = TRUE), "\n")

    labs <- attr(e19$Q7, "labels", exact = TRUE)
    lab_tab <- tibble::tibble(q7_code = unname(labs), label = repair(names(labs)))

    ballot <- tibble::tibble(cty = cty[voter], q7_code = q7[voter]) |>
      dplyr::filter(!is.na(q7_code)) |>
      dplyr::count(cty, q7_code, name = "n_voters") |>
      dplyr::group_by(cty) |>
      dplyr::mutate(share = round(100 * n_voters / sum(n_voters), 1)) |>
      dplyr::ungroup() |>
      dplyr::left_join(lab_tab, by = "q7_code")

    c19 <- ches |> dplyr::filter(year == 2019) |>
      dplyr::select(cty, ches_party = party, ches_id = party_id, family)

    cand <- purrr::map_dfr(seq_len(nrow(ballot)), function(i) {
      b <- ballot[i, ]
      cc <- c19 |> dplyr::filter(cty == b$cty)
      m  <- cc |> dplyr::filter(purrr::map_lgl(ches_party, \(p)
                                               !is.na(b$label) && abbr_in_label(p, b$label)))
      b |> dplyr::mutate(
        ches_candidate = paste(m$ches_party, collapse = ";"),
        ches_family    = paste(m$family, collapse = ";"),
        n_candidates   = nrow(m))
    })

    cat("\nsummary: ballot parties", nrow(cand), "| exactly one candidate",
        sum(cand$n_candidates == 1), "| none", sum(cand$n_candidates == 0),
        "| several", sum(cand$n_candidates > 1), "\n")

    cat("\nCHES 2019 FAMILY-1 PARTIES and whether any ballot label matched:\n")
    f1 <- c19 |> dplyr::filter(family == 1) |>
      dplyr::mutate(matched_codes = purrr::map2_chr(cty, ches_party, \(k, p)
        paste(cand$q7_code[cand$cty == k &
                purrr::map_lgl(strsplit(cand$ches_candidate, ";"), \(s) p %in% s)],
              collapse = ";")))
    print(as.data.frame(f1), row.names = FALSE)

    cat("\nballot parties with share >= 3% and NO CHES candidate (check by hand):\n")
    print(as.data.frame(cand |> dplyr::filter(n_candidates == 0, share >= 3) |>
                          dplyr::select(cty, q7_code, share, label)), row.names = FALSE)

    tmpl <- cand |> dplyr::mutate(prr = NA_integer_, note = "") |>
      dplyr::select(cty, q7_code, label, n_voters, share, ches_candidate,
                    ches_family, n_candidates, prr, note) |>
      dplyr::arrange(cty, dplyr::desc(share))
    out <- file.path(paths$output, "17_linkage2019_TEMPLATE.csv")
    readr::write_csv(tmpl, out, na = "")
    cat("\nwritten:", out, "\n")
    cat("Fill prr (1/0) and note by hand under the rule: contemporaneous CHES\n")
    cat("(2019) family 1. Record joint lists and CHES cross-wave inconsistencies\n")
    cat("in note. Save as output/17_prr_linkage_2019.csv. Do NOT edit the template\n")
    cat("in Excel without checking it keeps UTF-8 and the literal string 'NA'.\n")
  })

  hdr("DONE -- return the full console output")
  sink(); close(con)
})

cat("\nALL REQUESTED STEPS FINISHED. Logs are in logs/, results in output/ and figures/.\n")
