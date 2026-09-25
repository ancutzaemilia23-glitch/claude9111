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
