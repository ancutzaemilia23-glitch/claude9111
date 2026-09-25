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
