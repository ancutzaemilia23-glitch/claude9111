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
