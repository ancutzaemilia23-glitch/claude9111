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
