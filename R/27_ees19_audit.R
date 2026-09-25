# =============================================================================
# 27_ees19_audit.R
# Purpose : Discovery audit of the EES 2019 Voter Study (ZA7581 v2.0.1) and
#           export of the party-code list for the CHES linkage (script 29,
#           not yet written -- the number 28 is taken by the Bayesian script).
#
# RULES:
#   - The script refuses to run until FREEZE_2019.txt exists in the project
#     root (date; OSF link optional), recording that the coding is fixed.
#   - It computes NO joint distribution of dissatisfaction and vote choice,
#     and no slope. Only single-variable marginals and code lists, which the
#     linkage and the harmonisation need.
#
# Two-pass use (discovery first, as in scripts 01-05):
#   Pass 1: leave the CONFIG names as NULL. The script prints candidate
#           variables for country, EP vote, satisfaction with democracy and
#           controls, then stops. Return the console output.
#   Pass 2: set the CONFIG names from the pass-1 output (agree them first),
#           run again. Exports the party-code template for linkage.
#
# Inputs : ZA7581_v2-0-1.sav (located automatically under D:/Cluj)
# Outputs: output/27_ees19_dictionary.csv, output/27_ees19_party_codes.csv,
#          logs/27_console.txt
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - ZA7581 is read with encoding = "CP1252" and labels repaired, exactly as
#     in scripts 16 and 17. With the declared encoding the party labels in
#     the exported template were mojibake (or the read failed, see 15b C).
#   - helper `find()` renamed `find_var()`: it masked utils::find().
#   - header references to "script 28" (the 2019 linkage) corrected.
#   - paths overridable by environment variables.
# =============================================================================

while (sink.number() > 0) sink()
closeAllConnections()
PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
RAW   <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
paths <- list(output = file.path(PROJ, "output"), logs = file.path(PROJ, "logs"))

# ---- Gate: the 2019 coding decisions must be frozen first -------------------
# Formal OSF pre-registration is optional (lean Q2 plan). What is NOT optional:
# the position table (script 26) and the classification rules are fixed before
# EES 2019 is opened. Create FREEZE_2019.txt in the project root with the date
# and, if deposited, the OSF link.
gate <- file.path(PROJ, "FREEZE_2019.txt")
if (!file.exists(gate)) stop(
  "Create ", gate, " first: one line with today's date, one line stating that ",
  "output/26_prr_position_coding.csv and the linkage rules are frozen ",
  "(add the OSF link if you deposit).")
gate_txt <- readLines(gate, warn = FALSE)
if (!any(grepl("20[0-9]{2}-[0-9]{2}-[0-9]{2}", gate_txt)))
  stop("FREEZE_2019.txt must contain a date in YYYY-MM-DD format.")

pkgs <- c("haven", "dplyr", "readr", "tibble", "stringr", "purrr")
miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss)) stop("install.packages(c(",
                       paste(sprintf('"%s"', miss), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
options(width = 160, max.print = 100000)

con <- file(file.path(paths$logs, "27_console.txt"), open = "wt")
sink(con, split = TRUE)
hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")
cat("Freeze record:\n"); cat(gate_txt, sep = "\n")
# same repair as scripts 16/17: UTF-8 bytes that were decoded as CP1252
repair <- function(s) {
  out <- iconv(s, from = "UTF-8", to = "CP1252"); Encoding(out) <- "UTF-8"
  ifelse(!is.na(out) & validUTF8(out), out, s)
}

# ------------------------- CONFIG (pass 2) -----------------------------------
COUNTRY_VAR <- NULL   # e.g. "hCountry"  -- set from pass-1 output
VOTE_VAR    <- NULL   # EP 2019 vote recall
SWD_VAR     <- NULL   # satisfaction with democracy in own country
CTRL_VARS   <- NULL   # c(age = "...", gender = "...", educ = "...", urban = "...")


# ========================= 1. FILE AND DICTIONARY ============================

hdr("1. FILE AND DICTIONARY")
sav <- list.files(RAW, pattern = "^ZA7581.*\\.sav$", recursive = TRUE,
                  full.names = TRUE)
if (length(sav) != 1) stop("Expected exactly one ZA7581 .sav under ", RAW, "; found ",
                           length(sav), ": ", paste(sav, collapse = "; "))
cat("file:", sav, "\n")
d <- haven::read_sav(sav, user_na = TRUE, encoding = "CP1252")   # FIX: as in 16/17
cat("rows:", nrow(d), " columns:", ncol(d), "\n")

lab_of <- function(x) { l <- attr(x, "label"); if (is.null(l)) NA_character_ else repair(as.character(l))[1] }
vlabs  <- function(x) { l <- haven::val_labels(x); if (length(l)) names(l) <- repair(names(l)); l }
dict <- tibble::tibble(var = names(d),
                       label = purrr::map_chr(d, lab_of),
                       n_distinct = purrr::map_int(d, ~ dplyr::n_distinct(.x)),
                       has_value_labels = purrr::map_lgl(d, ~ length(haven::val_labels(.x)) > 0))
readr::write_csv(dict, file.path(paths$output, "27_ees19_dictionary.csv"), na = "")
cat("dictionary written:", nrow(dict), "variables\n")

find_var <- function(rx) dict |> dplyr::filter(stringr::str_detect(   # CHANGE: renamed
  paste(var, label), stringr::regex(rx, ignore_case = TRUE)))


# ========================= 2. CANDIDATES (pass 1) ============================

hdr("2. CANDIDATE VARIABLES -- choose and set CONFIG")
show <- function(title, rx) {
  cat("\n---", title, "---\n")
  print(as.data.frame(find_var(rx) |> dplyr::select(var, label, n_distinct)), row.names = FALSE)
}
show("country",                       "country|cntry|member state")
show("EP 2019 vote recall",           "european parliament|EP elect|european elect")
show("satisfaction with democracy",   "democracy|satisf")
show("age / birth year",              "\\bage\\b|year of birth|born")
show("gender / sex",                  "gender|\\bsex\\b")
show("education",                     "educ|school|study")
show("urban / rural",                 "urban|rural|village|town|city|size of")
show("weights (not used; unweighted as in 2024)", "weight")

if (is.null(COUNTRY_VAR) || is.null(VOTE_VAR) || is.null(SWD_VAR) || is.null(CTRL_VARS)) {
  hdr("PASS 1 COMPLETE -- return this output; CONFIG is set before pass 2")
  sink(); close(con); stop("Pass 1 finished (intended stop).", call. = FALSE)
}


# ========================= 3. COUNTRY AND VOTE CODES (pass 2) ================

hdr("3. COUNTRY VARIABLE AND EP VOTE CODES")
stopifnot(all(c(COUNTRY_VAR, VOTE_VAR, SWD_VAR, unname(CTRL_VARS)) %in% names(d)))
d$cty_lab <- repair(as.character(haven::as_factor(d[[COUNTRY_VAR]], levels = "labels")))
print(as.data.frame(dplyr::count(d, cty_lab)), row.names = FALSE)

cat("\nEP vote variable:", VOTE_VAR, "--", lab_of(d[[VOTE_VAR]]), "\n")
vl <- vlabs(d[[VOTE_VAR]])
codes <- d |>
  dplyr::transmute(cty = cty_lab, code_raw = as.numeric(d[[VOTE_VAR]])) |>
  dplyr::count(cty, code_raw, name = "n") |>
  dplyr::mutate(label = names(vl)[match(code_raw, unname(vl))],
                ches_party = NA_character_, prr = NA_integer_, linkage_note = NA_character_)
readr::write_csv(codes, file.path(paths$output, "27_ees19_party_codes.csv"), na = "")
cat("party-code template written:", nrow(codes), "country x code rows\n",
    "(columns ches_party / prr / linkage_note are filled in the linkage step, from\n",
    " ballot labels and CHES only -- never from vote-by-dissatisfaction tables)\n")


# ========================= 4. HARMONISATION ITEMS (marginals only) ===========

hdr("4. SATISFACTION ITEM AND CONTROLS: WORDING, CODES, MARGINALS")
item <- function(v) {
  cat("\n---", v, "---\n label:", lab_of(d[[v]]), "\n")
  l <- vlabs(d[[v]])
  if (length(l)) print(data.frame(code = unname(l), label = names(l)), row.names = FALSE)
  print(table(as.numeric(d[[v]]), useNA = "ifany"))
}
item(SWD_VAR)
cat("\nPre-registered binary: 1 = 'not very' / 'not at all' satisfied.",
    "Check that the 2019 codes map onto the 2024 q2 coding before the linkage step.\n")
purrr::walk(unname(CTRL_VARS), item)

hdr("DONE -- return the full console output (logs/27_console.txt)")
sink(); close(con)
