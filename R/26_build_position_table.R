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
