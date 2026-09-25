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
