# =============================================================================
# tests/make_synthetic.R -- SYNTHETIC data for a smoke test of the pipeline.
#
# Builds, under $INFPRR_PROJ and $INFPRR_RAW, files with the SAME NAMES AND
# STRUCTURE as the real inputs (ZA8868 .sav, 02_party_frame.csv,
# 05_prr_linkage.csv, the World Bank workbook), filled with random numbers.
# Nothing here is real data and no result from it means anything; it only
# checks that the code runs end to end and that the guards fire.
#
# Deliberately planted problems, so the fixes can be seen working:
#   - q6 codes 96 and 98 are declared SPSS user-missing (zap_labels -> NA);
#     RUN_ANALYSIS must report them as "q6 = NA previously coded classified".
#   - Latvia's PRR party has the CHES abbreviation "NA".
#   - the World Bank sheet uses "Czechia" and "Slovak Republic".
# =============================================================================

suppressPackageStartupMessages({ library(haven); library(dplyr); library(tibble); library(readr) })
set.seed(1)
PROJ <- Sys.getenv("INFPRR_PROJ"); RAW <- Sys.getenv("INFPRR_RAW")
stopifnot(nzchar(PROJ), nzchar(RAW))
for (p in file.path(PROJ, c("output", "derived", "logs"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW, recursive = TRUE, showWarnings = FALSE)

cty <- c(Austria=40, Belgium=561, Bulgaria=100, Croatia=191, Cyprus=196, `Czech Republic`=203,
         Denmark=208, Estonia=233, Finland=246, France=250, Germany=276, Greece=300,
         Hungary=348, Ireland=372, Italy=380, Latvia=428, Lithuania=440, Luxembourg=442,
         Malta=470, Netherlands=528, Poland=616, Portugal=620, Romania=642, Slovakia=703,
         Slovenia=705, Spain=724, Sweden=752)
iso2 <- c(Austria="AT",Belgium="BE",Bulgaria="BG",Croatia="HR",Cyprus="CY",`Czech Republic`="CZ",
          Denmark="DK",Estonia="EE",Finland="FI",France="FR",Germany="DE",Greece="EL",Hungary="HU",
          Ireland="IE",Italy="IT",Latvia="LV",Lithuania="LT",Luxembourg="LU",Malta="MT",
          Netherlands="NL",Poland="PL",Portugal="PT",Romania="RO",Slovakia="SK",Slovenia="SI",
          Spain="ES",Sweden="SE")
CEE <- c("Bulgaria","Croatia","Czech Republic","Estonia","Hungary","Latvia","Lithuania",
         "Poland","Romania","Slovakia","Slovenia")
NOPRR <- c("Ireland","Luxembourg","Malta")

# ---- party frame and linkage ------------------------------------------------
prr_of <- function(k) switch(k, Hungary = c(34801, 34803), Slovakia = c(70304, 70303),
                             Italy = c(38005, 38006), Finland = 24604, Croatia = 19101,
                             cty[[k]] * 100 + 3)
other_of <- function(k) c(cty[[k]] * 100 + c(11, 12, 17, 18), if (k == "Bulgaria") 10001)
frame <- bind_rows(lapply(names(cty), function(k) {
  codes <- other_of(k)
  prr <- if (k %in% NOPRR) numeric(0) else prr_of(k)
  tibble(q6_code = c(codes, prr), cty = k,
         party_label = paste(k, "party", c(codes, prr)),
         is_prr = c(rep(0, length(codes)), rep(1, length(prr))),
         modal_slot = c(rep(NA, length(codes)), seq_along(prr)),
         n_voters = 100, vote_share_sample = 10)
}))
link <- frame |> transmute(q6_code, cty, party_label,
                           ches_party = ifelse(cty == "Latvia" & is_prr == 1, "NA",
                                               paste0("P", q6_code)),
                           prr = is_prr)
write_csv(frame |> select(-is_prr), file.path(PROJ, "output", "02_party_frame.csv"), na = "")
write_csv(link, file.path(PROJ, "output", "05_prr_linkage.csv"), na = "")

# ---- respondents ------------------------------------------------------------
inf <- setNames(ifelse(names(cty) %in% CEE, runif(27, 16, 29), runif(27, 8, 22)), names(cty))
N <- 500
d <- bind_rows(lapply(names(cty), function(k) {
  dis <- sample(c(1:4, 98), N, TRUE, prob = c(.2, .3, .25, .2, .05))
  dbin <- as.integer(dis %in% 3:4)
  vote <- rbinom(N, 1, .75)
  prr_codes <- if (k %in% NOPRR) numeric(0) else prr_of(k)
  p_prr <- plogis(-1.8 + (1.2 - 0.04 * inf[[k]]) * dbin)
  is_prr <- rbinom(N, 1, p_prr) == 1 & length(prr_codes) > 0
  oc <- other_of(k); other <- oc[sample.int(length(oc), N, TRUE)]
  q6 <- ifelse(is_prr, prr_codes[sample.int(max(1, length(prr_codes)), N, TRUE)], other)
  q6[vote == 0] <- 90
  q6[sample.int(N, 15)] <- sample(c(96, 98), 15, TRUE)          # planted user-missing
  tibble(cty = k, q5 = ifelse(vote == 1, 1, 2), q6 = q6, q2 = dis,
         q3 = sample(1:4, N, TRUE), q4 = sample(1:2, N, TRUE),
         d4_age = sample(18:90, N, TRUE), d3 = sample(1:2, N, TRUE),
         Edu_rec = sample(1:3, N, TRUE), d8 = sample(1:3, N, TRUE),
         q10 = sample(0:10, N, TRUE),
         d12_NUTSII = paste0(iso2[[k]], sample(if (k %in% c("Cyprus","Estonia","Latvia","Luxembourg","Malta")) "00" else c("11","12","21","22"), N, TRUE)))
}))
d$resp_id <- seq_len(nrow(d))
for (j in 1:8) d[[paste0("q9_", j)]] <- sample(c(0:10, 98), nrow(d), TRUE)
cvals <- setNames(seq_along(cty), names(cty))
sav <- d |> mutate(
  country = labelled(unname(cvals[cty]), labels = cvals),
  q6 = labelled_spss(q6, labels = c(`Did not vote` = 90, `Refused` = 96, `DK` = 98),
                     na_values = c(96, 98)),
  q6n = q6, Weight1 = 1, Weight2 = 1) |>
  select(-cty)
haven::write_sav(sav, file.path(RAW, "ZA8868_v1-0-0.sav"))

# ---- World Bank workbook ----------------------------------------------------
wb_name <- names(cty); wb_name[wb_name == "Czech Republic"] <- "Czechia"
wb_name[wb_name == "Slovakia"] <- "Slovak Republic"
mk <- function(shift) {
  x <- tibble(`Country Name` = wb_name)
  for (y in 2008:2021) x[[as.character(y)]] <- inf + shift + rnorm(27, 0, .5)
  x
}
writexl::write_xlsx(list(DGE_p = mk(0), MIMIC_p = mk(2), SEMP_p = mk(-3)),
                    Sys.getenv("INFPRR_WB"))

# ---- stand-in for script 20's output (for testing script 21 only) -----------
cat("synthetic inputs written under", PROJ, "and", RAW, "\n")
