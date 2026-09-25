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
