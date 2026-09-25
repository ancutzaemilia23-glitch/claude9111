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
