# =============================================================================
# 18_itn_correction_and_KH.R
#
# DECLARED CHANGES (both decided before seeing any result from this script):
#   (1) CLASSIFICATION CORRECTION. ITN (Bulgaria, EES code 10001) is CHES 2024
#       family 1 and on the ballot, but script 05's matcher never linked it.
#       Under the paper's stated rule it is PRR. Added here. POT (Romania)
#       and SALF (Spain) are family 1 but have no EES 2024 ballot code:
#       documented, not recoded.
#   (2) INFERENCE UPGRADE. Stage two re-estimated by REML with the
#       Knapp-Hartung adjustment (metafor, test = "knha"), the current
#       small-k standard, alongside the conference DL + t estimator.
#   Every result is printed for BOTH classifications x BOTH estimators so the
#   effect of each change is separately visible.
#
# What ITN touches: Bulgaria's PRR vote slope, its govt-disapproval and
# EU-dissatisfaction variants, and its PTV outcome. The challenger outcome
# already contained ITN (script RUN_ANALYSIS Part 8) and is unchanged.
#
# Inputs : derived/analysis_individual.rds, output/05_prr_linkage.csv,
#          output/02_party_frame.csv, output/10_moderator.csv, raw ZA8868
# Outputs: output/05b_prr_linkage.csv
#          output/18_country_slopes_corrected.csv
#          output/18_stage_two_comparison.csv
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - NUMCOLS now includes b/se: 10_moderator.csv was written with NA as the
#     string "NA", which a na = "" read turns into a CHARACTER column, so
#     `chk$b - chk$b_old` could fail with "non-numeric argument".
#   - The reproduction check compared metafor with the transcribed, rounded
#     console value (-0.0099, 0.0036) at 5e-5 tolerance, which can fail or
#     pass by rounding luck. It now compares with the hand-coded DL estimator
#     recomputed on the same data (tolerance 1e-8); the transcribed target is
#     still printed for reference.
#   - The "slopes that differ" table divided the PTV slope by 100 while
#     labelling it "PTV points" (print only).
#   - stops if ITN has more than one row in the ballot frame (tibble() would
#     silently recycle and add two ITN linkage rows).
# =============================================================================

while (sink.number() > 0) sink()
PROJ <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")
RAW  <- Sys.getenv("INFPRR_RAW",  "D:/Cluj")
paths <- list(raw_ees24 = file.path(RAW, "ZA8868_v1-0-0.sav"),
              derived = file.path(PROJ, "derived"),
              output  = file.path(PROJ, "output"),
              logs    = file.path(PROJ, "logs"))

pkgs <- c("haven","dplyr","tidyr","purrr","tibble","readr","metafor")
miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss)) stop("install.packages(c(",
                       paste(sprintf('"%s"', miss), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
options(width = 150, max.print = 100000)
set.seed(20260914)                                   # same seed as RUN_ANALYSIS

con <- file(file.path(paths$logs, "18_console.txt"), open = "wt")
sink(con, split = TRUE)
hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")
cat("metafor", as.character(packageVersion("metafor")), "\n")

# FIX: b/se added -- otherwise they can arrive as character (see header)
NUMCOLS <- c("q6_code","modal_slot","n_voters","vote_share_sample","prr",
             "informality","incumbent","cee","recent_gov","b","se")
read_keepNA <- function(f) readr::read_csv(f, show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(dplyr::any_of(NUMCOLS), ~ suppressWarnings(as.numeric(.x))))
DK <- 98
miss_dk <- function(x) ifelse(x %in% DK, NA_real_, x)


# ========================= 1. CORRECTED LINKAGE ==============================

hdr("1. CORRECTED PRR LINKAGE (declared)")
link  <- read_keepNA(file.path(paths$output, "05_prr_linkage.csv"))
frame <- read_keepNA(file.path(paths$output, "02_party_frame.csv"))
old_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
stopifnot(!10001 %in% old_codes, 10001 %in% frame$q6_code)

itn_frame <- frame |> dplyr::filter(q6_code == 10001)
stopifnot(nrow(itn_frame) == 1)                      # FIX: see header
link_b <- dplyr::bind_rows(
  link,
  tibble::tibble(q6_code = 10001, cty = "Bulgaria", ches_party = "ITN", prr = 1,
                 note = "CHES 2024 family 1; on ballot; missed by script-05 matcher. Declared correction, script 18.",
                 party_label = itn_frame$party_label, n_voters = itn_frame$n_voters,
                 vote_share_sample = itn_frame$vote_share_sample),
  tibble::tibble(q6_code = NA_real_, cty = "Romania", ches_party = "POT", prr = 0,
                 note = "CHES 2024 family 1 (Dec 2024 national vote); not on EES 2024 Romanian ballot."))
if (!any(link_b$ches_party == "Salf" & link_b$cty == "Spain", na.rm = TRUE))
  cat("NOTE: no Salf row found; expected one from script 05.\n")
new_codes <- link_b$q6_code[link_b$prr == 1 & !is.na(link_b$q6_code)]
cat("PRR ballot codes: conference", length(old_codes), "| corrected", length(new_codes), "\n")
cat("ITN modal PTV slot:", itn_frame$modal_slot, "\n")
readr::write_csv(link_b, file.path(paths$output, "05b_prr_linkage.csv"), na = "")


# ========================= 2. INDIVIDUAL DATA ================================

hdr("2. OUTCOMES UNDER BOTH CLASSIFICATIONS")
d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
  dplyr::mutate(
    dissatisfied  = dplyr::case_when(dissat_nat %in% c(3,4) ~ 1L,
                                     dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
    dissat_eu_bin = dplyr::case_when(dissat_eu %in% c(3,4) ~ 1L,
                                     dissat_eu %in% c(1,2) ~ 0L, TRUE ~ NA_integer_),
    prr_conf = ifelse(classified, as.integer(q6_raw %in% old_codes), NA_integer_),
    prr_corr = ifelse(classified, as.integer(q6_raw %in% new_codes), NA_integer_))
# CHANGE: compare values, not storage type (an older rds may hold prr as double)
stopifnot(identical(as.integer(d$prr_conf), as.integer(d$prr)))
cat("respondents whose PRR coding changes:",
    sum(d$prr_conf != d$prr_corr, na.rm = TRUE), "(all should be Bulgaria)\n")
print(table(d$cty[which(d$prr_conf != d$prr_corr)]))

# PTV: Bulgaria's max over PRR slots now includes ITN's slot
d$ptv_conf <- d$prr_ptv
d$ptv_corr <- d$prr_ptv
slots_bg <- frame |> dplyr::filter(cty == "Bulgaria", q6_code %in% new_codes,
                                   !is.na(modal_slot)) |> dplyr::pull(modal_slot) |> unique()
cat("Bulgarian PRR PTV slots, corrected:", paste(slots_bg, collapse = ", "), "\n")
raw_q9 <- haven::read_sav(paths$raw_ees24, user_na = TRUE,
                          col_select = c("resp_id", paste0("q9_", 1:8)))
q9 <- raw_q9 |> dplyr::mutate(dplyr::across(-resp_id,
                                            \(x) miss_dk(as.numeric(haven::zap_labels(x)))))
bg <- which(d$cty == "Bulgaria")
vals <- as.matrix(q9[match(d$resp_id[bg], q9$resp_id), paste0("q9_", slots_bg), drop = FALSE])
newp <- suppressWarnings(apply(vals, 1, max, na.rm = TRUE)); newp[is.infinite(newp)] <- NA
d$ptv_corr[bg] <- newp
cat("Bulgarian PTV changed for", sum(d$ptv_conf[bg] != d$ptv_corr[bg], na.rm = TRUE),
    "respondents\n")


# ========================= 3. STAGE ONE ======================================

hdr("3. STAGE ONE")
prr_ctys <- d |> dplyr::filter(classified) |> dplyr::group_by(cty) |>
  dplyr::summarise(a = sum(prr_conf) > 0, .groups = "drop") |>
  dplyr::filter(a) |> dplyr::pull(cty)

slope <- function(dat, yvar, xvar = "dissatisfied", extra = NULL) {
  vars <- c(xvar, "age","female","educ","urban", extra)
  purrr::map_dfr(prr_ctys, function(k) {
    dd <- dat |> dplyr::filter(cty == k) |>
      dplyr::select(y = dplyr::all_of(yvar), dplyr::all_of(vars)) |> stats::na.omit()
    if (nrow(dd) < 100 || stats::var(dd$y) == 0)
      return(tibble::tibble(cty = k, b = NA_real_, se = NA_real_))
    ct <- summary(stats::lm(stats::reformulate(vars, "y"), data = dd))$coefficients
    tibble::tibble(cty = k, b = ct[xvar, 1], se = ct[xvar, 2])
  })
}
vot <- d |> dplyr::filter(classified, cty %in% prr_ctys)
ptv <- d |> dplyr::filter(cty %in% prr_ctys)

S <- list()
for (cl in c("conf","corr")) {
  y <- paste0("prr_", cl)
  S[[cl]] <- slope(vot, y) |>
    dplyr::left_join(slope(vot, y, extra = "govt_disapp") |>
                       dplyr::rename(b_gd = b, se_gd = se), by = "cty") |>
    dplyr::left_join(slope(vot, y, xvar = "dissat_eu_bin") |>
                       dplyr::rename(b_eu = b, se_eu = se), by = "cty") |>
    dplyr::left_join(slope(ptv |> dplyr::filter(!is.na(.data[[paste0("ptv_", cl)]])),
                           paste0("ptv_", cl)) |>
                       dplyr::rename(b_ptv = b, se_ptv = se), by = "cty") |>
    dplyr::mutate(classification = cl)
}
mod <- read_keepNA(file.path(paths$output, "10_moderator.csv")) |>
  dplyr::select(cty, informality, incumbent, cee)
slopes <- dplyr::bind_rows(S) |> dplyr::left_join(mod, by = "cty")

# check: conference slopes reproduce RUN_ANALYSIS Part 2
old <- read_keepNA(file.path(paths$output, "10_moderator.csv")) |> dplyr::select(cty, b_old = b)
chk <- slopes |> dplyr::filter(classification == "conf") |> dplyr::left_join(old, by = "cty")
cat("max |conference slope - RUN_ANALYSIS slope|:",
    signif(max(abs(chk$b - chk$b_old), na.rm = TRUE), 3), "(should be ~0)\n")

cat("\nslopes that differ between classifications (b in pp, b_ptv in PTV points):\n")
print(as.data.frame(slopes |>
                      dplyr::select(cty, classification, b, b_gd, b_eu, b_ptv) |>
                      tidyr::pivot_wider(names_from = classification, values_from = c(b, b_gd, b_eu, b_ptv)) |>
                      dplyr::filter(abs(b_conf - b_corr) > 1e-10 | abs(b_ptv_conf - b_ptv_corr) > 1e-10) |>
                      dplyr::mutate(dplyr::across(dplyr::starts_with(c("b_conf","b_corr","b_gd","b_eu")),
                                                  ~ round(100 * .x, 2)),
                                    # FIX: was round(.x / 100, 3) -- PTV is already in points
                                    dplyr::across(dplyr::starts_with("b_ptv"), ~ round(.x, 3)))),
      row.names = FALSE)
readr::write_csv(slopes, file.path(paths$output, "18_country_slopes_corrected.csv"), na = "")


# ========================= 4. STAGE TWO ======================================

hdr("4. STAGE TWO: DL + t (conference) vs REML + Knapp-Hartung (journal)")

fit <- function(df, yv, sv, form, method, test) {
  df <- df |> dplyr::filter(!is.na(.data[[yv]]), !is.na(.data[[sv]]))
  m <- tryCatch(metafor::rma(yi = df[[yv]], sei = df[[sv]], mods = form, data = df,
                             method = method, test = test,
                             control = list(stepadj = 0.5, maxiter = 1000)),
                error = function(e) NULL)
  if (is.null(m)) return(tibble::tibble(k = nrow(df), g1 = NA, se = NA, p = NA,
                                        lo = NA, hi = NA, tau = NA))
  tibble::tibble(k = m$k, g1 = unname(coef(m)["informality"]),
                 se = unname(m$se[names(coef(m)) == "informality"]),
                 p  = unname(m$pval[names(coef(m)) == "informality"]),
                 lo = unname(m$ci.lb[names(coef(m)) == "informality"]),
                 hi = unname(m$ci.ub[names(coef(m)) == "informality"]),
                 tau = 100 * sqrt(m$tau2))
}

# CHANGE: hand-coded DL estimator from RUN_ANALYSIS, used as an exact target
dl_hand <- function(y, se, x) {
  v <- se^2; X <- cbind(1, x); k <- length(y); p <- 2
  W <- diag(1/v); A <- t(X) %*% W %*% X
  r <- y - X %*% solve(A, t(X) %*% W %*% y)
  Qe <- as.numeric(t(r) %*% W %*% r)
  P  <- W - W %*% X %*% solve(A, t(X) %*% W)
  tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
  W2 <- diag(1/(v + tau2)); A2 <- t(X) %*% W2 %*% X
  c(g1 = unname(solve(A2, t(X) %*% W2 %*% y)[2]), se = unname(sqrt(diag(solve(A2)))[2]))
}

# reproduction check against the conference hand-coded estimate
e_conf <- slopes |> dplyr::filter(classification == "conf", incumbent == 0)
r0 <- fit(e_conf, "b", "se", ~ informality, "DL", "t")
h0 <- dl_hand(e_conf$b, e_conf$se, e_conf$informality)
cat(sprintf("metafor DL+t: g1 = %.6f, se = %.6f | hand-coded: g1 = %.6f, se = %.6f\n",
            r0$g1, r0$se, h0["g1"], h0["se"]))
cat("(conference console, rounded: -0.0099, 0.0036)\n")
if (abs(r0$g1 - h0["g1"]) > 1e-8 || abs(r0$se - h0["se"]) > 1e-8)
  stop("metafor does not reproduce the hand-coded estimator; stop and report.")

specs <- tibble::tribble(
  ~label,                       ~sample,  ~yv,     ~sv,      ~form,
  "PRR vote, primary",          "est19",  "b",     "se",     "~ informality",
  "PRR vote, all 24",           "all24",  "b",     "se",     "~ informality",
  "  + post-communist dummy (H4)", "est19", "b",   "se",     "~ informality + cee",
  "  older member states",      "old",    "b",     "se",     "~ informality",
  "  post-communist states",    "post",   "b",     "se",     "~ informality",
  "net of govt disapproval",    "est19",  "b_gd",  "se_gd",  "~ informality",
  "EU dissatisfaction (H2)",    "est19",  "b_eu",  "se_eu",  "~ informality",
  "PRR propensity to vote",     "est19",  "b_ptv", "se_ptv", "~ informality")

pick <- function(df, s) switch(s,
                               est19 = df |> dplyr::filter(incumbent == 0),
                               all24 = df,
                               old   = df |> dplyr::filter(incumbent == 0, cee == 0),
                               post  = df |> dplyr::filter(incumbent == 0, cee == 1))

res <- purrr::pmap_dfr(specs, function(label, sample, yv, sv, form) {
  purrr::map_dfr(c("conf","corr"), function(cl) {
    df <- pick(slopes |> dplyr::filter(classification == cl), sample)
    dplyr::bind_rows(
      fit(df, yv, sv, stats::as.formula(form), "DL",   "t")    |> dplyr::mutate(est = "DL+t"),
      fit(df, yv, sv, stats::as.formula(form), "REML", "knha") |> dplyr::mutate(est = "REML+KH")) |>
      dplyr::mutate(spec = label, classification = cl)
  })
})
out <- res |> dplyr::select(spec, classification, est, k, g1, se, lo, hi, p, tau) |>
  dplyr::mutate(dplyr::across(c(g1, se, lo, hi), ~ round(.x, 4)),
                p = round(p, 4), tau = round(tau, 2))
print(as.data.frame(out), row.names = FALSE)
cat("\nPTV coefficients are in PTV points x informality, not pp.\n")
readr::write_csv(out, file.path(paths$output, "18_stage_two_comparison.csv"), na = "")


# ===================== 5. PERMUTATION AND LEAVE-ONE-OUT ======================

hdr("5. PERMUTATION AND LOO, CORRECTED CLASSIFICATION")
e_corr <- slopes |> dplyr::filter(classification == "corr", incumbent == 0)

m_kh <- metafor::rma(yi = b, sei = se, mods = ~ informality, data = e_corr,
                     method = "REML", test = "knha",
                     control = list(stepadj = 0.5, maxiter = 1000))
pt <- metafor::permutest(m_kh, iter = 9999, progbar = FALSE)
cat(sprintf("REML+KH, corrected: g1 = %.4f | permutation p = %.4f (9999 draws)\n",
            coef(m_kh)["informality"], pt$pval[2]))

m_dl <- metafor::rma(yi = b, sei = se, mods = ~ informality, data = e_corr,
                     method = "DL", test = "t")
pt2 <- metafor::permutest(m_dl, iter = 9999, progbar = FALSE)
cat(sprintf("DL+t,    corrected: g1 = %.4f | permutation p = %.4f (conference value 0.0152)\n",
            coef(m_dl)["informality"], pt2$pval[2]))

loo <- purrr::map_dfr(e_corr$cty, function(k) {
  f <- fit(e_corr |> dplyr::filter(cty != k), "b", "se", ~ informality, "REML", "knha")
  f |> dplyr::mutate(dropped = k)
}) |> dplyr::arrange(g1)
cat("\nleave-one-out, REML+KH, corrected:\n")
print(as.data.frame(loo |> dplyr::transmute(dropped, g1 = round(g1, 4), se = round(se, 4),
                                            p = round(p, 4), flag = ifelse(p >= .05, "p >= .05", ""))), row.names = FALSE)
cat(sprintf("\nsign stable: %s | p < .05 in %d of %d\n",
            all(loo$g1 < 0), sum(loo$p < .05), nrow(loo)))

hdr("DONE -- return the full console output")
sink(); close(con)
