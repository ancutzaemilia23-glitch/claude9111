# =============================================================================
# 14_robustness.R -- the specifications Section 3 promises
#
# Section 3 makes four forward references that are not yet estimated:
#   3.2.2 -> 4.4  sensitivity to the eleven radical-right parties absent from
#                 CHES 2024 and consequently coded zero
#   3.2.4 -> 4.4  specifications adding left-right self-placement, immigration
#                 attitudes and economic perceptions to the first stage
#   3.3   -> supp pooled cross-level interaction, as a specification check
#   (new)         alternative informality series from the same database
#
# Run AFTER RUN_ANALYSIS.R.
# Output: output/TABLE4_robustness.csv, 14_robustness_slopes.csv
#
# CHANGES IN THE REPLICATION-PACKAGE VERSION
#   - paths defined unconditionally. With the old `exists("paths")` test, a
#     session that had run script 13 first had no paths$raw_informal and
#     section 4 failed in read_excel(NULL).
#   - Section 2: `imm_restr` and `econ_retro` are NOT created by RUN_ANALYSIS
#     (its rds has only lr among the three). The script now stops with a
#     clear message instead of a dplyr "column doesn't exist" error. I have
#     not invented the EES item numbers: tell me which q-variables they are.
#   - Section 4: "Czechia" -> "Czech Republic" added to the rename (RUN_ANALYSIS
#     had it, this script did not, so the Czech Republic could silently drop
#     from the MIMIC and SEMP series). Sheet names are checked before use.
#   - baseline first-stage n computed once, not re-estimated in every loop.
# =============================================================================

PROJ  <- Sys.getenv("INFPRR_PROJ", "D:/Cluj/informality-prr")   # FIX: unconditional
paths <- list(derived = file.path(PROJ, "derived"),
              output  = file.path(PROJ, "output"),
              raw_informal = Sys.getenv("INFPRR_WB",
                "C:/Users/Lenovo/Downloads/informal-economy-database (1).xlsx"))
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(tibble)
  library(tidyr); library(readxl)})
hdr <- function(x) cat("\n\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

d <- readRDS(file.path(paths$derived, "analysis_individual.rds")) |>
  dplyr::mutate(dissatisfied = dplyr::case_when(
    dissat_nat %in% c(3,4) ~ 1L, dissat_nat %in% c(1,2) ~ 0L, TRUE ~ NA_integer_))
mod <- readr::read_csv(file.path(paths$output, "10_moderator.csv"),
                       show_col_types = FALSE, na = "") |>
  dplyr::mutate(dplyr::across(c(informality, incumbent, cee, b, se),
                              ~ suppressWarnings(as.numeric(.x)))) |>
  dplyr::select(cty, informality, incumbent, cee, b_base = b, se_base = se)

metareg <- function(y, se, X, label, show = TRUE) {
  ok <- stats::complete.cases(cbind(y, se, X))
  y <- y[ok]; v <- se[ok]^2; X <- cbind(1, as.matrix(X)[ok, , drop = FALSE])
  k <- length(y); p <- ncol(X)
  W <- diag(1/v); A <- t(X) %*% W %*% X
  b <- solve(A, t(X) %*% W %*% y); r <- y - X %*% b
  Qe <- as.numeric(t(r) %*% W %*% r)
  P <- W - W %*% X %*% solve(A, t(X) %*% W)
  tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
  W2 <- diag(1/(v + tau2)); A2 <- t(X) %*% W2 %*% X
  b2 <- solve(A2, t(X) %*% W2 %*% y); V <- solve(A2); sb <- sqrt(diag(V))
  tv <- b2[2]/sb[2]; pv <- 2*stats::pt(-abs(tv), k-p)
  if (show) cat(sprintf("%-38s k=%2d  g1 = %8.4f  se %6.4f  t %6.2f  p %.4f  tau %5.2f\n",
                        label, k, b2[2], sb[2], tv, pv, 100*sqrt(tau2)))
  invisible(tibble::tibble(spec = label, g1 = b2[2], se = sb[2],
                           p = pv, k = k, tau = 100*sqrt(tau2)))
}

prr_ctys <- d |> dplyr::filter(classified) |> dplyr::group_by(cty) |>
  dplyr::summarise(a = sum(prr) > 0, .groups="drop") |>
  dplyr::filter(a) |> dplyr::pull(cty)
dv <- d |> dplyr::filter(classified, cty %in% prr_ctys)

stage1 <- function(dat, yvar, extra = NULL) {
  vars <- c("dissatisfied","age","female","educ","urban", extra)
  purrr::map_dfr(sort(unique(dat$cty)), function(k) {
    dd <- dat |> dplyr::filter(cty == k) |>
      dplyr::select(y = dplyr::all_of(yvar), dplyr::all_of(vars)) |> stats::na.omit()
    if (nrow(dd) < 100 || stats::var(dd$y) == 0)
      return(tibble::tibble(cty = k, n = nrow(dd), b = NA_real_, se = NA_real_))
    ct <- summary(stats::lm(stats::as.formula(
      paste("y ~", paste(vars, collapse=" + "))), data = dd))$coefficients
    tibble::tibble(cty = k, n = nrow(dd), b = unname(ct["dissatisfied",1]),
                   se = unname(ct["dissatisfied",2]))
  })
}

R <- list()

## ---- 1. Alternative radical-right classification (Section 4.4) -------------
hdr("1. SENSITIVITY TO THE ELEVEN PARTIES ABSENT FROM CHES 2024")
cat("Section 3.2.2 codes these zero because CHES 2024 does not classify them.\n")
cat("Here they are coded as radical right and the analysis is re-run.\n\n")

EXTRA_PRR <- c(70306L,  # Kotlebovci-LSNS, Slovakia
               27614L,  # Die Heimat, Germany
               25009L,  # Les Patriotes, France
               52809L, 52818L,  # JA21, BVNL, Netherlands
               75209L,  # Alternativ for Sverige
               62008L, 62012L,  # ADN, Ergue-te, Portugal
               10008L,  # VMRO, Bulgaria
               44209L,  # ADR, Luxembourg
               47003L)  # Imperium Europa, Malta

link <- readr::read_csv(file.path(paths$output, "05_prr_linkage.csv"),
                        show_col_types = FALSE, na = "") |>
  dplyr::mutate(q6_code = suppressWarnings(as.numeric(q6_code)),
                prr = suppressWarnings(as.numeric(prr)))
base_codes <- link$q6_code[link$prr == 1 & !is.na(link$q6_code)]
alt_codes  <- unique(c(base_codes, EXTRA_PRR))

dv <- dv |> dplyr::mutate(prr_alt = as.integer(q6_raw %in% alt_codes))
cat(sprintf("radical-right share: baseline %.1f%%, extended %.1f%%\n",
            100*mean(dv$prr), 100*mean(dv$prr_alt)))
cat("Luxembourg and Malta remain excluded: they gain a radical-right option\n")
cat("under this coding but were not in the estimation sample.\n\n")

s_alt <- stage1(dv, "prr_alt") |> dplyr::rename(b_alt = b, se_alt = se)
e_alt <- mod |> dplyr::left_join(s_alt, by="cty") |> dplyr::filter(incumbent == 0)
R$base <- metareg(e_alt$b_base, e_alt$se_base, e_alt[,"informality"], "baseline classification")
R$alt  <- metareg(e_alt$b_alt,  e_alt$se_alt,  e_alt[,"informality"], "extended classification (+11 parties)")

## ---- 2. Extended control sets (Section 4.4) --------------------------------
hdr("2. ADDING THE EXCLUDED INDIVIDUAL CONTROLS")
cat("Section 3.2.4 excludes these from the baseline and promises them here.\n")
cat("The point is to observe what they absorb, and at what cost in sample.\n\n")

specs <- list("+ left-right"          = "lr",
              "+ immigration"         = "imm_restr",
              "+ economic perception"  = "econ_retro",
              "+ all three"            = c("lr","imm_restr","econ_retro"))
# FIX: fail with an actionable message if a control is not in the rds
absent <- setdiff(unique(unlist(specs)), names(dv))
if (length(absent)) {
  cat("NOT IN analysis_individual.rds:", paste(absent, collapse = ", "), "\n",
      "RUN_ANALYSIS creates only `lr` (q10). Add the immigration and economic-\n",
      "perception items to the rds build (names from the ZA8868 codebook), then\n",
      "re-run. The specifications needing them are SKIPPED in this run.\n")
  specs <- specs[vapply(specs, \(v) all(v %in% names(dv)), logical(1))]
}
n_base <- sum(stage1(dv, "prr")$n, na.rm = TRUE)       # CHANGE: computed once
for (nm in names(specs)) {
  s <- stage1(dv, "prr", specs[[nm]])
  e <- mod |> dplyr::left_join(s, by="cty") |> dplyr::filter(incumbent == 0)
  cat(sprintf("   [retains %.1f%% of the baseline first-stage sample]\n",
              100*sum(s$n, na.rm=TRUE)/n_base))
  R[[nm]] <- metareg(e$b, e$se, e[,"informality"], nm)
}

## ---- 3. Pooled cross-level interaction (supplementary) ---------------------
hdr("3. POOLED CROSS-LEVEL INTERACTION")

pooled <- dv |> dplyr::inner_join(mod, by="cty") |>
  dplyr::filter(incumbent == 0) |>
  dplyr::select(prr, dissatisfied, informality, age, female, educ, urban, cty) |>
  stats::na.omit() |>
  dplyr::mutate(inf_c = informality - mean(informality))

m_pool <- stats::lm(prr ~ dissatisfied * inf_c + age + female + educ + urban +
                      factor(cty), data = pooled)
ct  <- summary(m_pool)$coefficients
key <- grep("dissatisfied:inf_c", rownames(ct), value = TRUE)
cat(sprintf("N = %d individuals, %d countries\n", nrow(pooled),
            dplyr::n_distinct(pooled$cty)))
cat(sprintf("interaction = %9.5f  conventional se %8.5f  p %.4f\n",
            ct[key,1], ct[key,2], ct[key,4]))

# inf_c is collinear with the country dummies and is aliased away by lm();
# model.matrix() still returns its column, so drop the aliased ones first
keep <- !is.na(stats::coef(m_pool))
X <- stats::model.matrix(m_pool)[, keep, drop = FALSE]
u <- stats::resid(m_pool)
XtXi <- solve(crossprod(X))
meat <- matrix(0, ncol(X), ncol(X))
for (g in unique(pooled$cty)) {
  i <- pooled$cty == g
  meat <- meat + tcrossprod(crossprod(X[i, , drop = FALSE], u[i]))
}
G <- dplyr::n_distinct(pooled$cty); n <- nrow(X); kk <- ncol(X)
Vc   <- XtXi %*% meat %*% XtXi * (G/(G-1)) * ((n-1)/(n-kk))
se_c <- sqrt(diag(Vc))[match(key, colnames(X))]
cat(sprintf("%12s  country-clustered se %8.5f  p %.4f\n", "",
            se_c, 2*stats::pt(-abs(ct[key,1]/se_c), G-1)))
cat("NOTE: CR1 with G = 19 clusters over-rejects; script 21 uses CR2 +\n",
    "Satterthwaite (clubSandwich), which is the defensible small-G choice.\n")
cat("\nTwo-stage estimate (RUN_ANALYSIS Part 6): see output/10_robustness.csv.\n")


## ---- 4. Alternative informality series (Section 4.4) -----------------------
hdr("4. ALTERNATIVE INFORMALITY SERIES FROM THE SAME DATABASE")

WB <- paths$raw_informal
wb_sheets <- readxl::excel_sheets(WB)
grab <- function(sheet) {
  # FIX: say which sheets exist instead of a bare readxl error
  if (!sheet %in% wb_sheets)
    stop("sheet '", sheet, "' not in workbook. Sheets: ", paste(wb_sheets, collapse = ", "))
  raw <- suppressMessages(readxl::read_excel(WB, sheet = sheet))
  nc <- names(raw)[grepl("economy|country", names(raw), ignore.case=TRUE)][1]
  yc <- intersect(as.character(2010:2020),
                  names(raw)[grepl("^(19|20)[0-9]{2}$", names(raw))])
  raw |> dplyr::select(dplyr::all_of(c(nc, yc))) |>
    dplyr::rename(wb = dplyr::all_of(nc)) |>
    tidyr::pivot_longer(-wb, names_to="y", values_to="v") |>
    dplyr::mutate(v = suppressWarnings(as.numeric(v)),
                  # FIX: Czechia added, as in RUN_ANALYSIS
                  cty = dplyr::recode(wb, "Slovak Republic" = "Slovakia",
                                      "Czechia" = "Czech Republic")) |>
    dplyr::group_by(cty) |> dplyr::summarise(x = mean(v, na.rm=TRUE), .groups="drop") |>
    dplyr::filter(!is.nan(x))
}
alt_mod <- mod |>
  dplyr::left_join(grab("MIMIC_p") |> dplyr::rename(mimic = x), by="cty") |>
  dplyr::left_join(grab("SEMP_p")  |> dplyr::rename(semp  = x), by="cty")

cat("countries missing an alternative series:",
    paste(alt_mod$cty[is.na(alt_mod$mimic) | is.na(alt_mod$semp)], collapse = ", "), "\n")
cat("correlations between the three series, estimation sample:\n")
ee <- alt_mod |> dplyr::filter(incumbent == 0)
print(round(stats::cor(ee[,c("informality","mimic","semp")],
                       use="complete.obs"), 3))
cat("\n")
R$dge   <- metareg(ee$b_base, ee$se_base, ee[,"informality"], "DGE (baseline)")
R$mimic <- metareg(ee$b_base, ee$se_base, ee[,"mimic"], "MIMIC series")
R$semp  <- metareg(ee$b_base, ee$se_base, ee[,"semp"],  "self-employment share")
cat("\nCoefficients are not comparable in size across series: the three have\n")
cat("different scales. Sign and significance are what is comparable.\n")

## ---- 5. Table --------------------------------------------------------------
hdr("5. TABLE 4")
tab <- dplyr::bind_rows(R) |>
  dplyr::mutate(dplyr::across(c(g1, se), ~ round(.x, 4)),
                p = round(p, 4), tau = round(tau, 2))
print(as.data.frame(tab), row.names = FALSE)
readr::write_csv(tab, file.path(paths$output, "TABLE4_robustness.csv"), na = "")
readr::write_csv(alt_mod |> dplyr::left_join(s_alt, by="cty"),
                 file.path(paths$output, "14_robustness_slopes.csv"), na = "")
cat("\nWritten: TABLE4_robustness.csv, 14_robustness_slopes.csv\n")
