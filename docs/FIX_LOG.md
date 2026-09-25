# Fix log: static analysis and corrections

This log covers every script in the dump. Each change is also marked in the
code with `# FIX:` (a bug) or `# CHANGE:` (a deliberate change in behaviour or
output). Where more than one version of a script was pasted (20, 23, 26, 27,
28), the **last** version is the one kept. It already contained the earlier
versions' fixes.

Severity: **H** = can change a published number · **M** = can crash or give
wrong output in some sessions or data states · **L** = print, label or
robustness only.

## Cross-cutting issues

| # | Sev | Issue | Where | Fix |
|---|-----|-------|-------|-----|
| X1 | **H** | `classified = !q6_raw %in% SPECIAL` is `TRUE` for `NA`. `haven::zap_labels()` turns SPSS **user-missing** codes into `NA` (checked here: codes 96/98 declared missing → `NA`). Those respondents were counted as classified **non-PRR voters**, which inflates the denominator and shrinks the slopes. | RUN_ANALYSIS (every later script inherits `classified` from the rds) | Excluded, both in the rebuild and on load of an existing rds. The count is printed. **If it prints 0, no result changes.** If it prints more than 0, every estimate changes and the change must be reported as a correction. |
| X2 | M | `on.exit()` at top level. Under `source()` it fires straight away and closes the log (tested here: the log file came out **empty**). Under `Rscript` it is ignored. | RUN_ANALYSIS, 15 | Removed. `sink(); close(con)` now runs explicitly at the end. |
| X3 | M | `if (!exists("paths")) ...` makes a script depend on whatever an earlier script left in the session. Script 13 run after 12 got `paths$derived = NULL`, and 14 run after 13 got `paths$raw_informal = NULL`. | 12, 13, 14 | Paths are now defined unconditionally in each script. |
| X4 | M | CSVs were written with `na = "NA"` and read back with `na = ""`, so a missing number came back as the string `"NA"`. The column then turned **character**, and any column not coerced in `NUMCOLS` broke arithmetic (e.g. `b` in script 18). | all writers | `write_csv(..., na = "")` everywhere. `b`/`se` added to 18's `NUMCOLS`. |
| X5 | L | Label "post-2004" / "Post-2004 accession" is attached to the `cee` dummy, which **excludes Cyprus** (a 2004 entrant). A reviewer will spot this in Figure 1. | 12, 13, RUN_ANALYSIS, 18, 21, 22 | Relabelled "post-communist". The variable itself is unchanged. |
| X6 | L | Hard-coded Windows paths. | all | `Sys.getenv("INFPRR_PROJ" / "INFPRR_RAW" / "INFPRR_WB")`. The defaults are your original paths, so nothing changes on your machine. |

## Per file

**RUN_ANALYSIS.R**
- X1, X2 (above).
- L — Part 2 printed `t = b/(100*se)` after `b` and `se` had already been ×100, so `t` showed 100× too small. Print only.
- L — Part 8 `gap` bug (the one you documented in 12) was still present here. Print only.
- M — `age = as.numeric(ees$d4_age)` keeps user-missing codes as numeric ages. It now uses `num()`, and the age range is printed. Output is identical if no codes are declared (the rebuild prints the declared codes).
- CHANGE — writes `10_loo.csv` and `10_robustness.csv` so Table 3 can be computed rather than typed.

**12_figures.R**: X3, X5. `pdf()` cannot encode "–" under Windows' default encoding, so it now uses `cairo_pdf()`. `text(pos = NA)` is guarded. "p = 0.000" now prints as "p < 0.001".

**13_secondary_outcomes.R**: X3, X5. **Table 3 was typed in by hand** (-0.0074, 0.1134, …). That is the most common way a replication package ends up disagreeing with its paper. It is now built from `10_robustness.csv`. Its numbers are the conference (pre-ITN, DL + t) ones, as before.

**14_robustness.R**: X3.
- M — `imm_restr` and `econ_retro` are **not created anywhere I have seen**. The RUN_ANALYSIS rds only contains `lr`. The script now skips those specifications with a clear message. **I did not invent their EES item numbers.**
- M — the MIMIC/SEMP series did not rename "Czechia", so the Czech Republic could silently drop out.
- Hard-coded "-0.0099 / 0.0036" print removed.

**15b**: prints the declared user-missing codes of q6 (the mechanism behind X1). `na.rm` added to the mismatch counts.

**16, 17**: create `logs/` if it is missing. In 17 C, short CHES abbreviations ("V", "SD") are now matched as whole words, not substrings. That script is discovery only.

**18_itn_correction_and_KH.R**
- M — the reproduction check compared metafor with the **rounded, transcribed** -0.0099/0.0036 at a 5e-5 tolerance. It now compares with the hand-coded DL estimator recomputed on the same data (1e-8).
- L — the PTV slope was printed `/100` while labelled "PTV points".
- M — stops if ITN has more than one frame row. Otherwise `tibble()` would recycle it into two linkage rows.

**20_regional_build.R**
- **H** — joins on `unit` used dplyr's default `na_matches = "na"`. Respondents **without a region code** would therefore take the EQI and unemployment values of any EQI row whose `region_code` is `NA`. The `nrow == 25904` guard only catches duplication. Fixed with `na_matches = "never"`, and EQI rows with a missing code are counted and dropped.
- L — "PRIMARY (19)" in section F actually covered 22 countries (Ireland, Luxembourg and Malta pass the filter because their `prr` is 0, not `NA`).

**21_core_regional_model.R**
- M — `sample(m)` in the randomisation inference: for a length-1 `m >= 1`, R samples from `1:m`. Harmless today (`m = 0`), but fragile. Replaced by `m[sample.int(n)]`.
- Added an FWL self-check against `lm()`.

**22_opposition_prr.R**: typed `NA`; `fit()` no longer aborts the script on a too-small subset.

**23_scale_check.R**
- M — if no script-18 reference matched, `max(abs(...), na.rm=TRUE)` returned `-Inf` and the anchor check **passed**. It now requires a reference slope for every country.
- Records which warning triggered Firth.

**23b_logit_heterogeneity.R** (was a console snippet): the loop printed unlabelled lines. Now labelled and saved.

**26_build_position_table.R**
- Of the five pasted versions, the third ended in `close(con)1`, which is a parse error.
- A literal `Ö` in the regex breaks under non-UTF-8 sourcing. It is now `\u00d6`.
- M — `which.max()` on an all-`NA` vote vector silently picks the **first** party as "main". The new `main_rule` column flags these cases.
- Diacritics in the free-text `source` notes were transliterated to ASCII.

**27_ees19_audit.R**: now reads with `encoding = "CP1252"` plus `repair()`, as 16 and 17 already did. `find()` masked `utils::find`. The comment referring to "script 28" was wrong, because 28 is the Bayesian script.

**28_bayes_hierarchical.R**: section 3b is wrapped in a guard, so a failure there no longer loses the slow Part B. It prints brms's `df` prior slot before applying `constant(4)`. The stop messages are more informative.

**28b_onestage_diagnostics.R** (was a console snippet; your "28b errors"):
- `round()` on a tibble with a character column errors.
- ESS was checked only for fixed effects. It now covers `b_`, `sd_` and `cor_` with R-hat.
- The slope is explicitly at *mean* informality. The OR scale and the cabinet − non-cabinet difference were added.

## Not included, and why

- **Scripts 01–06 were never pasted.** `02_party_frame.csv` and `05_prr_linkage.csv` come from 02 and 05, and 13 cites results from 06. **The pipeline cannot be reproduced from raw data without them.** Please send them next.
- `15_verify_extensions.R` is superseded by 15b (as its own header says).
- The draft "Script 22 — Stage-1 scale robustness" is superseded by 23. It read `data/ees24_classified.rds` and used variables (`cntry`, `dissat`, `edu`) that exist nowhere in the pipeline.
- The first version of 20 is superseded by the second.
- Console one-liners: `install.packages(...)`, `unzip("D:/clientelism-index.zip", ...)`, `list.files(... "clientel" ...)`, the Bulgaria `cls` inspection (now Guard 2 in 26) and the cmdstan setup. These are documented in README instead.
- A numbering collision remains: there are two different "22" scripts in your history, and 27 used to refer to a "28" linkage. The 2019 linkage should become **29**.

## Open questions I cannot resolve without you

1. What does 15b B0 print for "q6 system NA"? Or what does RUN_ANALYSIS now print for "q6 = NA previously coded classified"? (X1)
2. Which ZA8868 items should `imm_restr` and `econ_retro` be?
3. Scripts 01–06 (content), plus script 24 if it exists (23 refers to it).
4. The authors, affiliations and ORCIDs for `CITATION.cff`.
