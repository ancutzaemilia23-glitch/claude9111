# Informality and the political translation of institutional dissatisfaction

Replication code for a two-stage analysis. **Stage 1** estimates, within each
country, how much dissatisfaction with national democracy raises the
probability of a populist radical-right (PRR) vote (EES 2024, ZA8868). **Stage
2** regresses those country slopes on informal-economy size (World Bank DGE),
using random-effects meta-regression. Extensions cover within-country regional
variation (EQI), a scale-robustness gate (script 23), PRR cabinet status
(scripts 26 and 28), and preparatory work for an EES 2019 replication (script 27).

All code is R. What was fixed, and why, is in [`docs/FIX_LOG.md`](docs/FIX_LOG.md).

## Setup

- **R ≥ 4.3.** The native pipe and `\(x)` need 4.1, and UTF-8 source files on Windows need 4.2.
- `Rscript R/00_install_packages.R`, then freeze your environment with
  `renv::init(bare = TRUE); renv::snapshot()` and commit `renv.lock`.
  **No `renv.lock` is committed yet**, because the versions must come from
  *your* machine.
- Script 28 needs Stan: CmdStan via `cmdstanr` (preferred), or `rstan`. On
  Windows this requires Rtools.

## Data (not redistributed)

| File | Source | Used by |
|---|---|---|
| `ZA8868_v1-0-0.sav` | GESIS, EES 2024 Voter Study | RUN_ANALYSIS, 15b, 16, 18, 20 |
| `ZA7581_v2-0-1.sav` | GESIS, EES 2019 Voter Study | 15b, 16, 17, 27 |
| `1999-2024_CHES_dataset_meansV2.csv` | Chapel Hill Expert Survey | 15b, 16, 17, 26 |
| `informal-economy-database*.xlsx` | World Bank | RUN_ANALYSIS, 14 |
| `qog_eqi_long_24.csv` | QoG EQI | 20 |
| `lfst_r_lfu3rt*.xlsx` | Eurostat regional unemployment | 20 |
| `clientelism-index.csv` | OWID / V-Dem (unzip `clientelism-index.zip`) | 20 |

Locations default to the author's machine. To run elsewhere, set:

```
INFPRR_PROJ   project folder (output/, derived/, logs/, figures/)   default D:/Cluj/informality-prr
INFPRR_RAW    folder with the .sav and CHES files                   default D:/Cluj
INFPRR_WB     full path of the World Bank workbook
INFPRR_SEARCH folder scanned by script 20 for EQI/Eurostat/OWID     default D:/
```

## Running

```
Rscript reproduce.R          # everything below, each script in a fresh R process
Rscript reproduce.R core     # RUN_ANALYSIS -> 14 only
```

Order and status:

| Step | Script | Role |
|---|---|---|
| 0 | **01–06 (missing from this repository)** | build `02_party_frame.csv` and `05_prr_linkage.csv` |
| 1 | `RUN_ANALYSIS.R` | stage 1 + stage 2; conference results; LOO, region test, permutation |
| 2 | `12_figures.R` | Figures 1–2 |
| 3 | `13_secondary_outcomes.R` | PTV, abstention; Tables 1–3 |
| 4 | `14_robustness.R` | Table 4 |
| 5 | `18_itn_correction_and_KH.R` | declared ITN correction; REML + Knapp–Hartung |
| 6 | `20_regional_build.R` → `21_core_regional_model.R` | within-country EQI test (M1 is confirmatory) |
| 7 | `22_opposition_prr.R` | **exploratory** (outcome chosen after results) |
| 8 | `23_scale_check.R`, `23b_…` | logit / semi-elasticity gate (declared pass/fail rule) |
| 9 | `26_build_position_table.R` | PRR cabinet coding, 2019 and 2024 |
| 10 | `28_bayes_hierarchical.R` → `28b_…` | **exploratory**; Bayesian meta-regression and one-stage logit |
| audit | `15b`, `16`, `17`, `27` | discovery only; `27` is gated by `FREEZE_2019.txt` |

Smoke test on synthetic data (no real data needed):
`bash tests/run_synthetic.sh`. It runs RUN_ANALYSIS, 12, 13, 14, 18 and 22,
plus `tests/test_estimators.R`, which checks the hand-coded DL meta-regression
against `metafor` in 200 random cases. It verifies that the code runs. **None
of its numbers mean anything.**

## Reproducibility statement (draft; complete the bracketed items)

- **Environment.** R [version], packages frozen in `renv.lock` [to commit].
  `run_logs/sessionInfo.txt` is written by `reproduce.R`.
- **Data versions.** ZA8868 v1.0.0; ZA7581 v2.0.1; CHES 1999–2024 trend
  means v2; World Bank Informal Economy Database [download date]; EQI 2024
  long file; Eurostat `lfst_r_lfu3rt` [extraction date]. Informality is the
  DGE series averaged over 2010–2020.
- **Randomness.** Fixed seeds: 20260914 (RUN_ANALYSIS, 18, 22), 20260924
  (21), 20260925 (23), 20260926 (28). The permutation and randomisation tests
  use 9 999 and 1 999 draws. Their p-values reproduce exactly only on the same
  R version, because the RNG sampling method changed in R 3.6.
- **Hardware / runtime.** [CPU, RAM]. Scripts 1–5 take minutes. Script 21's
  randomisation inference takes minutes per model. Script 28 Part B takes
  [hours].
- **Expected outputs.** CSVs in `output/`, figures in `figures/`, console
  logs in `logs/`. Table 3 is computed from `output/10_robustness.csv`, not
  typed in.
- **Known limitations.**
  1. Scripts 01–06 are not yet in the package.
  2. Survey weights (`Weight1`/`Weight2`) are read but not used.
  3. Reported turnout far exceeds actual turnout.
  4. Informality is strongly confounded with the post-communist dummy.
  5. Scripts 22 and 28 are exploratory, because their outcome or moderator was chosen after seeing earlier results.
  6. Several cabinet dates in script 26 are marked `verified = FALSE`.
