#!/usr/bin/env bash
# Smoke test on SYNTHETIC data: builds fake inputs in a temp folder and runs
# the scripts that do not need the Stan toolchain, EQI/Eurostat/OWID files or
# CHES. Usage: bash tests/run_synthetic.sh   (from the repository root)
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="${TMPDIR:-/tmp}/infprr_synthetic"
rm -rf "$TMP"; mkdir -p "$TMP"
export INFPRR_PROJ="$TMP/proj" INFPRR_RAW="$TMP/raw" INFPRR_WB="$TMP/raw/wb.xlsx"
Rscript "$ROOT/tests/make_synthetic.R"
for s in RUN_ANALYSIS 12_figures 13_secondary_outcomes 14_robustness \
         18_itn_correction_and_KH 22_opposition_prr; do
  echo "=== $s"; Rscript "$ROOT/R/$s.R" > "$TMP/$s.out" 2>&1 || { tail -30 "$TMP/$s.out"; exit 1; }
done
Rscript "$ROOT/tests/test_estimators.R"
echo "ALL SYNTHETIC RUNS COMPLETED -- outputs in $TMP"
