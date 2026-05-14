#!/usr/bin/env bash
# =============================================================================
# REPLICATE.sh — Full replication of Nepal Cost-of-Inaction Analysis
# Run from the analysis/ directory:
#   bash REPLICATE.sh
# =============================================================================
set -e  # stop on any error

PROJECT_DIR="/Users/hgrueso/Dropbox/Documents/Laboral/AImpact Lab/Clients/UNICEF/Projects/Investment Cases/Nepal/analysis"
cd "$PROJECT_DIR"

echo "========================================================"
echo " Nepal Cost-of-Inaction Analysis — Full Replication"
echo " $(date)"
echo "========================================================"
echo ""

# ── Folder structure ──────────────────────────────────────────────────────────
echo "[0/8] Creating output folders..."
mkdir -p outputs/figures outputs/tables reports R

# ── Check R is available ──────────────────────────────────────────────────────
if ! command -v Rscript &> /dev/null; then
  echo "ERROR: Rscript not found. Install R from https://cran.r-project.org"
  exit 1
fi
echo "      R found: $(Rscript --version 2>&1 | head -1)"

# ── Check Quarto is available ─────────────────────────────────────────────────
if ! command -v quarto &> /dev/null; then
  echo "WARNING: quarto not found. Report will not render."
  echo "         Install from https://quarto.org"
  QUARTO=false
else
  QUARTO=true
  echo "      Quarto found: $(quarto --version)"
fi

# ── Check data files exist ────────────────────────────────────────────────────
echo ""
echo "[0/8] Checking data files..."
DATA_DIR="data/NLSSIV_microdata/data/stata format"
REQUIRED_FILES=("S01.dta" "S07.dta" "S08.dta" "S09.dta" "S10.dta" "poverty.dta")
for f in "${REQUIRED_FILES[@]}"; do
  if [ ! -f "$DATA_DIR/$f" ]; then
    echo "ERROR: Missing data file: $DATA_DIR/$f"
    echo "       Download NLSS IV microdata from NSO Nepal and place in:"
    echo "       $DATA_DIR/"
    exit 1
  fi
  echo "      ✓ $f"
done

echo ""
echo "========================================================"
echo " STEP 1/8 — Explore microdata structure"
echo "========================================================"
Rscript R/00_explore_microdata.R 2>&1 | tee outputs/log_01_explore.txt
echo "✓ Step 1 complete"

echo ""
echo "========================================================"
echo " STEP 2/8 — Check variable value distributions"
echo "========================================================"
Rscript R/00b_check_values.R 2>&1 | tee outputs/log_02_check_values.txt
echo "✓ Step 2 complete"

echo ""
echo "========================================================"
echo " STEP 3/8 — Load, merge and clean data"
echo "========================================================"
Rscript R/01_load_clean_v2.R 2>&1 | tee outputs/log_03_load_clean.txt
echo "✓ Step 3 complete — outputs/cleaned_data_v2.RData"

echo ""
echo "========================================================"
echo " STEP 4/8 — Fix wage construction"
echo "========================================================"
Rscript R/00d_fix_wages.R 2>&1 | tee outputs/log_04_fix_wages.txt
echo "✓ Step 4 complete"

echo ""
echo "========================================================"
echo " STEP 5/8 — Descriptive figures and tables"
echo "========================================================"
Rscript R/02_descriptives.R 2>&1 | tee outputs/log_05_descriptives.txt
echo "✓ Step 5 complete — outputs/figures/fig*.png"

echo ""
echo "========================================================"
echo " STEP 6/8 — Regression models (enrolment, NEET, employment)"
echo "========================================================"
Rscript R/03_regressions_v2.R 2>&1 | tee outputs/log_06_regressions.txt
echo "✓ Step 6 complete — outputs/regression_results_v2.RData"

echo ""
echo "========================================================"
echo " STEP 7/8 — Mincer wage equations and CoI projection"
echo "========================================================"
Rscript R/03b_mincer.R 2>&1 | tee outputs/log_07_mincer.txt
echo "✓ Step 7 complete"

echo ""
echo "========================================================"
echo " STEP 7b/8 — Wage visualisations"
echo "========================================================"
Rscript R/03c_wage_plots.R 2>&1 | tee outputs/log_07b_wage_plots.txt
echo "✓ Step 7b complete"

echo ""
echo "========================================================"
echo " STEP 8/8 — Causal forest (heterogeneous treatment effects)"
echo "========================================================"
echo "  NOTE: First run installs grf package (~2 min)"
echo "  NOTE: Forest training takes ~5-10 min with num.trees=2000"
Rscript R/03d_causal_forest.R 2>&1 | tee outputs/log_08_causal_forest.txt
echo "✓ Step 8 complete — outputs/causal_forest_results.RData"

echo ""
echo "========================================================"
echo " RENDERING REPORT"
echo "========================================================"
if [ "$QUARTO" = true ]; then
  quarto render reports/nepal_nlss_analysis.qmd 2>&1 | tee outputs/log_report.txt
  echo "✓ Report rendered — reports/nepal_nlss_analysis.pdf"
  quarto render reports/nepal_nlss_slides.qmd 2>&1 | tee outputs/log_slides.txt
  echo "✓ Slides rendered — reports/nepal_nlss_slides.pdf"
else
  echo "  Skipping — quarto not available"
fi

echo ""
echo "========================================================"
echo " REPLICATION COMPLETE — $(date)"
echo "========================================================"
echo ""
echo " Key outputs:"
echo "   outputs/cleaned_data_v2.RData       — Merged analytical dataset"
echo "   outputs/regression_results_v2.RData — LPM regression results"
echo "   outputs/causal_forest_results.RData — Causal forest CATEs"
echo "   outputs/figures/                     — All figures (PNG)"
echo "   outputs/tables/                      — All tables (CSV)"
echo "   reports/nepal_nlss_analysis.pdf      — Full report"
echo "   reports/nepal_nlss_slides.pdf        — Presentation slides"
echo ""
echo " Runtime (approximate):"
echo "   Load + clean:      2 min"
echo "   Regressions:       3 min"
echo "   Causal forest:    10 min"
echo "   Total:           ~15 min"
