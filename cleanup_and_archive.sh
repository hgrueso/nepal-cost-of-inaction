#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────────────────────
# cleanup_and_archive.sh
# 1. Archives non-essential files into archive/ subfolders
# 2. Removes accidental files from the repo root
# 3. Configures git to identify project as R (not HTML)
# Run from analysis/ root
# ──────────────────────────────────────────────────────────────────────────────

set -e

echo "═══════════════════════════════════════════════════════════════════"
echo "  Cleanup and Archive — Nepal Cost of Inaction Repository"
echo "═══════════════════════════════════════════════════════════════════"

# ── 1. Create archive structure ──────────────────────────────────────────────
echo ""
echo "→ Step 1/4: Creating archive folders..."
mkdir -p archive/R archive/reports archive/outputs/logs
echo "  ✓ archive/R, archive/reports, archive/outputs/logs created"

# ── 2. Archive non-essential R scripts ───────────────────────────────────────
echo ""
echo "→ Step 2/4: Archiving non-essential R scripts..."

# ESSENTIAL R scripts (keep in R/):
#   00d_fix_wages.R              # wage cleaning - upstream of everything
#   01_load_clean_v2.R           # current data loader
#   02_descriptives.R            # descriptives
#   03_regressions_v2.R          # current regression
#   03b_mincer.R                 # Mincer
#   03d_causal_forest.R          # CF
#   03e_fix_coi.R                # CoI
#   03l_coi_scaled.R             # CoI scaled to population
#   03m_boys_and_intersectionality.R   # boys + intersectionality
#   04_slides_figures.R          # final policy figures

R_ARCHIVE=(
  "04_slides_figures.R.bak"
  "04_slides_figures.R.bak3"
  "03c_wage_plots.R"        # superseded by 03b
  "03f_new_figures.R"       # iteration
  "03g_slides_fixes.R"      # iteration
  "03h_final_fixes.R"       # iteration
  "03i_descriptives_and_primary.R"   # superseded by 02 + 04
  "03j_fix_labels_and_cate.R"        # iteration
  "03k_final_figures.R"     # superseded by 04
  "03_regressions.R"        # superseded by _v2
  "01_load_clean.R"         # superseded by _v2
  "00_explore_microdata.R"  # exploratory
  "00_patch.R"              # one-time
  "00_master.R"             # superseded by REPLICATE.sh
  "00b_check_values.R"      # diagnostic
  "00c_diagnose_rural.R"    # diagnostic
  "00e_diagnostics.R"       # diagnostic
)

for f in "${R_ARCHIVE[@]}"; do
  if [ -f "R/$f" ]; then
    mv "R/$f" "archive/R/"
    echo "  ↪ archived R/$f"
  fi
done

# ── 3. Archive non-essential reports ─────────────────────────────────────────
echo ""
echo "→ Step 3/4: Archiving old reports..."

REPORT_ARCHIVE=(
  "nepal_nlss_slides.qmd"
  "nepal_nlss_slides.pdf"
  "nepal_nlss_slides.html"
  "nepal_nlss_slides_v2.qmd"
  "nepal_nlss_slides_v2.pdf"
  "nepal_nlss_slides_v3.qmd"
  "nepal_nlss_slides_v3.pdf"
  "nepal_nlss_slides_v3.tex"
  "custom.scss"
  "nepal_nlss_analysis.qmd"
  "nepal_nlss_analysis.pdf"
)

for f in "${REPORT_ARCHIVE[@]}"; do
  if [ -f "reports/$f" ]; then
    mv "reports/$f" "archive/reports/"
    echo "  ↪ archived reports/$f"
  fi
done

# Move build directories
for d in nepal_nlss_slides_v3_files nepal_nlss_slides_files; do
  if [ -d "reports/$d" ]; then
    mv "reports/$d" "archive/reports/"
    echo "  ↪ archived reports/$d/"
  fi
done

# ── 4. Archive logs and clean stray files in root ────────────────────────────
echo ""
echo "→ Step 4/4: Archiving logs and removing stray files..."

# Move all log files from outputs/ to archive/outputs/logs
for f in outputs/log_*.txt outputs/*_output.txt; do
  if [ -f "$f" ]; then
    mv "$f" "archive/outputs/logs/"
    echo "  ↪ archived $f"
  fi
done

# Remove stray patch and diagnose files from root
for f in patch.sh patch2.sh patch3.sh diagnose.sh diagnose_vars.R Rplots.pdf; do
  if [ -f "$f" ]; then
    rm "$f"
    echo "  ✗ removed $f (stray helper)"
  fi
done

# Old superseded .RData files in outputs (keep only the _v2 / current ones)
OLD_RDATA=("analysis_results.RData" "descriptive_results.RData" "cleaned_data.RData")
for f in "${OLD_RDATA[@]}"; do
  if [ -f "outputs/$f" ]; then
    mv "outputs/$f" "archive/outputs/"
    echo "  ↪ archived outputs/$f"
  fi
done

# Old variable codebook (superseded)
if [ -f "outputs/variable_codebook.csv" ]; then
  mv "outputs/variable_codebook.csv" "archive/outputs/"
  echo "  ↪ archived outputs/variable_codebook.csv"
fi

# ── 5. Tell GitHub this is R (linguist override) ─────────────────────────────
echo ""
echo "→ Final: Configuring GitHub language detection..."
cat > .gitattributes << 'GIT'
# Tell GitHub's linguist this is an R project, not HTML
*.html linguist-generated=true
*.css linguist-generated=true
reports/*.html linguist-vendored=true
archive/* linguist-vendored=true
*.R linguist-detectable=true
GIT
echo "  ✓ .gitattributes created (forces R detection)"

# Update .gitignore to exclude archive from commits (optional - or keep them)
cat > .gitignore << 'GIT'
# Raw microdata — not redistributed
data/NLSSIV_microdata/
data/*.dta
data/*.sav

# Build artefacts and R workspace
.Rhistory
.RData
.Rproj.user/
*.Rproj
Rplots.pdf

# Generated outputs
outputs/*.RData
outputs/log_*.txt
outputs/*_output.txt

# Quarto build artefacts
reports/*_files/
reports/.quarto/
reports/*.tex
reports/*.log
reports/*.aux
reports/*.knit.md

# Archive folder (locally kept, but not pushed to GitHub)
archive/

# OS files
.DS_Store
Thumbs.db
GIT
echo "  ✓ .gitignore updated (archive/ now excluded from pushes)"

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "  Cleanup complete."
echo "  Essential files remain in R/, outputs/, reports/."
echo "  Non-essential files moved to archive/ (kept locally, not pushed)."
echo "═══════════════════════════════════════════════════════════════════"
echo ""
echo "  To push the cleaned repository:"
echo ""
echo "    git rm -rf --cached archive/ 2>/dev/null || true"
echo "    git add -A"
echo "    git commit -m 'Cleanup: archive iteration scripts; fix R language detection'"
echo "    git push"
echo ""
