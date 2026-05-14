#!/usr/bin/env bash
# Nepal Cost-of-Inaction — full replication pipeline
# Run from analysis/ root

set -e
mkdir -p outputs/figures outputs/tables

echo "=== Stage 1: Data preparation ==="
Rscript R/00d_fix_wages.R
Rscript R/01_load_clean_v2.R
Rscript R/00e_diagnostics.R

echo "=== Stage 2: Descriptives ==="
Rscript R/02_descriptives.R
Rscript R/03i_descriptives_and_primary.R

echo "=== Stage 3: Regressions ==="
Rscript R/03_regressions_v2.R
Rscript R/03b_mincer.R
Rscript R/03c_wage_plots.R

echo "=== Stage 4: Causal forest ==="
Rscript R/03d_causal_forest.R

echo "=== Stage 5: Cost of inaction ==="
Rscript R/03e_fix_coi.R
Rscript R/03l_coi_scaled.R

echo "=== Stage 6: Boys + intersectionality ==="
Rscript R/03m_boys_and_intersectionality.R

echo "=== Stage 7: Final policy figures ==="
Rscript R/04_slides_figures.R

echo "=== Stage 8: Render slides (HTML + PDF) ==="
quarto render reports/nepal_slides_en.qmd

echo ""
echo "Pipeline complete. Outputs:"
echo "  Slides:     reports/nepal_slides_en.html / .pdf"
echo "  Figures:    outputs/figures/"
echo "  Tables:     outputs/tables/"
