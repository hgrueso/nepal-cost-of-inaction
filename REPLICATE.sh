#!/usr/bin/env bash
# Nepal cost of inaction: full pipeline from the raw NLSS IV microdata.
# Place the NLSS IV .dta files in data/NLSSIV_microdata/data/stata format/ and run from the analysis/ root:
#   bash REPLICATE.sh
# Stage 1 rebuilds outputs/cleaned_data_v2.RData from the raw files. To rerun only the analysis on an
# existing cleaned file, use: bash run_pipeline.sh
set -e
mkdir -p outputs/logs outputs/figures/slides outputs/tables

echo "=== Stage 1: cleaned file from the raw microdata ==="
Rscript R/01_load_clean_v2.R  > outputs/logs/01_load_clean_v2.log 2>&1
Rscript R/00d_fix_wages.R     > outputs/logs/00d_fix_wages.log 2>&1
Rscript R/00f_secondary_flag.R > outputs/logs/00f_secondary_flag.log 2>&1

echo "=== Stage 2: sample and weight correction, models, figures, tables, key numbers ==="
bash run_pipeline.sh

echo "=== Stage 3: slide decks ==="
for d in nepal_slides_en nepal_slides_short; do
  for f in beamer pptx; do quarto render "reports/$d.qmd" --to "$f" > "outputs/logs/render_${d}_$f.log" 2>&1; done
done
echo "Done. Key numbers: outputs/tables/key_numbers.csv | Logs: outputs/logs/"
