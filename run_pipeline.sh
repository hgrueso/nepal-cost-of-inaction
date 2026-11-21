#!/usr/bin/env bash
# Rebuilds every table and figure used in the report and slides from the cleaned data.
# Run from the analysis/ root:  bash run_pipeline.sh
# Each script writes its log to outputs/logs/; a one-line status per script goes to outputs/logs/_status.txt
mkdir -p outputs/logs outputs/tables outputs/figures/slides
: > outputs/logs/_status.txt
run() {
  local s="$1" t0=$SECONDS st
  printf '%-28s ' "$s"
  if Rscript "R/$s.R" > "outputs/logs/$s.log" 2>&1; then st=OK; else st=FAILED; fi
  echo "$st ($((SECONDS - t0))s)"; echo "$s $st" >> outputs/logs/_status.txt
  [ "$st" = OK ]
}
run 01b_fix_sample_and_weights || { echo "The data correction did not run; stopping. See outputs/logs/01b_fix_sample_and_weights.log"; exit 1; }
for s in 03_regressions_v2 03b_mincer 03d_causal_forest 04_slides_figures 03p_mincer_main 05_census_anchor_coi \
         06_age_disagg_figures 07_final_charts 09_causal_forest_figure 08_slide_figures 10_v5_comment_checks 12_key_numbers; do
  run "$s"
done
echo; echo "Done. Status:"; cat outputs/logs/_status.txt
