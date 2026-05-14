# =============================================================================
# 00_explore_microdata.R — Explore full NLSS IV microdata files
# Run this BEFORE 01_load_clean.R to understand variable structures
# =============================================================================
# HOW TO RUN:
#   Rscript R/00_explore_microdata.R
# =============================================================================

library(haven)
library(tidyverse)

BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
cat("Looking for files in:", BASE, "\n\n")

# Helper: quick summary of a .dta file
explore_file <- function(fname, vars_to_show = NULL) {
  path <- file.path(BASE, fname)
  if (!file.exists(path)) { cat("  NOT FOUND:", path, "\n"); return(invisible(NULL)) }
  df <- read_dta(path)
  cat("────────────────────────────────────────\n")
  cat("FILE:", fname, "  |  rows:", nrow(df), "  cols:", ncol(df), "\n")
  vv <- data.frame(
    name  = names(df),
    label = sapply(df, function(x) { l <- attr(x,"label"); if(is.null(l)) "" else as.character(l)[1] })
  )
  print(vv, row.names = FALSE)
  if (!is.null(vars_to_show)) {
    cat("\n  --- VALUE DISTRIBUTIONS ---\n")
    for (v in vars_to_show) {
      if (v %in% names(df)) {
        cat("\n  ", v, ":\n")
        x <- df[[v]]
        if (inherits(x, "haven_labelled")) print(table(as_factor(x), useNA="always"))
        else if (is.numeric(x)) print(summary(x))
        else print(table(x, useNA="always"))
      }
    }
  }
  cat("\n")
  invisible(df)
}

# =============================================================================
# KEY FILES — explore in order of priority
# =============================================================================

cat("\n\n=== S01: HOUSEHOLD ROSTER (demographics, caste, marital status) ===\n")
explore_file("S01.dta",
  vars_to_show = c("Q01_02","Q01_03","Q01_05","Q01_07","Q01_08"))

cat("\n\n=== S07: EDUCATION (literacy, attendance, highest level completed) ===\n")
explore_file("S07.dta",
  vars_to_show = c("Q07_02","Q07_03","Q07_05","Q07_06","Q07_11","Q07_12"))

cat("\n\n=== S08: HEALTH AND DISABILITY (Washington Group Q08_16-Q08_21) ===\n")
explore_file("S08.dta",
  vars_to_show = c("Q08_16","Q08_17","Q08_18","Q08_19","Q08_20","Q08_21"))

cat("\n\n=== S09: EMPLOYMENT (last 7 days activity, job status) ===\n")
explore_file("S09.dta",
  vars_to_show = c("Q09_02","Q09_03","Q09_04","Q09_05","Q09_10","Q09_15","Q09_18","Q09_21"))

cat("\n\n=== S10: WAGE JOBS (salary/wage work, earnings) ===\n")
explore_file("S10.dta",
  vars_to_show = c("Q10_03","Q10_05","Q10_06","Q10_07","Q10_09_A","Q10_09_B"))

cat("\n\n=== POVERTY: Weights and consumption ===\n")
explore_file("poverty.dta",
  vars_to_show = c("poor","quintile_pcep","pcep","prov","ad_4"))

cat("\n\n=== WEIGHT: Survey weights ===\n")
explore_file("weight.dta")

cat("\n\n=== S04: MIGRATION ===\n")
explore_file("S04.dta",
  vars_to_show = c("Q04_02","Q04_05","Q04_09"))

cat("\n\n=== S03: ACCESS TO FACILITIES (distance to school, health) ===\n")
explore_file("S03.dta",
  vars_to_show = c("S03_CODE","S03_DESC","Q03_02_B","Q03_02_C","Q03_04"))

cat("\nDone. Check outputs above to verify variable codings before running 01_load_clean.R\n")
