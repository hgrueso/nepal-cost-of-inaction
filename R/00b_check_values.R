# =============================================================================
# 00b_check_values.R — Check value distributions for key variables
# Run after 00_explore_microdata.R confirmed file structure
# =============================================================================
library(haven); library(tidyverse)

BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
rd <- function(f) read_dta(file.path(BASE, f))

s01 <- rd("S01.dta"); s07 <- rd("S07.dta")
s08 <- rd("S08.dta"); s09 <- rd("S09.dta")
s10 <- rd("S10.dta")

pv <- function(df, v) {
  cat("\n──", v, "──\n")
  x <- df[[v]]
  if (inherits(x, "haven_labelled")) print(table(as_factor(x), useNA="always"))
  else print(summary(x))
}

cat("=== S01 ===\n")
pv(s01, "q01_02")   # sex
pv(s01, "q01_03")   # age
pv(s01, "q01_05")   # marital status
pv(s01, "q01_07")   # caste

cat("\n=== S07 ===\n")
pv(s07, "q07_02")   # can you read
pv(s07, "q07_03")   # can you write
pv(s07, "q07_05")   # school type last attended (non-enrolled)
pv(s07, "q07_06")   # highest level completed (non-enrolled)
pv(s07, "q07_11")   # currently attending school type
pv(s07, "q07_12")   # highest level if currently enrolled

cat("\n=== S08 Washington Group ===\n")
for (v in paste0("q08_", 16:21)) pv(s08, v)

cat("\n=== S09 Employment ===\n")
pv(s09, "q09_02")   # wage work last 7 days
pv(s09, "q09_03")   # self-employment last 7 days
pv(s09, "q09_04")   # unpaid family work last 7 days
pv(s09, "q09_05")   # absent from job but will return
pv(s09, "q09_10")   # employment status
pv(s09, "q09_18")   # looked for work last 30 days
pv(s09, "q09_21")   # willing to work if opportunity

cat("\n=== S10 Wages ===\n")
pv(s10, "q10_03")   # worked for wages last 12 months
pv(s10, "q10_05")   # condition of work (formal/informal)
cat("\nq10_09_a (take-home pay):\n")
x <- as.numeric(s10$q10_09_a)
cat("N non-missing:", sum(!is.na(x) & x>0), "\n")
print(summary(x[x>0]))
