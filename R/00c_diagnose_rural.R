library(haven); library(tidyverse)

BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
s01     <- read_dta(file.path(BASE, "S01.dta"))
poverty <- read_dta(file.path(BASE, "poverty.dta"))

cat("=== POVERTY ad_4 before merge ===\n")
cat("Class:", class(poverty$ad_4), "\n")
cat("Values:\n"); print(table(as.numeric(poverty$ad_4), useNA="always"))

cat("\n=== hh_number overlap ===\n")
cat("Unique hh in S01:    ", length(unique(s01$hh_number)), "\n")
cat("Unique hh in poverty:", length(unique(poverty$hh_number)), "\n")
cat("Overlap:             ", length(intersect(s01$hh_number, poverty$hh_number)), "\n")

cat("\n=== After merge — does ad_4 survive? ===\n")
df <- left_join(s01, poverty %>% select(hh_number, ad_4, ind_wt, prov, poor,
                                         quintile_pcep, pcep),
                by = "hh_number")
cat("Columns:", paste(names(df), collapse=", "), "\n\n")
cat("ad_4 after merge:\n")
cat("Class:", class(df$ad_4), "\n")
print(table(as.numeric(df$ad_4), useNA="always"))
cat("\nas.numeric values unique:", unique(as.numeric(df$ad_4)), "\n")
