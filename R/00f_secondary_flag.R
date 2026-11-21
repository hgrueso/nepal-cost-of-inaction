# =============================================================================
# 00f_secondary_flag.R
# Adds the current-grade flags used in the descriptive figures to the cleaned file:
#   in_secondary      currently attending grade 9-10 (Class 9, Class 10, SLC)
#   in_sec_or_higher  currently attending grade 9-12
# Same definition as archive/R/03g_slides_fixes.R, without that script's other steps.
# Run after 01_load_clean_v2.R and 00d_fix_wages.R. Keeps every object in the file.
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(haven) })
RD <- "outputs/cleaned_data_v2.RData"
e <- new.env(); load(RD, envir = e)
s07 <- read_dta(file.path("data", "NLSSIV_microdata", "data", "stata format", "S07.dta"))
g <- s07 %>%
  select(psu_number, hh_number, idcode, q07_12) %>%
  mutate(grade_lbl = as.character(as_factor(q07_12)),
         in_secondary = case_when(grade_lbl %in% c("Class 9", "Class 10", "SLC") ~ 1L,
                                  !is.na(grade_lbl) ~ 0L, TRUE ~ NA_integer_),
         in_sec_or_higher = case_when(grade_lbl %in% c("Class 9", "Class 10", "SLC", "Intermediate/Class 12") ~ 1L,
                                      !is.na(grade_lbl) ~ 0L, TRUE ~ NA_integer_)) %>%
  select(psu_number, hh_number, idcode, in_secondary, in_sec_or_higher)
e$df <- e$df %>%
  select(-any_of(c("in_secondary", "in_sec_or_higher"))) %>%
  left_join(g, by = c("psu_number", "hh_number", "idcode")) %>%
  mutate(in_secondary = replace_na(in_secondary, 0L), in_sec_or_higher = replace_na(in_sec_or_higher, 0L))
cat(sprintf("Youth 15-24 attending grade 9-10: %.1f%% (unweighted)\n",
            100 * mean(e$df$in_secondary[e$df$age_num >= 15 & e$df$age_num <= 24], na.rm = TRUE)))
save(list = ls(e), envir = e, file = RD)
cat("Saved", RD, "\n")
