# =============================================================================
# 00e_diagnostics.R — Run from analysis/ directory to check all issues
# =============================================================================
library(haven); library(tidyverse); library(srvyr); library(fixest)
load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")
load("outputs/causal_forest_results.RData")

cat("=== 1. SEX CODING CHECK ===\n")
cat("female: 0=Male, 1=Female\n")
cat("N Male:", sum(df$female==0,na.rm=TRUE), "| N Female:", sum(df$female==1,na.rm=TRUE),"\n")
df %>% filter(youth_1524==1, !is.na(neet), !is.na(sex_lbl)) %>%
  group_by(sex_lbl) %>%
  summarise(pct_neet=round(mean(neet)*100,1), pct_enrol=round(mean(attend_any)*100,1), n=n()) %>%
  print()

cat("\n=== 2. SURVEY WEIGHTS ===\n")
cat("ind_wt range:", round(range(df$ind_wt,na.rm=TRUE),2), "\n")
cat("N with weight:", sum(!is.na(df$ind_wt)), "\n")

cat("\n=== 3. WEIGHTED ENROLMENT BY SEX AND PROVINCE (youth 15-24) ===\n")
svy_b %>% group_by(province_lbl, sex_lbl) %>%
  summarise(pct=round(survey_mean(attend_any==1)*100,1), .groups="drop") %>%
  pivot_wider(names_from=sex_lbl, values_from=pct) %>%
  mutate(diff=Female-Male) %>% print()

cat("\n=== 4. OLS FE vs CF CATE DIRECTION ===\n")
ols_fe <- fixef(reg_results_v2$m2$full)[["province_lbl"]]
cat("OLS province FE (NEET model):\n")
print(round(sort(ols_fe,decreasing=TRUE)*100,1))
cat("\nCF CATE by province (NEET outcome):\n")
cf_results$cate_neet_all %>% filter(dimension=="Province") %>%
  select(group,mean_cate) %>%
  mutate(mean_cate=round(mean_cate*100,1)) %>% arrange(mean_cate) %>% print()

cat("\n=== 5. OTHER CASTE GROUP ===\n")
cat("What is in 'Others & Not stated':\n")
cat("N:", sum(df$caste_lbl=="Others & Not stated", na.rm=TRUE), "\n")
df %>% filter(caste_lbl=="Others & Not stated") %>%
  summarise(n=n(), pct_f=round(mean(female,na.rm=TRUE)*100,1),
            mean_age=round(mean(age_num,na.rm=TRUE),1),
            pct_poor=round(mean(ppoor,na.rm=TRUE)*100,1)) %>% print()

cat("\n=== 6. NEET RATES BY SEX AND PROVINCE ===\n")
svy_b %>% group_by(province_lbl, sex_lbl) %>%
  summarise(pct_neet=round(survey_mean(neet==1,na.rm=TRUE)*100,1), .groups="drop") %>%
  pivot_wider(names_from=sex_lbl, values_from=pct_neet) %>%
  mutate(diff_F_minus_M=Female-Male) %>% print()
