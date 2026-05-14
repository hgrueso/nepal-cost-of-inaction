library(haven); library(tidyverse); library(srvyr)

BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
s10  <- read_dta(file.path(BASE, "S10.dta"))

s10_fixed <- s10 %>%
  mutate(
    cond       = as.character(as_factor(q10_05)),
    days_wkd   = as.numeric(q10_06),
    daily_cash = as.numeric(q10_07),
    salary_ann = as.numeric(q10_09_a),
    contract   = as.numeric(q10_11_a),
    annual_wage_fixed = case_when(
      cond == "Long term salary" & salary_ann > 0  ~ salary_ann,
      cond == "Daily wage" & daily_cash > 0 & days_wkd > 0 ~ daily_cash * days_wkd,
      cond == "Contract"   & contract > 0           ~ contract,
      TRUE ~ NA_real_)
  ) %>%
  group_by(psu_number, hh_number, idcode) %>%
  slice_max(order_by=coalesce(annual_wage_fixed,0), n=1, with_ties=FALSE) %>%
  ungroup() %>%
  select(psu_number, hh_number, idcode, annual_wage_fixed, cond,
         days_wkd, daily_cash, salary_ann)

cat("Corrected wage distribution:\n")
print(summary(s10_fixed$annual_wage_fixed[s10_fixed$annual_wage_fixed>0]))

load("outputs/cleaned_data_v2.RData")
KEY <- c("psu_number","hh_number","idcode")

df <- df %>%
  select(-any_of(c("annual_wage_fixed","cond","days_wkd","daily_cash",
                    "salary_ann","annual_wage_clean","monthly_wage","log_wage"))) %>%
  left_join(s10_fixed, by=KEY) %>%
  mutate(
    annual_wage_clean = if_else(annual_wage_fixed > 0, annual_wage_fixed, NA_real_),
    monthly_wage      = annual_wage_clean / 12,
    log_wage          = if_else(!is.na(annual_wage_clean), log(annual_wage_clean), NA_real_)
  )

cat("\nN wage workers (corrected):", sum(!is.na(df$annual_wage_clean)), "\n")
cat("Median monthly wage:", round(median(df$monthly_wage, na.rm=TRUE)), "NPR\n\n")

df_c          <- df %>% filter(age_num>=15, age_num<=64)
df_b          <- df %>% filter(age_num>=15, age_num<=24)
df_b1         <- df %>% filter(age_num>=15, age_num<=18)
df_b2         <- df %>% filter(age_num>=19, age_num<=24)
df_girls      <- df_b  %>% filter(female==1)
df_girls1     <- df_b1 %>% filter(female==1)
df_girls2     <- df_b2 %>% filter(female==1)
df_wage_w     <- df_c  %>% filter(female==1, !is.na(log_wage))
df_wage_w_sal <- df_c  %>% filter(female==1, !is.na(log_wage), cond=="Long term salary")

cat("Women wage workers (all):", nrow(df_wage_w), "\n")
cat("Women salary workers:    ", nrow(df_wage_w_sal), "\n\n")

cat("Median monthly wage by edu_cat (all workers):\n")
df_wage_w %>% filter(!is.na(edu_cat)) %>%
  group_by(edu_cat) %>%
  summarise(n=n(), med=round(median(monthly_wage,na.rm=TRUE))) %>% print()

cat("\nMedian monthly wage by edu_cat (salary only):\n")
df_wage_w_sal %>% filter(!is.na(edu_cat)) %>%
  group_by(edu_cat) %>%
  summarise(n=n(), med=round(median(monthly_wage,na.rm=TRUE))) %>% print()

make_svy <- function(data) {
  d <- data %>% filter(!is.na(ind_wt), !is.na(province_num))
  srvyr::as_survey_design(d, ids=psu_number, strata=province_num,
                           weights=ind_wt, nest=TRUE)
}
svy_all <- make_svy(df); svy_b <- make_svy(df_b); svy_b1 <- make_svy(df_b1)
svy_b2 <- make_svy(df_b2); svy_girls <- make_svy(df_girls)
svy_girls1 <- make_svy(df_girls1); svy_girls2 <- make_svy(df_girls2)
svy_c <- make_svy(df_c)

save(df, df_b, df_b1, df_b2, df_girls, df_girls1, df_girls2,
     df_c, df_wage_w, df_wage_w_sal,
     svy_all, svy_b, svy_b1, svy_b2, svy_girls, svy_girls1, svy_girls2, svy_c,
     sample_flow, miss_tbl,
     file="outputs/cleaned_data_v2.RData")
cat("\n✓ Saved corrected data.\n")
cat("  Run: Rscript R/03b_mincer.R\n")
