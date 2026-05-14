# =============================================================================
# 03b_mincer.R — Mincer wage equations, restricted to salary workers
# =============================================================================
library(tidyverse); library(fixest); library(modelsummary)

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

stars   <- c("*"=0.10, "**"=0.05, "***"=0.01)
gof_map <- tribble(~raw,~clean,~fmt,"nobs","N",0,"r.squared","R2",3)

cat("N salary workers (Mincer sample):", nrow(df_wage_w_sal), "\n")

prep_mincer <- function(data) {
  data %>%
    filter(!is.na(edu_cat), !is.na(log_wage)) %>%
    mutate(
      edu_yrs = case_when(
        as.character(edu_cat) == "No education"     ~  0,
        as.character(edu_cat) == "Primary"          ~  5,
        as.character(edu_cat) == "Lower secondary"  ~  8,
        as.character(edu_cat) == "Secondary"        ~ 10,
        as.character(edu_cat) == "Higher secondary" ~ 12,
        as.character(edu_cat) == "Tertiary"         ~ 16,
        TRUE ~ NA_real_),
      experience  = pmax(age_num - edu_yrs - 6, 0, na.rm=TRUE),
      experience2 = experience^2,
      edu_yrs_c   = edu_yrs - mean(edu_yrs, na.rm=TRUE)
    ) %>% filter(!is.na(edu_yrs))
}

sal   <- prep_mincer(df_wage_w_sal)
all_w <- prep_mincer(df_wage_w)

cat("Mincer sample (salary): N =", nrow(sal), "\n")
cat("Mean log wage by education (salary workers):\n")
sal %>% group_by(edu_cat) %>%
  summarise(n=n(), mean_log=round(mean(log_wage),3),
            mean_monthly=round(mean(monthly_wage, na.rm=TRUE))) %>% print()

m_sal_base <- feols(log_wage ~ edu_yrs_c + I(edu_yrs_c^2) +
                      experience + experience2 + rural,
                    data=sal, weights=~ind_wt, se="hc1")
m_sal_pfe  <- feols(log_wage ~ edu_yrs_c + I(edu_yrs_c^2) +
                      experience + experience2 + rural + caste_broad | province_lbl,
                    data=sal, weights=~ind_wt, se="hc1")
m_sal_cat  <- feols(log_wage ~ edu_cat + experience + experience2 +
                      rural + caste_broad | province_lbl,
                    data=sal, weights=~ind_wt, se="hc1")
m_all_pfe  <- feols(log_wage ~ edu_yrs_c + I(edu_yrs_c^2) +
                      experience + experience2 + rural + caste_broad | province_lbl,
                    data=all_w, weights=~ind_wt, se="hc1")

cat("\n=== MINCER RESULTS ===\n")
modelsummary(
  list("(1) Salary Basic"=m_sal_base,"(2) Salary+FE"=m_sal_pfe,
       "(3) Edu Levels"=m_sal_cat,   "(4) All Workers"=m_all_pfe),
  stars=stars, gof_map=gof_map,
  coef_map=c("edu_yrs_c"="Years of schooling",
             "I(edu_yrs_c^2)"="Years squared",
             "experience"="Experience",
             "experience2"="Experience squared",
             "rural"="Rural",
             "caste_broadDalit"="Dalit",
             "edu_catPrimary"="Primary",
             "edu_catLower secondary"="Lower Secondary",
             "edu_catSecondary"="Secondary",
             "edu_catHigher secondary"="Higher Secondary",
             "edu_catTertiary"="Tertiary"),
  title="Mincer Wage Equations — Women 15-64"
)

r_per_yr <- coef(m_sal_pfe)["edu_yrs_c"] * 100
median_wage <- median(sal$monthly_wage, na.rm=TRUE)
cat(sprintf("\n=== RETURNS & COST-OF-INACTION ===\n"))
cat(sprintf("Return per year of schooling: %.1f%%\n", r_per_yr))
cat(sprintf("Median monthly wage (salary workers): NPR %s\n",
            format(round(median_wage), big.mark=",")))

for (r in c(r_per_yr, 9.0)) {
  foregone <- median_wage * 12 * ((1+r/100)^2 - 1)
  pv       <- foregone * (1-(1.05)^(-35)) / 0.05
  cat(sprintf("\n  Return=%.1f%%: foregone annual NPR %s | PV NPR %s\n",
              r, format(round(foregone),big.mark=","), format(round(pv),big.mark=",")))
}

reg_results_v2$m5 <- list(sal_base=m_sal_base, sal_pfe=m_sal_pfe,
                            sal_cat=m_sal_cat, all_pfe=m_all_pfe,
                            returns_pct=r_per_yr, median_wage=median_wage)
save(reg_results_v2, file="outputs/regression_results_v2.RData")
cat("\n✓ Saved.\n")
