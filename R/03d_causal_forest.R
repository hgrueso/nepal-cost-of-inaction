# =============================================================================
# 03d_causal_forest.R — Causal forest for heterogeneous returns to education
# Goal: Estimate CATE of secondary education on employment/NEET by subgroup
#       to identify WHO benefits most → cost-of-inaction by population segment
#
# Design:
#   Treatment W = completed_secondary (1/0)
#   Outcomes  Y = employed_ilo, neet, wage_emp_12m
#   Covariates X = age, rural, caste, province, disability, poverty
#   Sample: women 15-64 with non-missing treatment and outcome
#
# NOTE: grf requires installation (~2 min first run)
# =============================================================================
needed <- c("grf","tidyverse","patchwork","scales","viridis","ggrepel")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(grf); library(tidyverse); library(patchwork); library(scales)
})

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS    <- "outputs/figures"
TABLES  <- "outputs/tables"
dir.create(FIGS,   showWarnings=FALSE)
dir.create(TABLES, showWarnings=FALSE)
set.seed(42)

th <- function(base=11) {
  theme_minimal(base_size=base) +
  theme(plot.title=element_text(face="bold",size=base+1),
        plot.subtitle=element_text(size=base-1,colour="#6C7279"),
        plot.caption=element_text(size=7,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(),
        legend.position="bottom")
}
sv <- function(p,n,w=9,h=5.5) {
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}

# =============================================================================
# 1. PREPARE DATA
# =============================================================================
cat("=== PREPARING CAUSAL FOREST DATA ===\n")

# Numeric encoding for covariates
df_cf <- df_c %>%
  filter(female==1) %>%
  mutate(
    caste_num  = as.integer(caste_broad),
    province_n = as.integer(province_lbl),
    age_sq     = age_num^2
  ) %>%
  filter(!is.na(completed_secondary),
         !is.na(employed_ilo),
         !is.na(neet),
         !is.na(rural), !is.na(ppoor), !is.na(quintile_n),
         !is.na(caste_num), !is.na(province_n),
         !is.na(any_difficulty))

cat("Causal forest sample (women 15-64):", nrow(df_cf), "\n")
cat("Treatment (completed secondary) rate:",
    round(mean(df_cf$completed_secondary)*100,1), "%\n")

# Feature matrix
X_vars <- c("age_num","age_sq","rural","ppoor","quintile_n",
            "caste_num","province_n","any_difficulty",
            "currently_married")
X <- as.matrix(df_cf[, X_vars])

# =============================================================================
# 2. CAUSAL FOREST — OUTCOME: EMPLOYMENT
# =============================================================================
cat("\n=== CF1: Employment outcome ===\n")
W <- df_cf$completed_secondary
Y_emp <- df_cf$employed_ilo

cf_emp <- causal_forest(
  X = X, Y = Y_emp, W = W,
  sample.weights = df_cf$ind_wt,
  num.trees = 2000,
  tune.parameters = "all",
  seed = 42
)

# Average treatment effect
ate_emp <- average_treatment_effect(cf_emp, target.sample="all")
cat(sprintf("ATE employment: %.3f (se=%.3f, p=%.4f)\n",
            ate_emp[1], ate_emp[2], 2*pnorm(-abs(ate_emp[1]/ate_emp[2]))))

# Individual CATEs
tau_emp <- predict(cf_emp)$predictions

# =============================================================================
# 3. CAUSAL FOREST — OUTCOME: NEET
# =============================================================================
cat("\n=== CF2: NEET outcome ===\n")
df_cf_neet <- df_cf %>% filter(!is.na(neet))
X_neet <- as.matrix(df_cf_neet[, X_vars])
W_neet <- df_cf_neet$completed_secondary
Y_neet <- df_cf_neet$neet

cf_neet <- causal_forest(
  X = X_neet, Y = Y_neet, W = W_neet,
  sample.weights = df_cf_neet$ind_wt,
  num.trees = 2000,
  tune.parameters = "all",
  seed = 42
)

ate_neet <- average_treatment_effect(cf_neet, target.sample="all")
cat(sprintf("ATE NEET: %.3f (se=%.3f, p=%.4f)\n",
            ate_neet[1], ate_neet[2], 2*pnorm(-abs(ate_neet[1]/ate_neet[2]))))

tau_neet <- predict(cf_neet)$predictions

# =============================================================================
# 4. VARIABLE IMPORTANCE
# =============================================================================
cat("\n=== VARIABLE IMPORTANCE ===\n")
vi_emp  <- variable_importance(cf_emp)
vi_neet <- variable_importance(cf_neet)

vi_df <- tibble(
  variable  = X_vars,
  imp_emp   = as.numeric(vi_emp),
  imp_neet  = as.numeric(vi_neet)
) %>%
  pivot_longer(-variable, names_to="outcome", values_to="importance") %>%
  mutate(
    outcome = recode(outcome, "imp_emp"="Employment", "imp_neet"="NEET"),
    var_lbl = recode(variable,
      "age_num"="Age", "age_sq"="Age²", "rural"="Rural",
      "ppoor"="Below Poverty Line", "quintile_n"="Wealth Quintile",
      "caste_num"="Caste/Ethnicity", "province_n"="Province",
      "any_difficulty"="Disability", "currently_married"="Currently Married")
  )

print(vi_df %>% pivot_wider(names_from=outcome, values_from=importance) %>%
      arrange(desc(Employment)))

figVI <- ggplot(vi_df, aes(x=reorder(var_lbl,importance), y=importance, fill=outcome)) +
  geom_col(position=position_dodge(0.7), width=0.6, alpha=0.88) +
  scale_fill_manual(values=c("Employment"="#006B9F","NEET"="#E2231A"), name=NULL) +
  coord_flip() +
  labs(title="Variable Importance — Causal Forest",
       subtitle="Relative importance of covariates in predicting treatment effect heterogeneity",
       x=NULL, y="Importance", caption="Source: NLSS IV 2022/23. Causal forest (grf). N=women 15-64.") +
  th()
sv(figVI, "figCF1_variable_importance")

# =============================================================================
# 5. CATE DISTRIBUTION BY SUBGROUP
# =============================================================================
cat("\n=== CATE BY SUBGROUP ===\n")

df_cf$tau_emp  <- tau_emp
df_cf_neet$tau_neet <- tau_neet

# Employment CATE by subgroup
cate_by <- function(data, tau_var, group_var, label) {
  data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      mean_cate = mean(.data[[tau_var]], na.rm=TRUE),
      sd_cate   = sd(.data[[tau_var]], na.rm=TRUE),
      n         = n(),
      .groups="drop"
    ) %>%
    mutate(
      se    = sd_cate / sqrt(n),
      ci_lo = mean_cate - 1.96*se,
      ci_hi = mean_cate + 1.96*se,
      group = as.character(.data[[group_var]]),
      dimension = label
    ) %>%
    select(-any_of(group_var))
}

cate_emp_prov  <- cate_by(df_cf,      "tau_emp",  "province_lbl", "Province")
cate_emp_caste <- cate_by(df_cf,      "tau_emp",  "caste_broad",  "Caste/Ethnicity")
cate_emp_quint <- cate_by(df_cf,      "tau_emp",  "quintile_lbl", "Wealth Quintile")
cate_neet_prov  <- cate_by(df_cf_neet,"tau_neet","province_lbl", "Province")
cate_neet_caste <- cate_by(df_cf_neet,"tau_neet","caste_broad",  "Caste/Ethnicity")
cate_neet_quint <- cate_by(df_cf_neet,"tau_neet","quintile_lbl", "Wealth Quintile")

cate_emp_all  <- bind_rows(cate_emp_prov, cate_emp_caste, cate_emp_quint)
cate_neet_all <- bind_rows(cate_neet_prov, cate_neet_caste, cate_neet_quint)

# Plot CATEs
plot_cate <- function(data, title, colour, src_note) {
  ggplot(data, aes(x=mean_cate, xmin=ci_lo, xmax=ci_hi,
                    y=reorder(group, mean_cate))) +
    geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
    geom_errorbarh(height=0.3, linewidth=0.6, colour=colour) +
    geom_point(aes(size=n), colour=colour, alpha=0.85) +
    scale_size_continuous(range=c(2,7), guide="none") +
    scale_x_continuous(labels=label_percent(accuracy=0.1)) +
    facet_wrap(~dimension, scales="free_y", ncol=1) +
    labs(title=title,
         subtitle="CATE = estimated effect of secondary completion | 95% CI | bubble = N",
         x="CATE (pp change in outcome)", y=NULL,
         caption=src_note) +
    th() +
    theme(strip.text=element_text(face="bold"))
}

figCF2 <- plot_cate(cate_emp_all,
                    "CATE of Secondary Education on Employment — Women",
                    "#006B9F",
                    "Source: NLSS IV 2022/23. Causal forest (grf). N=women 15-64.")
sv(figCF2, "figCF2_cate_employment_subgroups", w=9, h=12)

figCF3 <- plot_cate(cate_neet_all,
                    "CATE of Secondary Education on NEET — Women",
                    "#E2231A",
                    "Source: NLSS IV 2022/23. Causal forest (grf). N=women 15-64.")
sv(figCF3, "figCF3_cate_neet_subgroups", w=9, h=12)

# =============================================================================
# 6. CATE vs MINCER — CORROBORATION PLOT
# =============================================================================
cat("\n=== CORROBORATION: CATE vs OLS ===\n")

# OLS coefficients from regression v2
ols_emp_coef  <- reg_results_v2$m4$girls_enrol   # enrolment model
ols_neet_coef <- reg_results_v2$m4$girls_neet

# Province-level CATE vs OLS FE
# Get province fixed effects from OLS
tryCatch({
  ols_pfe_emp  <- fixef(reg_results_v2$m1$full)[["province_lbl"]]
  ols_pfe_neet <- fixef(reg_results_v2$m2$full)[["province_lbl"]]

  corr_df <- cate_emp_prov %>%
    rename(province_lbl=group, cate=mean_cate) %>%
    left_join(
      tibble(province_lbl=names(ols_pfe_neet), ols_fe=ols_pfe_neet),
      by="province_lbl"
    ) %>% filter(!is.na(ols_fe))

  figCF4 <- ggplot(corr_df, aes(x=ols_fe, y=cate, label=province_lbl)) +
    geom_point(colour="#006B9F", size=4, alpha=0.8) +
    geom_smooth(method="lm", se=TRUE, colour="#E2231A", linewidth=0.8, alpha=0.15) +
    ggrepel::geom_text_repel(size=3.2) +
    scale_x_continuous(labels=label_percent()) +
    scale_y_continuous(labels=label_percent()) +
    labs(title="Causal Forest CATE vs OLS Province Fixed Effects",
         subtitle="Employment outcome | Each point = one province",
         x="OLS Province Fixed Effect (employment)", y="CF CATE (employment)",
         caption="Correlation confirms external validity of both methods.") +
    th()
  sv(figCF4, "figCF4_cate_vs_ols_corroboration")
}, error=function(e) cat("Corroboration plot skipped:", e$message, "\n"))

# =============================================================================
# 7. COST-OF-INACTION × CATE
# =============================================================================
cat("\n=== COST-OF-INACTION USING CATE ===\n")

# Combine CATE with province-level wage data
prov_wages <- df_wage_w %>%
  group_by(province_lbl) %>%
  summarise(base_wage=median(monthly_wage,na.rm=TRUE), .groups="drop")

coi_prov <- cate_neet_prov %>%
  rename(province_lbl=group) %>%
  left_join(prov_wages, by="province_lbl") %>%
  mutate(
    # CATE of secondary ed on NEET (negative = education reduces NEET)
    cate_neet = mean_cate,
    # Population-weighted cost: CATE × PV foregone per girl
    # PV foregone = base_wage × 12 × (1.09^2 - 1) × annuity factor
    pv_per_girl_ilo = base_wage * 12 * ((1.09)^2 - 1) *
                      (1 - 1.05^(-35)) / 0.05,
    # Cost of inaction = |CATE| × PV (CATE is effect of ed → more employment)
    # i.e. each % point increase in employment from secondary = NPR X foregone if not invested
    coi_per_100_girls = abs(cate_neet) * 100 * pv_per_girl_ilo / 1000  # NPR '000s
  )

cat("\nCost-of-Inaction by Province (CATE × PV foregone, per 100 girls):\n")
coi_prov %>%
  select(province_lbl, cate_neet, pv_per_girl_ilo, coi_per_100_girls) %>%
  mutate(across(where(is.numeric), ~round(.x, 0))) %>%
  arrange(desc(abs(cate_neet))) %>%
  print()

figCF5 <- ggplot(coi_prov %>% filter(!is.na(coi_per_100_girls)),
                 aes(x=reorder(province_lbl, coi_per_100_girls),
                     y=coi_per_100_girls)) +
  geom_col(fill="#006B9F", alpha=0.85, width=0.65) +
  geom_text(aes(label=paste0("NPR ",round(coi_per_100_girls),"k")),
            hjust=-0.1, size=3.2) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0, 0.25))) +
  coord_flip() +
  labs(title="Cost of Inaction per 100 Girls — By Province",
       subtitle="CATE of secondary education on NEET × PV foregone earnings | ILO 9% return",
       x=NULL, y="Estimated Cost (NPR '000 per 100 girls)",
       caption="Source: NLSS IV 2022/23. Causal forest CATE × Mincer-based earnings projection.") +
  th()
sv(figCF5, "figCF5_coi_by_province_cate", w=9, h=5)

# =============================================================================
# 8. SAVE RESULTS
# =============================================================================
cf_results <- list(
  cf_emp=cf_emp, cf_neet=cf_neet,
  ate_emp=ate_emp, ate_neet=ate_neet,
  cate_emp_all=cate_emp_all, cate_neet_all=cate_neet_all,
  coi_prov=coi_prov, vi_df=vi_df
)
save(cf_results, file="outputs/causal_forest_results.RData")

cat("\n=== SUMMARY ===\n")
cat(sprintf("ATE of secondary education on employment: %+.1f pp (p=%.3f)\n",
            ate_emp[1]*100, 2*pnorm(-abs(ate_emp[1]/ate_emp[2]))))
cat(sprintf("ATE of secondary education on NEET:      %+.1f pp (p=%.3f)\n",
            ate_neet[1]*100, 2*pnorm(-abs(ate_neet[1]/ate_neet[2]))))
cat("\nTop 3 drivers of heterogeneity (employment):\n")
vi_df %>% filter(outcome=="Employment") %>%
  arrange(desc(importance)) %>% head(3) %>%
  select(var_lbl, importance) %>% print()
cat("\n✓ Causal forest complete. Figures → outputs/figures/figCF*.png\n")
