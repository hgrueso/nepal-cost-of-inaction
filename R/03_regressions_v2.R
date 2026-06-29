# =============================================================================
# 03_regressions_v2.R — Regression models 
# Key additions vs v1:
#   - Models run separately for 15-18 and 19-24 age groups
#   - Wealth excluded in sensitivity models (reviewer: "wealth overshadows")
#   - Intersectionality: province x caste interaction
#   - Disability included
#   - Mincer wage equation (now possible with actual salaries)
# =============================================================================

needed <- c("tidyverse","fixest","modelsummary","broom","marginaleffects")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({library(tidyverse); library(fixest); library(modelsummary); library(broom)})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")

load(file.path(OUTPUT_PATH, "cleaned_data_v2.RData"))
cat("Data loaded.\n\n")

stars   <- c("*"=0.10, "**"=0.05, "***"=0.01)
gof_map <- tribble(~raw,~clean,~fmt, "nobs","N",0, "r.squared","R2",3, "adj.r.squared","Adj. R2",3)

# Coefficient labels
cm <- c(
  "female"                          = "Female",
  "rural"                           = "Rural",
  "ppoor"                           = "Below Poverty Line",
  "quintile_n"                      = "Expenditure Quintile (1-5)",
  "any_difficulty"                  = "Any Functional Difficulty (Disability)",
  "child_marriage"                  = "Child Marriage (proxy)",
  "currently_married"               = "Currently Married",
  "age_num"                         = "Age",
  "I(age_num^2)"                    = "Age Squared",
  "caste_broadJanajati"             = "Janajati (ref: Brahmin/Chhetri)",
  "caste_broadDalit"                = "Dalit",
  "caste_broadReligious/Linguistic" = "Religious/Linguistic",
  "caste_broadOther"                = "Other Caste"
)

# =============================================================================
# MODEL SET 1 — School enrolment: 15-24, with/without wealth, by age group
# =============================================================================
cat("=== M1: Enrolment ===\n")

# Full controls WITH wealth
m1_full <- feols(attend_any ~ rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_girls, weights=~ind_wt, se="hc1")

# Sensitivity WITHOUT wealth (reviewer comment)
m1_nowt <- feols(attend_any ~ rural + ppoor + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_girls, weights=~ind_wt, se="hc1")

# Secondary school age only (15-18)
m1_1518 <- feols(attend_any ~ rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_girls1, weights=~ind_wt, se="hc1")

# Older youth (19-24)
m1_1924 <- feols(attend_any ~ rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_girls2, weights=~ind_wt, se="hc1")

# =============================================================================
# MODEL SET 2 — NEET: with/without wealth, by sex and age group
# =============================================================================
cat("=== M2: NEET ===\n")

m2_full <- feols(neet ~ female + rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_b, weights=~ind_wt, se="hc1")

m2_nowt <- feols(neet ~ female + rural + ppoor + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_b, weights=~ind_wt, se="hc1")

# Girls only — intersectionality
m2_girls_full <- feols(neet ~ rural + ppoor + quintile_n + any_difficulty +
                         caste_broad + currently_married | province_lbl,
                       data=df_girls, weights=~ind_wt, se="hc1")

# By age group
m2_1518 <- feols(neet ~ female + rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_b1, weights=~ind_wt, se="hc1")

m2_1924 <- feols(neet ~ female + rural + ppoor + quintile_n + any_difficulty +
                   caste_broad + currently_married | province_lbl,
                 data=df_b2, weights=~ind_wt, se="hc1")

# =============================================================================
# MODEL SET 3 — Province x Caste interaction (intersectionality)
# =============================================================================
cat("=== M3: Intersectionality ===\n")

# Are caste penalties larger in specific provinces?
m3_interact <- feols(neet ~ female + rural + ppoor + quintile_n +
                       caste_broad * province_lbl,
                     data=df_b, weights=~ind_wt, se="hc1")

# Compute predicted NEET for each province-caste combination
# (for heatmap in descriptives)
cat("Province x Caste NEET predictions computed in descriptives script\n")

# =============================================================================
# MODEL SET 4 — Enrolment vs NEET story (reviewer: boys lag enrolment but not NEET)
# =============================================================================
cat("=== M4: Gender gap decomposition ===\n")

# Separate models for boys and girls to examine asymmetry
m4_girls <- feols(neet ~ rural + ppoor + quintile_n + caste_broad | province_lbl,
                  data=filter(df_b, female==1), weights=~ind_wt, se="hc1")

m4_boys  <- feols(neet ~ rural + ppoor + quintile_n + caste_broad | province_lbl,
                  data=filter(df_b, female==0), weights=~ind_wt, se="hc1")

m4_girls_enrol <- feols(attend_any ~ rural + ppoor + quintile_n + caste_broad | province_lbl,
                         data=filter(df_b, female==1, !is.na(rural), !is.na(ppoor)),
                         weights=~ind_wt, se="hc1")

m4_boys_enrol  <- feols(attend_any ~ rural + ppoor + quintile_n + caste_broad | province_lbl,
                         data=filter(df_b, female==0, !is.na(rural), !is.na(ppoor)),
                         weights=~ind_wt, se="hc1")

# =============================================================================
# MODEL SET 5 — Mincer wage equation (now possible with actual salaries)
# =============================================================================
cat("=== M5: Mincer returns to education ===\n")

if (nrow(df_wage_w) > 100) {
  df_wage_w <- df_wage_w %>%
    mutate(
      edu_yrs = case_when(
        as.character(edu_cat) == "No education"     ~  0,
        as.character(edu_cat) == "Primary"          ~  5,
        as.character(edu_cat) == "Lower secondary"  ~  8,
        as.character(edu_cat) == "Secondary"        ~ 10,
        as.character(edu_cat) == "Higher secondary" ~ 12,
        as.character(edu_cat) == "Tertiary"         ~ 16,
        TRUE ~ NA_real_
      ),
      experience   = pmax(age_num - edu_yrs - 6, 0, na.rm = TRUE),
      experience2  = experience^2,
      edu_yrs_cent = edu_yrs - mean(edu_yrs, na.rm = TRUE)
    )
  cat("edu_yrs distribution:\n"); print(table(df_wage_w$edu_yrs, useNA="always"))
  cat("N non-NA edu_yrs_cent:", sum(!is.na(df_wage_w$edu_yrs_cent)), "\n")
  cat("N non-NA log_wage:    ", sum(!is.na(df_wage_w$log_wage)), "\n")

  m5a <- tryCatch(feols(log_wage ~ edu_yrs_cent + I(edu_yrs_cent^2) + experience + experience2 + rural,
               data=df_wage_w, weights=~ind_wt, se="hc1"), error=function(e){cat("M5a error:",conditionMessage(e),"\n"); NULL})

  m5b <- feols(log_wage ~ edu_yrs_cent + I(edu_yrs_cent^2) + experience + experience2 + rural +
                 caste_broad | province_lbl,
               data=df_wage_w, weights=~ind_wt, se="hc1")

  m5c <- feols(log_wage ~ edu_cat + experience + experience2 + rural +
                 caste_broad | province_lbl,
               data=df_wage_w, weights=~ind_wt, se="hc1")

  cat("Mincer models run. N wage earners:", nrow(df_wage_w), "\n")
  cat("N with valid wages + edu_yrs:", sum(!is.na(df_wage_w$log_wage) & !is.na(df_wage_w$edu_yrs)), "\n")
  if (!is.null(m5b) && "edu_yrs_cent" %in% names(coef(m5b)))
    cat("Returns to schooling (M5b, per year):",
        round(coef(m5b)["edu_yrs_cent"]*100, 2), "%\n")
} else {
  cat("Too few wage earners for Mincer (n=", nrow(df_wage_w), ") — check Q10_09_A\n")
  m5a <- m5b <- m5c <- NULL
}

# =============================================================================
# MODEL SET 6 — Disability: enrolment and NEET
# =============================================================================
cat("=== M6: Disability ===\n")

# Check disability sample size
cat("N with any_difficulty=1:", sum(df$any_difficulty==1, na.rm=TRUE), "\n")
cat("N girls 15-24 with disability:", sum(df_girls$any_difficulty==1, na.rm=TRUE), "\n")

m6_enrol <- feols(attend_any ~ any_difficulty + rural + ppoor + quintile_n +
                    caste_broad | province_lbl,
                  data=df_girls, weights=~ind_wt, se="hc1")

m6_neet  <- feols(neet ~ any_difficulty + rural + ppoor + quintile_n +
                    caste_broad | province_lbl,
                  data=df_girls, weights=~ind_wt, se="hc1")

# =============================================================================
# PRINT SENSITIVITY: WITH vs WITHOUT WEALTH
# =============================================================================
cat("\n=== SENSITIVITY: Wealth in/out of enrolment model ===\n")
modelsummary(
  list("With wealth quintile"=m1_full, "Without wealth quintile"=m1_nowt),
  stars=stars, coef_map=cm, gof_map=gof_map,
  title="Enrolment — Wealth Sensitivity (Girls 15-24, Province FE)"
)

cat("\n=== SENSITIVITY: Wealth in/out of NEET model ===\n")
modelsummary(
  list("With wealth quintile"=m2_full, "Without wealth quintile"=m2_nowt),
  stars=stars, coef_map=cm, gof_map=gof_map,
  title="NEET — Wealth Sensitivity (Youth 15-24, Province FE)"
)

cat("\n=== AGE GROUP COMPARISON ===\n")
modelsummary(
  list("NEET 15-18"=m2_1518, "NEET 19-24"=m2_1924,
       "Enrolment 15-18"=m1_1518, "Enrolment 19-24"=m1_1924),
  stars=stars, coef_map=cm, gof_map=gof_map,
  title="Outcomes by Age Group (Province FE)"
)

cat("\n=== GENDER ASYMMETRY: Girls vs Boys ===\n")
cat("NEET models:\n")
modelsummary(list("Girls"=m4_girls, "Boys"=m4_boys),
             stars=stars, coef_map=cm, gof_map=gof_map,
             title="NEET Predictors by Sex (Youth 15-24)")
cat("Enrolment models:\n")
modelsummary(list("Girls"=m4_girls_enrol, "Boys"=m4_boys_enrol),
             stars=stars, coef_map=cm, gof_map=gof_map,
             title="Enrolment Predictors by Sex (Youth 15-24)")

cat("\n=== DISABILITY ===\n")
modelsummary(list("Enrolment"=m6_enrol, "NEET"=m6_neet),
             stars=stars, coef_map=cm, gof_map=gof_map,
             title="Disability Effects (Girls 15-24, Province FE)")

# =============================================================================
# KEY FINDINGS NARRATIVE
# =============================================================================
cat("\n=== KEY FINDINGS FOR NARRATIVE ===\n")
e_dalit   <- coef(m1_full)["caste_broadDalit"]
e_disab   <- coef(m1_full)["any_difficulty"]
n_female  <- coef(m2_full)["female"]
n_dalit   <- coef(m2_full)["caste_broadDalit"]
n_poverty <- coef(m2_full)["ppoor"]
n_disab   <- coef(m2_full)["any_difficulty"]
cat(sprintf("ENROLMENT (girls 15-24, with wealth, province FE):\n"))
cat(sprintf("  Dalit: %+.1f pp | Disability: %+.1f pp\n", e_dalit*100, ifelse(is.na(e_disab), NA, e_disab*100)))
cat(sprintf("\nNEET (youth 15-24, with wealth, province FE):\n"))
cat(sprintf("  Female: %+.1f pp | Dalit: %+.1f pp | Poor: %+.1f pp | Disability: %+.1f pp\n",
            n_female*100, n_dalit*100, n_poverty*100, ifelse(is.na(n_disab), NA, n_disab*100)))
e_dalit_nw  <- coef(m1_nowt)["caste_broadDalit"]
n_dalit_nw  <- coef(m2_nowt)["caste_broadDalit"]
cat(sprintf("\nWithout wealth controls:\n"))
cat(sprintf("  Dalit enrolment penalty: %+.1f pp (was %+.1f pp with wealth)\n", e_dalit_nw*100, e_dalit*100))
cat(sprintf("  Dalit NEET premium: %+.1f pp (was %+.1f pp with wealth)\n", n_dalit_nw*100, n_dalit*100))
cat("  => If Dalit penalty INCREASES without wealth, wealth was masking/absorbing caste effect\n")
cat("  => If unchanged, caste penalty is truly independent of wealth\n")

# =============================================================================
# SAVE
# =============================================================================
reg_results_v2 <- list(
  m1=list(full=m1_full, no_wealth=m1_nowt, age1518=m1_1518, age1924=m1_1924),
  m2=list(full=m2_full, no_wealth=m2_nowt, girls=m2_girls_full,
          age1518=m2_1518, age1924=m2_1924),
  m3=list(interact=m3_interact),
  m4=list(girls_neet=m4_girls, boys_neet=m4_boys,
          girls_enrol=m4_girls_enrol, boys_enrol=m4_boys_enrol),
  m5=list(base=m5a, province_fe=m5b, edu_levels=m5c),
  m6=list(enrol=m6_enrol, neet=m6_neet),
  stars=stars, gof_map=gof_map, cm=cm
)

save(reg_results_v2, file=file.path(OUTPUT_PATH, "regression_results_v2.RData"))
cat("\n✓ Saved → outputs/regression_results_v2.RData\n\n")
