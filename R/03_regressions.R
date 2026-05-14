# =============================================================================
# 03_regressions.R — Regression models | Nepal NLSS IV 2022/23
# Outcomes: school enrolment, NEET, wage employment (binary)
# No individual wages available in this file → employment used as proxy
# =============================================================================

needed <- c("tidyverse", "fixest", "modelsummary", "broom", "marginaleffects")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(tidyverse); library(fixest)
  library(modelsummary); library(broom)
})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
if (!exists("OUTPUT_PATH"))  OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")

load(file.path(OUTPUT_PATH, "cleaned_data.RData"))
if (!exists("desc_results"))
  load(file.path(OUTPUT_PATH, "descriptive_results.RData"))
cat("Data loaded.\n\n")

stars  <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)
gof_map <- tribble(~raw, ~clean, ~fmt,
                   "nobs",          "N",       0,
                   "r.squared",     "R2",      3,
                   "adj.r.squared", "Adj. R2", 3)

src_note <- "Source: NLSS IV 2022/23. HC1 standard errors. *p<0.1, **p<0.05, ***p<0.01."

# =============================================================================
# ANALYTICAL DATASETS
# Girls 15-24 → enrolment + NEET
# Youth 15-24 → gender comparison
# Women 15-64 → employment + wage employment
# =============================================================================

girls  <- df_girls   # female, 15-24
youth  <- df_b       # both sexes, 15-24
women  <- df_c %>% filter(female == 1)

# =============================================================================
# MODEL SET 1 — School enrolment (girls 15-24)
# LPM with increasing controls
# =============================================================================
cat("=== MODEL SET 1: School Enrolment (girls 15-24) ===\n")

m1a <- feols(attend_school ~ rural + ppoor + quintile_n,
             data = girls, weights = ~ind_wt, se = "hc1")

m1b <- feols(attend_school ~ rural + ppoor + quintile_n +
               caste_broad + currently_married,
             data = girls, weights = ~ind_wt, se = "hc1")

m1c <- feols(attend_school ~ rural + ppoor + quintile_n +
               caste_broad + currently_married | province_lbl,
             data = girls, weights = ~ind_wt, se = "hc1")

cat("M1a R2:", round(r2(m1a)["r2"], 3),
    "| M1b R2:", round(r2(m1b)["r2"], 3),
    "| M1c R2:", round(r2(m1c)["r2"], 3), "\n\n")

# =============================================================================
# MODEL SET 2 — NEET (youth 15-24)
# =============================================================================
cat("=== MODEL SET 2: NEET (youth 15-24) ===\n")

m2a <- feols(neet ~ female + rural + ppoor + quintile_n,
             data = youth, weights = ~ind_wt, se = "hc1")

m2b <- feols(neet ~ female + rural + ppoor + quintile_n +
               caste_broad + currently_married,
             data = youth, weights = ~ind_wt, se = "hc1")

m2c <- feols(neet ~ female + rural + ppoor + quintile_n +
               caste_broad + currently_married | province_lbl,
             data = youth, weights = ~ind_wt, se = "hc1")

cat("M2a R2:", round(r2(m2a)["r2"], 3),
    "| M2b R2:", round(r2(m2b)["r2"], 3),
    "| M2c R2:", round(r2(m2c)["r2"], 3), "\n\n")

# =============================================================================
# MODEL SET 3 — Wage employment (women 15-64)
# =============================================================================
cat("=== MODEL SET 3: Wage Employment (women 15-64) ===\n")

# Restrict to non-missing wage_employed
women_w <- women %>% filter(!is.na(wage_employed))

m3a <- feols(wage_employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2),
             data = women_w, weights = ~ind_wt, se = "hc1")

m3b <- feols(wage_employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2) +
               caste_broad + currently_married,
             data = women_w, weights = ~ind_wt, se = "hc1")

m3c <- feols(wage_employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2) +
               caste_broad + currently_married | province_lbl,
             data = women_w, weights = ~ind_wt, se = "hc1")

cat("M3a R2:", round(r2(m3a)["r2"], 3),
    "| M3b R2:", round(r2(m3b)["r2"], 3),
    "| M3c R2:", round(r2(m3c)["r2"], 3), "\n\n")

# =============================================================================
# MODEL SET 4 — Employment (all women 15-64, broader outcome)
# =============================================================================
cat("=== MODEL SET 4: Employment (women 15-64) ===\n")

m4a <- feols(employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2),
             data = women, weights = ~ind_wt, se = "hc1")

m4b <- feols(employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2) +
               caste_broad + currently_married,
             data = women, weights = ~ind_wt, se = "hc1")

m4c <- feols(employed ~ rural + ppoor + quintile_n + age_num + I(age_num^2) +
               caste_broad + currently_married | province_lbl,
             data = women, weights = ~ind_wt, se = "hc1")

cat("M4a R2:", round(r2(m4a)["r2"], 3),
    "| M4b R2:", round(r2(m4b)["r2"], 3),
    "| M4c R2:", round(r2(m4c)["r2"], 3), "\n\n")

# =============================================================================
# COEFFICIENT MAPS
# =============================================================================
cm_main <- c(
  "female"                      = "Female",
  "rural"                       = "Rural",
  "ppoor"                       = "Below Poverty Line",
  "quintile_n"                  = "Expenditure Quintile (1→5)",
  "age_num"                     = "Age",
  "I(age_num^2)"                = "Age Squared",
  "currently_married"           = "Currently Married",
  "caste_broadJanajati"         = "Janajati (ref: Brahmin/Chhetri)",
  "caste_broadDalit"            = "Dalit",
  "caste_broadReligious/Linguistic" = "Religious/Linguistic",
  "caste_broadOther"            = "Other Caste"
)

# =============================================================================
# PRINT SUMMARY TABLES
# =============================================================================
cat("=== TABLE: Enrolment Models ===\n")
modelsummary(
  list("(1) Base" = m1a, "(2) + Controls" = m1b, "(3) + Province FE" = m1c),
  stars = stars, coef_map = cm_main, gof_map = gof_map,
  title = "LPM: School Enrolment (Girls 15-24)"
)

cat("\n=== TABLE: NEET Models ===\n")
modelsummary(
  list("(1) Base" = m2a, "(2) + Controls" = m2b, "(3) + Province FE" = m2c),
  stars = stars, coef_map = cm_main, gof_map = gof_map,
  title = "LPM: NEET (Youth 15-24)"
)

cat("\n=== TABLE: Wage Employment Models ===\n")
modelsummary(
  list("(1) Base" = m3a, "(2) + Controls" = m3b, "(3) + Province FE" = m3c),
  stars = stars, coef_map = cm_main, gof_map = gof_map,
  title = "LPM: Wage Employment (Women 15-64)"
)

# =============================================================================
# COEFFICIENT PLOT
# =============================================================================
cat("\nBuilding coefficient plot...\n")

coef_data <- bind_rows(
  tidy(m1c, conf.int = TRUE) %>%
    filter(!grepl("province|Intercept", term)) %>%
    mutate(model = "School Enrolment\n(Girls 15-24)"),
  tidy(m2c, conf.int = TRUE) %>%
    filter(!grepl("province|Intercept", term)) %>%
    mutate(model = "NEET\n(Youth 15-24)"),
  tidy(m4c, conf.int = TRUE) %>%
    filter(!grepl("province|Intercept|age", term)) %>%
    mutate(model = "Employment\n(Women 15-64)")
) %>%
  mutate(term_lbl = recode(term,
    "female"                          = "Female",
    "rural"                           = "Rural",
    "ppoor"                           = "Below Poverty Line",
    "quintile_n"                      = "Expenditure Quintile",
    "currently_married"               = "Currently Married",
    "caste_broadJanajati"             = "Janajati",
    "caste_broadDalit"                = "Dalit",
    "caste_broadReligious/Linguistic" = "Religious/Ling.",
    "caste_broadOther"                = "Other Caste"
  ))

fig_coef <- ggplot(coef_data,
                   aes(x = estimate, xmin = conf.low, xmax = conf.high,
                       y = reorder(term_lbl, estimate),
                       colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60",
             linewidth = 0.5) +
  geom_errorbarh(height = 0.3, linewidth = 0.7,
                 position = position_dodge(0.55)) +
  geom_point(size = 2.5, position = position_dodge(0.55)) +
  facet_wrap(~model, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = c("#1CABE2","#E2231A","#00833D"), guide = "none") +
  labs(title    = "Inequality Predictors — Province Fixed-Effects Models",
       subtitle = "LPM coefficients | 95% CI | ref: Brahmin/Chhetri, non-poor, urban",
       x = "Coefficient (percentage point change in outcome)",
       y = NULL,
       caption = src_note) +
  theme_minimal(base_size = 10) +
  theme(strip.text       = element_text(face = "bold", size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold"))

ggsave(file.path(FIGURES_PATH, "fig_coef_plot.pdf"), fig_coef,
       width = 11, height = 5.5)
ggsave(file.path(FIGURES_PATH, "fig_coef_plot.png"), fig_coef,
       width = 11, height = 5.5, dpi = 300)
cat("  ✓ fig_coef_plot\n")

# =============================================================================
# KEY MARGINAL EFFECTS — print for report narrative
# =============================================================================
cat("\n=== KEY FINDINGS FOR NARRATIVE ===\n")

# Enrolment
e_dalit    <- coef(m1c)["caste_broadDalit"]
e_married  <- coef(m1c)["currently_married"]
e_quintile <- coef(m1c)["quintile_n"]
e_poverty  <- coef(m1c)["ppoor"]
e_rural    <- coef(m1c)["rural"]

cat(sprintf("ENROLMENT (girls 15-24, province FE):\n"))
cat(sprintf("  Dalit vs Brahmin/Chhetri: %+.1f pp\n", e_dalit * 100))
cat(sprintf("  Currently married:        %+.1f pp\n", e_married * 100))
cat(sprintf("  Per quintile step up:     %+.1f pp\n", e_quintile * 100))
cat(sprintf("  Below poverty line:       %+.1f pp\n", e_poverty * 100))
cat(sprintf("  Rural (vs urban):         %+.1f pp\n", e_rural * 100))

# NEET
n_female   <- coef(m2c)["female"]
n_dalit    <- coef(m2c)["caste_broadDalit"]
n_married  <- coef(m2c)["currently_married"]
n_poverty  <- coef(m2c)["ppoor"]

cat(sprintf("\nNEET (youth 15-24, province FE):\n"))
cat(sprintf("  Female vs male:            %+.1f pp\n", n_female  * 100))
cat(sprintf("  Dalit vs Brahmin/Chhetri:  %+.1f pp\n", n_dalit   * 100))
cat(sprintf("  Currently married:         %+.1f pp\n", n_married * 100))
cat(sprintf("  Below poverty line:        %+.1f pp\n", n_poverty * 100))

# =============================================================================
# COLLECT AND SAVE
# =============================================================================
reg_results <- list(
  m1 = list(base = m1a, controls = m1b, province_fe = m1c),
  m2 = list(base = m2a, controls = m2b, province_fe = m2c),
  m3 = list(base = m3a, controls = m3b, province_fe = m3c),
  m4 = list(base = m4a, controls = m4b, province_fe = m4c),
  coef_data = coef_data,
  cm_main   = cm_main,
  gof_map   = gof_map,
  stars     = stars
)

results <- list(desc = desc_results, reg = reg_results,
                samples = list(n_total  = nrow(df),
                               n_girls  = nrow(df_girls),
                               n_youth  = nrow(df_b),
                               n_women  = nrow(women),
                               n_wage_w = nrow(df_wage_w)))

save(results, file = file.path(OUTPUT_PATH, "analysis_results.RData"))
cat("\n✓ analysis_results.RData saved.\n")
cat("  Next: quarto render reports/nepal_nlss_analysis.qmd\n\n")
