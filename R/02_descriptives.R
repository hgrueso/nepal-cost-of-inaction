# =============================================================================
# 02_descriptives.R — Descriptive tables and figures | Nepal NLSS IV 2022/23
# All variable codings verified. Key limitations:
#   - edu_category/yearschooling = CURRENT STUDENTS ONLY (21k obs)
#   - No literacy variable available in this merged file
#   - Individual wages not available; wage_employed (YES/NO) used instead
#   - NEET interpreted for youth 15-24 only (all-age rate is meaningless)
# =============================================================================

needed <- c("tidyverse", "srvyr", "patchwork", "scales", "viridis", "kableExtra")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(tidyverse); library(srvyr)
  library(patchwork); library(scales); library(viridis)
})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
if (!exists("OUTPUT_PATH"))  OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")

load(file.path(OUTPUT_PATH, "cleaned_data.RData"))
cat("Data loaded. N =", nrow(df), "\n\n")

# =============================================================================
# THEME
# =============================================================================
ub   <- "#1CABE2"   # UNICEF blue
ud   <- "#006B9F"   # UNICEF dark blue
ug   <- "#00833D"   # UNICEF green
ur   <- "#E2231A"   # UNICEF red
ugr  <- "#6C7279"   # UNICEF grey

theme_aimpact <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1, colour = "#222222"),
      plot.subtitle = element_text(size = base_size - 1, colour = ugr),
      plot.caption  = element_text(size = 7, colour = ugr, hjust = 0),
      axis.text     = element_text(colour = "#444444"),
      axis.title    = element_text(colour = "#444444", size = base_size - 1),
      panel.grid.major = element_line(colour = "#EBEBEB", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold"),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(FIGURES_PATH, paste0(name, ".pdf")), p,
         width = w, height = h, device = cairo_pdf)
  ggsave(file.path(FIGURES_PATH, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300)
  cat("  ✓", name, "\n")
}

src <- "Source: NLSS IV 2022/23. Survey weights applied."

# Helper: weighted mean + CI for a binary outcome
svy_pct <- function(svy_obj, var, grp = NULL) {
  if (is.null(grp)) {
    svy_obj %>%
      filter(!is.na(.data[[var]])) %>%
      summarise(pct = survey_mean(.data[[var]], vartype = "ci") * 100)
  } else {
    svy_obj %>%
      filter(!is.na(.data[[var]]), !is.na(.data[[grp]])) %>%
      group_by(.data[[grp]]) %>%
      summarise(pct = survey_mean(.data[[var]], vartype = "ci") * 100)
  }
}

# =============================================================================
# SECTION 1 — SAMPLE PROFILE
# =============================================================================
cat("Building sample profile tables...\n")

# Province distribution
t_province <- svy_girls %>%
  group_by(province_lbl) %>%
  summarise(n = n(), pct = survey_mean(rep(1, n()), vartype = NULL) * 100) %>%
  mutate(pct = n / sum(n) * 100)

# Caste distribution
t_caste <- svy_girls %>%
  group_by(caste_broad) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)

# Poverty
t_poverty <- svy_girls %>%
  summarise(
    pct_poor  = survey_mean(ppoor == 1, na.rm = TRUE) * 100,
    pct_rural = survey_mean(rural == 1, na.rm = TRUE) * 100,
    pct_wage  = survey_mean(wage_employed == 1, na.rm = TRUE) * 100
  )

readr::write_csv(t_province, file.path(TABLES_PATH, "t1_province_dist.csv"))
readr::write_csv(t_caste,    file.path(TABLES_PATH, "t1_caste_dist.csv"))
cat("  ✓ Sample profile tables\n")

# =============================================================================
# SECTION 2 — EDUCATION OUTCOMES (current students only — flag in all titles)
# =============================================================================
cat("Building education figures...\n")

# ── Fig 1: School attendance (= currently enrolled) by province, girls vs boys
fig1_data <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct = survey_mean(attend_school == 1, vartype = "ci") * 100) %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

fig1 <- ggplot(fig1_data,
               aes(x = reorder(province_lbl, pct), y = pct,
                   fill = sex_lbl, colour = sex_lbl)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.88) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.4) +
  scale_fill_manual(values   = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_colour_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(title    = "Current School Enrolment by Province",
       subtitle = "Youth 15–24 | Proxy: non-missing education module",
       x = NULL, y = "% Currently Enrolled",
       caption = src) +
  theme_aimpact()
save_fig(fig1, "fig1_enrolment_by_province")

# ── Fig 2: Enrolment by wealth quintile, girls vs boys
fig2_data <- svy_b %>%
  group_by(quintile_lbl, sex_lbl) %>%
  summarise(pct = survey_mean(attend_school == 1, vartype = "ci") * 100) %>%
  filter(!is.na(quintile_lbl), !is.na(sex_lbl))

fig2 <- ggplot(fig2_data,
               aes(x = quintile_lbl, y = pct,
                   group = sex_lbl, colour = sex_lbl)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp, fill = sex_lbl),
              alpha = 0.1, colour = NA) +
  scale_colour_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_fill_manual(values   = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "School Enrolment by Expenditure Quintile",
       subtitle = "Youth 15–24",
       x = "Expenditure Quintile", y = "% Currently Enrolled",
       caption = src) +
  theme_aimpact()
save_fig(fig2, "fig2_enrolment_by_quintile")

# ── Fig 3: Enrolment by caste, girls
fig3_data <- svy_girls %>%
  group_by(caste_broad) %>%
  summarise(pct = survey_mean(attend_school == 1, vartype = "ci") * 100) %>%
  filter(!is.na(caste_broad))

fig3 <- ggplot(fig3_data,
               aes(x = reorder(caste_broad, pct), y = pct)) +
  geom_col(fill = ub, alpha = 0.88, width = 0.6) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                width = 0.25, linewidth = 0.5, colour = ud) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.2, size = 3.2) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(title    = "School Enrolment by Caste/Ethnicity — Girls",
       subtitle = "Girls 15–24",
       x = NULL, y = "% Currently Enrolled",
       caption = src) +
  theme_aimpact()
save_fig(fig3, "fig3_enrolment_by_caste")

# ── Table 2: Education by subgroup (girls 15-24)
edu_by_subgroup <- function(svy_obj, grp, label) {
  svy_obj %>%
    group_by(.data[[grp]]) %>%
    summarise(
      pct_enrolled  = survey_mean(attend_school == 1, na.rm = TRUE) * 100,
      pct_wage_work = survey_mean(wage_employed == 1, na.rm = TRUE) * 100,
      pct_neet      = survey_mean(neet == 1,          na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(.data[[grp]])) %>%
    rename(group = .data[[grp]]) %>%
    mutate(group = as.character(group), dimension = label)
}

t2 <- bind_rows(
  edu_by_subgroup(svy_girls, "province_lbl", "Province"),
  edu_by_subgroup(svy_girls, "caste_broad",  "Caste/Ethnicity"),
  edu_by_subgroup(svy_girls, "quintile_lbl", "Expenditure Quintile"),
  edu_by_subgroup(svy_girls, "urban_lbl",    "Urban/Rural")
)
readr::write_csv(t2, file.path(TABLES_PATH, "t2_outcomes_by_subgroup.csv"))
cat("  ✓ Table 2 saved\n")

# =============================================================================
# SECTION 3 — NEET AND EMPLOYMENT (youth 15-24 only)
# =============================================================================
cat("Building employment/NEET figures...\n")

# ── Fig 4: NEET by province (youth 15-24 — meaningful age range)
fig4_data <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct = survey_mean(neet == 1, vartype = "ci") * 100) %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

fig4 <- ggplot(fig4_data,
               aes(x = reorder(province_lbl, -pct), y = pct, fill = sex_lbl)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.88) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.4) +
  scale_fill_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "NEET Rate by Province",
       subtitle = "Youth 15–24 | Not in Employment, Education or Training",
       x = NULL, y = "% NEET",
       caption = src) +
  theme_aimpact()
save_fig(fig4, "fig4_neet_by_province")

# ── Fig 5: Employment gradient by enrolment status (proxy for education)
fig5_data <- svy_c %>%
  filter(female == 1) %>%
  group_by(province_lbl) %>%
  summarise(
    pct_emp  = survey_mean(employed == 1,      vartype = "ci") * 100,
    pct_wage = survey_mean(wage_employed == 1, vartype = "ci") * 100,
    pct_neet = survey_mean(neet == 1,          vartype = "ci") * 100
  ) %>% filter(!is.na(province_lbl))

fig5a <- ggplot(fig5_data, aes(x = reorder(province_lbl, pct_emp), y = pct_emp)) +
  geom_col(fill = ug, alpha = 0.85, width = 0.65) +
  geom_errorbar(aes(ymin = pct_emp_low, ymax = pct_emp_upp),
                width = 0.25, linewidth = 0.5) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(title = "Employment Rate by Province", subtitle = "Women 15–64",
       x = NULL, y = "% Employed", caption = src) +
  theme_aimpact()

fig5b <- ggplot(fig5_data, aes(x = reorder(province_lbl, pct_wage), y = pct_wage)) +
  geom_col(fill = ub, alpha = 0.85, width = 0.65) +
  geom_errorbar(aes(ymin = pct_wage_low, ymax = pct_wage_upp),
                width = 0.25, linewidth = 0.5) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(title = "Wage Employment Rate by Province", subtitle = "Women 15–64",
       x = NULL, y = "% in Wage Work", caption = src) +
  theme_aimpact()

fig5 <- fig5a + fig5b +
  plot_annotation(title = "Employment by Province — Women 15–64",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
save_fig(fig5, "fig5_employment_by_province", w = 12, h = 5)

# ── Fig 6: NEET by caste (girls 15-24)
fig6_data <- svy_girls %>%
  group_by(caste_broad) %>%
  summarise(pct = survey_mean(neet == 1, vartype = "ci") * 100) %>%
  filter(!is.na(caste_broad))

fig6 <- ggplot(fig6_data,
               aes(x = reorder(caste_broad, pct), y = pct)) +
  geom_col(fill = ur, alpha = 0.85, width = 0.65) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                width = 0.25, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.2, size = 3.2) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(title    = "NEET Rate by Caste/Ethnicity — Girls",
       subtitle = "Girls 15–24",
       x = NULL, y = "% NEET", caption = src) +
  theme_aimpact()
save_fig(fig6, "fig6_neet_by_caste")

# =============================================================================
# SECTION 4 — POVERTY AND OVERLAPPING DISADVANTAGE
# =============================================================================
cat("Building poverty and inequality figures...\n")

# ── Fig 7: Heatmap — NEET by province × wealth quintile (girls)
fig7_data <- svy_girls %>%
  group_by(province_lbl, quintile_lbl) %>%
  summarise(pct = survey_mean(neet == 1, na.rm = TRUE) * 100, n = n()) %>%
  filter(!is.na(province_lbl), !is.na(quintile_lbl), n >= 15)

fig7 <- ggplot(fig7_data,
               aes(x = quintile_lbl, y = province_lbl, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            size = 3,
            colour = ifelse(fig7_data$pct > 50, "white", "#222222")) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                        name = "% NEET", limits = c(0, NA)) +
  labs(title    = "NEET Rate: Province × Wealth Quintile — Girls",
       subtitle = "Girls 15–24 | Cells with n < 15 excluded",
       x = "Expenditure Quintile", y = NULL, caption = src) +
  theme_aimpact() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(fig7, "fig7_heatmap_neet_province_quintile", w = 9, h = 5)

# ── Fig 8: Province scatter — enrolment vs NEET (girls)
fig8_data <- svy_girls %>%
  group_by(province_lbl) %>%
  summarise(
    pct_enrol = survey_mean(attend_school == 1, na.rm = TRUE) * 100,
    pct_neet  = survey_mean(neet == 1,          na.rm = TRUE) * 100,
    pct_poor  = survey_mean(ppoor == 1,          na.rm = TRUE) * 100
  ) %>% filter(!is.na(province_lbl))

fig8 <- ggplot(fig8_data,
               aes(x = pct_enrol, y = pct_neet,
                   size = pct_poor, label = province_lbl)) +
  geom_point(colour = ub, alpha = 0.75) +
  geom_text(nudge_y = 1.2, size = 3.2, colour = "#333333") +
  scale_size_continuous(name = "Poverty rate (%)", range = c(3, 10)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "Province Performance: Enrolment vs. NEET",
       subtitle = "Girls 15–24 | Bubble size = poverty rate",
       x = "% Currently Enrolled", y = "% NEET", caption = src) +
  theme_aimpact()
save_fig(fig8, "fig8_province_scatter", w = 8, h = 6)

# ── Fig 9: NEET by wealth quintile and sex (youth 15-24)
fig9_data <- svy_b %>%
  group_by(quintile_lbl, sex_lbl) %>%
  summarise(pct = survey_mean(neet == 1, vartype = "ci") * 100) %>%
  filter(!is.na(quintile_lbl), !is.na(sex_lbl))

fig9 <- ggplot(fig9_data,
               aes(x = quintile_lbl, y = pct,
                   group = sex_lbl, colour = sex_lbl)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp, fill = sex_lbl),
              alpha = 0.1, colour = NA) +
  scale_colour_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_fill_manual(values   = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "NEET Rate by Wealth Quintile and Sex",
       subtitle = "Youth 15–24",
       x = "Expenditure Quintile", y = "% NEET", caption = src) +
  theme_aimpact()
save_fig(fig9, "fig9_neet_by_quintile_sex")

# =============================================================================
# SECTION 5 — TOP GAPS SUMMARY
# =============================================================================
cat("Computing top gaps...\n")

# Province gap in enrolment (girls)
prov_enrol <- svy_girls %>%
  group_by(province_lbl) %>%
  summarise(pct = survey_mean(attend_school == 1, na.rm = TRUE) * 100) %>%
  filter(!is.na(province_lbl))
gap_province <- diff(range(prov_enrol$pct))

# Quintile gap
quint_enrol <- svy_girls %>%
  group_by(quintile_n) %>%
  summarise(pct = survey_mean(attend_school == 1, na.rm = TRUE) * 100) %>%
  filter(!is.na(quintile_n))
gap_quintile <- diff(range(quint_enrol$pct))

# Gender gap in NEET (youth 15-24)
gender_neet <- svy_b %>%
  group_by(sex_lbl) %>%
  summarise(pct = survey_mean(neet == 1, na.rm = TRUE) * 100) %>%
  filter(!is.na(sex_lbl))
gap_gender <- abs(diff(gender_neet$pct))

# Caste gap (Brahmin vs Dalit enrolment, girls)
caste_enrol <- svy_girls %>%
  filter(caste_broad %in% c("Brahmin/Chhetri", "Dalit")) %>%
  group_by(caste_broad) %>%
  summarise(pct = survey_mean(attend_school == 1, na.rm = TRUE) * 100) %>%
  filter(!is.na(caste_broad))
gap_caste <- if (nrow(caste_enrol) == 2) abs(diff(caste_enrol$pct)) else NA_real_

# Poverty gap
pov_enrol <- svy_girls %>%
  group_by(ppoor) %>%
  summarise(pct = survey_mean(attend_school == 1, na.rm = TRUE) * 100) %>%
  filter(!is.na(ppoor))
gap_poverty <- if (nrow(pov_enrol) == 2) abs(diff(pov_enrol$pct)) else NA_real_

top_gaps <- tibble(
  Gap = c("Province gap (highest vs. lowest enrolment)",
          "Richest vs. Poorest quintile (enrolment)",
          "Poor vs. Non-poor (enrolment)",
          "Gender gap in NEET (girls vs. boys, 15-24)",
          "Caste gap: Brahmin/Chhetri vs. Dalit (enrolment)"),
  `Gap (pp)` = round(c(gap_province, gap_quintile, gap_poverty,
                        gap_gender, gap_caste), 1)
) %>% filter(!is.na(`Gap (pp)`))

readr::write_csv(top_gaps, file.path(TABLES_PATH, "top_gaps.csv"))

fig10 <- ggplot(top_gaps,
                aes(x = reorder(Gap, `Gap (pp)`), y = `Gap (pp)`)) +
  geom_col(fill = ur, alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(round(`Gap (pp)`, 1), " pp")),
            hjust = -0.15, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(title    = "Magnitude of Key Inequality Gaps — Girls 15–24",
       subtitle = "Percentage point gaps in school enrolment or NEET",
       x = NULL, y = "Gap (percentage points)", caption = src) +
  theme_aimpact()
save_fig(fig10, "fig10_top_gaps")

cat("\nTop gaps:\n"); print(top_gaps)

# =============================================================================
# COLLECT ALL RESULTS
# =============================================================================
desc_results <- list(
  t_province = t_province, t_caste = t_caste, t_poverty = t_poverty,
  t2 = t2, top_gaps = top_gaps, sample_flow = sample_flow
)
save(desc_results, file = file.path(OUTPUT_PATH, "descriptive_results.RData"))
cat("\n✓ All figures and tables saved.\n")
cat("  Next: Rscript R/03_regressions.R\n\n")
