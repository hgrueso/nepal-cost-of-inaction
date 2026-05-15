# =============================================================================
# 04_slides_figures.R — Policy-ready figures for Nepal CoI deck
# Single source of truth. Generates all PNGs + CSVs needed by the QMD.
# =============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(srvyr); library(scales)
  library(fixest); library(broom); library(patchwork)
})
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")
load("outputs/regression_results_v2.RData")

FIGS   <- "outputs/figures"; TABLES <- "outputs/tables"
dir.create(FIGS,   showWarnings=FALSE, recursive=TRUE)
dir.create(TABLES, showWarnings=FALSE, recursive=TRUE)

# UNICEF palette
ub  <- "#1CABE2"; ud  <- "#00377C"; ug  <- "#00833D"
ur  <- "#E2231A"; ugr <- "#6C7279"

th_policy <- function(b = 12) theme_minimal(base_size = b) + theme(
  plot.title    = element_text(face = "bold", size = b + 2, colour = "#222"),
  plot.subtitle = element_text(size = b, colour = ugr),
  plot.caption  = element_text(size = b - 3, colour = ugr, hjust = 0),
  axis.title    = element_text(size = b, colour = "#333"),
  axis.text     = element_text(size = b - 1, colour = "#333"),
  panel.grid.minor       = element_blank(),
  panel.grid.major.y     = element_line(colour = "#EBEBEB", linewidth = 0.4),
  panel.grid.major.x     = element_blank(),
  legend.position = "bottom",
  legend.text     = element_text(size = b - 1),
  strip.text      = element_text(face = "bold", size = b)
)

sv <- function(p, n, w = 10, h = 5.5) {
  ggsave(file.path(FIGS, paste0(n, ".png")), p,
         width = w, height = h, dpi = 300, bg = "white")
  cat("  v", n, "\n")
}

make_svy <- function(d) srvyr::as_survey_design(
  d %>% filter(!is.na(ind_wt), !is.na(province_num)),
  ids = psu_number, strata = province_num, weights = ind_wt, nest = TRUE)
svy_b <- make_svy(df_b)

# =============================================================================
# D1 — Overall gender gap (4 outcomes)
# =============================================================================
cat("\n=== D1 ===\n")
gap_overall <- svy_b %>% group_by(sex_lbl) %>%
  summarise(
    Enrolled  = survey_mean(attend_any   == 1, vartype = "ci") * 100,
    Secondary = survey_mean(in_secondary == 1, vartype = "ci") * 100,
    Employed  = survey_mean(employed_ilo == 1, na.rm = TRUE, vartype = "ci") * 100,
    NEET      = survey_mean(neet         == 1, na.rm = TRUE, vartype = "ci") * 100,
    .groups   = "drop") %>%
  filter(!is.na(sex_lbl))

make_long <- function(d, base) {
  d %>% select(sex_lbl, value = all_of(base),
               lo = paste0(base, "_low"), hi = paste0(base, "_upp")) %>%
    mutate(outcome = base)
}

g_long <- bind_rows(
  make_long(gap_overall, "Enrolled"),
  make_long(gap_overall, "Secondary"),
  make_long(gap_overall, "Employed"),
  make_long(gap_overall, "NEET")
) %>%
  mutate(outcome = factor(outcome, c("Enrolled", "Secondary", "Employed", "NEET")))

figD1 <- ggplot(g_long, aes(x = outcome, y = value, fill = sex_lbl)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.92) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(0.7), width = 0.18, linewidth = 0.4) +
  geom_text(aes(label = paste0(round(value, 0), "%"), y = hi + 1.5),
            position = position_dodge(0.7), size = 4.2, fontface = "bold") +
  scale_fill_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Girls enrol more, work less, and end up NEET twice as often",
       subtitle = "Survey-weighted estimates for youth aged 15-24 | NLSS IV 2022/23",
       x = NULL, y = "% of youth 15-24",
       caption = "The gender gap is not in schools - it is in labour markets after schools.") +
  th_policy()
sv(figD1, "d1_overall_gap", w = 11, h = 5.5)

# =============================================================================
# D2 — NEET by province x sex
# =============================================================================
cat("\n=== D2 ===\n")
neet_prov <- svy_b %>% group_by(province_lbl, sex_lbl) %>%
  summarise(pct = survey_mean(neet == 1, na.rm = TRUE, vartype = "ci") * 100,
            .groups = "drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

figD2 <- ggplot(neet_prov,
  aes(x = reorder(as.character(province_lbl), pct), y = pct, fill = sex_lbl)) +
  geom_col(position = position_dodge(0.72), width = 0.65, alpha = 0.92) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                position = position_dodge(0.72), width = 0.18, linewidth = 0.4) +
  geom_text(aes(label = paste0(round(pct, 0), "%"), y = pct_upp + 1),
            position = position_dodge(0.72), size = 3.2, hjust = -0.1) +
  scale_fill_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(title = "Madhesh and Lumbini show the widest female NEET gap",
       subtitle = "Female NEET rate exceeds male in every province | Youth 15-24",
       x = NULL, y = "% NEET",
       caption = "Source: NLSS IV 2022/23 | survey-weighted with 95% CI") +
  th_policy()
sv(figD2, "d2_neet_by_province", w = 11, h = 5.5)

# =============================================================================
# D3 — Intersectionality (corrected narrative)
# =============================================================================
cat("\n=== D3 ===\n")
intersect_data <- df_girls %>%
  filter(!is.na(caste_broad),
         caste_broad %in% c("Dalit", "Brahmin/Chhetri", "Janajati", "Religious/Linguistic"),
         !is.na(currently_married), !is.na(rural)) %>%
  mutate(profile = case_when(
    currently_married == 1 & rural == 1 ~ "Married + Rural",
    currently_married == 1 & rural == 0 ~ "Married + Urban",
    currently_married == 0 & rural == 1 ~ "Unmarried + Rural",
    currently_married == 0 & rural == 0 ~ "Unmarried + Urban")) %>%
  group_by(caste_broad, profile) %>%
  summarise(neet = mean(neet, na.rm = TRUE) * 100, n = n(), .groups = "drop") %>%
  filter(n >= 15, !is.na(profile)) %>%
  mutate(profile = factor(profile,
            c("Married + Rural", "Married + Urban",
              "Unmarried + Rural", "Unmarried + Urban")),
         caste_broad = factor(caste_broad,
            c("Religious/Linguistic", "Dalit", "Janajati", "Brahmin/Chhetri")))

figD3 <- ggplot(intersect_data,
    aes(x = profile, y = neet, fill = caste_broad)) +
  geom_col(position = position_dodge(0.78, preserve = "single"),
           width = 0.7, alpha = 0.92) +
  geom_text(aes(label = paste0(round(neet, 0), "%"), y = neet + 1.5),
            position = position_dodge(0.78, preserve = "single"),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Religious/Linguistic" = "#9467BD",
                                "Dalit" = "#E2231A",
                                "Janajati" = "#00833D",
                                "Brahmin/Chhetri" = "#00377C"),
                    name = "Caste/Ethnicity") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Marriage drives near-universal NEET - regardless of caste",
       subtitle = "Married girls reach 80-98% NEET across all caste groups | Girls 15-24",
       x = NULL, y = "% NEET",
       caption = "Cells with n<15 dropped. Religious/Linguistic concentrated in Madhesh/Lumbini.") +
  th_policy() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))
sv(figD3, "d3_intersectionality", w = 12, h = 5.5)

# =============================================================================
# D3b — Enrolment intersectionality (mirror of D3 but enrolment outcome)
# =============================================================================
cat("\n=== D3b ===\n")
intersect_enrol <- df_girls %>%
  filter(!is.na(caste_broad),
         caste_broad %in% c("Dalit", "Brahmin/Chhetri", "Janajati", "Religious/Linguistic"),
         !is.na(currently_married), !is.na(rural)) %>%
  mutate(profile = case_when(
    currently_married == 1 & rural == 1 ~ "Married + Rural",
    currently_married == 1 & rural == 0 ~ "Married + Urban",
    currently_married == 0 & rural == 1 ~ "Unmarried + Rural",
    currently_married == 0 & rural == 0 ~ "Unmarried + Urban")) %>%
  group_by(caste_broad, profile) %>%
  summarise(enrol = mean(attend_any, na.rm = TRUE) * 100, n = n(), .groups = "drop") %>%
  filter(n >= 15, !is.na(profile)) %>%
  mutate(profile = factor(profile,
            c("Married + Rural", "Married + Urban",
              "Unmarried + Rural", "Unmarried + Urban")),
         caste_broad = factor(caste_broad,
            c("Religious/Linguistic", "Dalit", "Janajati", "Brahmin/Chhetri")))

figD3b <- ggplot(intersect_enrol,
    aes(x = profile, y = enrol, fill = caste_broad)) +
  geom_col(position = position_dodge(0.78, preserve = "single"),
           width = 0.7, alpha = 0.92) +
  geom_text(aes(label = paste0(round(enrol, 0), "%"), y = enrol + 1.5),
            position = position_dodge(0.78, preserve = "single"),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Religious/Linguistic" = "#9467BD",
                                "Dalit" = "#E2231A",
                                "Janajati" = "#00833D",
                                "Brahmin/Chhetri" = "#00377C"),
                    name = "Caste/Ethnicity") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Marriage collapses enrolment - regardless of caste",
       subtitle = "Married girls show enrolment rates near zero across all caste groups | Girls 15-24",
       x = NULL, y = "% Enrolled",
       caption = "Cells with n<15 dropped. Enrolment at any school level.") +
  th_policy() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))
sv(figD3b, "d3b_enrolment_intersectionality", w = 12, h = 5.5)


# =============================================================================
# R1 — Coefficient plot in pp
# =============================================================================
cat("\n=== R1 ===\n")
coef_data <- bind_rows(
  tidy(reg_results_v2$m1$full, conf.int = TRUE) %>% mutate(outcome = "Enrolment\n(Girls 15-24)"),
  tidy(reg_results_v2$m2$full, conf.int = TRUE) %>% mutate(outcome = "NEET\n(Youth 15-24)")
) %>%
  filter(term %in% c("female", "rural", "ppoor", "quintile_n", "currently_married",
                      "any_difficulty", "caste_broadDalit", "caste_broadJanajati",
                      "caste_broadReligious/Linguistic")) %>%
  mutate(
    term_lbl = recode(term,
      "female" = "Female", "rural" = "Rural", "ppoor" = "Below poverty line",
      "quintile_n" = "Per wealth quintile step",
      "currently_married" = "Currently married", "any_difficulty" = "Disability",
      "caste_broadDalit" = "Dalit", "caste_broadJanajati" = "Janajati",
      "caste_broadReligious/Linguistic" = "Religious/Linguistic"),
    sig = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                    p.value < 0.10 ~ "*", TRUE ~ ""),
    estimate_pp = estimate * 100, lo_pp = conf.low * 100, hi_pp = conf.high * 100,
    lbl = paste0(ifelse(estimate_pp > 0, "+", ""), round(estimate_pp, 1), " pp", sig))

figR1 <- ggplot(coef_data,
    aes(x = estimate_pp, y = reorder(term_lbl, abs(estimate_pp)))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo_pp, xmax = hi_pp),
                orientation = "y", width = 0.3, linewidth = 0.5, colour = ugr) +
  geom_point(aes(colour = ifelse(estimate_pp > 0, ur, ud)), size = 3, alpha = 0.85) +
  geom_text(aes(label = lbl, x = hi_pp + 5), hjust = 0, size = 3.2, fontface = "bold") +
  scale_colour_identity() +
  scale_x_continuous(labels = label_number(suffix = " pp"),
                     expand = expansion(mult = c(0.05, 0.30))) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(title = "Marriage is by far the strongest barrier to girls' inclusion",
       subtitle = "LPM with province fixed effects | All coefficients in pp",
       x = "Effect on outcome (pp)", y = NULL,
       caption = "Reference: Brahmin/Chhetri, urban, non-poor. *p<0.10 **p<0.05 ***p<0.01") +
  th_policy() + theme(panel.grid.major.y = element_blank())
sv(figR1, "r1_coef_pp", w = 13, h = 5.5)

# =============================================================================
# R2 — Causal forest CATE by caste
# =============================================================================
cat("\n=== R2 ===\n")
cate_caste <- cf_results$cate_neet_all %>%
  filter(dimension == "Caste/Ethnicity", group != "Other") %>%
  mutate(group = as.character(group)) %>%
  arrange(mean_cate) %>%
  mutate(group = factor(group, levels = group))

figR2 <- ggplot(cate_caste, aes(x = mean_cate * 100, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -13.2, linetype = "dotted", colour = "#333") +
  geom_errorbar(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                orientation = "y", width = 0.3, linewidth = 0.6, colour = ur) +
  geom_point(size = 4, colour = ur, alpha = 0.88) +
  geom_text(aes(label = paste0(round(mean_cate * 100, 1), " pp"),
                x = ci_hi * 100 + 0.3),
            hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = -13.2, y = 0.5, label = "ATE = -13.2 pp",
           hjust = 0, vjust = 0, size = 3, colour = "#333", fontface = "italic") +
  scale_x_continuous(labels = label_number(suffix = " pp"),
                     expand = expansion(mult = c(0.05, 0.30))) +
  labs(title = "Secondary education reduces NEET by ~13 pp - for every caste",
       subtitle = "Causal forest CATE | Women 15-64 | Negative = education works",
       x = "Reduction in NEET probability (pp)", y = NULL,
       caption = "Effects stable across groups. Dalit women benefit ~2 pp less.") +
  th_policy()
sv(figR2, "r2_cate_caste", w = 11, h = 5)

# =============================================================================
# MINCER LIFETIME EARNINGS — additional schooling -> lifetime earnings
# Powerful policy plot: years of schooling vs PV lifetime earnings (NPR)
# =============================================================================
cat("\n=== Mincer lifetime earnings ===\n")

# Auto-detect education years variable name (varies across cleaning versions)
edu_var <- intersect(c("edu_yrs_c", "edu_yrs", "years_edu", "schooling_yrs",
                        "years_schooling", "yos"), names(df))[1]
exp_var <- intersect(c("experience", "exp", "exper", "years_exp"), names(df))[1]
age_var <- intersect(c("age_num", "age", "age_years"), names(df))[1]

if (is.na(edu_var)) {
  cat("WARNING: No education-years variable found. Available 'edu' vars:\n")
  print(grep("edu|school|year", names(df), value = TRUE, ignore.case = TRUE))
  # Construct fallback from edu_cat if available
  if ("edu_cat" %in% names(df)) {
    df$edu_yrs_c <- as.numeric(factor(df$edu_cat,
      levels = c("None","Primary","Lower secondary","Secondary",
                  "Higher secondary","Tertiary"))) * 2 - 2  # 0,2,4,6,8,10
    edu_var <- "edu_yrs_c"
    cat("Created edu_yrs_c from edu_cat (None=0, Primary=2, ..., Tertiary=10)\n")
  } else {
    edu_var <- "edu_cat"  # last resort
  }
}
cat("Using:", edu_var, "|", exp_var, "|", age_var, "\n")

df_sal <- df %>%
  filter(!is.na(monthly_wage), monthly_wage > 0,
         !is.na(.data[[edu_var]]),
         !is.na(.data[[age_var]]),
         .data[[age_var]] >= 15, .data[[age_var]] <= 64) %>%
  mutate(log_wage = log(monthly_wage))

# Handle missing experience variable
if (is.na(exp_var)) {
  df_sal$experience <- pmax(df_sal[[age_var]] - df_sal[[edu_var]] - 6, 0)
  exp_var <- "experience"
}
df_sal$experience2 <- df_sal[[exp_var]]^2

# Models with child marriage interaction (girls only)
df_sal_g <- df_sal %>% filter(female == 1)
df_sal_b <- df_sal %>% filter(female == 0)

mincer_formula <- as.formula(paste0("log_wage ~ ", edu_var,
                                     " + ", exp_var, " + experience2 + rural | province_lbl"))
m_g_basic <- feols(mincer_formula, data = df_sal_g, se = "hetero")
m_b_basic <- feols(mincer_formula, data = df_sal_b, se = "hetero")

# Build the projection: each additional year of schooling -> PV lifetime gain
# Assume base wage = median provincial wage; project across 35 working years
build_lifetime <- function(model, base_wage = 2500, T_years = 35, r = 0.05) {
  beta <- coef(model)[edu_var]
  years <- seq(0, 12, by = 1)
  # PV of lifetime monthly earnings at education level s, relative to s=0
  pv <- sapply(years, function(s) {
    wage_monthly <- base_wage * exp(beta * s)
    wage_monthly * 12 * (1 - (1 + r)^(-T_years)) / r
  })
  tibble(years_schooling = years, pv_lifetime_npr = pv,
         pv_lifetime_lakh = pv / 1e5)  # convert to lakh
}

life_girls <- build_lifetime(m_g_basic) %>% mutate(sex = "Girls")
life_boys  <- build_lifetime(m_b_basic) %>% mutate(sex = "Boys")
life_both  <- bind_rows(life_girls, life_boys)

figMINCER_lifetime <- ggplot(life_both,
    aes(x = years_schooling, y = pv_lifetime_lakh, colour = sex)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 10, linetype = "dashed", colour = ugr) +
  annotate("text", x = 10.2, y = 5, label = "Secondary\ncompletion",
           hjust = 0, size = 3, colour = ugr, fontface = "italic") +
  scale_colour_manual(values = c("Girls" = ub, "Boys" = ud), name = NULL) +
  scale_x_continuous(breaks = seq(0, 12, 2),
                     labels = c("0", "2\n(primary)", "4", "6", "8\n(lower sec)",
                                "10\n(secondary)", "12\n(higher sec)")) +
  labs(title = "Each additional year of schooling raises lifetime earnings substantially",
       subtitle = "Projected present-value of lifetime earnings | Base wage NPR 2,500/month",
       x = "Years of schooling", y = "Lifetime earnings (NPR Lakh, PV)",
       caption = paste0(
         "Girls return: ", round(coef(m_g_basic)[edu_var] * 100, 1),
         "% per year of schooling | Boys: ",
         round(coef(m_b_basic)[edu_var] * 100, 1),
         "% | 35-year horizon, 5% discount rate.")) +
  th_policy()
sv(figMINCER_lifetime, "m1_mincer_lifetime", w = 11, h = 5.5)

# =============================================================================
# CHILD MARRIAGE INTERACTION — does education protect MORE if married?
# =============================================================================
cat("\n=== Child marriage interaction ===\n")

# Interaction LPM: NEET ~ secondary * married + controls
df_int <- df_b %>%
  filter(!is.na(neet), !is.na(completed_secondary), !is.na(currently_married),
         !is.na(rural), !is.na(ppoor), !is.na(caste_broad), female == 1)

m_int <- feols(neet ~ completed_secondary * currently_married +
                 rural + ppoor + quintile_n + any_difficulty + caste_broad |
                 province_lbl, data = df_int, se = "hetero")

# Get marginal effect: NEET reduction from secondary education,
# separately for married and unmarried girls
int_data <- tibble(
  group = c("Unmarried girls", "Married girls"),
  effect = c(coef(m_int)["completed_secondary"],
             coef(m_int)["completed_secondary"] + coef(m_int)["completed_secondary:currently_married"]) * 100
)
int_data$effect_se <- c(
  sqrt(vcov(m_int)["completed_secondary", "completed_secondary"]),
  sqrt(vcov(m_int)["completed_secondary", "completed_secondary"] +
       vcov(m_int)["completed_secondary:currently_married", "completed_secondary:currently_married"] +
       2 * vcov(m_int)["completed_secondary", "completed_secondary:currently_married"])
) * 100
int_data$lo <- int_data$effect - 1.96 * int_data$effect_se
int_data$hi <- int_data$effect + 1.96 * int_data$effect_se

figM2 <- ggplot(int_data, aes(x = group, y = effect, fill = group)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(effect, 1), " pp"),
                y = ifelse(effect < 0, lo - 1.5, hi + 1)),
            size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Unmarried girls" = ub, "Married girls" = ur),
                    guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(title = "Secondary education protects unmarried girls more strongly",
       subtitle = "Effect of secondary completion on NEET probability, by marital status",
       x = NULL, y = "Effect on NEET (pp)",
       caption = "Interaction term in LPM with province FE. Marriage erodes much of the education return.") +
  th_policy()
sv(figM2, "m2_marriage_interaction", w = 10, h = 5)

# =============================================================================
# COI CALCULATIONS — based on actual numbers
# =============================================================================
cat("\n=== COI: striking numbers ===\n")

prov_data <- tribble(
  ~province_lbl,     ~base_wage, ~gdp_nprb,
  "Bagmati",          7000,       2074,
  "Koshi",            2500,        903,
  "Gandaki",          2292,        517,
  "Madhesh",          2000,        748,
  "Lumbini",          1500,        814,
  "Karnali",          1066,        244,
  "Sudurpashchim",    1042,        404
)

pop_excluded <- df_girls %>%
  filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(total_wt   = sum(ind_wt, na.rm = TRUE),
            pct_no_sec = mean(completed_secondary == 0, na.rm = TRUE),
            .groups    = "drop") %>%
  mutate(province_lbl = as.character(province_lbl),
         pop_share  = total_wt / sum(total_wt),
         pop_girls  = round(1.4e6 * pop_share),
         n_excluded = round(pop_girls * pct_no_sec))

cate_prov <- cf_results$cate_neet_all %>%
  filter(dimension == "Province") %>%
  mutate(province_lbl = as.character(group), cate_abs = abs(mean_cate))

coi <- pop_excluded %>%
  left_join(prov_data, by = "province_lbl") %>%
  left_join(cate_prov %>% select(province_lbl, cate_abs), by = "province_lbl") %>%
  mutate(
    pv_per_girl     = base_wage * 12 * ((1.09)^2 - 1) * (1 - 1.05^(-35)) / 0.05,
    wage_months_lost = pv_per_girl / base_wage,
    coi_per_100k    = cate_abs * 100 * pv_per_girl / 1000,
    total_coi_b     = cate_abs * n_excluded * pv_per_girl / 1e9,
    coi_pct_gdp     = total_coi_b / gdp_nprb * 100
  )

national_coi_b    <- sum(coi$total_coi_b, na.rm = TRUE)
national_coi_usdm <- national_coi_b * 1000 / 133
national_excluded <- sum(coi$n_excluded, na.rm = TRUE)

cat(sprintf("NATIONAL CoI: NPR %.1fB (~USD %.0fM) | %s girls\n",
            national_coi_b, national_coi_usdm,
            format(national_excluded, big.mark = ",")))

readr::write_csv(coi, file.path(TABLES, "coi_full.csv"))

# C1 — Total CoI by province (single color, ordered)
figC1 <- ggplot(coi %>% filter(!is.na(total_coi_b)),
    aes(x = reorder(province_lbl, total_coi_b), y = total_coi_b)) +
  geom_col(fill = ud, alpha = 0.9, width = 0.65) +
  geom_text(aes(label = paste0("NPR ", round(total_coi_b, 1), "B"),
                y = total_coi_b + 0.15),
            hjust = 0, size = 3.8, fontface = "bold") +
  scale_y_continuous(labels = label_number(suffix = "B", scale = 1),
                     expand = expansion(mult = c(0, 0.30))) +
  coord_flip() +
  labs(title = paste0("Nepal forgoes NPR ", round(national_coi_b, 1),
                       " billion (~USD ", round(national_coi_usdm),
                       "M) by not educating its girls"),
       subtitle = paste0("Total lifetime productivity loss across ",
                          format(national_excluded, big.mark = ","),
                          " girls aged 15-24 without secondary completion"),
       x = NULL, y = "Cost of Inaction (NPR Billion)",
       caption = "ILO 9% return | 35-year horizon | 5% discount rate") +
  th_policy()
sv(figC1, "c1_total_coi_billion", w = 12, h = 5.5)

# C3 — CoI vs national budgets (replaces the empty C2 wage-months plot)
benchmarks <- tribble(
  ~item, ~value_b, ~lbl,
  "Annual CoI\n(adolescent girls)",                  national_coi_b, paste0("NPR ", round(national_coi_b, 1), "B"),
  "Nepal Health Sector\nbudget (2024/25)",           84,             "NPR 84B",
  "Nepal Education Sector\nbudget (2024/25)",        197,            "NPR 197B")
benchmarks$item <- factor(benchmarks$item, levels = benchmarks$item)

figC3 <- ggplot(benchmarks, aes(x = item, y = value_b)) +
  geom_col(aes(fill = ifelse(grepl("CoI", item), ur, ud)),
           alpha = 0.9, width = 0.55) +
  geom_text(aes(label = lbl, y = value_b + 5), size = 4.5, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = label_number(suffix = "B", scale = 1),
                     expand = expansion(mult = c(0, 0.20))) +
  labs(title = paste0("Nepal's cost of inaction = ",
                       round(national_coi_b / 84 * 100),
                       "% of the annual health budget"),
       subtitle = "One-cohort lifetime productivity loss vs annual sector spending",
       x = NULL, y = "NPR Billion",
       caption = "Sources: NLSS IV 2022/23 | Ministry of Finance Nepal (2024/25 fiscal year)") +
  th_policy() + theme(axis.text.x = element_text(size = 11))
sv(figC3, "c3_coi_vs_budget", w = 11, h = 5.5)

# Save summary numbers
summary_nums <- list(
  national_coi_b           = round(national_coi_b, 1),
  national_coi_usdm        = round(national_coi_usdm),
  national_excluded        = national_excluded,
  coi_pct_health_budget    = round(national_coi_b / 84 * 100),
  coi_pct_education_budget = round(national_coi_b / 197 * 100),
  girls_return_pct         = round(coef(m_g_basic)[edu_var] * 100, 1),
  boys_return_pct          = round(coef(m_b_basic)[edu_var] * 100, 1)
)
saveRDS(summary_nums, "outputs/coi_summary_numbers.RData")

# =============================================================================
# D4 — Gender gap by age (15-18 vs 19-24)
# =============================================================================
cat("\n=== D4: age groups ===\n")
svy_b1 <- make_svy(df_b1); svy_b2 <- make_svy(df_b2)

gap_age <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(Enrolled  = survey_mean(attend_any   == 1, vartype = "ci") * 100,
              Secondary = survey_mean(in_secondary == 1, vartype = "ci") * 100,
              Employed  = survey_mean(employed_ilo == 1, na.rm = TRUE, vartype = "ci") * 100,
              NEET      = survey_mean(neet         == 1, na.rm = TRUE, vartype = "ci") * 100,
              .groups   = "drop") %>%
    mutate(age = "15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(Enrolled  = survey_mean(attend_any   == 1, vartype = "ci") * 100,
              Secondary = survey_mean(in_secondary == 1, vartype = "ci") * 100,
              Employed  = survey_mean(employed_ilo == 1, na.rm = TRUE, vartype = "ci") * 100,
              NEET      = survey_mean(neet         == 1, na.rm = TRUE, vartype = "ci") * 100,
              .groups   = "drop") %>%
    mutate(age = "19-24")
) %>% filter(!is.na(sex_lbl))

make_long_age <- function(d, base) {
  d %>% select(sex_lbl, age, value = all_of(base),
               lo = paste0(base, "_low"), hi = paste0(base, "_upp")) %>%
    mutate(outcome = base)
}
ga_long <- bind_rows(
  make_long_age(gap_age, "Enrolled"),
  make_long_age(gap_age, "Secondary"),
  make_long_age(gap_age, "Employed"),
  make_long_age(gap_age, "NEET")
) %>% mutate(outcome = factor(outcome,
              c("Enrolled", "Secondary", "Employed", "NEET")))

figD4 <- ggplot(ga_long, aes(x = outcome, y = value, fill = sex_lbl)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.92) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(0.7), width = 0.18, linewidth = 0.4) +
  geom_text(aes(label = paste0(round(value, 0), "%"), y = hi + 1.5),
            position = position_dodge(0.7), size = 3.4, fontface = "bold") +
  scale_fill_manual(values = c("Male" = ud, "Female" = ub), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.20))) +
  facet_wrap(~ age) +
  labs(title = "The gender NEET gap explodes after age 18",
       subtitle = "Youth outcomes by age group and sex | NLSS IV 2022/23",
       x = NULL, y = "% of youth",
       caption = "At 15-18 the gap is +9 pp; at 19-24 it reaches +25 pp.") +
  th_policy()
sv(figD4, "d4_gap_by_age", w = 12, h = 5.5)

# =============================================================================
# A1 — Boys' regression
# =============================================================================
cat("\n=== A1: boys regression ===\n")
df_boys_reg <- df_b %>%
  filter(female == 0, !is.na(neet), !is.na(rural), !is.na(ppoor),
         !is.na(quintile_n), !is.na(currently_married),
         !is.na(any_difficulty), !is.na(caste_broad))

m_boys_neet <- feols(neet ~ rural + ppoor + quintile_n + currently_married +
                       any_difficulty + caste_broad | province_lbl,
                     data = df_boys_reg, se = "hetero")

boys_full <- tidy(m_boys_neet, conf.int = TRUE) %>%
  mutate(
    Variable = recode(term,
      "rural" = "Rural", "ppoor" = "Below poverty line",
      "quintile_n" = "Per wealth quintile step",
      "currently_married" = "Currently married",
      "any_difficulty" = "Disability",
      "caste_broadDalit" = "Dalit", "caste_broadJanajati" = "Janajati",
      "caste_broadReligious/Linguistic" = "Religious/Linguistic",
      "caste_broadOther" = "Other caste"),
    Coefficient = sprintf("%.3f", estimate),
    SE = sprintf("%.3f", std.error),
    `95% CI` = sprintf("[%.3f, %.3f]", conf.low, conf.high),
    `p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) %>%
  select(Variable, Coefficient, SE, `95% CI`, `p-value`)
readr::write_csv(boys_full, file.path(TABLES, "boys_reg_full.csv"))

boys_coef <- tidy(m_boys_neet, conf.int = TRUE) %>%
  mutate(
    term_lbl = recode(term,
      "rural" = "Rural", "ppoor" = "Below poverty line",
      "quintile_n" = "Per wealth quintile step",
      "currently_married" = "Currently married",
      "any_difficulty" = "Disability",
      "caste_broadDalit" = "Dalit", "caste_broadJanajati" = "Janajati",
      "caste_broadReligious/Linguistic" = "Religious/Linguistic",
      "caste_broadOther" = "Other caste"),
    sig = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                    p.value < 0.10 ~ "*", TRUE ~ ""),
    pp = estimate * 100, lo = conf.low * 100, hi = conf.high * 100,
    lbl = paste0(ifelse(pp > 0, "+", ""), round(pp, 1), " pp", sig))

figA1 <- ggplot(boys_coef,
    aes(x = pp, y = reorder(term_lbl, abs(pp)))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo, xmax = hi),
                orientation = "y", width = 0.3, linewidth = 0.5, colour = ugr) +
  geom_point(aes(colour = ifelse(pp > 0, ur, ud)), size = 3, alpha = 0.85) +
  geom_text(aes(label = lbl, x = hi + 3), hjust = 0, size = 3.2, fontface = "bold") +
  scale_colour_identity() +
  scale_x_continuous(labels = label_number(suffix = " pp"),
                     expand = expansion(mult = c(0.05, 0.25))) +
  labs(title = "What drives NEET for boys aged 15-24",
       subtitle = "Marriage matters much less for boys; poverty matters more",
       x = "Effect on NEET (pp)", y = NULL,
       caption = "LPM with province FE | HC1 SE | Ref: Brahmin/Chhetri, urban, non-poor.") +
  th_policy() + theme(panel.grid.major.y = element_blank())
sv(figA1, "a1_boys_coef", w = 11, h = 5)

# =============================================================================
# Mincer full tables (girls and boys, all coefficients)
# =============================================================================
cat("\n=== Mincer tables for appendix ===\n")
format_mincer <- function(model) {
  tidy(model, conf.int = TRUE) %>%
    mutate(
      Variable = recode(term,
        !!edu_var := "Years of schooling",
        "edu_yrs_c"  = "Years of schooling (centred)",
        "experience" = "Experience (years)",
        "experience2" = "Experience squared",
        "rural" = "Rural"),
      Coefficient = sprintf("%.4f", estimate),
      SE = sprintf("%.4f", std.error),
      `95% CI` = sprintf("[%.4f, %.4f]", conf.low, conf.high),
      `p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) %>%
    select(Variable, Coefficient, SE, `95% CI`, `p-value`)
}

mincer_girls_tbl <- format_mincer(m_g_basic) %>%
  bind_rows(tibble(Variable = "N",
                   Coefficient = as.character(nobs(m_g_basic)),
                   SE = "", `95% CI` = "", `p-value` = "")) %>%
  bind_rows(tibble(Variable = "R-squared",
                   Coefficient = sprintf("%.3f", r2(m_g_basic)["r2"]),
                   SE = "", `95% CI` = "", `p-value` = ""))
mincer_boys_tbl <- format_mincer(m_b_basic) %>%
  bind_rows(tibble(Variable = "N",
                   Coefficient = as.character(nobs(m_b_basic)),
                   SE = "", `95% CI` = "", `p-value` = "")) %>%
  bind_rows(tibble(Variable = "R-squared",
                   Coefficient = sprintf("%.3f", r2(m_b_basic)["r2"]),
                   SE = "", `95% CI` = "", `p-value` = ""))

readr::write_csv(mincer_girls_tbl, file.path(TABLES, "mincer_girls_full.csv"))
readr::write_csv(mincer_boys_tbl,  file.path(TABLES, "mincer_boys_full.csv"))

cat("\nALL OUTPUTS SAVED. Run quarto next.\n")
