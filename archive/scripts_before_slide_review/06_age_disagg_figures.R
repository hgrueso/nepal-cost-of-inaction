# =============================================================================
# 06_age_disagg_figures.R
# Addresses Nepal CO review comments on age disaggregation and the distinction
# between child marriage and current marital status (Sofia Af Hallstrom),
# and splits the combined enrolment/NEET coefficient figure into two
# (comment on Figure 4).
#
# Outputs:
#   b1_marriage_by_age.png        marriage prevalence 15-18 vs 19-24, child vs adult marriage
#   b2_outcomes_marital_age.png   enrolment and NEET by marital category and age group
#   b3_neet_marriage_caste_age.png    NEET x marriage x caste, faceted by age group
#   b4_enrol_marriage_caste_age.png   enrolment version
#   b5_coef_by_age.png            key LPM coefficients estimated separately by age group
#   r1a_coef_enrolment.png        split of the combined coefficient figure (enrolment)
#   r1b_coef_neet.png             split of the combined coefficient figure (NEET)
#   tables/age_disagg_stats.csv   the numbers behind the text edits
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(scales); library(broom) })
load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS <- "outputs/figures"; TABLES <- "outputs/tables"
dir.create(FIGS, showWarnings = FALSE, recursive = TRUE)
ub <- "#1CABE2"; ud <- "#00377C"; ug <- "#00833D"; ur <- "#E2231A"; ugr <- "#6C7279"

th_policy <- function(b = 12) theme_minimal(base_size = b) + theme(
  plot.title = element_text(face = "bold", size = b + 2, colour = "#222"),
  plot.subtitle = element_text(size = b, colour = ugr),
  plot.caption = element_text(size = b - 3, colour = ugr, hjust = 0),
  axis.title = element_text(size = b, colour = "#333"),
  axis.text = element_text(size = b - 1, colour = "#333"),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(colour = "#EBEBEB", linewidth = 0.4),
  panel.grid.major.x = element_blank(),
  legend.position = "bottom", strip.text = element_text(face = "bold", size = b))
sv <- function(p, n, w = 11, h = 5.5) {
  ggsave(file.path(FIGS, paste0(n, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  cat("  saved", n, "\n")
}

# ── Base: girls 15-24 with age group and marital category ────────────────────
gg <- df_girls %>%
  filter(age_num >= 15, age_num <= 24, !is.na(ind_wt)) %>%
  mutate(
    agegrp = ifelse(age_num <= 18, "Ages 15-18", "Ages 19-24"),
    marital_cat = case_when(
      currently_married == 1 & age_num < 18 ~ "Married before 18",
      currently_married == 1                 ~ "Currently married",
      TRUE                                   ~ "Not married"),
    marital_cat = factor(marital_cat,
      c("Not married", "Currently married", "Married before 18")))

cat("=== Key disaggregated statistics ===\n")
stats <- gg %>%
  group_by(agegrp, marital_cat) %>%
  summarise(n = n(),
            pct_of_agegrp = NA_real_,
            enrol = 100 * weighted.mean(attend_any, ind_wt, na.rm = TRUE),
            neet  = 100 * weighted.mean(neet, ind_wt, na.rm = TRUE),
            wsum  = sum(ind_wt), .groups = "drop") %>%
  group_by(agegrp) %>%
  mutate(pct_of_agegrp = 100 * wsum / sum(wsum)) %>%
  ungroup() %>% select(-wsum)
print(as.data.frame(stats %>% mutate(across(where(is.numeric), ~round(.x, 1)))), right = FALSE)
readr::write_csv(stats, file.path(TABLES, "age_disagg_stats.csv"))

# ── B1: marriage prevalence by age group, child vs adult marriage ────────────
b1 <- gg %>%
  group_by(agegrp, marital_cat) %>%
  summarise(wsum = sum(ind_wt), .groups = "drop") %>%
  group_by(agegrp) %>% mutate(pct = 100 * wsum / sum(wsum)) %>% ungroup() %>%
  filter(marital_cat != "Not married")

figB1 <- ggplot(b1, aes(x = agegrp, y = pct, fill = marital_cat)) +
  geom_col(width = 0.6, alpha = 0.92) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5), colour = "white",
            fontface = "bold", size = 3.6) +
  scale_fill_manual(values = c("Currently married" = ub,
                               "Married before 18" = ur), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Marriage rises sharply across the adolescent transition",
       subtitle = "Share of girls currently married, by age group and age at marriage | Girls 15-24",
       x = NULL, y = "% currently married",
       caption = "NLSS IV 2022/23, survey-weighted. Age at first marriage is not recorded in NLSS IV; married 19-24-year-olds are shown by current status only.") +
  th_policy()
sv(figB1, "b1_marriage_by_age", w = 9, h = 5)

# ── B2: enrolment and NEET by marital category and age group ─────────────────
b2 <- stats %>% filter(n >= 15) %>%
  pivot_longer(c(enrol, neet), names_to = "outcome", values_to = "pct") %>%
  mutate(outcome = recode(outcome, enrol = "Enrolment", neet = "NEET"))

figB2 <- ggplot(b2, aes(x = marital_cat, y = pct, fill = agegrp)) +
  geom_col(position = position_dodge(0.7, preserve = "single"), width = 0.62, alpha = 0.92) +
  geom_text(aes(label = paste0(round(pct), "%"), y = pct + 2.5),
            position = position_dodge(0.7, preserve = "single"),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Ages 15-18" = ub, "Ages 19-24" = ud), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  facet_wrap(~ outcome) +
  labs(title = "Marriage, not age alone, drives the collapse in enrolment",
       subtitle = "Outcomes by marital category and age group | Girls 15-24",
       x = NULL, y = "%",
       caption = "NLSS IV 2022/23, survey-weighted. Cells with fewer than 15 observations dropped.") +
  th_policy() + theme(axis.text.x = element_text(size = 9))
sv(figB2, "b2_outcomes_marital_age", w = 12, h = 5.5)

# ── B3 and B4: marriage x caste, faceted by age group ────────────────────────
mk_caste_age <- function(outvar, outlbl, fname, title) {
  d <- gg %>%
    filter(caste_broad %in% c("Dalit", "Brahmin/Chhetri", "Janajati", "Religious/Linguistic")) %>%
    mutate(mar = ifelse(currently_married == 1, "Married", "Unmarried")) %>%
    group_by(agegrp, caste_broad, mar) %>%
    summarise(pct = 100 * weighted.mean(.data[[outvar]], ind_wt, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    filter(n >= 15)
  fig <- ggplot(d, aes(x = caste_broad, y = pct, fill = mar)) +
    geom_col(position = position_dodge(0.7, preserve = "single"), width = 0.62, alpha = 0.92) +
    geom_text(aes(label = paste0(round(pct), "%"), y = pct + 2.5),
              position = position_dodge(0.7, preserve = "single"), size = 2.7, fontface = "bold") +
    scale_fill_manual(values = c("Married" = ur, "Unmarried" = ug), name = NULL) +
    scale_y_continuous(labels = label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.14))) +
    facet_wrap(~ agegrp) +
    labs(title = title,
         subtitle = paste0(outlbl, " by marital status and caste, separately for ages 15-18 and 19-24"),
         x = NULL, y = outlbl,
         caption = "NLSS IV 2022/23, survey-weighted. Cells with fewer than 15 observations dropped.") +
    th_policy() + theme(axis.text.x = element_text(size = 8.5, angle = 20, hjust = 1))
  sv(fig, fname, w = 12, h = 5.5)
}
mk_caste_age("neet", "% NEET", "b3_neet_marriage_caste_age",
             "The marriage penalty on NEET holds in both age groups")
mk_caste_age("attend_any", "% enrolled", "b4_enrol_marriage_caste_age",
             "Married girls' enrolment collapses in both age groups")

# ── B5: LPM coefficients estimated separately by age group ───────────────────
fml_enrol <- attend_any ~ rural + ppoor + quintile_n + currently_married +
  any_difficulty + caste_broad + factor(province_num)
fml_neet  <- neet ~ rural + ppoor + quintile_n + currently_married +
  any_difficulty + caste_broad + factor(province_num)

fit_age <- function(fml, dat, out, ag) {
  m <- lm(fml, data = dat, weights = ind_wt)
  tidy(m, conf.int = TRUE) %>% mutate(outcome = out, agegrp = ag)
}
coef_age <- bind_rows(
  fit_age(fml_enrol, gg %>% filter(agegrp == "Ages 15-18"), "Enrolment", "Ages 15-18"),
  fit_age(fml_enrol, gg %>% filter(agegrp == "Ages 19-24"), "Enrolment", "Ages 19-24"),
  fit_age(fml_neet,  gg %>% filter(agegrp == "Ages 15-18"), "NEET", "Ages 15-18"),
  fit_age(fml_neet,  gg %>% filter(agegrp == "Ages 19-24"), "NEET", "Ages 19-24")) %>%
  filter(term %in% c("rural", "ppoor", "quintile_n", "currently_married",
                     "any_difficulty", "caste_broadDalit")) %>%
  mutate(term_lbl = recode(term,
      rural = "Rural", ppoor = "Below poverty line",
      quintile_n = "Per wealth quintile step",
      currently_married = "Currently married",
      any_difficulty = "Disability", caste_broadDalit = "Dalit"),
    est_pp = estimate * 100, lo = conf.low * 100, hi = conf.high * 100)
readr::write_csv(coef_age, file.path(TABLES, "coef_by_age.csv"))

figB5 <- ggplot(coef_age,
    aes(x = est_pp, y = reorder(term_lbl, abs(est_pp)), colour = agegrp)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                width = 0.25, linewidth = 0.5,
                position = position_dodge(0.55)) +
  geom_point(size = 2.6, alpha = 0.9, position = position_dodge(0.55)) +
  scale_colour_manual(values = c("Ages 15-18" = ub, "Ages 19-24" = ud), name = NULL) +
  scale_x_continuous(labels = label_number(suffix = " pp"),
                     expand = expansion(mult = c(0.06, 0.12))) +
  facet_wrap(~ outcome, ncol = 2) +
  labs(title = "The marriage association strengthens with age; most other factors are stable",
       subtitle = "LPM coefficients estimated separately for ages 15-18 and 19-24 | Girls",
       x = "Association with outcome (pp)", y = NULL,
       caption = "NLSS IV 2022/23, survey-weighted LPM with province fixed effects. Reference: Brahmin/Chhetri, urban, non-poor.") +
  th_policy() + theme(panel.grid.major.y = element_blank())
sv(figB5, "b5_coef_by_age", w = 12.5, h = 5.5)

# ── R1 split: the combined coefficient figure as two separate figures ────────
mk_r1 <- function(model, outlbl, fname, title) {
  cd <- tidy(model, conf.int = TRUE) %>%
    filter(term %in% c("female", "rural", "ppoor", "quintile_n", "currently_married",
                       "any_difficulty", "caste_broadDalit", "caste_broadJanajati",
                       "caste_broadReligious/Linguistic")) %>%
    mutate(term_lbl = recode(term,
        female = "Female", rural = "Rural", ppoor = "Below poverty line",
        quintile_n = "Per wealth quintile step",
        currently_married = "Currently married", any_difficulty = "Disability",
        caste_broadDalit = "Dalit", caste_broadJanajati = "Janajati",
        `caste_broadReligious/Linguistic` = "Religious/Linguistic"),
      sig = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                      p.value < 0.10 ~ "*", TRUE ~ ""),
      est_pp = estimate * 100, lo = conf.low * 100, hi = conf.high * 100,
      lbl = paste0(ifelse(est_pp > 0, "+", ""), round(est_pp, 1), " pp", sig))
  fig <- ggplot(cd, aes(x = est_pp, y = reorder(term_lbl, abs(est_pp)))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                  width = 0.3, linewidth = 0.5, colour = ugr) +
    geom_point(colour = ifelse(cd$est_pp > 0, ur, ud), size = 3, alpha = 0.85) +
    geom_text(aes(label = lbl, x = hi + 4), hjust = 0, size = 3.2, fontface = "bold") +
    scale_x_continuous(labels = label_number(suffix = " pp"),
                       expand = expansion(mult = c(0.06, 0.30))) +
    labs(title = title,
         subtitle = paste0(outlbl, " | LPM with province fixed effects | coefficients in pp"),
         x = "Association with outcome (pp)", y = NULL,
         caption = "NLSS IV 2022/23. Reference: Brahmin/Chhetri, urban, non-poor. *p<0.10 **p<0.05 ***p<0.01") +
    th_policy() + theme(panel.grid.major.y = element_blank())
  sv(fig, fname, w = 9, h = 5.2)
}
mk_r1(reg_results_v2$m1$full, "Enrolment, girls 15-24", "r1a_coef_enrolment",
      "What is associated with girls' enrolment")
mk_r1(reg_results_v2$m2$full, "NEET, youth 15-24", "r1b_coef_neet",
      "What is associated with youth NEET")

cat("\n=== DONE ===\n")
