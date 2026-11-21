# 11_sample_and_weight_check.R
# Two checks on how the analysis sample was built, and what they do to the results:
#   (1) is ind_wt a household-level weight (household weight x household size)?
#   (2) does the roster include people who are not household members
#       (no education or employment data collected for them)?
# Every headline number is then recomputed three ways: as published, household
# members only, and household members with the person-level weight (hhs_wt).
suppressPackageStartupMessages({ library(tidyverse); library(haven) })
options(width = 220)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")
TAB <- "outputs/tables"; RAW <- "data/NLSSIV_microdata/data/stata format"
CENSUS_F <- 14911027; CENSUS_ALL <- 29164578
KEY <- c("psu_number", "hh_number", "idcode")
lab <- function(x) toupper(trimws(as.character(haven::as_factor(x))))
chr_key <- function(d) d %>% mutate(across(all_of(KEY), ~ as.character(haven::zap_labels(.x))))
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else 100 * sum(x[ok] * w[ok]) / sum(w[ok]) }
big <- function(x) format(round(x), big.mark = ",")

# ---- bring in the raw membership and schooling-status questions --------------
s01 <- read_dta(file.path(RAW, "S01.dta")); names(s01) <- tolower(names(s01))
s07 <- read_dta(file.path(RAW, "S07.dta")); names(s07) <- tolower(names(s07))
add01 <- chr_key(s01) %>% distinct(across(all_of(KEY)), .keep_all = TRUE) %>%
  transmute(across(all_of(KEY)), q0109 = lab(q01_09),
            memcat = if ("mem_cat" %in% names(s01)) lab(mem_cat) else NA_character_)
add07 <- chr_key(s07) %>% distinct(across(all_of(KEY)), .keep_all = TRUE) %>%
  transmute(across(all_of(KEY)), in_s07 = 1L, q0704 = lab(q07_04))
if (!all(c("hhs_wt", "hhsize") %in% names(df))) {   # household weight not kept in the cleaned file: take it from the poverty file
  pov <- read_dta(file.path(RAW, "poverty.dta")); names(pov) <- tolower(names(pov))
  hk <- c("psu_number", "hh_number")
  pov <- pov %>% mutate(across(all_of(hk), ~ as.character(haven::zap_labels(.x)))) %>% distinct(across(all_of(hk)), .keep_all = TRUE) %>%
    select(all_of(hk), any_of(c("hhs_wt", "hhsize")))
  df <- df %>% select(-any_of(c("hhs_wt", "hhsize"))) %>% mutate(across(all_of(hk), ~ as.character(haven::zap_labels(.x)))) %>% left_join(pov, by = hk)
  df_wage_w <- df_wage_w %>% select(-any_of(c("hhs_wt", "hhsize"))) %>% mutate(across(all_of(hk), ~ as.character(haven::zap_labels(.x)))) %>% left_join(pov, by = hk)
}
d <- chr_key(df) %>% left_join(add01, by = KEY) %>% left_join(add07, by = KEY) %>%
  mutate(in_s07 = coalesce(in_s07, 0L), has_level = !is.na(edu_cat), has_emp = !is.na(employed_ilo))

cat("\n================ A1. What is ind_wt? ================\n")
cat("ind_wt / (hhs_wt x hhsize), all rows:\n"); print(summary(d$ind_wt / (d$hhs_wt * d$hhsize)))
hh <- d %>% distinct(psu_number, hh_number, .keep_all = TRUE)
cat(sprintf("Households in sample: %s | roster rows: %s | rows per household: %.2f | mean hhsize variable: %.2f\n",
            big(nrow(hh)), big(nrow(d)), nrow(d) / nrow(hh), mean(hh$hhsize, na.rm = TRUE)))
cat(sprintf("Sum over HOUSEHOLDS: hhs_wt = %s (households in Nepal, census 2021: 6,660,841) | ind_wt = %s (census population: %s)\n",
            big(sum(hh$hhs_wt, na.rm = TRUE)), big(sum(hh$ind_wt, na.rm = TRUE)), big(CENSUS_ALL)))
cat(sprintf("Sum over PERSONS:    ind_wt = %s | hhs_wt = %s\n", big(sum(d$ind_wt, na.rm = TRUE)), big(sum(d$hhs_wt, na.rm = TRUE))))

cat("\n================ A2. Who is on the roster? ================\n")
cat("q01_09 'Is household member?':\n"); print(table(d$q0109, useNA = "ifany"))
cat("mem_cat 'Household/Absentee member category':\n"); print(table(d$memcat, useNA = "ifany"))
is_yes <- d$q0109 %in% c("YES", "Y", "1")
if (mean(is_yes) >= 0.6) { d$member <- is_yes; cat("Member definition used: q01_09 = YES\n")
} else { d$member <- d$has_emp | d$in_s07 == 1L | d$age_num < 10
  cat("q01_09 labels not recognised; member definition used: has education or employment record\n") }
cat(sprintf("Members: %s of %s roster rows (%.1f%%) | sum of hhs_wt over members = %s\n",
            big(sum(d$member)), big(nrow(d)), 100 * mean(d$member), big(sum(d$hhs_wt[d$member], na.rm = TRUE))))
cat("Roster rows with / without education-module record, by membership:\n")
print(table(member = d$member, in_education_module = d$in_s07))

cat("\n================ A3. Girls 15-24: who has no education level recorded? ================\n")
g <- d %>% filter(female == 1, age_num >= 15, age_num <= 24)
print(as.data.frame(g %>% count(member, has_level, has_emp, enrolled = attend_any)), right = FALSE)
cat("Schooling status question (q07_04) by whether a level is recorded:\n")
print(table(q07_04 = g$q0704, has_level = g$has_level, useNA = "ifany"))
m1924 <- d %>% filter(female == 0, age_num >= 19, age_num <= 24)
cat(sprintf("Men 19-24: %s on roster, of which members: %s (%.0f%%)\n", big(nrow(m1924)), big(sum(m1924$member)), 100 * mean(m1924$member)))

# ---- headline numbers three ways ---------------------------------------------
stats <- function(D, wcol) {
  D$w <- D[[wcol]]
  fem <- D %>% filter(female == 1, !is.na(w))
  y <- D %>% filter(age_num >= 15, age_num <= 24, !is.na(w), !is.na(female))
  gi <- y %>% filter(female == 1); bo <- y %>% filter(female == 0)
  g1 <- gi %>% filter(age_num <= 18); g2 <- gi %>% filter(age_num >= 19)
  mar <- function(x, m) x %>% filter(currently_married == m)
  N <- sum(gi$w) / sum(fem$w) * CENSUS_F
  le8 <- as.integer(gi$completed_secondary == 0); oos <- as.integer(gi$completed_secondary == 0 & gi$attend_any == 0)
  c("Sample: girls 15-24 (n)" = nrow(gi), "Sample: boys 15-24 (n)" = nrow(bo),
    "Girls enrolled (%)" = wm(gi$attend_any, gi$w), "Boys enrolled (%)" = wm(bo$attend_any, bo$w),
    "Girls NEET (%)" = wm(gi$neet, gi$w), "Boys NEET (%)" = wm(bo$neet, bo$w),
    "NEET gap 15-18 (pp)" = wm(g1$neet, g1$w) - wm(bo$neet[bo$age_num <= 18], bo$w[bo$age_num <= 18]),
    "NEET gap 19-24 (pp)" = wm(g2$neet, g2$w) - wm(bo$neet[bo$age_num >= 19], bo$w[bo$age_num >= 19]),
    "Girls NEET, Madhesh (%)" = wm(gi$neet[gi$province_lbl == "Madhesh"], gi$w[gi$province_lbl == "Madhesh"]),
    "Girls NEET, Lumbini (%)" = wm(gi$neet[gi$province_lbl == "Lumbini"], gi$w[gi$province_lbl == "Lumbini"]),
    "Married, girls 15-18 (%)" = wm(g1$currently_married, g1$w), "Married, women 19-24 (%)" = wm(g2$currently_married, g2$w),
    "Enrolled: married 15-18 (%)" = wm(mar(g1, 1)$attend_any, mar(g1, 1)$w), "Enrolled: unmarried 15-18 (%)" = wm(mar(g1, 0)$attend_any, mar(g1, 0)$w),
    "NEET: married 15-18 (%)" = wm(mar(g1, 1)$neet, mar(g1, 1)$w), "NEET: unmarried 15-18 (%)" = wm(mar(g1, 0)$neet, mar(g1, 0)$w),
    "Enrolled: married 19-24 (%)" = wm(mar(g2, 1)$attend_any, mar(g2, 1)$w), "NEET: married 19-24 (%)" = wm(mar(g2, 1)$neet, mar(g2, 1)$w),
    "Girls with grade 9 or above (%)" = wm(gi$completed_secondary, gi$w),
    "Girls grade 8 or below (%)" = wm(le8, gi$w), "  of which out of school (%)" = wm(oos, gi$w),
    "Girls 15-24 as share of females (%)" = 100 * sum(gi$w) / sum(fem$w),
    "Girls 15-24, census-anchored (N)" = N, "Girls 15-24, sum of weights" = sum(gi$w),
    "Grade 8 or below (N)" = N * wm(le8, gi$w) / 100, "  of which out of school (N)" = N * wm(oos, gi$w) / 100)
}
lpm <- function(D, wcol) {
  D$w <- D[[wcol]]; y <- D %>% filter(age_num >= 15, age_num <= 24)
  m1 <- lm(attend_any ~ rural + ppoor + quintile_n + any_difficulty + caste_broad + currently_married + factor(province_lbl),
           data = y %>% filter(female == 1), weights = w)
  m2 <- lm(neet ~ female + rural + ppoor + quintile_n + any_difficulty + caste_broad + currently_married + factor(province_lbl),
           data = y, weights = w)
  c("LPM enrolment: married (pp)" = 100 * unname(coef(m1)["currently_married"]), "LPM enrolment: disability (pp)" = 100 * unname(coef(m1)["any_difficulty"]),
    "LPM enrolment: n" = nobs(m1), "LPM NEET: married (pp)" = 100 * unname(coef(m2)["currently_married"]),
    "LPM NEET: female (pp)" = 100 * unname(coef(m2)["female"]), "LPM NEET: disability (pp)" = 100 * unname(coef(m2)["any_difficulty"]), "LPM NEET: n" = nobs(m2))
}
pvf <- function(r) ((1 + r)^2 - 1) * (1 - 1.05^(-35)) / 0.05
prov_wages <- tribble(~province_lbl, ~base_wage, "Bagmati", 7000, "Koshi", 2500, "Gandaki", 2292, "Madhesh", 2000,
                      "Lumbini", 1500, "Karnali", 1066, "Sudurpashchim", 1042)
cate_old <- cf_results$cate_neet_all %>% filter(dimension == "Province") %>%
  transmute(province_lbl = as.character(group), cate_abs = abs(mean_cate))
coi <- function(D, wcol, oos_only = FALSE, cate = cate_old, r = 0.09) {
  D$w <- D[[wcol]]
  fem <- D %>% filter(female == 1, !is.na(w)); gi <- fem %>% filter(age_num >= 15, age_num <= 24)
  N <- sum(gi$w) / sum(fem$w) * CENSUS_F
  p <- gi %>% filter(!is.na(province_lbl)) %>%
    mutate(excl = as.integer(completed_secondary == 0 & (!oos_only | attend_any == 0))) %>%
    group_by(province_lbl) %>% summarise(tw = sum(w), pe = sum(w * excl) / sum(w), .groups = "drop") %>%
    mutate(province_lbl = as.character(province_lbl), n_excl = N * tw / sum(tw) * pe) %>%
    left_join(prov_wages, by = "province_lbl") %>% left_join(cate, by = "province_lbl") %>%
    mutate(coi_b = cate_abs * n_excl * base_wage * 12 * pvf(r) / 1e9)
  c(N = sum(p$n_excl), coi = sum(p$coi_b, na.rm = TRUE), setNames(p$coi_b, p$province_lbl))
}
coi_rows <- function(D, wcol, cate = cate_old) {
  a <- coi(D, wcol, FALSE, cate); b <- coi(D, wcol, TRUE, cate)
  c("CoI, all at grade 8 or below (NPR bn)" = unname(a["coi"]), "  = USD million" = unname(a["coi"]) * 1e9 / 133 / 1e6,
    "  as % health budget" = unname(a["coi"]) / 86.24 * 100, "  as % education budget" = unname(a["coi"]) / 203.66 * 100,
    "  at boys' 12% premium (NPR bn)" = unname(coi(D, wcol, FALSE, cate, 0.12)["coi"]),
    "CoI, only those out of school (NPR bn)" = unname(b["coi"]), "  = USD million " = unname(b["coi"]) * 1e9 / 133 / 1e6,
    "Break-even per girl (USD)" = unname(a["coi"]) * 1e9 / 133 / unname(a["N"]),
    "Madhesh share of CoI (%)" = 100 * unname(a["Madhesh"]) / unname(a["coi"]))
}
M <- d %>% filter(member)
cols <- list("As published" = list(d, "ind_wt"), "Members only" = list(M, "ind_wt"), "Members + person weight" = list(M, "hhs_wt"))
res <- sapply(cols, function(a) c(stats(a[[1]], a[[2]]), lpm(a[[1]], a[[2]]), coi_rows(a[[1]], a[[2]])))
cat("\n================ B. Headline numbers three ways ================\n")
out <- as.data.frame(res) %>% rownames_to_column("Indicator") %>%
  mutate(across(-Indicator, ~ ifelse(abs(.x) >= 1000, format(round(.x), big.mark = ","), format(round(.x, 1), nsmall = 1))))
print(out, right = FALSE, row.names = FALSE)
readr::write_csv(as.data.frame(res) %>% rownames_to_column("Indicator"), file.path(TAB, "check_members_weights.csv"))
cat("\nProvincial CoI (NPR bn), members + person weight, all at grade 8 or below:\n")
print(round(coi(M, "hhs_wt")[-(1:2)], 2))

cat("\n================ C. Returns to schooling (women in wage work) ================\n")
tryCatch({
  ww <- chr_key(df_wage_w) %>% mutate(edu_yrs = case_when(as.character(edu_cat) == "No education" ~ 0, as.character(edu_cat) == "Primary" ~ 5,
        as.character(edu_cat) == "Lower secondary" ~ 8, as.character(edu_cat) == "Secondary" ~ 10,
        as.character(edu_cat) == "Higher secondary" ~ 12, as.character(edu_cat) == "Tertiary" ~ 16, TRUE ~ NA_real_),
      experience = pmax(age_num - edu_yrs - 6, 0, na.rm = TRUE), experience2 = experience^2,
      edu_c = edu_yrs - mean(edu_yrs, na.rm = TRUE))
  for (wc in c("ind_wt", "hhs_wt")) {
    ww$w <- ww[[wc]]
    m <- lm(log_wage ~ edu_c + I(edu_c^2) + experience + experience2 + rural + caste_broad + factor(province_lbl), data = ww, weights = w)
    cat(sprintf("  weight %-7s: return at mean schooling = %.1f%% per year | curvature = %.3f | n = %d\n",
                wc, 100 * coef(m)["edu_c"], coef(m)["I(edu_c^2)"], nobs(m)))
  }
}, error = function(e) cat("  could not re-estimate:", conditionMessage(e), "\n"))

cat("\n================ D. Causal forest with the person weight (takes a few minutes) ================\n")
tryCatch({
  suppressPackageStartupMessages(library(grf))
  cf <- M %>% filter(female == 1, age_num >= 15, age_num <= 64) %>%
    mutate(caste_num = as.integer(caste_broad), province_n = as.integer(province_lbl), age_sq = age_num^2) %>%
    filter(!is.na(completed_secondary), !is.na(employed_ilo), !is.na(neet), !is.na(rural), !is.na(ppoor), !is.na(quintile_n),
           !is.na(caste_num), !is.na(province_n), !is.na(any_difficulty), !is.na(hhs_wt))
  Xv <- c("age_num", "age_sq", "rural", "ppoor", "quintile_n", "caste_num", "province_n", "any_difficulty", "currently_married")
  set.seed(42)
  f <- causal_forest(X = as.matrix(cf[, Xv]), Y = cf$neet, W = cf$completed_secondary, sample.weights = cf$hhs_wt,
                     num.trees = 2000, tune.parameters = "all", seed = 42)
  ate <- average_treatment_effect(f, target.sample = "all")
  cat(sprintf("  n = %s | effect of grade 9+ on NEET = %.1f pp (SE %.1f) | published: -13.2 pp\n", big(nrow(cf)), 100 * ate[1], 100 * ate[2]))
  cf$tau <- predict(f)$predictions
  cate_new <- cf %>% group_by(province_lbl) %>% summarise(mean_cate = mean(tau), .groups = "drop") %>%
    transmute(province_lbl = as.character(province_lbl), cate_abs = abs(mean_cate))
  cat("  Effect by province (pp), new vs published:\n")
  print(as.data.frame(cate_new %>% left_join(cate_old %>% rename(published = cate_abs), by = "province_lbl") %>%
                        mutate(new = round(-100 * cate_abs, 1), published = round(-100 * published, 1)) %>% select(province_lbl, new, published)), right = FALSE)
  r2 <- coi_rows(M, "hhs_wt", cate_new)
  cat(sprintf("  CoI with the new forest: %.1f bn (all at grade 8 or below) | %.1f bn (out of school only)\n", r2[1], r2[6]))
  saveRDS(list(ate = ate, cate_prov = cate_new), "outputs/check_forest_person_weight.rds")
}, error = function(e) cat("  forest not re-run:", conditionMessage(e), "\n"))
cat("\n=== DONE ===\n")
