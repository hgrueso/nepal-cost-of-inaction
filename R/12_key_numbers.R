# =============================================================================
# 12_key_numbers.R
# Every number quoted in the report, the slides and the two-pager, computed in
# one place from the corrected data and saved to outputs/tables/key_numbers.csv.
# Run last (after the regressions, the causal forest, 03p and 05).
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(haven); library(survey); library(sandwich) })
options(width = 200, survey.lonely.psu = "adjust")
load("outputs/cleaned_data_v2.RData")
stopifnot("ind_wt_hh" %in% names(df))            # needs 01b_fix_sample_and_weights.R
TAB <- "outputs/tables"; CENSUS_F <- 14911027; CENSUS_ALL <- 29164578; USD <- 133
BUD <- c(health = 86.24, social_security = 106, education = 203.66)
GDP <- c(Bagmati = 2074, Koshi = 903, Lumbini = 814, Madhesh = 748, Gandaki = 517, Sudurpashchim = 404, Karnali = 244)   # NPR bn, source to confirm
WAGE <- c(Bagmati = 7000, Koshi = 2500, Gandaki = 2292, Madhesh = 2000, Lumbini = 1500, Karnali = 1066, Sudurpashchim = 1042)
pvf <- function(r) ((1 + r)^2 - 1) * (1 - 1.05^(-35)) / 0.05
wm  <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else 100 * sum(x[ok] * w[ok]) / sum(w[ok]) }
K <- list(); add <- function(section, item, value) K[[length(K) + 1]] <<- tibble(section = section, item = item, value = as.numeric(value))
sec <- function(title, expr) { cat("\n=====", title, "=====\n"); tryCatch(expr, error = function(e) cat("  FAILED:", conditionMessage(e), "\n")) }
show <- function(x) print(as.data.frame(x), right = FALSE, row.names = FALSE)
y <- df_b %>% filter(!is.na(female), !is.na(ind_wt)) %>%
  mutate(sex = ifelse(female == 1, "Girls", "Boys"), age = ifelse(age_num <= 18, "15-18", "19-24"),
         school = ifelse(completed_secondary == 1, "Grade 9 or above", "Grade 8 or below"),
         status = case_when(completed_secondary == 1 ~ "Grade 9 or above", attend_any == 1 ~ "Grade 8 or below, still enrolled",
                            TRUE ~ "Grade 8 or below, out of school"))
g <- y %>% filter(female == 1)
des <- svydesign(ids = ~psu_number, strata = ~province_num, weights = ~ind_wt, data = y %>% filter(!is.na(province_num)), nest = TRUE)

sec("A. Sample and weights", {
  a <- c("Persons listed on roster" = if (exists("flow")) flow$n[1] else NA, "Household members (analysis file)" = nrow(df),
         "Sum of person weights" = sum(df$ind_wt, na.rm = TRUE), "Census 2021 population" = CENSUS_ALL,
         "Youth 15-24 (n)" = nrow(y), "Girls 15-24 (n)" = sum(y$female == 1), "Boys 15-24 (n)" = sum(y$female == 0),
         "Girls 15-18 (n)" = sum(g$age == "15-18"), "Girls 19-24 (n)" = sum(g$age == "19-24"),
         "Women 15-64 (n)" = sum(df_c$female == 1, na.rm = TRUE))
  for (i in seq_along(a)) add("A sample", names(a)[i], a[i]); show(tibble(item = names(a), value = format(round(a), big.mark = ",")))
})

sec("B. Enrolment, employment and NEET by sex (%, survey-weighted)", {
  b <- bind_rows(y %>% mutate(age = "15-24"), y) %>% group_by(age, sex) %>%
    summarise(n = n(), enrolled = wm(attend_any, ind_wt), employed = wm(employed_ilo, ind_wt), neet = wm(neet, ind_wt), .groups = "drop")
  show(b %>% mutate(across(enrolled:neet, ~ round(.x, 1))))
  for (i in seq_len(nrow(b))) for (v in c("enrolled", "employed", "neet")) add("B outcomes", paste(v, b$sex[i], b$age[i]), b[[v]][i])
  for (a in c("15-24", "15-18", "19-24")) for (v in c("attend_any", "neet")) {
    d1 <- if (a == "15-24") des else subset(des, age == a)
    m <- svyglm(as.formula(paste(v, "~ female")), design = d1); s <- summary(m)$coefficients["female", ]
    cat(sprintf("  Girls minus boys, %s, ages %s: %+.1f pp (SE %.1f, p = %.3f)\n", ifelse(v == "neet", "NEET", "enrolment"), a, 100 * s[1], 100 * s[2], s[4]))
    add("B outcomes", paste("gap girls-boys", ifelse(v == "neet", "neet", "enrolled"), a), 100 * s[1])
    add("B outcomes", paste("gap girls-boys", ifelse(v == "neet", "neet", "enrolled"), a, "p-value"), s[4])
  }
})

sec("C. NEET by province and sex (%)", {
  p <- y %>% filter(!is.na(province_lbl)) %>% group_by(province = as.character(province_lbl), sex) %>%
    summarise(n = n(), neet = wm(neet, ind_wt), .groups = "drop") %>%
    pivot_wider(names_from = sex, values_from = c(n, neet)) %>% mutate(gap = neet_Girls - neet_Boys) %>% arrange(desc(neet_Girls))
  sh <- g %>% filter(!is.na(province_lbl)) %>% group_by(province = as.character(province_lbl)) %>%
    summarise(w = sum(ind_wt), enrolled_girls = wm(attend_any, ind_wt), .groups = "drop") %>% mutate(share_of_girls = 100 * w / sum(w)) %>% select(-w)
  p <- left_join(p, sh, by = "province"); show(p %>% mutate(across(where(is.double), ~ round(.x, 1))))
  for (i in seq_len(nrow(p))) { add("C province", paste("NEET girls", p$province[i]), p$neet_Girls[i]); add("C province", paste("NEET boys", p$province[i]), p$neet_Boys[i])
    add("C province", paste("share of girls 15-24", p$province[i]), p$share_of_girls[i]) }
  readr::write_csv(p, file.path(TAB, "key_neet_by_province.csv"))
})

sec("D. Marriage (girls 15-24)", {
  m <- bind_rows(y %>% mutate(age = "15-24"), y) %>% group_by(age, sex) %>% summarise(married = wm(currently_married, ind_wt), .groups = "drop")
  show(m %>% mutate(married = round(married, 1)))
  for (i in seq_len(nrow(m))) add("D marriage", paste("married", m$sex[i], m$age[i]), m$married[i])
  mm <- bind_rows(g %>% mutate(age = "15-24"), g) %>% filter(!is.na(currently_married)) %>%
    group_by(age, married = ifelse(currently_married == 1, "Married", "Not married")) %>%
    summarise(n = n(), enrolled = wm(attend_any, ind_wt), employed = wm(employed_ilo, ind_wt), neet = wm(neet, ind_wt), .groups = "drop")
  show(mm %>% mutate(across(enrolled:neet, ~ round(.x, 1))))
  for (i in seq_len(nrow(mm))) for (v in c("enrolled", "neet")) add("D marriage", paste(v, mm$married[i], mm$age[i]), mm[[v]][i])
})

N1524 <- NA; pe <- NULL
sec("E. Schooling status of girls 15-24 and census-anchored counts", {
  fem <- df %>% filter(female == 1, !is.na(ind_wt)); share <- sum(g$ind_wt) / sum(fem$ind_wt); N1524 <<- share * CENSUS_F
  cat(sprintf("Girls 15-24 as share of females: %.2f%% | census-anchored: %s | sum of survey weights: %s\n",
              100 * share, format(round(N1524), big.mark = ","), format(round(sum(g$ind_wt)), big.mark = ",")))
  add("E schooling", "girls 15-24 share of females (%)", 100 * share); add("E schooling", "girls 15-24 census-anchored (N)", N1524)
  add("E schooling", "girls 15-24 sum of weights (N)", sum(g$ind_wt))
  e <- bind_rows(g %>% mutate(age = "15-24"), g) %>% group_by(age, status) %>% summarise(n = n(), w = sum(ind_wt), .groups = "drop") %>%
    group_by(age) %>% mutate(pct = 100 * w / sum(w)) %>% ungroup() %>% mutate(N_census = ifelse(age == "15-24", N1524 * pct / 100, NA)) %>% select(-w)
  show(e %>% mutate(pct = round(pct, 1), N_census = round(N_census)))
  for (i in seq_len(nrow(e))) { add("E schooling", paste(e$status[i], e$age[i], "(%)"), e$pct[i]); if (!is.na(e$N_census[i])) add("E schooling", paste(e$status[i], "(N)"), e$N_census[i]) }
  bsec <- wm(y$completed_secondary[y$female == 0], y$ind_wt[y$female == 0]); cat(sprintf("Boys 15-24 with grade 9 or above: %.1f%%\n", bsec))
  add("E schooling", "Grade 9 or above Boys 15-24 (%)", bsec)
  readr::write_csv(e, file.path(TAB, "key_schooling_status.csv"))
})

sec("F. NEET, employment and enrolment by sex and schooling (%)", {
  f <- y %>% group_by(age, school, sex) %>% summarise(n = n(), neet = wm(neet, ind_wt), employed = wm(employed_ilo, ind_wt), enrolled = wm(attend_any, ind_wt), .groups = "drop")
  show(f %>% mutate(across(neet:enrolled, ~ round(.x, 1))))
  fo <- y %>% filter(age == "19-24", attend_any == 0) %>% group_by(school, sex) %>% summarise(n = n(), neet = wm(neet, ind_wt), employed = wm(employed_ilo, ind_wt), .groups = "drop")
  cat("Ages 19-24, out of school only:\n"); show(fo %>% mutate(across(neet:employed, ~ round(.x, 1))))
  for (i in seq_len(nrow(f))) for (v in c("neet", "employed")) add("F by schooling", paste(v, f$sex[i], f$age[i], f$school[i]), f[[v]][i])
  for (i in seq_len(nrow(fo))) for (v in c("neet", "employed")) add("F by schooling", paste(v, fo$sex[i], "19-24 out of school", fo$school[i]), fo[[v]][i])
  readr::write_csv(f, file.path(TAB, "key_neet_by_sex_schooling.csv"))
})

sec("G. Why girls 15-24 left school (q07_08)", {
  s07 <- read_dta("data/NLSSIV_microdata/data/stata format/S07.dta"); names(s07) <- tolower(names(s07))
  KEY <- c("psu_number", "hh_number", "idcode"); kid <- function(d) do.call(paste, c(lapply(d[KEY], function(x) as.character(haven::zap_labels(x))), sep = "_"))
  r <- g %>% filter(attend_any == 0); r$reason <- as.character(as_factor(s07$q07_08))[match(kid(r), kid(s07))]; r <- r %>% filter(!is.na(reason))
  t <- bind_rows(r %>% mutate(age = "15-24"), r) %>% group_by(age, reason) %>% summarise(n = n(), w = sum(ind_wt), .groups = "drop") %>%
    group_by(age) %>% mutate(pct = 100 * w / sum(w)) %>% ungroup() %>% select(-w) %>% arrange(age, desc(pct))
  cat("Girls 15-24 out of school with a stated reason: n =", nrow(r), "\n"); show(t %>% filter(age == "15-24") %>% mutate(pct = round(pct, 1)))
  for (i in which(t$age == "15-24")) add("G reasons", t$reason[i], t$pct[i]); add("G reasons", "n with a stated reason", nrow(r))
  readr::write_csv(t, file.path(TAB, "key_reasons_left_school.csv"))
})

sec("H. Linear probability models (pp, survey-weighted, province fixed effects, HC1)", {
  lp <- function(fml, d, label) { d <- d %>% mutate(caste_broad = droplevels(factor(caste_broad)))
    m <- lm(fml, data = d, weights = ind_wt); V <- sandwich::vcovHC(m, type = "HC1"); b <- coef(m); b <- b[!is.na(b)]; s <- sqrt(diag(V))[names(b)]
    keep <- !grepl("province|Intercept", names(b)); b <- b[keep]; s <- s[keep]
    tibble(model = label, term = names(b), pp = 100 * unname(b), se = 100 * unname(s), p = 2 * pnorm(-abs(unname(b / s))), n = nobs(m)) }
  fe <- attend_any ~ rural + ppoor + quintile_n + any_difficulty + caste_broad + currently_married + factor(province_lbl)
  fn <- neet ~ female + rural + ppoor + quintile_n + any_difficulty + caste_broad + currently_married + factor(province_lbl)
  fg <- update(fn, . ~ . - female)
  h <- bind_rows(lp(fe, g, "Enrolment, girls 15-24"), lp(fe, g %>% filter(age == "15-18"), "Enrolment, girls 15-18"), lp(fe, g %>% filter(age == "19-24"), "Enrolment, girls 19-24"),
                 lp(fn, y, "NEET, youth 15-24"), lp(fg, g, "NEET, girls 15-24"), lp(fg, g %>% filter(age == "15-18"), "NEET, girls 15-18"),
                 lp(fg, g %>% filter(age == "19-24"), "NEET, girls 19-24"), lp(fg, y %>% filter(female == 0), "NEET, boys 15-24"))
  show(h %>% mutate(across(c(pp, se), ~ round(.x, 1)), p = round(p, 3)))
  for (i in seq_len(nrow(h))) add("H regressions", paste(h$model[i], "|", h$term[i]), h$pp[i])
  readr::write_csv(h, file.path(TAB, "key_lpm_coefficients.csv"))
  mi <- lm(neet ~ completed_secondary * currently_married + rural + ppoor + quintile_n + any_difficulty + caste_broad + factor(province_lbl), data = g, weights = ind_wt)
  bi <- coef(mi); cat(sprintf("NEET difference associated with grade 9 or above: unmarried girls %+.1f pp | married girls %+.1f pp\n",
                              100 * bi["completed_secondary"], 100 * (bi["completed_secondary"] + bi["completed_secondary:currently_married"])))
  add("H regressions", "grade 9+ on NEET, unmarried girls", 100 * bi["completed_secondary"])
  add("H regressions", "grade 9+ on NEET, married girls", 100 * (bi["completed_secondary"] + bi["completed_secondary:currently_married"]))
})

sec("I. Disability (girls 15-24)", {
  d <- g %>% filter(!is.na(any_difficulty)) %>% group_by(disability = any_difficulty) %>%
    summarise(n = n(), w = sum(ind_wt), enrolled = wm(attend_any, ind_wt), neet = wm(neet, ind_wt), .groups = "drop") %>% mutate(share = 100 * w / sum(w)) %>% select(-w)
  show(d %>% mutate(across(enrolled:share, ~ round(.x, 1))))
  for (i in seq_len(nrow(d))) for (v in c("share", "enrolled", "neet")) add("I disability", paste(v, ifelse(d$disability[i] == 1, "with disability", "without disability")), d[[v]][i])
})

cate <- NULL
sec("J. Causal forest (women 15-64): effect of grade 9 or above on NEET", {
  load("outputs/causal_forest_results.RData"); a <- cf_results$ate_neet
  a <- unname(a); cat(sprintf("Average effect: %.1f pp (SE %.1f; 95%% interval %.1f to %.1f)\n", 100 * a[1], 100 * a[2], 100 * (a[1] - 1.96 * a[2]), 100 * (a[1] + 1.96 * a[2])))
  add("J forest", "average effect (pp)", 100 * a[1]); add("J forest", "average effect SE (pp)", 100 * a[2])
  cc <- cf_results$cate_neet_all %>% transmute(dimension, group = as.character(group), n, effect_pp = 100 * mean_cate, lo = 100 * ci_lo, hi = 100 * ci_hi)
  show(cc %>% mutate(across(effect_pp:hi, ~ round(.x, 1))))
  for (i in seq_len(nrow(cc))) add("J forest", paste(cc$dimension[i], cc$group[i]), cc$effect_pp[i])
  cate <<- cf_results$cate_neet_all %>% filter(dimension == "Province") %>% transmute(province = as.character(group), cate_abs = abs(mean_cate))
  assign("ate", a, envir = globalenv())
})

sec("K. Cost of inaction", {
  stopifnot(!is.null(cate), !is.na(N1524))
  build <- function(oos_only, r = 0.09) g %>% filter(!is.na(province_lbl)) %>%
    mutate(excl = as.integer(completed_secondary == 0 & (!oos_only | attend_any == 0))) %>%
    group_by(province = as.character(province_lbl)) %>% summarise(tw = sum(ind_wt), pe = sum(ind_wt * excl) / sum(ind_wt), .groups = "drop") %>%
    mutate(girls = N1524 * tw / sum(tw), n_excl = girls * pe, wage = WAGE[province], gdp = GDP[province]) %>% left_join(cate, by = "province") %>%
    mutate(coi_bn = cate_abs * n_excl * wage * 12 * pvf(r) / 1e9, share = 100 * coi_bn / sum(coi_bn), pct_gdp = 100 * coi_bn / gdp) %>% select(-tw)
  k <- build(FALSE); ko <- build(TRUE); k12 <- build(FALSE, 0.12)
  cat("All girls 15-24 at grade 8 or below (r = 9%):\n"); show(k %>% arrange(desc(coi_bn)) %>% mutate(across(c(girls, n_excl), round), across(c(pe, cate_abs, coi_bn, share, pct_gdp), ~ round(.x, 3))))
  tot <- sum(k$coi_bn); toto <- sum(ko$coi_bn); nn <- sum(k$n_excl); no <- sum(ko$n_excl)
  out <- c("Girls at grade 8 or below (N)" = nn, "  of whom out of school (N)" = no, "CoI, NPR bn (r = 9%)" = tot, "CoI, USD million" = tot * 1e3 / USD,
           "CoI % of health budget" = 100 * tot / BUD[["health"]], "CoI % of social security allowances" = 100 * tot / BUD[["social_security"]],
           "CoI % of education budget" = 100 * tot / BUD[["education"]], "CoI per girl (NPR)" = tot * 1e9 / nn, "CoI per girl (USD)" = tot * 1e9 / USD / nn,
           "CoI, NPR bn (r = 12%)" = sum(k12$coi_bn), "CoI out of school only, NPR bn" = toto, "CoI out of school only, USD million" = toto * 1e3 / USD,
           "CoI out of school only % of health budget" = 100 * toto / BUD[["health"]])
  if (exists("ate")) { a1 <- abs(unname(ate[1])); a2 <- unname(ate[2])
    out <- c(out, "CoI with the forest effect at its lower 95% bound, NPR bn" = tot * (a1 - 1.96 * a2) / a1,
             "CoI with the forest effect at its upper 95% bound, NPR bn" = tot * (a1 + 1.96 * a2) / a1) }
  # the total above is a lifetime loss for the whole stock of girls now aged 15-24 (ten yearly cohorts), not a yearly figure
  p24 <- g %>% filter(age == "19-24", !is.na(province_lbl)) %>% group_by(province = as.character(province_lbl)) %>%
    summarise(pe24 = sum(ind_wt * (completed_secondary == 0)) / sum(ind_wt), .groups = "drop")
  ky <- k %>% left_join(p24, by = "province") %>% mutate(n_year = girls / 10 * pe24, coi_year = cate_abs * n_year * wage * 12 * pvf(0.09) / 1e9)
  out <- c(out, "One yearly cohort: girls ending at grade 8 or below (N, attainment of 19-24-year-olds)" = sum(ky$n_year),
           "CoI per yearly cohort, NPR bn" = sum(ky$coi_year), "CoI per yearly cohort, USD million" = sum(ky$coi_year) * 1e3 / USD,
           "CoI per yearly cohort % of health budget" = 100 * sum(ky$coi_year) / BUD[["health"]],
           "One tenth of the 15-24 total, NPR bn" = tot / 10, "Forgone earnings per year for today's 15-24 stock, NPR bn" = tot / ((1 - 1.05^(-35)) / 0.05))
  show(tibble(item = names(out), value = round(out, 2)))
  for (i in seq_along(out)) add("K cost", trimws(names(out)[i]), out[i])
  for (i in seq_len(nrow(k))) { add("K cost", paste("CoI NPR bn", k$province[i]), k$coi_bn[i]); add("K cost", paste("CoI share %", k$province[i]), k$share[i])
    add("K cost", paste("CoI % of provincial GDP", k$province[i]), k$pct_gdp[i]); add("K cost", paste("girls at grade 8 or below (N)", k$province[i]), k$n_excl[i]) }
  res <- tryCatch(readRDS("outputs/census_anchor_results.RData"), error = function(e) NULL)
  if (!is.null(res)) cat(sprintf("Check against 05_census_anchor_coi.R: %.2f bn there, %.2f bn here\n", res$coi_b_r09, tot))
  readr::write_csv(k, file.path(TAB, "key_coi_by_province.csv")); readr::write_csv(ko, file.path(TAB, "key_coi_by_province_out_of_school.csv"))
})

sec("L. Returns to schooling (from 03p_mincer_main.R)", {
  r <- readr::read_csv(file.path(TAB, "mincer_main_returns.csv"), show_col_types = FALSE); show(r %>% mutate(across(where(is.double), ~ round(.x, 1))))
  for (i in seq_len(nrow(r))) for (v in c("avg_return", "at_primary_5", "at_lower_sec_8", "at_secondary_10", "at_higher_sec_12", "n")) add("L returns", paste(r$specification[i], "|", r$sex[i], "|", v), r[[v]][i])
  l <- readr::read_csv(file.path(TAB, "mincer_by_level.csv"), show_col_types = FALSE); show(l %>% mutate(across(where(is.double), ~ round(.x, 1))))
  for (i in seq_len(nrow(l))) add("L returns", paste("monthly earnings", l$sex[i], l$level[i]), l$monthly_earnings[i])
  h <- readRDS("outputs/heckman_results.RData"); cat(sprintf("Selection: rho = %.2f, inverse Mills ratio p = %.3f\n", h$rho, h$imr_pval))
  add("L returns", "heckman rho", h$rho); add("L returns", "heckman IMR p-value", h$imr_pval)
})

key <- bind_rows(K); readr::write_csv(key, file.path(TAB, "key_numbers.csv"))
cat("\nSaved", nrow(key), "numbers to outputs/tables/key_numbers.csv\n=== DONE ===\n")
