# =============================================================================
# 03p_mincer_main.R
# Returns to schooling. One specification, used everywhere in the report and
# slides (text, lifetime-earnings figure, annex tables, selection test).
#   Sample     wage earners aged 15-64 (household members), women and men separately
#   Outcome    log monthly wage earnings
#   Schooling  years: 0 never attended / no level, 5 primary, 8 lower secondary,
#              10 secondary, 12 higher secondary, 16 tertiary
#   Model      ln(w) = a + b1*S + b2*S^2 + experience + experience^2 + rural + caste + province,
#              survey-weighted, HC1 standard errors
#   Selection  Heckman (1979) two-step on the same equation for women; exclusion
#              restrictions: children under 5, other adult women, household size, wealth quintile
# Replaces 03n_heckman_mincer.R and the wage section of 04_slides_figures.R.
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(scales); library(sandwich); library(sampleSelection) })
load("outputs/cleaned_data_v2.RData")
stopifnot("ind_wt_hh" %in% names(df), "edu_yrs" %in% names(df))   # needs 01b_fix_sample_and_weights.R
FIGS <- "outputs/figures"; TABLES <- "outputs/tables"
dir.create(FIGS, showWarnings = FALSE, recursive = TRUE); dir.create(TABLES, showWarnings = FALSE, recursive = TRUE)
ub <- "#1CABE2"; ud <- "#00377C"; ug <- "#00833D"; ur <- "#E2231A"; ugr <- "#6C7279"
th_policy <- function(b = 12) theme_minimal(base_size = b) + theme(
  plot.title = element_text(face = "bold", size = b + 2, colour = "#222222"), plot.subtitle = element_text(size = b, colour = ugr),
  plot.caption = element_text(size = b - 3, colour = ugr, hjust = 0), panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(colour = "#EBEBEB", linewidth = 0.4), panel.grid.major.x = element_blank(), legend.position = "bottom")
sv <- function(p, n, w = 10, h = 5.5) { ggsave(file.path(FIGS, paste0(n, ".png")), p, width = w, height = h, dpi = 300, bg = "white"); cat("  saved", n, "\n") }
C0 <- 8                                   # schooling is centred at 8 years (end of basic education)
LV <- c("No schooling" = 0, "Primary" = 5, "Lower secondary" = 8, "Secondary" = 10, "Higher secondary" = 12, "Tertiary" = 16)
ANN <- (1 - 1.05^(-35)) / 0.05            # 35 working years, 5% discount rate

# ---- estimation file ----------------------------------------------------------
hh_comp <- df %>% group_by(psu_number, hh_number) %>%
  summarise(n_children_u4 = sum(age_num < 4, na.rm = TRUE), n_children_u5 = sum(age_num < 5, na.rm = TRUE),
            n_children_u6 = sum(age_num < 6, na.rm = TRUE),
            n_women_1564 = sum(female == 1 & age_num >= 15 & age_num <= 64, na.rm = TRUE), .groups = "drop")
d <- df %>% left_join(hh_comp, by = c("psu_number", "hh_number")) %>%
  filter(age_num >= 15, age_num <= 64, !is.na(female), !is.na(edu_yrs), !is.na(rural), !is.na(caste_broad), !is.na(province_num),
         !is.na(quintile_n), !is.na(currently_married), !is.na(hhsize), !is.na(ind_wt)) %>%
  mutate(sc = edu_yrs - C0, sc2 = sc^2, experience = pmax(age_num - edu_yrs - 6, 0), experience2 = experience^2,
         wage_obs = as.integer(!is.na(monthly_wage) & monthly_wage > 0), lwage = ifelse(wage_obs == 1, log(monthly_wage), NA_real_),
         n_other_women = pmax(n_women_1564 - as.integer(female == 1), 0),
         level = factor(names(LV)[match(edu_yrs, LV)], names(LV)), prov = factor(province_num),
         salaried = if ("cond" %in% names(df)) as.integer(cond %in% "Long term salary") else NA_integer_,
         attended = as.integer(!is.na(edu_cat)))
cat(sprintf("Women 15-64: %d, with wage earnings %d (%.1f%%) | Men 15-64: %d, with wage earnings %d (%.1f%%)\n",
            sum(d$female == 1), sum(d$female == 1 & d$wage_obs == 1), 100 * mean(d$wage_obs[d$female == 1]),
            sum(d$female == 0), sum(d$female == 0 & d$wage_obs == 1), 100 * mean(d$wage_obs[d$female == 0])))

f_out <- lwage ~ sc + sc2 + experience + experience2 + rural + caste_broad + prov
fit <- function(dat, weighted = TRUE) {
  dd <- dat %>% filter(wage_obs == 1) %>% mutate(caste_broad = droplevels(factor(caste_broad)), prov = droplevels(prov))
  dd$w_ <- if (weighted) dd$ind_wt else 1
  m <- lm(f_out, data = dd, weights = w_)
  list(b = coef(m), V = sandwich::vcovHC(m, type = "HC1"), S = dd$edu_yrs, w = dd$w_, n = nobs(m), r2 = summary(m)$r.squared, m = m)
}
# marginal return at s years: b1 + 2*b2*(s - C0), with delta-method standard error
mr <- function(b, V, s) { k <- 2 * (s - C0)
  c(est = unname(b["sc"] + k * b["sc2"]), se = unname(sqrt(V["sc", "sc"] + k^2 * V["sc2", "sc2"] + 2 * k * V["sc", "sc2"]))) * 100 }
row_ret <- function(label, sex, b, V, S, w, n) { sbar <- weighted.mean(S, w); a <- mr(b, V, sbar)
  tibble(specification = label, sex = sex, n = n, mean_years = round(sbar, 2), avg_return = a[["est"]], avg_return_se = a[["se"]],
         at_primary_5 = mr(b, V, 5)[["est"]], at_lower_sec_8 = mr(b, V, 8)[["est"]], at_secondary_10 = mr(b, V, 10)[["est"]],
         at_higher_sec_12 = mr(b, V, 12)[["est"]], at_tertiary_16 = mr(b, V, 16)[["est"]]) }
run <- function(label, dat, weighted = TRUE) bind_rows(lapply(c(1, 0), function(fx) {
  dd <- dat %>% filter(female == fx); if (sum(dd$wage_obs) < 100) return(NULL)
  f <- fit(dd, weighted); row_ret(label, ifelse(fx == 1, "Women", "Men"), f$b, f$V, f$S, f$w, f$n) }))

cat("\n=== Returns to schooling (% per year) ===\n")
returns <- bind_rows(
  run("Main: all wage earners, survey-weighted", d),
  run("All wage earners, unweighted", d, weighted = FALSE),
  run("Excluding those who never attended school", d %>% filter(attended == 1)),
  if (!all(is.na(d$salaried))) run("Long-term salaried workers only", d %>% filter(wage_obs == 0 | salaried == 1)))

# ---- selection test (women): Heckman two-step on the same equation ------------
dw <- d %>% filter(female == 1) %>% mutate(caste_broad = droplevels(factor(caste_broad)))
ols_u <- fit(dw, weighted = FALSE); ols_w <- fit(dw, weighted = TRUE)
heck <- function(child) tryCatch({
  sel_f <- as.formula(paste("wage_obs ~ sc + sc2 + experience + experience2 + currently_married + rural + caste_broad + prov + quintile_n +",
                            child, "+ n_other_women + hhsize"))
  hk <- selection(sel_f, f_out, data = dw, method = "2step")
  est <- summary(hk)$estimate; b <- coef(hk, part = "outcome"); V <- vcov(hk, part = "outcome")
  list(hk = hk, b = b, V = V[names(b), names(b)], rho = est["rho", "Estimate"], imr = est["invMillsRatio", "Estimate"], imr_p = est["invMillsRatio", 4],
       excl = est[c(child, "n_other_women", "hhsize", "quintile_n"), , drop = FALSE])
}, error = function(e) { cat("Heckman (", child, ") failed:", conditionMessage(e), "\n"); NULL })
hk5 <- heck("n_children_u5"); hk4 <- heck("n_children_u4"); hk6 <- heck("n_children_u6")
hrow <- function(label, h) if (is.null(h)) NULL else row_ret(label, "Women", h$b, h$V, ols_u$S, ols_u$w, ols_u$n)
returns <- bind_rows(returns, hrow("Heckman two-step, unweighted (children under 5)", hk5),
                     hrow("Heckman two-step, unweighted (children under 4)", hk4), hrow("Heckman two-step, unweighted (children under 6)", hk6))
print(as.data.frame(returns %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), right = FALSE)
readr::write_csv(returns, file.path(TABLES, "mincer_main_returns.csv"))
if (!is.null(hk5)) {
  cat(sprintf("\nSelection: rho = %.3f | inverse Mills ratio = %.3f (p = %.4f)\n", hk5$rho, hk5$imr, hk5$imr_p))
  cat("Exclusion restrictions in the selection equation (children under 5 specification):\n"); print(round(hk5$excl, 4))
}

# ---- coefficient tables for the annex (main specification) --------------------
coef_tbl <- function(f) { se <- sqrt(diag(f$V)); b <- f$b[names(se)]; p <- 2 * pnorm(-abs(b / se))
  nm <- c("(Intercept)" = "Constant", sc = paste0("Years of schooling (centred at ", C0, ")"), sc2 = "Years of schooling squared",
          experience = "Experience (years)", experience2 = "Experience squared", rural = "Rural")
  tibble(term = names(b), Coefficient = sprintf("%.4f", b), SE = sprintf("%.4f", se),
         `95% CI` = sprintf("[%.4f, %.4f]", b - 1.96 * se, b + 1.96 * se), `p-value` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))) %>%
    filter(!grepl("^prov", term)) %>%
    mutate(Variable = ifelse(term %in% names(nm), nm[term], sub("^caste_broad", "", term))) %>% select(Variable, Coefficient, SE, `95% CI`, `p-value`) %>%
    bind_rows(tibble(Variable = c("Province fixed effects", "N", "R-squared"), Coefficient = c("Yes", as.character(f$n), sprintf("%.3f", f$r2)),
                     SE = "", `95% CI` = "", `p-value` = "")) }
f_w <- fit(d %>% filter(female == 1)); f_m <- fit(d %>% filter(female == 0))
readr::write_csv(coef_tbl(f_w), file.path(TABLES, "mincer_girls_full.csv"))
readr::write_csv(coef_tbl(f_m), file.path(TABLES, "mincer_boys_full.csv"))
cat("\nMain specification, women:\n"); print(as.data.frame(coef_tbl(f_w)), right = FALSE)
cat("\nMain specification, men:\n");   print(as.data.frame(coef_tbl(f_m)), right = FALSE)

# ---- earnings by level of schooling (level dummies, same controls) ------------
by_level <- bind_rows(lapply(c(1, 0), function(fx) {
  dd <- d %>% filter(female == fx, wage_obs == 1) %>% mutate(caste_broad = droplevels(factor(caste_broad)), prov = droplevels(prov), level = droplevels(level))
  m <- lm(lwage ~ level + experience + experience2 + rural + caste_broad + prov, data = dd, weights = ind_wt)
  bind_rows(lapply(levels(dd$level), function(L) { nd <- dd; nd$level <- factor(L, levels(dd$level))
    tibble(sex = ifelse(fx == 1, "Women", "Men"), level = L, years = LV[[L]], n = sum(dd$level == L),
           monthly_earnings = exp(weighted.mean(predict(m, newdata = nd), dd$ind_wt))) })) })) %>%
  group_by(sex) %>% arrange(years, .by_group = TRUE) %>%
  mutate(pv_lifetime = monthly_earnings * 12 * ANN,
         return_per_year_vs_previous_level = 100 * (log(monthly_earnings) - lag(log(monthly_earnings))) / (years - lag(years))) %>% ungroup()
cat("\n=== Earnings by level of schooling (wage earners with average characteristics) ===\n")
print(as.data.frame(by_level %>% mutate(across(c(monthly_earnings, pv_lifetime), round), return_per_year_vs_previous_level = round(return_per_year_vs_previous_level, 1))), right = FALSE)
readr::write_csv(by_level, file.path(TABLES, "mincer_by_level.csv"))

main_w <- returns %>% filter(grepl("^Main", specification), sex == "Women"); main_m <- returns %>% filter(grepl("^Main", specification), sex == "Men")
pl <- by_level %>% filter(n >= 15) %>% group_by(sex) %>% arrange(years, .by_group = TRUE) %>%
  mutate(rel = monthly_earnings / first(monthly_earnings)) %>% ungroup() %>% mutate(sex = factor(sex, c("Women", "Men")))
readr::write_csv(pl %>% select(sex, level, years, n, monthly_earnings, rel), file.path(TABLES, "mincer_relative_earnings.csv"))
lab_x <- scale_x_continuous(breaks = unname(LV), labels = paste0(unname(LV), "\n", c("none", "primary", "lower sec.", "secondary", "higher sec.", "tertiary")))
# M1: earnings relative to the same sex with no schooling (the return to schooling, shown for each level)
figM1 <- ggplot(pl, aes(x = years, y = rel, colour = sex)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.8) +
  geom_text(data = pl %>% filter(years >= 10), aes(label = sprintf("x%.1f", rel), vjust = ifelse(sex == "Women", -0.9, 1.9)), size = 3.6, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(Women = ub, Men = ud), name = NULL) + lab_x +
  scale_y_continuous(labels = function(v) paste0("x", v), limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Schooling raises women's earnings far more than men's",
       subtitle = "Monthly earnings at each level of schooling, relative to wage earners of the same sex with no schooling",
       x = "Years of schooling (level completed)", y = "Earnings relative to no schooling",
       caption = sprintf("Average return per year of schooling: women %.1f%%, men %.1f%% (Mincer equation, survey-weighted).\nPredicted earnings for a wage earner with average characteristics. In money terms men earn more at every level (see annex).",
                         main_w$avg_return, main_m$avg_return)) +
  th_policy()
sv(figM1, "m1_mincer_lifetime", w = 11, h = 5.5)
# M1b: the same predictions in money terms (lifetime present value)
figM1b <- ggplot(pl, aes(x = years, y = pv_lifetime / 1e5, colour = sex)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.8) +
  scale_colour_manual(values = c(Women = ub, Men = ud), name = NULL) + lab_x +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Projected lifetime earnings by level of schooling and sex",
       subtitle = "Wage earners aged 15-64 | present value over 35 working years, 5% discount rate",
       x = "Years of schooling (level completed)", y = "Lifetime earnings (NPR lakh, present value)",
       caption = "Predicted earnings at each level for a wage earner with average characteristics (Mincer equation with level indicators, survey-weighted).") +
  th_policy()
sv(figM1b, "m1b_earnings_levels", w = 11, h = 5.5)

# ---- selection figure and results ----------------------------------------------
pick <- function(pat) returns %>% filter(sex == "Women", grepl(pat, specification))
cmp <- bind_rows(pick("^Main"), pick("^All wage earners, unweighted"), pick("under 5"), pick("under 4"), pick("under 6")) %>%
  transmute(Model = c("OLS, survey-weighted (main estimate)", "OLS, unweighted", "Heckman two-step (children under 5)",
                      "Heckman two-step (children under 4)", "Heckman two-step (children under 6)")[seq_len(n())],
            `Return per year (%)` = round(avg_return, 1), `Std. error (pp)` = round(avg_return_se, 2))
cat("\n=== Selection test ===\n"); print(as.data.frame(cmp), right = FALSE)
readr::write_csv(cmp, file.path(TABLES, "heckman_comparison.csv"))
if (!is.null(hk5)) {
  shift <- cmp$`Return per year (%)`[3] - cmp$`Return per year (%)`[2]
  fd <- cmp %>% transmute(model = Model, ret = `Return per year (%)`, se = `Std. error (pp)`, lo = ret - 1.96 * se, hi = ret + 1.96 * se, ypos = rev(seq_len(n())))
  figHK <- ggplot(fd, aes(x = ret, y = ypos)) +
    geom_vline(xintercept = 9, linetype = "dotted", colour = ug) +
    annotate("text", x = 9.2, y = 0.55, label = "ILO benchmark 9%", hjust = 0, size = 3, colour = ug, fontface = "italic") +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.22, linewidth = 0.5, colour = ugr, orientation = "y") +
    geom_point(colour = ifelse(grepl("main", fd$model), ur, ud), size = ifelse(grepl("main", fd$model), 5, 3), alpha = 0.9) +
    geom_text(aes(label = paste0(ret, "%"), x = hi + 0.6), hjust = 0, size = 3.4, fontface = "bold") +
    scale_y_continuous(breaks = fd$ypos, labels = fd$model, expand = expansion(mult = c(0.15, 0.12))) +
    scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.05, 0.20))) +
    labs(title = sprintf("Selection correction changes the estimated return by %.1f percentage points", abs(shift)),
         subtitle = "Average return per year of schooling, women 15-64 | OLS and Heckman selection model",
         x = "Average return per year of schooling (%)", y = NULL,
         caption = sprintf("Heckman (1979) two-step; rho = %.2f, inverse Mills ratio p = %.3f.\nExclusion restrictions: children under 5, other adult women, household size, wealth quintile.", hk5$rho, hk5$imr_p)) +
    th_policy() + theme(panel.grid.major.y = element_blank())
  sv(figHK, "n1_heckman_returns", w = 11, h = 4.5)
}
heckman_results <- list(returns = returns, by_level = by_level, comparison = cmp,
  main_women = main_w$avg_return / 100, main_men = main_m$avg_return / 100,
  ols_amr = cmp$`Return per year (%)`[2] / 100, heckman_amr_u5 = if (!is.null(hk5)) cmp$`Return per year (%)`[3] / 100 else NA,
  rho = if (!is.null(hk5)) hk5$rho else NA, imr_coef = if (!is.null(hk5)) hk5$imr else NA, imr_pval = if (!is.null(hk5)) hk5$imr_p else NA)
saveRDS(heckman_results, "outputs/heckman_results.RData")
cat("\n=== DONE ===\n")
