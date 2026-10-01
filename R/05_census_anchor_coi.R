# =============================================================================
# 05_census_anchor_coi.R
# Addresses Nepal CO review comments on survey weights and population counts
# (Mirak Raj Angdembe; Jose Maria Bendito Prieto).
#
# Method (as suggested by the reviewer): NLSS IV provides the PROPORTIONS
# (which are valid regardless of weight inflation); Census 2021 provides the
# absolute population BASE. Absolute counts = census base x NLSS proportions.
#
# Census anchor: total female population 14,911,027
#   Source: NSO Nepal, National Population and Housing Census 2021, final
#   report (March 2023). Total population 29,164,578.
# Under-18 reference used by the reviewer: approx. 10 million children.
#
# Also produces the return-rate robustness requested in review (r = 9% ILO
# headline, r = 12% boys' premium scenario).
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(scales) })
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")

TABLES <- "outputs/tables"; dir.create(TABLES, showWarnings = FALSE, recursive = TRUE)

CENSUS_TOTAL        <- 29164578   # Census 2021 total population
CENSUS_FEMALE_TOTAL <- 14911027   # Census 2021 total female population
CENSUS_U18_REF      <- 10000000   # reviewer's reference for under-18 (approx.)
USD_NPR             <- 133        # approximate rate used in the report (8.1B -> 61M)

# =============================================================================
# STEP 1 - Diagnostics: what do the NLSS IV weights actually sum to?
# =============================================================================
cat("=== STEP 1: Weight diagnostics ===\n")
w_all    <- sum(df$ind_wt, na.rm = TRUE)
w_u18    <- sum(df$ind_wt[df$age_num < 18], na.rm = TRUE)
w_female <- sum(df$ind_wt[df$female == 1], na.rm = TRUE)
w_g1524  <- sum(df$ind_wt[df$female == 1 & df$age_num >= 15 & df$age_num <= 24], na.rm = TRUE)

cat(sprintf("  Sum of ind_wt, all individuals : %12.0f  (census total:  %10.0f, ratio %.2f)\n",
            w_all, CENSUS_TOTAL, w_all / CENSUS_TOTAL))
cat(sprintf("  Sum of ind_wt, under 18        : %12.0f  (reviewer ref: ~%10.0f, ratio %.2f)\n",
            w_u18, CENSUS_U18_REF, w_u18 / CENSUS_U18_REF))
cat(sprintf("  Sum of ind_wt, all females     : %12.0f  (census female: %10.0f, ratio %.2f)\n",
            w_female, CENSUS_FEMALE_TOTAL, w_female / CENSUS_FEMALE_TOTAL))
cat(sprintf("  Sum of ind_wt, girls 15-24     : %12.0f\n", w_g1524))
cat("  If the ratios are far from 1, raw weight totals should not be used as population counts.\n")

# =============================================================================
# STEP 2 - Census-anchored population of girls 15-24
#   NLSS proportion (weighted share of 15-24 among all females) x census female total
# =============================================================================
cat("\n=== STEP 2: Census-anchored girls 15-24 ===\n")
share_f1524 <- w_g1524 / w_female
N_f1524_census <- share_f1524 * CENSUS_FEMALE_TOTAL
cat(sprintf("  NLSS weighted share of 15-24 among females : %.4f\n", share_f1524))
cat(sprintf("  Census-anchored girls 15-24                : %12.0f\n", N_f1524_census))
cat(sprintf("  (Previous analysis used a fixed anchor of 1,400,000)\n"))

# Weighted share of girls 15-24 not completing secondary
d_g <- df_girls %>% filter(!is.na(completed_secondary), !is.na(ind_wt))
p_no_sec <- weighted.mean(d_g$completed_secondary == 0, d_g$ind_wt)
cat(sprintf("  Weighted share not completing secondary    : %.4f\n", p_no_sec))
N_excluded_census <- N_f1524_census * p_no_sec
cat(sprintf("  Census-anchored N excluded                 : %12.0f  (previous: 680,231)\n",
            N_excluded_census))

# =============================================================================
# STEP 3 - Recompute the CoI with the census-anchored N (provincial build-up)
# =============================================================================
cat("\n=== STEP 3: Census-anchored CoI ===\n")
prov_wages <- tribble(
  ~province_lbl,    ~base_wage,
  "Bagmati",         7000,
  "Koshi",           2500,
  "Gandaki",         2292,
  "Madhesh",         2000,
  "Lumbini",         1500,
  "Karnali",         1066,
  "Sudurpashchim",   1042)

cate_prov <- cf_results$cate_neet_all %>%
  filter(dimension == "Province") %>%
  mutate(province_lbl = as.character(group), cate_abs = abs(mean_cate))

pv_factor <- function(r, dS = 2, delta = 0.05, T = 35)
  ((1 + r)^dS - 1) * (1 - (1 + delta)^(-T)) / delta

coi_build <- df_girls %>%
  filter(!is.na(province_lbl), !is.na(ind_wt)) %>%
  group_by(province_lbl) %>%
  summarise(total_weight = sum(ind_wt, na.rm = TRUE),
            pct_no_secondary = weighted.mean(completed_secondary == 0, ind_wt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(province_lbl   = as.character(province_lbl),
         pop_share      = total_weight / sum(total_weight),
         pop_girls_prov = N_f1524_census * pop_share,
         n_excluded     = pop_girls_prov * pct_no_secondary) %>%
  left_join(prov_wages, by = "province_lbl") %>%
  left_join(cate_prov %>% select(province_lbl, cate_abs), by = "province_lbl") %>%
  mutate(
    coi_b_r09 = cate_abs * n_excluded * base_wage * 12 * pv_factor(0.09) / 1e9,
    coi_b_r12 = cate_abs * n_excluded * base_wage * 12 * pv_factor(0.12) / 1e9)

nat_N      <- sum(coi_build$n_excluded, na.rm = TRUE)
nat_coi_09 <- sum(coi_build$coi_b_r09, na.rm = TRUE)
nat_coi_12 <- sum(coi_build$coi_b_r12, na.rm = TRUE)

cat("\nProvincial build-up (census-anchored, r = 9%):\n")
coi_build %>%
  transmute(province_lbl, n_excluded = round(n_excluded),
            coi_b_r09 = round(coi_b_r09, 2)) %>%
  arrange(desc(coi_b_r09)) %>% print(n = 10)

# =============================================================================
# STEP 4 - Old vs new comparison and robustness
# =============================================================================
cat("\n=== STEP 4: Headline comparison ===\n")
comparison <- tribble(
  ~quantity,                          ~previous,            ~census_anchored,
  "Girls 15-24 (national)",           "1,400,000 (fixed)",  format(round(N_f1524_census), big.mark = ","),
  "N not completing secondary",       "680,231",            format(round(nat_N), big.mark = ","),
  "CoI, r = 9% ILO (NPR billion)",    "8.1",                sprintf("%.1f", nat_coi_09),
  "CoI, r = 9% ILO (USD million)",    "61",                 sprintf("%.0f", nat_coi_09 * 1e9 / USD_NPR / 1e6),
  "CoI, r = 12% boys (NPR billion)",  "-",                  sprintf("%.1f", nat_coi_12),
  "CoI as % health budget 86.24B",    "9.4",                sprintf("%.1f", nat_coi_09 / 86.24 * 100),
  "CoI as % soc. sec. allow. 106B",   "7.6",                sprintf("%.1f", nat_coi_09 / 106 * 100),
  "CoI as % education budget 203.66B","4.0",                sprintf("%.1f", nat_coi_09 / 203.66 * 100),
  "Break-even per girl (USD)",        "90",                 sprintf("%.0f", nat_coi_09 * 1e9 / USD_NPR / nat_N))
print(as.data.frame(comparison), right = FALSE)

readr::write_csv(comparison, file.path(TABLES, "census_anchor_comparison.csv"))
readr::write_csv(coi_build,  file.path(TABLES, "coi_census_anchored_province.csv"))
census_results <- list(
  share_f1524 = share_f1524, N_f1524_census = N_f1524_census,
  p_no_sec = p_no_sec, N_excluded = nat_N,
  coi_b_r09 = nat_coi_09, coi_b_r12 = nat_coi_12,
  w_all = w_all, w_u18 = w_u18, w_female = w_female, w_g1524 = w_g1524)
saveRDS(census_results, "outputs/census_anchor_results.RData")
cat("\nSaved: outputs/census_anchor_results.RData, tables/census_anchor_comparison.csv\n")
cat("=== DONE ===\n")
