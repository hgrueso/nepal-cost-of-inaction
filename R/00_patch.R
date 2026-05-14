# =============================================================================
# 00_patch.R — Fix binary variable codings in cleaned_data.RData
# Run ONCE after 01_load_clean.R: Rscript R/00_patch.R
# =============================================================================

suppressPackageStartupMessages({
  library(haven); library(tidyverse); library(srvyr)
})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
OUTPUT_PATH <- file.path(PROJECT_ROOT, "outputs")

# Load raw data (needed to re-read employment_status labels)
DATA_PATH <- file.path(PROJECT_ROOT, "data", "NLSS oct 20.dta")
cat("Loading raw data...\n")
raw <- read_dta(DATA_PATH)

# Load cleaned data
load(file.path(OUTPUT_PATH, "cleaned_data.RData"))
cat("Cleaned data loaded. Applying fixes...\n\n")

# =============================================================================
# DIAGNOSE employment_status integer coding
# =============================================================================
cat("=== DIAGNOSING employment_status ===\n")
cat("Integer values:\n")
print(table(as.integer(raw$employment_status), useNA = "always"))
cat("Factor labels:\n")
print(table(as_factor(raw$employment_status), useNA = "always"))

# =============================================================================
# DIAGNOSE q10_03 / literacy
# =============================================================================
cat("\n=== DIAGNOSING q10_03 (literacy read) ===\n")
cat("Integer values:\n")
print(table(as.integer(raw$q10_03), useNA = "always"))
cat("Attr label:", attr(raw$q10_03, "label"), "\n")
# Check value labels
cat("Value labels:\n")
print(attr(raw$q10_03, "labels"))

# =============================================================================
# APPLY FIXES
# =============================================================================
df <- df %>% mutate(

  # ── Fix 1: employed ─────────────────────────────────────────────────────────
  # Use factor label matching (robust to numeric code ambiguity)
  employed = case_when(
    as.character(as_factor(raw$employment_status)) == "Employed"     ~ 1L,
    as.character(as_factor(raw$employment_status)) == "Not Employed" ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── Fix 2: in_lf ────────────────────────────────────────────────────────────
  # Same as employed in this dataset (unemployment not separately identified)
  in_lf = case_when(
    as.character(as_factor(raw$employment_status)) == "Employed"     ~ 1L,
    as.character(as_factor(raw$employment_status)) == "Not Employed" ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── Fix 3: literate ─────────────────────────────────────────────────────────
  # q10_03: "Can you read?" — check labels above to confirm 1=Yes or 1=No
  # Value labels will tell us. Coding below assumes 1=Yes, 2=No (standard)
  # IF output shows 1=No,2=Yes then swap the 1L/0L below
  literate = case_when(
    as.integer(raw$q10_03) == 1 ~ 1L,   # 1 = Yes (can read) — CHECK label output above
    as.integer(raw$q10_03) == 2 ~ 0L,   # 2 = No
    TRUE ~ NA_integer_
  ),

  # ── Fix 4: wage_employed — ensure 0/1 (currently YES=1, NO=0, NA=15342) ──
  wage_employed = case_when(
    as.character(as_factor(raw$q07_02)) == "YES" ~ 1L,
    as.character(as_factor(raw$q07_02)) == "NO"  ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── Fix 5: NEET — recompute with corrected employed ──────────────────────
  neet = case_when(
    employed == 1 | attend_school == 1 ~ 0L,
    employed == 0 & attend_school == 0 ~ 1L,
    TRUE ~ NA_integer_
  ),

  # ── Fix 6: completed_secondary — recode from edu_raw ─────────────────────
  # edu_raw: 1=Non-formal 2=Primary 3=Lower-sec 4=Upper-sec 5=Tertiary
  # Secondary = upper secondary or above (codes 4,5)
  completed_secondary = case_when(
    edu_raw >= 4              ~ 1L,
    edu_raw %in% c(1, 2, 3)  ~ 0L,
    TRUE ~ NA_integer_
  ),
  completed_lower_sec = case_when(
    edu_raw >= 3              ~ 1L,
    edu_raw %in% c(1, 2)     ~ 0L,
    TRUE ~ NA_integer_
  ),
  completed_primary = case_when(
    edu_raw >= 2              ~ 1L,
    edu_raw == 1              ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── Fix 7: hhhead_female — ensure 0/1 ────────────────────────────────────
  hhhead_female = case_when(
    as.integer(raw$hh_head_gender) == 2 ~ 1L,
    as.integer(raw$hh_head_gender) == 1 ~ 0L,
    TRUE ~ NA_integer_
  )
)

# =============================================================================
# FULL BINARY VARIABLE AUDIT
# =============================================================================
cat("\n=== BINARY VARIABLE AUDIT ===\n")
cat(sprintf("%-28s  %7s  %7s  %7s  %7s  %s\n",
            "Variable", "N(0)", "N(1)", "N(NA)", "Pct(1)", "OK?"))
cat(strrep("-", 75), "\n")

bin_vars <- c("female", "rural", "ppoor", "attend_school",
              "employed", "in_lf", "wage_employed", "neet", "literate",
              "currently_married", "child_marriage_proxy",
              "completed_primary", "completed_lower_sec", "completed_secondary",
              "hhhead_female", "any_migrant")

for (v in bin_vars) {
  if (v %in% names(df)) {
    n0  <- sum(df[[v]] == 0, na.rm = TRUE)
    n1  <- sum(df[[v]] == 1, na.rm = TRUE)
    nna <- sum(is.na(df[[v]]))
    pct <- if ((n0 + n1) > 0) round(n1 / (n0 + n1) * 100, 1) else NA
    ok  <- all(na.omit(df[[v]]) %in% c(0L, 1L))
    cat(sprintf("%-28s  %7d  %7d  %7d  %6.1f%%  %s\n",
                v, n0, n1, nna, pct, ifelse(ok, "✓", "✗ CHECK")))
  } else {
    cat(sprintf("%-28s  MISSING FROM DATASET\n", v))
  }
}

# =============================================================================
# EXPECTED VALUES FOR SANITY CHECK (NLSS IV 2022/23 Nepal)
# =============================================================================
cat("\n=== EXPECTED VALUES (NLSS IV 2022/23) ===\n")
cat("female       → ~47.7%  (NLSS IV: 47.7% female)\n")
cat("rural        → ~46.3%  (NLSS IV: urban majority now)\n")
cat("ppoor        → ~20.3%  (NLSS IV poverty rate)\n")
cat("employed     → ~34%    (all-age denominator includes children)\n")
cat("literate     → ~66-76% (NLSS IV: 76% for 15+, lower for all ages)\n")
cat("wage_employed→ ~75%    (of those with non-NA; seasonal workers excluded)\n")

# =============================================================================
# REBUILD SUBSAMPLES AND SURVEY DESIGNS
# =============================================================================
cat("\nRebuilding subsamples and survey designs...\n")

df_a      <- df %>% filter(age_num >= 10, age_num <= 19)
df_b      <- df %>% filter(age_num >= 15, age_num <= 24)
df_girls  <- df_b %>% filter(female == 1)
df_c      <- df %>% filter(age_num >= 15, age_num <= 64)
df_wage_w <- df_c %>% filter(female == 1, wage_employed == 1)

make_svy <- function(data)
  srvyr::as_survey_design(data, ids = psu_number, strata = province_num,
                           weights = ind_wt, nest = TRUE)

svy_all   <- make_svy(df)
svy_b     <- make_svy(df_b)
svy_girls <- make_svy(df_girls)
svy_c     <- make_svy(df_c)

cat("\nSample sizes after patch:\n")
cat("  Girls 15-24:       ", nrow(df_girls), "\n")
cat("  Women wage earners:", nrow(df_wage_w), "\n")

# =============================================================================
# SAVE PATCHED DATA
# =============================================================================
save(df, df_a, df_b, df_c, df_girls, df_wage_w,
     svy_all, svy_b, svy_girls, svy_c,
     sample_flow, miss_tbl, vars_df,
     file = file.path(OUTPUT_PATH, "cleaned_data.RData"))

cat("\n✓ Patched data saved → outputs/cleaned_data.RData\n")
cat("  If literate pct looks wrong, check '=== DIAGNOSING q10_03 ==='\n")
cat("  output above and swap 1L/0L in Fix 3 if needed.\n")
cat("  Next step: Rscript R/02_descriptives.R\n\n")
