# =============================================================================
# 01_load_clean_v2.R — Merge and clean full NLSS IV microdata
# Variable codings verified from 00b_check_values.R output
# =============================================================================

needed <- c("haven","tidyverse","srvyr")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(haven); library(tidyverse); library(srvyr)
})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
BASE         <- file.path(PROJECT_ROOT, "data", "NLSSIV_microdata", "data", "stata format")
OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
dir.create(TABLES_PATH,  showWarnings=FALSE, recursive=TRUE)
dir.create(FIGURES_PATH, showWarnings=FALSE, recursive=TRUE)

rd <- function(f) {
  df <- read_dta(file.path(BASE, f))
  cat("✓", f, "—", nrow(df), "rows x", ncol(df), "cols\n")
  df
}

# =============================================================================
# 1. LOAD
# =============================================================================
cat("\n=== LOADING ===\n")
s01     <- rd("S01.dta")
s07     <- rd("S07.dta")
s08     <- rd("S08.dta")
s09     <- rd("S09.dta")
s10     <- rd("S10.dta")
poverty <- rd("poverty.dta")

# =============================================================================
# 2. PREPARE EACH SECTION FOR MERGING
# Merge key: hh_number + idcode (both confirmed present in all files)
# =============================================================================
KEY <- c("psu_number","hh_number","idcode")

# S07 — keep education variables only
s07_slim <- s07 %>%
  select(all_of(KEY),
         q07_02, q07_03,              # literacy: can read / write
         q07_05, q07_06,              # school last attended + highest level (ex-students)
         q07_11, q07_12,              # currently attending + highest level (current students)
         q07_14, q07_15, q07_16,      # absences, reason, transport
         starts_with("q07_17"),       # schooling expenditures
         q07_18, q07_19,              # scholarship
         q07_09, q07_10, q07_20, q07_21) # years to complete class 8 / class 10

# S08 — Washington Group only
s08_slim <- s08 %>%
  select(all_of(KEY), q08_16, q08_17, q08_18, q08_19, q08_20, q08_21)

# S09 — employment last 7 days
s09_slim <- s09 %>%
  select(all_of(KEY),
         q09_02, q09_03, q09_04, q09_05,  # work last 7 days (4 types)
         q09_06, q09_07,                   # absent but returning
         q09_10,                            # employment status detail
         q09_15, q09_16,                   # sector, enterprise type
         q09_18, q09_21, q09_22)           # job search, willingness

# S10 — wage jobs (multiple rows per person; summarise to one row per person)
# For each person, take the job with the highest take-home pay (main job)
# Wage construction:
#   Long-term salary workers (q10_05 = "Long term salary"): use q10_09_a (annual take-home)
#   Daily wage workers (q10_05 = "Daily wage"): use q10_07 (cash/day) × q10_06 (months worked × ~26 days)
#   Contract workers: use q10_11_a (total contract pay)

s10_built <- s10 %>%
  mutate(
    cond      = as.character(as_factor(q10_05)),
    months_wkd = as.numeric(q10_06),
    days_wkd   = months_wkd * 26,         # months worked × 26 working days
    daily_pay  = as.numeric(q10_07),
    salary_ann = as.numeric(q10_09_a),    # take-home pay (annual, long-term workers)
    contract   = as.numeric(q10_11_a),
    # Annualised earnings — use whichever source is non-missing and > 0
    annual_wage = case_when(
      !is.na(salary_ann) & salary_ann > 0                          ~ salary_ann,
      !is.na(daily_pay)  & daily_pay > 0 & !is.na(days_wkd) & days_wkd > 0
                                                                    ~ daily_pay * days_wkd,
      !is.na(contract)   & contract > 0                            ~ contract,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(psu_number, hh_number, idcode) %>%
  slice_max(order_by = coalesce(annual_wage, 0), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(psu_number, hh_number, idcode, q10_03, q10_05, q10_06,
         annual_wage, salary_ann, daily_pay)

cat("\n  S10 unique wage workers after cleaning:", nrow(s10_built), "\n")
cat("  Wage distribution (annual NPR):\n")
print(summary(s10_built$annual_wage[s10_built$annual_wage > 0]))

# Poverty — household-level (9,600 rows), merge on hh_number only
pov_slim <- poverty %>%
  select(psu_number, hh_number, prov, domain, hhsize, hhs_wt, ind_wt,
         ad_4, pcep, pcep_food, pcep_nonfood, poor, quintile_pcep,
         pline, fpline, nfpline, paasche) %>%
  distinct(psu_number, hh_number, .keep_all=TRUE)  # unique at psu+hh level

# =============================================================================
# 3. MERGE ONTO S01 ROSTER
# =============================================================================
cat("\n=== MERGING ===\n")
df <- s01
cat("S01 base:", nrow(df), "\n")
df <- left_join(df, s07_slim, by=KEY); cat("+ S07:", nrow(df), "\n")
df <- left_join(df, s08_slim, by=KEY); cat("+ S08:", nrow(df), "\n")
df <- left_join(df, s09_slim, by=KEY); cat("+ S09:", nrow(df), "\n")
df <- left_join(df, s10_built %>% select(psu_number, hh_number, idcode,
                                          q10_03, q10_05, q10_06,
                                          annual_wage, salary_ann, daily_pay),
                by=KEY);                cat("+ S10:", nrow(df), "\n")
df <- left_join(df, pov_slim, by=c("psu_number","hh_number")); cat("+ poverty:", nrow(df), "\n")
cat("  Poverty match rate:", round(mean(!is.na(df$ind_wt))*100,1), "% of individuals have weights\n")

# Resolve duplicate column names from join (.x / .y suffixes)
x_cols <- grep("[.]x$", names(df), value=TRUE)
y_cols <- grep("[.]y$", names(df), value=TRUE)
if (length(x_cols) > 0) {
  cat("  Resolving duplicate columns:", paste(x_cols, collapse=", "), "\n")
  new_names <- sub("[.]x$", "", x_cols)
  names(df)[names(df) %in% x_cols] <- new_names
  df <- df[, !names(df) %in% y_cols]
}
cat("  Final columns:", ncol(df), "| psu_number present:", "psu_number" %in% names(df), "\n")

# =============================================================================
# 4. DERIVE CLEAN VARIABLES
# =============================================================================
cat("\n=== DERIVING VARIABLES ===\n")

df <- df %>% mutate(

  # ── Sex ──────────────────────────────────────────────────────────────────────
  female  = case_when(
    as.character(as_factor(q01_02)) == "Female" ~ 1L,
    as.character(as_factor(q01_02)) == "Male"   ~ 0L,
    TRUE ~ NA_integer_),
  sex_lbl = factor(female, 0:1, c("Male","Female")),

  # ── Age ──────────────────────────────────────────────────────────────────────
  age_num = as.numeric(q01_03),

  # Fine age groups (reviewer request: 15-18 vs 19-24)
  age_cat = case_when(
    age_num >= 10 & age_num <= 14 ~ "10-14",
    age_num >= 15 & age_num <= 18 ~ "15-18",
    age_num >= 19 & age_num <= 24 ~ "19-24",
    age_num >= 25 & age_num <= 29 ~ "25-29",
    age_num >= 30 & age_num <= 64 ~ "30-64",
    TRUE ~ NA_character_),
  age_cat = factor(age_cat, c("10-14","15-18","19-24","25-29","30-64")),

  # Sample flags
  youth_1518 = as.integer(age_num >= 15 & age_num <= 18),
  youth_1924 = as.integer(age_num >= 19 & age_num <= 24),
  youth_1524 = as.integer(age_num >= 15 & age_num <= 24),

  # ── Province ─────────────────────────────────────────────────────────────────
  province_num = as.integer(prov),
  province_lbl = factor(province_num, 1:7,
    c("Koshi","Madhesh","Bagmati","Gandaki","Lumbini","Karnali","Sudurpashchim")),

  # ── Urban / Rural ─────────────────────────────────────────────────────────────
  # ad_4: 1=Kathmandu, 2=Other urban, 3=Rural (numeric from poverty.dta)
  rural = case_when(
    as.numeric(ad_4) == 3 ~ 1L,
    as.numeric(ad_4) %in% c(1, 2) ~ 0L,
    TRUE ~ NA_integer_),
  urban_lbl = factor(rural, 0:1, c("Urban","Rural")),

  # ── Marital status ────────────────────────────────────────────────────────────
  mar_lbl = as.character(as_factor(q01_05)),
  currently_married = case_when(
    mar_lbl == "Married"       ~ 1L,
    mar_lbl %in% c("Never married","Widow/widower","Divorced","Separated") ~ 0L,
    TRUE ~ NA_integer_),

  # ── Child marriage (IMPROVED — reviewer approved) ─────────────────────────────
  # Girls aged 10-18 currently married = almost certainly married before 18
  child_marriage = case_when(
    female == 1 & age_num >= 10 & age_num <= 18 & currently_married == 1 ~ 1L,
    female == 1 & age_num >= 10 ~ 0L,
    TRUE ~ NA_integer_),

  # ── Caste / Ethnicity ─────────────────────────────────────────────────────────
  caste_lbl = as.character(as_factor(q01_07)),
  caste_broad = case_when(
    caste_lbl %in% c("Hill Caste","Madhesh/Tarai Caste")      ~ "Brahmin/Chhetri",
    caste_lbl %in% c("Mountain/Hill Janajati","Tarai Janajati") ~ "Janajati",
    caste_lbl %in% c("Hill Dalit","Madhesh/Tarai Dalit")       ~ "Dalit",
    caste_lbl == "Religions/Linguistic group"                   ~ "Religious/Linguistic",
    TRUE ~ "Other"),
  caste_broad = factor(caste_broad,
    c("Brahmin/Chhetri","Janajati","Dalit","Religious/Linguistic","Other")),

  # Also keep detailed 8-group caste for intersectional analysis
  caste_detail = factor(caste_lbl,
    c("Hill Caste","Madhesh/Tarai Caste",
      "Mountain/Hill Janajati","Tarai Janajati",
      "Hill Dalit","Madhesh/Tarai Dalit",
      "Religions/Linguistic group","Others & Not stated")),

  # ── Poverty ───────────────────────────────────────────────────────────────────
  ppoor      = as.integer(poor),
  pcep_n     = as.numeric(pcep),
  quintile_n = as.integer(quintile_pcep),
  quintile_lbl = factor(quintile_n, 1:5,
    c("Poorest (Q1)","Q2","Q3","Q4","Richest (Q5)")),

  # ── EDUCATION ─────────────────────────────────────────────────────────────────

  # Literacy: q07_02 "Can you read?" (YES/NO)
  literate = case_when(
    as.character(as_factor(q07_02)) == "YES" ~ 1L,
    as.character(as_factor(q07_02)) == "NO"  ~ 0L,
    TRUE ~ NA_integer_),

  # Currently attending: q07_11 non-NA = enrolled
  attend_any = as.integer(!is.na(q07_11) &
    as.character(as_factor(q07_11)) != ""),

  # School type for currently enrolled
  q07_11_lbl = as.character(as_factor(q07_11)),

  # Distinguish secondary vs higher education for currently enrolled
  # Secondary: government/private school + vocational + religious school
  # Higher education: college and university types
  attend_secondary = case_when(
    q07_11_lbl %in% c("COMMUNITY/GOVERNMENT SCHOOL","INSTITUTIONAL/PRIVATE SCHOOL",
                       "TECHNICAL/VOCATIONAL SCHOOL",
                       "GURUKUL / MADARSHA / GUMBA / GONPA") ~ 1L,
    attend_any == 1 ~ 0L,
    TRUE ~ 0L),

  attend_higher = case_when(
    q07_11_lbl %in% c("COMMUNITY (PUBLIC) COLLEGE","PRIVATE COLLEGE",
                       "CONSTITUENT (ANGIK) COLLEGE","OPEN UNIVERSITY",
                       "INTERNATIONALLY AFFILIATED COLLEGE","OTHER") ~ 1L,
    attend_any == 1 ~ 0L,
    TRUE ~ 0L),

  # Highest education level (from q07_06 for ex-students, q07_12 for enrolled)
  # Using label matching to avoid numeric code issues
  edu_lbl_raw = case_when(
    attend_any == 1 ~ as.character(as_factor(q07_12)),
    TRUE            ~ as.character(as_factor(q07_06))),

  edu_cat = case_when(
    edu_lbl_raw %in% c("Illiterate","Literate (levelless)") ~ "No education",
    edu_lbl_raw %in% c("Pre-school/kindergarden",
                        "Class 1","Class 2","Class 3","Class 4","Class 5") ~ "Primary",
    edu_lbl_raw %in% c("Class 6","Class 7","Class 8") ~ "Lower secondary",
    edu_lbl_raw %in% c("Class 9","Class 10","SEE/SLC","SLC") ~ "Secondary",
    edu_lbl_raw %in% c("Intermediate/Class 12") ~ "Higher secondary",
    edu_lbl_raw %in% c("Bachelor level","Master level or higher",
                        "Professional degree") ~ "Tertiary",
    TRUE ~ NA_character_),
  edu_cat = factor(edu_cat,
    c("No education","Primary","Lower secondary","Secondary",
      "Higher secondary","Tertiary")),

  completed_secondary    = as.integer(edu_cat %in% c("Secondary","Higher secondary","Tertiary")),
  completed_lower_sec    = as.integer(edu_cat %in% c("Lower secondary","Secondary",
                                                       "Higher secondary","Tertiary")),
  completed_primary      = as.integer(edu_cat %in% c("Primary","Lower secondary","Secondary",
                                                       "Higher secondary","Tertiary")),

  # ── DISABILITY (Washington Group) ────────────────────────────────────────────
  # "Yes - a lot of difficulty" OR "Cannot, at all" in ANY domain = functional difficulty
  across(c(q08_16,q08_17,q08_18,q08_19,q08_20,q08_21),
         ~as.character(as_factor(.x)), .names="{.col}_lbl"),

  # ── DISABILITY continues below ──
) # close first mutate block

df <- df %>%
  mutate(
    wg_max = pmax(
      as.integer(as_factor(q08_16)),
      as.integer(as_factor(q08_17)),
      as.integer(as_factor(q08_18)),
      as.integer(as_factor(q08_19)),
      as.integer(as_factor(q08_20)),
      as.integer(as_factor(q08_21)),
      na.rm = TRUE),
    # WG labels: 1=No difficulty, 2=Some, 3=A lot, 4=Cannot at all
    any_difficulty  = case_when(wg_max >= 3 ~ 1L, wg_max <= 2 ~ 0L, TRUE ~ NA_integer_),
    severe_disability = case_when(wg_max == 4 ~ 1L, wg_max < 4 ~ 0L, TRUE ~ NA_integer_)
  )

# ── EMPLOYMENT ────────────────────────────────────────────────────────────────
# q09_02: wage work last 7 days (YES/NO — all answered, n=30,899)
# q09_03: self-employment (NA for those who said YES to q09_02 — branching)
# q09_04: unpaid family work (NA branches further)
# q09_05: absent from paid job (36 YES)
# Strategy: flag YES to any = employed

df <- df %>%
  mutate(
    emp02 = case_when(as.character(as_factor(q09_02))=="YES"~1L,
                      as.character(as_factor(q09_02))=="NO" ~0L, TRUE~NA_integer_),
    # For q09_03: if emp02=1, person was routed around it — treat as 0
    emp03 = case_when(as.character(as_factor(q09_03))=="YES"~1L,
                      as.character(as_factor(q09_03))=="NO" ~0L,
                      emp02==1 ~ 0L,   # skip-routed: they had wage work
                      TRUE~NA_integer_),
    emp04 = case_when(as.character(as_factor(q09_04))=="YES"~1L,
                      as.character(as_factor(q09_04))=="NO" ~0L,
                      emp02==1 | emp03==1 ~ 0L,
                      TRUE~NA_integer_),
    emp05 = case_when(as.character(as_factor(q09_05))=="YES"~1L,
                      as.character(as_factor(q09_05))=="NO" ~0L,
                      TRUE~NA_integer_),

    # ILO employed: at least one form of work last 7 days
    employed_ilo = case_when(
      emp02==1 | emp03==1 | emp04==1 | emp05==1 ~ 1L,
      emp02==0 & (emp03==0 | is.na(emp03)) & (emp04==0 | is.na(emp04)) ~ 0L,
      TRUE ~ NA_integer_),

    # Wage employed specifically (last 7 days)
    wage_emp_7d = case_when(emp02==1~1L, emp02==0~0L, TRUE~NA_integer_),

    # Unemployed: not employed but looking (q09_18)
    seeking = case_when(as.character(as_factor(q09_18))=="YES"~1L,
                         as.character(as_factor(q09_18))=="NO" ~0L, TRUE~NA_integer_),
    unemployed_ilo = case_when(
      employed_ilo==0 & seeking==1 ~ 1L,
      employed_ilo==0 & seeking==0 ~ 0L,
      TRUE ~ NA_integer_),
    in_lf = case_when(employed_ilo==1|unemployed_ilo==1~1L, employed_ilo==0~0L, TRUE~NA_integer_),

    # Wage employment last 12 months (from S10)
    wage_emp_12m = case_when(
      as.character(as_factor(q10_03))=="YES"~1L,
      as.character(as_factor(q10_03))=="NO" ~0L,
      TRUE~NA_integer_),

    # Annual wages (from S10 cleaned above)
    annual_wage_clean = if_else(annual_wage > 0, annual_wage, NA_real_),
    log_wage = if_else(!is.na(annual_wage_clean), log(annual_wage_clean), NA_real_),
    monthly_wage = annual_wage_clean / 12,

    # ── NEET ─────────────────────────────────────────────────────────────────
    # ILO definition: not employed AND not in education/training
    neet = case_when(
      employed_ilo==1 | attend_any==1 ~ 0L,
      employed_ilo==0 & attend_any==0 ~ 1L,
      TRUE ~ NA_integer_)
  )

# =============================================================================
# 5. VERIFY
# =============================================================================
cat("\n=== VERIFICATION ===\n")
cat(sprintf("%-26s  %7s  %7s  %7s  %6s\n","Variable","N(0)","N(1)","N(NA)","Pct(1)"))
cat(strrep("-",60),"\n")
for (v in c("female","rural","ppoor","literate","attend_any","attend_secondary",
            "attend_higher","completed_secondary","employed_ilo","wage_emp_7d",
            "wage_emp_12m","neet","any_difficulty","child_marriage","currently_married")) {
  n0  <- sum(df[[v]]==0, na.rm=TRUE)
  n1  <- sum(df[[v]]==1, na.rm=TRUE)
  nna <- sum(is.na(df[[v]]))
  pct <- if(n0+n1>0) round(n1/(n0+n1)*100,1) else NA
  cat(sprintf("%-26s  %7d  %7d  %7d  %5.1f%%\n", v, n0, n1, nna, pct))
}

cat("\nNEET by sex, youth 15-24 (unweighted):\n")
df %>% filter(youth_1524==1, !is.na(sex_lbl), !is.na(neet)) %>%
  group_by(sex_lbl, age_cat) %>%
  summarise(pct_neet=round(mean(neet)*100,1), n=n(), .groups="drop") %>%
  print()

cat("\nEnrolment by type, youth 15-24:\n")
df %>% filter(youth_1524==1) %>%
  summarise(pct_any=round(mean(attend_any)*100,1),
            pct_sec=round(mean(attend_secondary)*100,1),
            pct_he=round(mean(attend_higher)*100,1)) %>% print()

cat("\nWage data quality:\n")
cat("  N with annual_wage >0:", sum(!is.na(df$annual_wage_clean)), "\n")
cat("  Median monthly wage (NPR):", round(median(df$monthly_wage, na.rm=TRUE)), "\n")

# =============================================================================
# 6. ANALYTICAL SAMPLES
# =============================================================================
df_a      <- df %>% filter(age_num >= 10, age_num <= 19)
df_b      <- df %>% filter(youth_1524 == 1)
df_b1     <- df %>% filter(youth_1518 == 1)   # 15-18: secondary school age
df_b2     <- df %>% filter(youth_1924 == 1)   # 19-24: post-secondary
df_girls  <- df_b  %>% filter(female == 1)
df_girls1 <- df_b1 %>% filter(female == 1)
df_girls2 <- df_b2 %>% filter(female == 1)
df_c      <- df %>% filter(age_num >= 15, age_num <= 64)
df_wage_w <- df_c  %>% filter(female == 1, !is.na(log_wage))

cat("\n=== SAMPLE SIZES ===\n")
for (nm in c("df","df_b","df_b1","df_b2","df_girls","df_girls1","df_girls2","df_c","df_wage_w"))
  cat(sprintf("  %-12s  n = %d\n", nm, nrow(get(nm))))

sample_flow <- tibble(
  Step = c("Full NLSS IV","Youth 15-24","  Girls 15-24",
           "  Secondary-age girls (15-18)","  Post-sec girls (19-24)",
           "Working-age (15-64)","Women with wage data"),
  N    = c(nrow(df), nrow(df_b), nrow(df_girls),
           nrow(df_girls1), nrow(df_girls2), nrow(df_c), nrow(df_wage_w))
) %>% mutate(`Share (%)`=round(N/nrow(df)*100,1))

# =============================================================================
# 7. SURVEY DESIGN
# =============================================================================
make_svy <- function(data) {
  d <- data %>% filter(!is.na(ind_wt), !is.na(province_num))
  if (nrow(d) < nrow(data))
    cat("  make_svy: dropping", nrow(data)-nrow(d), "rows with missing weights\n")
  # psu_number may be named psu_number.x after join — use whichever exists
  psu_col <- if ("psu_number" %in% names(d)) "psu_number" else
             if ("psu_number.x" %in% names(d)) "psu_number.x" else
             "hh_number"   # fallback to household as cluster
  cat("  make_svy: using", psu_col, "as PSU identifier\n")
  srvyr::as_survey_design(d, ids=!!sym(psu_col), strata=province_num,
                           weights=ind_wt, nest=TRUE)
}

svy_all    <- make_svy(df)
svy_b      <- make_svy(df_b)
svy_b1     <- make_svy(df_b1)
svy_b2     <- make_svy(df_b2)
svy_girls  <- make_svy(df_girls)
svy_girls1 <- make_svy(df_girls1)
svy_girls2 <- make_svy(df_girls2)
svy_c      <- make_svy(df_c)

# Missingness
key_vars <- c("female","age_num","province_lbl","rural","caste_broad",
              "quintile_n","ppoor","pcep_n","literate",
              "attend_any","attend_secondary","attend_higher",
              "completed_secondary","edu_cat",
              "employed_ilo","wage_emp_12m","neet","log_wage",
              "currently_married","child_marriage","any_difficulty")

miss_tbl <- tibble(
  variable = key_vars,
  n_obs    = sapply(key_vars, function(v) sum(!is.na(df[[v]]))),
  n_miss   = sapply(key_vars, function(v) sum( is.na(df[[v]]))),
  pct_miss = sapply(key_vars, function(v) round(mean(is.na(df[[v]]))*100,1))
)
cat("\n=== MISSINGNESS ===\n"); print(miss_tbl, n=Inf)
readr::write_csv(miss_tbl, file.path(TABLES_PATH, "missingness_v2.csv"))

# =============================================================================
# 8. SAVE
# =============================================================================
save(df, df_a, df_b, df_b1, df_b2, df_girls, df_girls1, df_girls2,
     df_c, df_wage_w,
     svy_all, svy_b, svy_b1, svy_b2,
     svy_girls, svy_girls1, svy_girls2, svy_c,
     sample_flow, miss_tbl,
     file = file.path(OUTPUT_PATH, "cleaned_data_v2.RData"))

cat("\n✓ Saved → outputs/cleaned_data_v2.RData\n")
cat("  Next: Rscript R/03_regressions_v2.R\n\n")

cat("=== DATA NOTES ===\n")
cat("1. attend_any: q07_11 non-NA = currently enrolled (ALL levels incl. HE)\n")
cat("   attend_secondary: school-type labels (govt/private school, vocational, religious)\n")
cat("   attend_higher: college/university labels only\n")
cat("2. employed_ilo: ILO 7-day definition; q09_03-q09_05 corrected for skip routing\n")
cat("3. annual_wage: long-term salary from q10_09_a; daily wage from q10_07 × days worked\n")
cat("4. any_difficulty: Washington Group — 'a lot of difficulty' or 'cannot at all'\n")
cat("5. child_marriage: girls 10-18 currently married (strong proxy per reviewer)\n")
cat("   Note: only 46,870 individuals in household roster (excludes absentees in S14)\n")

# TEMP DIAGNOSTIC — remove after fixing rural
# Run separately: Rscript -e "source('R/01_load_clean_v2.R')" won't work
# so add this to the end of the merge section temporarily
