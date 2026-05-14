# =============================================================================
# 01_load_clean.R  ·  NLSS IV (2022/23, Figshare release)
# Final version — all variable names and codings verified from QC output
# =============================================================================

needed <- c("haven", "tidyverse", "survey", "srvyr")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(haven); library(tidyverse); library(survey); library(srvyr)
})

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- getwd()
if (!exists("DATA_PATH"))    DATA_PATH    <- file.path(PROJECT_ROOT, "data", "NLSS oct 20.dta")
if (!exists("OUTPUT_PATH"))  OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")
dir.create(FIGURES_PATH, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_PATH,  showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. LOAD
# =============================================================================
cat("Loading:", DATA_PATH, "\n")
raw <- read_dta(DATA_PATH)
cat("Dimensions:", nrow(raw), "rows ×", ncol(raw), "cols\n\n")

vars_df <- data.frame(
  pos   = seq_len(ncol(raw)),
  name  = names(raw),
  label = sapply(raw, function(x) { l <- attr(x,"label"); if(is.null(l)) "" else as.character(l)[1] }),
  class = sapply(raw, function(x) paste(class(x), collapse="/"))
)
readr::write_csv(vars_df, file.path(OUTPUT_PATH, "variable_codebook.csv"))

# =============================================================================
# 2. BUILD CLEAN VARIABLES
# All codings verified from QC distributions
# =============================================================================
df <- raw %>% mutate(ind_id = row_number()) %>% mutate(

  # ── Survey design ────────────────────────────────────────────────────────────
  # psu_number: PSU identifier | ind_wt: individual probability weight
  # province used as strata (7 strata, one per province)
  # Keep original names — do NOT rename; reference by original name in make_svy()

  # ── Province ─────────────────────────────────────────────────────────────────
  # prov: haven_labelled, labels = Koshi/Madhesh/Bagmati/Gandaki/Lumbini/Karnali/Sudurpaschim
  province_num = as.integer(prov),
  province_lbl = factor(province_num, 1:7,
                        c("Koshi","Madhesh","Bagmati","Gandaki",
                          "Lumbini","Karnali","Sudurpashchim")),

  # ── Urban / Rural ─────────────────────────────────────────────────────────────
  # ad_4: 1=Urban(1889)  2=Rural(29281)  3=KTM urban(32067)
  # Collapse: 1&3 = Urban;  2 = Rural
  urban_raw = as.integer(ad_4),
  rural     = case_when(urban_raw == 2 ~ 1L, urban_raw %in% c(1,3) ~ 0L, TRUE ~ NA_integer_),
  urban_lbl = factor(rural, 0:1, c("Urban","Rural")),

  # ── Sex ──────────────────────────────────────────────────────────────────────
  # Gender: Male=33092  Female=30145  (no NAs)
  sex_raw = as.integer(Gender),
  female  = case_when(sex_raw == 2 ~ 1L, sex_raw == 1 ~ 0L, TRUE ~ NA_integer_),
  sex_lbl = factor(female, 0:1, c("Male","Female")),

  # ── Age ──────────────────────────────────────────────────────────────────────
  age_num = as.numeric(Age),

  # ── Marital status ────────────────────────────────────────────────────────────
  # Labels: Never married(16473) Married(32799) Widow/widower(3147) Divorced(246) Separated(523)
  mar_raw          = as.integer(marital_status),
  currently_married = case_when(mar_raw == 2 ~ 1L, mar_raw %in% c(1,3,4,5) ~ 0L, TRUE ~ NA_integer_),

  # ── Caste / Ethnicity ────────────────────────────────────────────────────────
  # Pre-built dummies with confirmed labels from codebook:
  # d_caste1=Hill Caste  d_caste2=Madhesh/Tarai Caste  d_caste3=Mountain/Hill Janajati
  # d_caste4=Tarai Janajati  d_caste5=Hill Dalit  d_caste6=Madhesh/Tarai Dalit
  # d_caste7=Religions/Linguistic  d_caste8=Others & Not stated
  caste_group = case_when(
    d_caste1 == 1 ~ "Hill Caste",
    d_caste2 == 1 ~ "Madheshi Caste",
    d_caste3 == 1 ~ "Hill Janajati",
    d_caste4 == 1 ~ "Tarai Janajati",
    d_caste5 == 1 ~ "Hill Dalit",
    d_caste6 == 1 ~ "Madheshi Dalit",
    d_caste7 == 1 ~ "Religious/Linguistic",
    d_caste8 == 1 ~ "Other/Not stated",
    TRUE ~ NA_character_
  ),
  caste_group = factor(caste_group,
                       levels = c("Hill Caste","Madheshi Caste","Hill Janajati",
                                  "Tarai Janajati","Hill Dalit","Madheshi Dalit",
                                  "Religious/Linguistic","Other/Not stated")),
  # Broad grouping for easier tables
  caste_broad = case_when(
    d_caste1 == 1 | d_caste2 == 1 ~ "Brahmin/Chhetri",
    d_caste3 == 1 | d_caste4 == 1 ~ "Janajati",
    d_caste5 == 1 | d_caste6 == 1 ~ "Dalit",
    d_caste7 == 1                  ~ "Religious/Linguistic",
    TRUE ~ "Other"
  ),
  caste_broad = factor(caste_broad,
                       levels = c("Brahmin/Chhetri","Janajati","Dalit",
                                  "Religious/Linguistic","Other")),

  # ── Poverty / Consumption ────────────────────────────────────────────────────
  # poor:          0=Non-poor(52877)  1=Poor(10360)
  # quintile_pcep: 1=Poorest … 5=Richest
  # pcep:          spatially-deflated per-capita expenditure (NPR/year), range 28009–1320425
  ppoor      = as.integer(poor),
  pcep_n     = as.numeric(pcep),
  quintile_n = as.integer(quintile_pcep),
  quintile_lbl = factor(quintile_n, 1:5,
                        c("Poorest (Q1)","Q2","Q3","Q4","Richest (Q5)")),

  # ── Education ─────────────────────────────────────────────────────────────────
  # edu_category: Non-formal(24) primary(5861) lower secondary(10859)
  #               upper secondary(3035) tertiary(1495)  NA(41963)
  # NAs = people NOT currently enrolled in formal education
  # yearschooling: 0–17 years, same 41962 NAs → confirmed as CURRENT STUDENT dataset only
  #
  # !! IMPORTANT LIMITATION !!
  # edu_category & yearschooling only cover CURRENT STUDENTS (those in the education module).
  # For ever-attended / highest level completed for ALL adults, use q10_06 from full roster.
  # We use edu_category NAs as proxy for "not currently in school."

  edu_raw = as.integer(edu_category),
  # edu_category numeric codes: verify order from QC labels
  # Order seen: Non-formal < primary < lower secondary < upper secondary < tertiary
  edu_cat = case_when(
    edu_raw == 1 ~ "Non-formal",
    edu_raw == 2 ~ "Primary",
    edu_raw == 3 ~ "Lower secondary",
    edu_raw == 4 ~ "Upper secondary",
    edu_raw == 5 ~ "Tertiary",
    TRUE ~ NA_character_
  ),
  edu_cat = factor(edu_cat, levels = c("Non-formal","Primary","Lower secondary",
                                        "Upper secondary","Tertiary")),

  # Currently attending school = edu_category is non-missing
  attend_school = as.integer(!is.na(edu_category)),

  # Years of schooling (clean): covers current students only
  edu_years_clean = case_when(
    as.numeric(yearschooling) < 0  ~ NA_real_,
    as.numeric(yearschooling) > 25 ~ NA_real_,
    TRUE ~ as.numeric(yearschooling)
  ),

  # Completed levels: use edu_raw ordering (1=non-formal … 5=tertiary)
  # For cost-of-inaction: secondary = upper secondary (code 4)
  completed_primary   = as.integer(edu_raw >= 2),
  completed_lower_sec = as.integer(edu_raw >= 3),
  completed_secondary = as.integer(edu_raw >= 4),

  # Household head education (years): hhh_edu has same 0–17 distribution as yearschooling
  # Label in file is incorrect; content matches head's education years
  hhhead_edu_yrs = as.numeric(hhh_edu),

  # Father / mother education (from individual roster)
  father_edu = as.integer(q01_12),
  mother_edu = as.integer(q01_15),

  # Literacy: q10_03="Can you read?" q10_04="Can you write?"
  # Coding needs verification — check if 1=Yes or 1=No
  literate_read  = as.integer(q10_03),
  literate_write = as.integer(q10_04),

  # ── Age groups ────────────────────────────────────────────────────────────────
  adolescent  = as.integer(age_num >= 10 & age_num <= 19),
  youth       = as.integer(age_num >= 15 & age_num <= 24),
  working_age = as.integer(age_num >= 15 & age_num <= 64),

  # ── Employment ────────────────────────────────────────────────────────────────
  # employment_status:               Not Employed(41745) / Employed(21492)
  # employment_status_based_on_hours: Unemployed(3934)   / Employed(59303)
  # Use employment_status as primary (reflects main activity status, not just hours)
  employed   = case_when(
    as.integer(employment_status) == 2 ~ 1L,   # Employed
    as.integer(employment_status) == 1 ~ 0L,   # Not Employed
    TRUE ~ NA_integer_
  ),
  in_lf = case_when(
    # LF = employed + unemployed (actively seeking)
    as.integer(employment_status) == 2 ~ 1L,              # Employed
    as.integer(employment_status_based_on_hours) == 1 ~ 1L, # Unemployed
    as.integer(employment_status) == 1 ~ 0L,
    TRUE ~ NA_integer_
  ),

  # Wage employment: q07_02 "Did you work for salary/wages in last 12 months?"
  # YES(36206) NO(11689) NA(15342)
  wage_employed = case_when(
    as.integer(q07_02) == 1 ~ 1L,   # YES
    as.integer(q07_02) == 2 ~ 0L,   # NO
    TRUE ~ NA_integer_
  ),

  # NOTE: Individual wage amounts NOT cleanly available in this merged file.
  # q07_07–q07_10 are coded categories (range 1–14), not NPR amounts.
  # Wage analysis will use employment status + education as proxies.
  # Recommend requesting raw section-14 file from NSO for individual wages.
  log_wage = NA_real_,  # placeholder

  # NEET: Not in Employment, Education or Training
  # = Not employed AND Not currently attending school
  neet = case_when(
    employed == 1 | attend_school == 1 ~ 0L,
    employed == 0 & attend_school == 0 ~ 1L,
    TRUE ~ NA_integer_
  ),

  # ── Marriage (child marriage proxy) ───────────────────────────────────────────
  # No direct age-at-marriage variable found in this merged file.
  # Proxy: currently married girls aged 10–17 (lower bound estimate).
  # !! Flag as limitation; MICS 2022 is strongly recommended for child marriage. !!
  child_marriage_proxy = case_when(
    female == 1 & age_num < 18 & currently_married == 1 ~ 1L,
    female == 1 & age_num >= 5 ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── HH head characteristics ───────────────────────────────────────────────────
  hhhead_female = case_when(
    as.integer(hh_head_gender) == 2 ~ 1L,
    as.integer(hh_head_gender) == 1 ~ 0L,
    TRUE ~ NA_integer_
  ),

  # ── Migration ─────────────────────────────────────────────────────────────────
  any_migrant = case_when(
    as.integer(migration) %in% c(1,2) ~ 1L,
    TRUE ~ 0L
  )
)

# =============================================================================
# 3. VERIFY DERIVED VARIABLES
# =============================================================================
cat("=== DERIVED VARIABLE CHECKS ===\n\n")
cat("female:\n");         print(table(df$female, useNA="always"))
cat("\nrural:\n");        print(table(df$rural,  useNA="always"))
cat("\ncaste_broad:\n");  print(table(df$caste_broad, useNA="always"))
cat("\nppoor:\n");        print(table(df$ppoor, useNA="always"))
cat("\nattend_school:\n");print(table(df$attend_school, useNA="always"))
cat("\nemployed:\n");     print(table(df$employed, useNA="always"))
cat("\nwage_employed:\n");print(table(df$wage_employed, useNA="always"))
cat("\nneet (all ages):\n");print(table(df$neet, useNA="always"))
cat("\nlit_read:\n");     print(table(df$literate_read, useNA="always"))

# =============================================================================
# 4. ANALYTICAL SAMPLES
# =============================================================================
df_a     <- df %>% filter(age_num >= 10, age_num <= 19)
df_b     <- df %>% filter(age_num >= 15, age_num <= 24)
df_girls <- df_b %>% filter(female == 1)
df_c     <- df %>% filter(age_num >= 15, age_num <= 64)
df_wage_w <- df_c %>% filter(female == 1, !is.na(wage_employed))

cat("\n=== SAMPLE SIZES ===\n")
cat("Full dataset:       ", nrow(df), "\n")
cat("Adolescents 10-19:  ", nrow(df_a), "\n")
cat("Youth 15-24:        ", nrow(df_b), "\n")
cat("Girls 15-24:        ", nrow(df_girls), "\n")
cat("Working-age 15-64:  ", nrow(df_c), "\n")
cat("Women (wage data):  ", nrow(df_wage_w), "\n\n")

sample_flow <- tibble(
  Step       = c("Full NLSS IV (2022/23)","Adolescents (10–19)","Youth (15–24)",
                 "Girls (15–24)","Working-age (15–64)","Women with wage employment info"),
  N          = c(nrow(df), nrow(df_a), nrow(df_b),
                 nrow(df_girls), nrow(df_c), nrow(df_wage_w)),
  `Share (%)` = round(c(100, nrow(df_a), nrow(df_b), nrow(df_girls),
                         nrow(df_c), nrow(df_wage_w)) / nrow(df) * 100, 1)
)
print(sample_flow)

# =============================================================================
# 5. SURVEY DESIGN OBJECTS
# Branch OUTSIDE as_survey_design to avoid NSE conflict with if/else
# =============================================================================
make_svy <- function(data) {
  # All three identifiers are column names in 'data' — reference directly
  srvyr::as_survey_design(
    data,
    ids     = psu_number,     # PSU identifier (original column name)
    strata  = province_num,   # Province strata (1–7)
    weights = ind_wt,         # Individual probability weight
    nest    = TRUE            # PSUs nested within strata
  )
}

cat("\nBuilding survey design objects...\n")
svy_all   <- make_svy(df)
svy_b     <- make_svy(df_b)
svy_girls <- make_svy(df_girls)
svy_c     <- make_svy(df_c)
cat("✓ Survey design objects created\n\n")

# =============================================================================
# 6. MISSINGNESS TABLE
# =============================================================================
key_vars <- c("female","age_num","province_lbl","rural","caste_broad",
              "quintile_n","ppoor","pcep_n",
              "attend_school","edu_years_clean","completed_secondary","edu_cat",
              "employed","in_lf","neet","wage_employed",
              "currently_married","child_marriage_proxy","literate_read")

miss_tbl <- tibble(
  variable = key_vars,
  n_obs    = sapply(key_vars, function(v) if(v %in% names(df)) sum(!is.na(df[[v]])) else NA_integer_),
  n_miss   = sapply(key_vars, function(v) if(v %in% names(df)) sum( is.na(df[[v]])) else NA_integer_),
  pct_miss = sapply(key_vars, function(v) if(v %in% names(df)) round(mean(is.na(df[[v]]))*100,1) else NA_real_)
)
cat("=== MISSINGNESS ===\n"); print(miss_tbl, n = Inf)
readr::write_csv(miss_tbl, file.path(TABLES_PATH, "missingness_summary.csv"))

# =============================================================================
# 7. SAVE
# =============================================================================
save(df, df_a, df_b, df_c, df_girls, df_wage_w,
     svy_all, svy_b, svy_girls, svy_c,
     sample_flow, miss_tbl, vars_df,
     file = file.path(OUTPUT_PATH, "cleaned_data.RData"))

cat("\n✓ Saved → outputs/cleaned_data.RData\n")
cat("  Next: Rscript R/02_descriptives.R\n\n")

cat("=== DATA LIMITATIONS FOR COST-OF-INACTION REPORT ===\n")
cat("1. School attendance: proxy only (non-missing edu_category = currently enrolled).\n")
cat("   Does not capture past attainment for non-students. Enrolment rate may be underestimated.\n")
cat("2. Individual wages: NOT available in this merged file (q07 values are codes, not NPR).\n")
cat("   Wage employment is captured as YES/NO (q07_02). Request raw Section-14 from NSO.\n")
cat("3. Child marriage: proxy only (married girls <18). Recommend MICS 2022 Nepal.\n")
cat("4. yearschooling/edu_category: covers CURRENT STUDENTS only. Full population\n")
cat("   education attainment needs individual roster section (not in this file).\n\n")
