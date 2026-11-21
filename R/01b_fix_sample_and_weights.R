# =============================================================================
# 01b_fix_sample_and_weights.R
# Run after the cleaning scripts and before any analysis script.
# Two corrections to the cleaned file, then every analysis sample is rebuilt:
#   1. Sample. The NLSS IV roster (S01) also lists people who are not household
#      members (q01_09 = No, mostly absentees). The education and employment
#      modules were not asked of them, so they are dropped.
#   2. Weight. ind_wt in the poverty file is a household-level population weight
#      (hhs_wt x hhsize). Each member of a household represents hhs_wt people,
#      so the person-level weight is hhs_wt. ind_wt is reset to hhs_wt and the
#      original is kept as ind_wt_hh.
# It also adds years of schooling (edu_yrs, 0 for those who never attended) and
# potential experience for the wage equations.
# Safe to re-run: it always starts from the as-built copy of the cleaned file.
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(haven); library(srvyr) })
RD  <- "outputs/cleaned_data_v2.RData"
BAK <- "outputs/cleaned_data_v2_asbuilt.RData"
RAW <- "data/NLSSIV_microdata/data/stata format"
TAB <- "outputs/tables"; dir.create(TAB, showWarnings = FALSE, recursive = TRUE)
big <- function(x) format(round(x), big.mark = ",")

cur <- new.env(); load(RD, envir = cur)
if (!"ind_wt_hh" %in% names(cur$df)) {            # cleaned file not corrected yet: keep a copy of it as built
  stopifnot(file.copy(RD, BAK, overwrite = TRUE)); src <- cur
  cat("As-built copy kept at", BAK, "\n")
} else {                                          # already corrected: start again from the as-built copy
  stopifnot(file.exists(BAK)); src <- new.env(); load(BAK, envir = src)
  cat("Starting from the as-built copy", BAK, "\n")
}
df0 <- src$df
stopifnot(all(c("psu_number", "hh_number", "idcode", "hhs_wt", "hhsize", "ind_wt", "age_num", "female") %in% names(df0)))

# ---- membership and schooling status from the raw files ----------------------
KEY <- c("psu_number", "hh_number", "idcode")
kid <- function(d) do.call(paste, c(lapply(d[KEY], function(x) as.character(haven::zap_labels(x))), sep = "_"))
lab <- function(x) toupper(trimws(as.character(haven::as_factor(x))))
s01 <- read_dta(file.path(RAW, "S01.dta")); names(s01) <- tolower(names(s01))
s07 <- read_dta(file.path(RAW, "S07.dta")); names(s07) <- tolower(names(s07))
k0 <- kid(df0)
if (anyDuplicated(k0)) cat("NOTE: duplicated person keys in the cleaned file:", sum(duplicated(k0)), "\n")
df0$hh_member  <- as.integer(lab(s01$q01_09)[match(k0, kid(s01))] == "YES")
df0$school_status <- lab(s07$q07_04)[match(k0, kid(s07))]
stopifnot(!anyNA(df0$hh_member), mean(df0$hh_member) > 0.6, mean(df0$hh_member) < 0.98)
stopifnot(max(abs(df0$ind_wt / (df0$hhs_wt * df0$hhsize) - 1), na.rm = TRUE) < 1e-6)   # ind_wt is hhs_wt x hhsize

# ---- corrected person file ----------------------------------------------------
df <- df0 %>%
  filter(hh_member == 1) %>%
  mutate(ind_wt_hh = ind_wt, ind_wt = hhs_wt,
         never_attended = as.integer(school_status %in% "NEVER ATTENDED SCHOOL"),
         edu_yrs = case_when(as.character(edu_cat) == "No education"     ~  0,
                             as.character(edu_cat) == "Primary"          ~  5,
                             as.character(edu_cat) == "Lower secondary"  ~  8,
                             as.character(edu_cat) == "Secondary"        ~ 10,
                             as.character(edu_cat) == "Higher secondary" ~ 12,
                             as.character(edu_cat) == "Tertiary"         ~ 16,
                             never_attended == 1                         ~  0,
                             TRUE ~ NA_real_),
         experience = pmax(age_num - edu_yrs - 6, 0))

cat("\n=== Sample and weights ===\n")
cat(sprintf("Roster rows: %s | household members kept: %s (%.1f%%) | dropped: %s\n",
            big(nrow(df0)), big(nrow(df)), 100 * nrow(df) / nrow(df0), big(nrow(df0) - nrow(df))))
cat(sprintf("Sum of person weights over members: %s (Census 2021: 29,164,578) | as built: %s\n",
            big(sum(df$ind_wt, na.rm = TRUE)), big(sum(df0$ind_wt, na.rm = TRUE))))
cat(sprintf("Females: %s (Census 2021: 14,911,027) | girls 15-24: %s\n",
            big(sum(df$ind_wt[df$female == 1], na.rm = TRUE)),
            big(sum(df$ind_wt[df$female == 1 & df$age_num >= 15 & df$age_num <= 24], na.rm = TRUE))))
stopifnot(sum(df$ind_wt, na.rm = TRUE) > 2.4e7, sum(df$ind_wt, na.rm = TRUE) < 3.4e7)

# ---- analysis samples (same definitions as the cleaning scripts) -------------
df_a      <- df %>% filter(age_num >= 10, age_num <= 19)
df_b      <- df %>% filter(age_num >= 15, age_num <= 24)
df_b1     <- df %>% filter(age_num >= 15, age_num <= 18)
df_b2     <- df %>% filter(age_num >= 19, age_num <= 24)
df_girls  <- df_b  %>% filter(female == 1)
df_girls1 <- df_b1 %>% filter(female == 1)
df_girls2 <- df_b2 %>% filter(female == 1)
df_c      <- df %>% filter(age_num >= 15, age_num <= 64)
df_wage_w <- df_c %>% filter(female == 1, !is.na(log_wage))
df_wage_w_sal <- if ("cond" %in% names(df)) df_wage_w %>% filter(cond == "Long term salary") else df_wage_w[0, ]

# the rebuilt samples must be the as-built samples minus non-members
for (nm in intersect(c("df_b", "df_b1", "df_b2", "df_girls", "df_girls1", "df_girls2", "df_c"), ls(src))) {
  old <- kid(src[[nm]]); new <- kid(get(nm)); mem <- k0[df0$hh_member == 1]
  if (!setequal(intersect(old, mem), new))
    cat(sprintf("  NOTE %s: rebuilt sample differs from as-built members (%d vs %d rows)\n", nm, length(new), length(intersect(old, mem))))
}
flow <- tibble(sample = c("Roster (all listed persons)", "Household members", "Youth 15-24", "  Girls 15-24", "  Boys 15-24",
                          "  Girls 15-18", "  Girls 19-24", "Working age 15-64", "  Women 15-64", "  Women 15-64 with wage earnings"),
               n_as_built = c(nrow(df0), NA, sum(df0$age_num >= 15 & df0$age_num <= 24, na.rm = TRUE),
                              sum(df0$age_num >= 15 & df0$age_num <= 24 & df0$female == 1, na.rm = TRUE),
                              sum(df0$age_num >= 15 & df0$age_num <= 24 & df0$female == 0, na.rm = TRUE), NA, NA,
                              sum(df0$age_num >= 15 & df0$age_num <= 64, na.rm = TRUE), NA, NA),
               n = c(nrow(df0), nrow(df), nrow(df_b), nrow(df_girls), sum(df_b$female == 0, na.rm = TRUE), nrow(df_girls1), nrow(df_girls2),
                     nrow(df_c), sum(df_c$female == 1, na.rm = TRUE), nrow(df_wage_w)))
print(as.data.frame(flow), right = FALSE)
readr::write_csv(flow, file.path(TAB, "sample_flow_corrected.csv"))

# ---- survey designs -----------------------------------------------------------
make_svy <- function(d) srvyr::as_survey_design(d %>% filter(!is.na(ind_wt), !is.na(province_num)),
                                                ids = psu_number, strata = province_num, weights = ind_wt, nest = TRUE)
svy_all <- make_svy(df); svy_b <- make_svy(df_b); svy_b1 <- make_svy(df_b1); svy_b2 <- make_svy(df_b2)
svy_girls <- make_svy(df_girls); svy_girls1 <- make_svy(df_girls1); svy_girls2 <- make_svy(df_girls2); svy_c <- make_svy(df_c)

save(df, df_a, df_b, df_b1, df_b2, df_girls, df_girls1, df_girls2, df_c, df_wage_w, df_wage_w_sal,
     svy_all, svy_b, svy_b1, svy_b2, svy_girls, svy_girls1, svy_girls2, svy_c, flow, file = RD)
cat("\nSaved corrected", RD, "\n=== DONE ===\n")
