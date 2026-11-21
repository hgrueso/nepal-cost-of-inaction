# 10_v5_comment_checks.R
# Numbers needed to answer the framing comments on report V5:
# what the secondary flag contains, who is still enrolled, NEET by sex and
# schooling, and the reasons girls give for leaving school.
suppressPackageStartupMessages({ library(tidyverse); library(haven) })
load("outputs/cleaned_data_v2.RData")
res <- tryCatch(readRDS("outputs/census_anchor_results.RData"), error = function(e) NULL)
N1524 <- if (!is.null(res)) res$N_f1524_census else 3214885
TAB <- "outputs/tables"
wm <- function(x, w) 100 * weighted.mean(x, w, na.rm = TRUE)
add_groups <- function(d) d %>%
  filter(!is.na(ind_wt), !is.na(completed_secondary)) %>%
  mutate(agegrp = ifelse(age_num <= 18, "15-18", "19-24"),
         sex = ifelse(female == 1, "Women", "Men"),
         sec = ifelse(completed_secondary == 1, "Grade 9 or above", "Grade 8 or below"))

cat("\n=== 1. What the 'completed secondary' flag contains (girls 15-24) ===\n")
if ("edu_lbl_raw" %in% names(df_girls)) {
  df_girls %>% count(completed_secondary, edu_cat, edu_lbl_raw) %>% as.data.frame() %>% print(right = FALSE)
} else if ("edu_cat" %in% names(df_girls)) {
  df_girls %>% count(completed_secondary, edu_cat) %>% as.data.frame() %>% print(right = FALSE)
}

cat("\n=== 2. Girls 15-24 by schooling status ===\n")
g <- add_groups(df_girls) %>%
  mutate(status = case_when(completed_secondary == 1 ~ "Grade 9 or above",
                            attend_any == 1 ~ "Grade 8 or below, still enrolled",
                            TRUE ~ "Grade 8 or below, out of school"))
t2 <- bind_rows(g, g %>% mutate(agegrp = "15-24")) %>%
  group_by(agegrp, status) %>% summarise(n = n(), w = sum(ind_wt), .groups = "drop") %>%
  group_by(agegrp) %>% mutate(pct = round(100 * w / sum(w), 1)) %>% ungroup() %>% select(-w)
print(as.data.frame(t2), right = FALSE)
p_out <- sum(t2$pct[t2$agegrp == "15-24" & t2$status == "Grade 8 or below, out of school"])
p_in  <- sum(t2$pct[t2$agegrp == "15-24" & t2$status == "Grade 8 or below, still enrolled"])
cat(sprintf("Census-anchored girls 15-24 at grade 8 or below: %s, of which out of school: %s, still enrolled: %s\n",
            format(round(N1524 * (p_out + p_in) / 100), big.mark = ","),
            format(round(N1524 * p_out / 100), big.mark = ","),
            format(round(N1524 * p_in / 100), big.mark = ",")))
readr::write_csv(t2, file.path(TAB, "v5_check_schooling_status.csv"))

cat("\n=== 3. NEET, employment and enrolment by sex and schooling (%) ===\n")
y <- add_groups(df_b) %>% filter(!is.na(female))
if (!"employed_ilo" %in% names(y)) y$employed_ilo <- NA_real_
t3 <- y %>% group_by(agegrp, sec, sex) %>%
  summarise(n = n(), neet = round(wm(neet, ind_wt), 1), employed = round(wm(employed_ilo, ind_wt), 1),
            enrolled = round(wm(attend_any, ind_wt), 1), .groups = "drop")
print(as.data.frame(t3), right = FALSE)
cat("\n--- Ages 19-24, only those out of school ---\n")
t3b <- y %>% filter(agegrp == "19-24", attend_any == 0) %>% group_by(sec, sex) %>%
  summarise(n = n(), neet = round(wm(neet, ind_wt), 1), employed = round(wm(employed_ilo, ind_wt), 1), .groups = "drop")
print(as.data.frame(t3b), right = FALSE)
readr::write_csv(t3, file.path(TAB, "v5_check_neet_by_sex_schooling.csv"))

cat("\n=== 4. Why girls 15-24 left school (q07_08, raw S07) ===\n")
tryCatch({
  s07 <- read_dta("data/NLSSIV_microdata/data/stata format/S07.dta")
  names(s07) <- tolower(names(s07))
  key <- intersect(c("psu_number", "hh_number", "idcode"), intersect(names(s07), names(df_girls)))
  ch <- function(d) d %>% mutate(across(all_of(key), ~ as.character(haven::zap_labels(.x))))
  r <- df_girls %>% filter(!is.na(ind_wt), attend_any == 0) %>% select(-any_of("q07_08")) %>% ch() %>%
    left_join(s07 %>% select(all_of(key), q07_08) %>% ch(), by = key) %>%
    filter(!is.na(q07_08)) %>% mutate(reason = as.character(as_factor(q07_08)))
  t4 <- r %>% group_by(reason) %>% summarise(n = n(), w = sum(ind_wt), .groups = "drop") %>%
    mutate(pct = round(100 * w / sum(w), 1)) %>% arrange(desc(pct)) %>% select(-w)
  cat("Girls 15-24 out of school with a stated reason: n =", nrow(r), "\n")
  print(as.data.frame(t4), right = FALSE)
  readr::write_csv(t4, file.path(TAB, "v5_check_reasons_left_school.csv"))
}, error = function(e) cat("Could not compute reasons:", conditionMessage(e), "\n"))
cat("\n=== DONE ===\n")
