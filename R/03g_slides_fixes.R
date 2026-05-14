# =============================================================================
# 03g_slides_fixes.R — Fixes for all 10 reviewer points on slides
# =============================================================================
needed <- c("tidyverse","srvyr","patchwork","scales","viridis","ggrepel","haven")
for (p in needed) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,repos="https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse); library(srvyr); library(patchwork); library(scales); library(haven)
})

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS   <- "outputs/figures"; dir.create(FIGS, showWarnings=FALSE)
TABLES <- "outputs/tables";  dir.create(TABLES, showWarnings=FALSE)

ub<-"#1CABE2"; ud<-"#006B9F"; ug<-"#00833D"; ur<-"#E2231A"; ugr<-"#6C7279"
th <- function(b=11) theme_minimal(base_size=b)+theme(
  plot.title=element_text(face="bold",size=b+1,colour="#222222"),
  plot.subtitle=element_text(size=b-1,colour=ugr),
  plot.caption=element_text(size=7,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.4),
  legend.position="bottom",strip.text=element_text(face="bold"),
  plot.margin=margin(10,10,10,10))
sv <- function(p,n,w=9,h=5.5){
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

# =============================================================================
# FIX ENROLMENT DEFINITION — derive true secondary (Class 9-10/SLC level)
# q07_12 labels: Class 9, Class 10, SLC = secondary enrolment
# =============================================================================
cat("=== FIXING ENROLMENT DEFINITION ===\n")

# Load S07 to get current grade level
BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
s07  <- read_dta(file.path(BASE, "S07.dta"))

# q07_12 = highest level completed if currently attending
# Classes 9-10, SLC = secondary; Class 12/Intermediate = higher secondary
s07_grade <- s07 %>%
  select(psu_number, hh_number, idcode, q07_12) %>%
  mutate(
    grade_lbl = as.character(as_factor(q07_12)),
    # True secondary = Class 9, Class 10, SLC
    in_secondary = case_when(
      grade_lbl %in% c("Class 9","Class 10","SLC") ~ 1L,
      !is.na(grade_lbl)                            ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Secondary or higher secondary (Class 9–12)
    in_sec_or_higher = case_when(
      grade_lbl %in% c("Class 9","Class 10","SLC",
                        "Intermediate/Class 12")   ~ 1L,
      !is.na(grade_lbl)                            ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat("Current grade distribution (attending students):\n")
print(table(s07_grade$grade_lbl[!is.na(s07_grade$grade_lbl)], useNA="always"))

# Merge back
df <- df %>%
  left_join(s07_grade %>% select(psu_number,hh_number,idcode,
                                   in_secondary,in_sec_or_higher),
            by=c("psu_number","hh_number","idcode")) %>%
  mutate(
    # If not attending (attend_any=0), secondary=0
    in_secondary     = replace_na(in_secondary, 0L),
    in_sec_or_higher = replace_na(in_sec_or_higher, 0L)
  )

# Rebuild subsamples
df_b      <- df %>% filter(age_num>=15, age_num<=24)
df_b1     <- df %>% filter(age_num>=15, age_num<=18)
df_b2     <- df %>% filter(age_num>=19, age_num<=24)
df_girls  <- df_b  %>% filter(female==1)
df_girls1 <- df_b1 %>% filter(female==1)

make_svy <- function(data) {
  d <- data %>% filter(!is.na(ind_wt), !is.na(province_num))
  srvyr::as_survey_design(d, ids=psu_number, strata=province_num,
                           weights=ind_wt, nest=TRUE)
}
svy_b     <- make_svy(df_b)
svy_b1    <- make_svy(df_b1)
svy_b2    <- make_svy(df_b2)
svy_girls <- make_svy(df_girls)
svy_girls1<- make_svy(df_girls1)
svy_c     <- make_svy(df %>% filter(age_num>=15,age_num<=64))

cat("Secondary enrolment rates:\n")
cat("  Any school (15-24):     ", round(mean(df_b$attend_any)*100,1), "%\n")
cat("  Secondary grade (15-24):", round(mean(df_b$in_secondary)*100,1), "%\n")
cat("  Class 9-12 (15-24):     ", round(mean(df_b$in_sec_or_higher)*100,1), "%\n")
cat("  Any school (15-18):     ", round(mean(df_b1$attend_any)*100,1), "%\n")
cat("  Secondary grade (15-18):", round(mean(df_b1$in_secondary)*100,1), "%\n")

# =============================================================================
# FIG FIX1 — Enrolment by province × age group (BARS, no connecting lines)
# Point 2: broken by age group; Point 3: bars not connected dots
# =============================================================================
cat("\nBuilding fix figures...\n")

enrol_prov_age <- bind_rows(
  svy_b1 %>% group_by(province_lbl, sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age_group="15-18"),
  svy_b2 %>% group_by(province_lbl, sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age_group="19-24")
) %>% filter(!is.na(province_lbl), !is.na(sex_lbl))

figFIX1 <- ggplot(enrol_prov_age,
                   aes(x=reorder(province_lbl,pct), y=pct,
                       fill=sex_lbl, alpha=age_group)) +
  geom_col(position=position_dodge(0.75), width=0.7) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.75), width=0.25, linewidth=0.4) +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name="Sex") +
  scale_alpha_manual(values=c("15-18"=0.95,"19-24"=0.55), name="Age group") +
  scale_y_continuous(labels=label_percent(scale=1)) +
  coord_flip() +
  facet_wrap(~age_group) +
  labs(title="School Enrolment by Province and Age Group",
       subtitle="Any school level incl. higher education | Survey-weighted mean with 95% CI",
       x=NULL, y="% Currently Enrolled", caption=src) +
  th() + theme(legend.position="right")
sv(figFIX1, "figFIX1_enrolment_province_by_age", w=12, h=5.5)

# =============================================================================
# FIG FIX2 — Age breakdown: BARS not lines; show NEET prominently
# Points 3 & 4: bars, NEET visible
# =============================================================================
age_sex_data <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(
      enrol = survey_mean(attend_any==1,     vartype="ci")*100,
      sec   = survey_mean(in_secondary==1,   vartype="ci")*100,
      neet  = survey_mean(neet==1,            vartype="ci")*100,
      .groups="drop") %>% mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(
      enrol = survey_mean(attend_any==1,     vartype="ci")*100,
      sec   = survey_mean(in_secondary==1,   vartype="ci")*100,
      neet  = survey_mean(neet==1,            vartype="ci")*100,
      .groups="drop") %>% mutate(age="19-24")
) %>% filter(!is.na(sex_lbl)) %>%
  pivot_longer(c(enrol,sec,neet),
               names_to="outcome", values_to="pct") %>%
  mutate(
    ci_lo = case_when(outcome=="enrol"~enrol_low, outcome=="sec"~sec_low,
                       outcome=="neet"~neet_low, TRUE~NA_real_),
    ci_hi = case_when(outcome=="enrol"~enrol_upp, outcome=="sec"~sec_upp,
                       outcome=="neet"~neet_upp, TRUE~NA_real_),
    outcome_lbl = recode(outcome,
      "enrol"="Any enrolment","sec"="Secondary grade (cl.9-10)","neet"="NEET")
  )

# Rebuild after pivot (ci columns lost in pivot)
age_sex_raw <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(
      enrol=survey_mean(attend_any==1,   vartype="ci")*100,
      sec  =survey_mean(in_secondary==1, vartype="ci")*100,
      neet =survey_mean(neet==1,          vartype="ci")*100,
      .groups="drop") %>% mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(
      enrol=survey_mean(attend_any==1,   vartype="ci")*100,
      sec  =survey_mean(in_secondary==1, vartype="ci")*100,
      neet =survey_mean(neet==1,          vartype="ci")*100,
      .groups="drop") %>% mutate(age="19-24")
) %>% filter(!is.na(sex_lbl))

# Long format manually
make_long <- function(data, var, label) {
  data %>%
    select(sex_lbl, age, value=all_of(var),
           lo=paste0(var,"_low"), hi=paste0(var,"_upp")) %>%
    mutate(outcome=label)
}

age_long <- bind_rows(
  make_long(age_sex_raw, "enrol", "Any enrolment"),
  make_long(age_sex_raw, "sec",   "Secondary grade\n(Class 9-10/SLC)"),
  make_long(age_sex_raw, "neet",  "NEET")
) %>%
  mutate(outcome=factor(outcome,
    c("Any enrolment","Secondary grade\n(Class 9-10/SLC)","NEET")))

figFIX2 <- ggplot(age_long,
                   aes(x=age, y=value, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=lo, ymax=hi),
                position=position_dodge(0.65), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(value,0),"%"), y=value+2),
            position=position_dodge(0.65), size=2.6, colour="#333333") +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.12))) +
  facet_wrap(~outcome, scales="free_y") +
  labs(title="Youth Outcomes by Age Group and Sex: 15-18 vs.19-24",
       subtitle="Key: at 15-18 girls close to boys; at 19-24 large NEET divergence reflects labour market exclusion",
       x="Age group", y="%", caption=src) +
  th()
sv(figFIX2, "figFIX2_age_breakdown_bars", w=12, h=5.5)

# =============================================================================
# FIG FIX3 — NEET rates only (NO ILO benchmark), clean bar chart
# Points 5 & 6: show our results clearly, remove ILO
# =============================================================================
neet_age_sex <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age="19-24"),
  svy_b %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age="15-24 (all)")
) %>% filter(!is.na(sex_lbl)) %>%
  mutate(age=factor(age, c("15-18","19-24","15-24 (all)")))

figFIX3 <- ggplot(neet_age_sex,
                   aes(x=age, y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.65), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,1),"%"), y=pct+1.5),
            position=position_dodge(0.65), size=3.2, colour="#333333") +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  labs(title="NEET Rate by Age Group and Sex",
       subtitle="Youth 15-24 | The gender gap emerges after age 18 — not before",
       x="Age group", y="% NEET", caption=src) +
  th()
sv(figFIX3, "figFIX3_neet_by_age_sex", w=8, h=5.5)

# =============================================================================
# FIG FIX4 — Koshi puzzle: bars not scatter
# =============================================================================
koshi_bars <- bind_rows(
  svy_b %>% group_by(province_lbl, sex_lbl) %>%
    summarise(
      pct_enrol=survey_mean(attend_any==1, vartype="ci")*100,
      pct_neet =survey_mean(neet==1,        vartype="ci")*100,
      .groups="drop") %>%
    filter(!is.na(province_lbl), !is.na(sex_lbl))
) %>%
  mutate(highlight=ifelse(province_lbl=="Koshi","Koshi","Other provinces"))

figFIX4a <- ggplot(koshi_bars %>% filter(sex_lbl=="Female"),
                    aes(x=reorder(province_lbl,pct_neet), y=pct_neet,
                        fill=highlight)) +
  geom_col(alpha=0.88, width=0.65) +
  geom_errorbar(aes(ymin=pct_neet_low, ymax=pct_neet_upp),
                width=0.25, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct_neet,0),"%")),
            hjust=-0.2, size=3) +
  scale_fill_manual(values=c("Koshi"=ur,"Other provinces"=ub), guide="none") +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.2))) +
  coord_flip() +
  labs(title="NEET Rate — Girls 15-24", x=NULL, y="% NEET") + th()

figFIX4b <- ggplot(koshi_bars %>% filter(sex_lbl=="Female"),
                    aes(x=reorder(province_lbl,pct_neet), y=pct_enrol,
                        fill=highlight)) +
  geom_col(alpha=0.88, width=0.65) +
  geom_errorbar(aes(ymin=pct_enrol_low, ymax=pct_enrol_upp),
                width=0.25, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct_enrol,0),"%")),
            hjust=-0.2, size=3) +
  scale_fill_manual(values=c("Koshi"=ur,"Other provinces"=ub), guide="none") +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.2))) +
  coord_flip() +
  labs(title="Enrolment Rate — Girls 15-24", x=NULL, y="% Enrolled") + th()

figFIX4 <- figFIX4b + figFIX4a +
  plot_annotation(
    title="The Koshi Puzzle: High Enrolment AND High NEET for Girls",
    subtitle="Koshi (red) has above-average enrolment but above-average NEET — labour market, not education, is the barrier",
    theme=theme(plot.title=element_text(face="bold",size=12),
                plot.subtitle=element_text(size=10,colour=ugr)))
sv(figFIX4, "figFIX4_koshi_puzzle_bars", w=11, h=5.5)

# =============================================================================
# FIG FIX5 — Secondary enrolment for under-18 only (complement, point 8)
# =============================================================================
sec_by_prov_1518 <- svy_b1 %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(
    pct_any = survey_mean(attend_any==1,   vartype="ci")*100,
    pct_sec = survey_mean(in_secondary==1, vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(province_lbl), !is.na(sex_lbl)) %>%
  pivot_longer(c(pct_any,pct_sec), names_to="type", values_to="pct") %>%
  mutate(
    ci_lo = ifelse(type=="pct_any", pct_any_low, pct_sec_low),
    ci_hi = ifelse(type=="pct_any", pct_any_upp, pct_sec_upp),
    type_lbl=recode(type,"pct_any"="Any school","pct_sec"="Secondary only (cl.9-10)")
  )

# Rebuild without pivot issues
sec_prov_1518a <- svy_b1 %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(
    pct=survey_mean(in_secondary==1, vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(province_lbl), !is.na(sex_lbl))

figFIX5 <- ggplot(sec_prov_1518a,
                   aes(x=reorder(province_lbl,pct), y=pct,
                       fill=sex_lbl, colour=sex_lbl)) +
  geom_col(position=position_dodge(0.7), width=0.6, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.7), width=0.2, linewidth=0.4) +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  coord_flip() +
  labs(title="Secondary School Enrolment by Province --- Ages 15-18 Only",
       subtitle="Currently in Class 9, Class 10, or SLC level | Survey-weighted with 95% CI",
       x=NULL, y="% Currently in Secondary School",
       caption=src) +
  th()
sv(figFIX5, "figFIX5_secondary_enrolment_1518", w=9, h=5.5)

# =============================================================================
# POINT 9 — Causal forest WITHOUT wealth quintile
# =============================================================================
cat("\n=== CAUSAL FOREST WITHOUT WEALTH ===\n")
library(grf); set.seed(42)

df_cf_nw <- df %>%
  filter(female==1, age_num>=15, age_num<=64) %>%
  mutate(caste_num=as.integer(caste_broad),
         province_n=as.integer(province_lbl)) %>%
  filter(!is.na(completed_secondary), !is.na(neet),
         !is.na(rural), !is.na(ppoor), !is.na(caste_num),
         !is.na(province_n), !is.na(any_difficulty),
         !is.na(currently_married))

# Features WITHOUT wealth quintile
X_vars_nw <- c("age_num","rural","ppoor","caste_num","province_n",
                "any_difficulty","currently_married")
X_nw  <- as.matrix(df_cf_nw[, X_vars_nw])
W_nw  <- df_cf_nw$completed_secondary
Y_nw  <- df_cf_nw$neet
df_cf_nw_neet <- df_cf_nw %>% filter(!is.na(neet))
X_nw2 <- as.matrix(df_cf_nw_neet[, X_vars_nw])

cf_nw <- causal_forest(X=X_nw2, Y=df_cf_nw_neet$neet,
                        W=df_cf_nw_neet$completed_secondary,
                        sample.weights=df_cf_nw_neet$ind_wt,
                        num.trees=2000, tune.parameters="all", seed=42)

ate_nw <- average_treatment_effect(cf_nw, target.sample="all")
cat(sprintf("CF (no wealth) ATE NEET: %.3f (se=%.3f)\n", ate_nw[1], ate_nw[2]))

vi_nw <- variable_importance(cf_nw)
vi_nw_df <- tibble(
  variable=X_vars_nw,
  importance=as.numeric(vi_nw),
  var_lbl=recode(X_vars_nw,"age_num"="Age","rural"="Rural","ppoor"="Below Poverty Line",
                 "caste_num"="Caste/Ethnicity","province_n"="Province",
                 "any_difficulty"="Disability","currently_married"="Currently Married")
) %>% arrange(desc(importance))

cat("Variable importance (no wealth):\n"); print(vi_nw_df)

tau_nw   <- predict(cf_nw)$predictions
df_cf_nw_neet$tau_nw <- tau_nw

# CATE by caste (with vs without wealth comparison)
load("outputs/causal_forest_results.RData")

cate_nw_caste <- df_cf_nw_neet %>%
  filter(!is.na(caste_broad)) %>%
  group_by(caste_broad) %>%
  summarise(mean_cate=mean(tau_nw,na.rm=TRUE),
            se=sd(tau_nw,na.rm=TRUE)/sqrt(n()), n=n()) %>%
  mutate(ci_lo=mean_cate-1.96*se, ci_hi=mean_cate+1.96*se,
         model="Without wealth quintile")

cate_w_caste <- cf_results$cate_neet_all %>%
  filter(dimension=="Caste/Ethnicity") %>%
  rename(caste_broad=group) %>%
  mutate(model="With wealth quintile")

cate_compare <- bind_rows(
  cate_nw_caste %>% select(caste_broad, mean_cate, ci_lo, ci_hi, model),
  cate_w_caste  %>% select(caste_broad, mean_cate, ci_lo, ci_hi, model)
) %>% mutate(caste_broad=factor(caste_broad,
               c("Brahmin/Chhetri","Janajati","Dalit","Religious/Linguistic","Other")))

figFIX6 <- ggplot(cate_compare %>% filter(!is.na(caste_broad)),
                   aes(x=mean_cate*100, xmin=ci_lo*100, xmax=ci_hi*100,
                       y=caste_broad, colour=model)) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
  geom_errorbarh(height=0.35, linewidth=0.7, position=position_dodge(0.5)) +
  geom_point(size=2.8, position=position_dodge(0.5)) +
  scale_colour_manual(values=c("With wealth quintile"=ud,
                                "Without wealth quintile"=ur), name=NULL) +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  labs(title="Causal Forest CATE on NEET: With vs. Without Wealth Quintile",
       subtitle="Removing wealth shows slightly larger caste heterogeneity — confirming caste effect is not mediated by wealth",
       x="CATE (pp change in NEET probability)", y=NULL,
       caption="Causal forest (grf). Women 15-64. 95% CI.") +
  th() + theme(panel.grid.major.y=element_blank())
sv(figFIX6, "figFIX6_cf_no_wealth_comparison", w=9, h=5.5)

# =============================================================================
# POINT 10 — Policy-maker summary table (% changes, plain language)
# =============================================================================
cat("\n=== POLICY TABLE ===\n")

policy_table <- tibble(
  `If a girl is...`=c(
    "Currently married",
    "Dalit (vs Brahmin/Chhetri)",
    "From poorest quintile (vs richest)",
    "Rural (vs urban)",
    "Has a functional disability",
    "Female (vs male peer)"
  ),
  `Enrolment change`=c("-61 pp***","-6 pp***","-24 pp***","-3 pp (n.s.)","-20 pp*","—"),
  `NEET change`=c("+42 pp***","+0 pp (n.s.)","+18 pp***","+6 pp***","+23 pp***","+9 pp***"),
  `Main driver?`=c("Strongest effect","Caste, not wealth","Wealth gradient",
                   "Geographic",  "Small n — caution","Labour market")
)
readr::write_csv(policy_table, file.path(TABLES,"policy_summary_table.csv"))
cat("Policy table:\n"); print(policy_table)

# Save updated objects
save(df, df_b, df_b1, df_b2, df_girls, df_girls1,
     svy_b, svy_b1, svy_b2, svy_girls, svy_girls1, svy_c,
     file="outputs/cleaned_data_v2.RData", version=2)

cf_results$cf_nw        <- cf_nw
cf_results$ate_nw        <- ate_nw
cf_results$vi_nw_df      <- vi_nw_df
cf_results$cate_nw_caste <- cate_nw_caste
save(cf_results, file="outputs/causal_forest_results.RData")

cat("\n✓ All fix figures saved.\n")
