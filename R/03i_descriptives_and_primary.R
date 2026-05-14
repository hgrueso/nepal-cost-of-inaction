# =============================================================================
# 03i_descriptives_and_primary.R — Descriptive stats table + primary enrolment
# =============================================================================
library(tidyverse); library(srvyr); library(scales)
load("outputs/cleaned_data_v2.RData")

FIGS   <- "outputs/figures"; TABLES <- "outputs/tables"
ud<-"#006B9F"; ub<-"#1CABE2"; ugr<-"#6C7279"
th <- function(b=10) theme_minimal(base_size=b)+theme(
  plot.title=element_text(face="bold",size=b+1),
  plot.subtitle=element_text(size=b-1,colour=ugr),
  plot.caption=element_text(size=6.5,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.35),
  legend.position="bottom", strip.text=element_text(face="bold"))
sv <- function(p,n,w=9,h=4.5){
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

# =============================================================================
# 1. DESCRIPTIVE STATISTICS TABLE — youth 15-24 by sex
# =============================================================================
cat("=== DESCRIPTIVE STATS ===\n")

# Key variables for youth 15-24
desc_vars <- list(
  list(var="attend_any",    label="Currently enrolled (any level)",      fmt="pct"),
  list(var="in_secondary",  label="Currently in secondary (cl.9-10/SLC)", fmt="pct"),
  list(var="attend_higher", label="Currently in higher education",        fmt="pct"),
  list(var="neet",          label="NEET",                                 fmt="pct"),
  list(var="employed_ilo",  label="Employed (ILO 7-day)",                  fmt="pct"),
  list(var="wage_emp_7d",   label="Wage employed (last 7 days)",          fmt="pct"),
  list(var="ppoor",         label="Below poverty line",                   fmt="pct"),
  list(var="quintile_n",    label="Wealth quintile (1=poorest)",          fmt="mean"),
  list(var="literate",      label="Literate",                             fmt="pct"),
  list(var="currently_married", label="Currently married",                fmt="pct"),
  list(var="child_marriage",    label="Child marriage proxy (girls 10-18 only)", fmt="pct"),
  list(var="any_difficulty",    label="Any functional difficulty (WG)",   fmt="pct"),
  list(var="rural",         label="Rural",                                fmt="pct"),
  list(var="age_num",       label="Age (years)",                          fmt="mean")
)

compute_desc <- function(data, sex_label) {
  lapply(desc_vars, function(v) {
    x <- data[[v$var]]
    x_num <- as.numeric(x)
    valid  <- x_num[!is.na(x_num)]
    if (length(valid) == 0) return(NULL)
    m  <- mean(valid, na.rm=TRUE)
    sd <- sd(valid,   na.rm=TRUE)
    n  <- sum(!is.na(x_num))
    tibble(
      Variable = v$label,
      Mean = if(v$fmt=="pct") paste0(round(m*100,1),"%") else as.character(round(m,2)),
      SD   = if(v$fmt=="pct") paste0(round(sd*100,1),"%") else as.character(round(sd,2)),
      N    = as.integer(n),
      Sex  = sex_label
    )
  }) %>% bind_rows()
}

girls_desc <- compute_desc(df_b %>% filter(female==1), "Girls")
boys_desc  <- compute_desc(df_b %>% filter(female==0), "Boys")

desc_tbl <- left_join(
  girls_desc %>% select(Variable, `Girls Mean`=Mean, `Girls SD`=SD, `Girls N`=N),
  boys_desc  %>% select(Variable, `Boys Mean`=Mean,  `Boys SD`=SD,  `Boys N`=N),
  by="Variable"
)
cat("\nDescriptive statistics (youth 15-24):\n")
print(desc_tbl)
readr::write_csv(desc_tbl, file.path(TABLES, "descriptive_stats_youth.csv"))

# =============================================================================
# 2. PRIMARY ENROLMENT BY PROVINCE — to check migration hypothesis
# Primary = currently in class 1-8
# =============================================================================
cat("\n=== PRIMARY ENROLMENT (sanity check for migration) ===\n")

# Derive primary enrolment (Class 1-8, still attending)
BASE <- file.path(getwd(), "data", "NLSSIV_microdata", "data", "stata format")
s07  <- haven::read_dta(file.path(BASE, "S07.dta"))
s07_primary <- s07 %>%
  select(psu_number, hh_number, idcode, q07_12) %>%
  mutate(
    grade_lbl = as.character(haven::as_factor(q07_12)),
    in_primary = case_when(
      grade_lbl %in% c("Pre-school/kindergarden",
                        "Class 1","Class 2","Class 3","Class 4","Class 5",
                        "Class 6","Class 7","Class 8") ~ 1L,
      !is.na(grade_lbl) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

df <- df %>%
  select(-any_of("in_primary")) %>%
  left_join(s07_primary %>% select(psu_number,hh_number,idcode,in_primary),
            by=c("psu_number","hh_number","idcode")) %>%
  mutate(in_primary = replace_na(in_primary, 0L))

df_b <- df %>% filter(age_num>=15, age_num<=24)
df_b1<- df %>% filter(age_num>=15, age_num<=18)

# Migration sanity check:
# If boys leave for work, we expect: older youth (19-24) boys have LOWER primary/secondary enrolment
# but the gap widens with age — boys drop out faster than girls as age increases
cat("\nEnrolment rates by age and sex:\n")
df_b %>% filter(!is.na(sex_lbl)) %>%
  group_by(sex_lbl, age_cat) %>%
  summarise(
    pct_any    = round(mean(attend_any)*100,1),
    pct_sec    = round(mean(in_secondary)*100,1),
    pct_primary= round(mean(in_primary)*100,1),
    pct_neet   = round(mean(neet,na.rm=TRUE)*100,1),
    pct_employed = round(mean(employed_ilo,na.rm=TRUE)*100,1),
    n=n(), .groups="drop"
  ) %>% print(n=Inf)

cat("\nNote: if boys leave for work, 19-24 male employment >> female employment\n")

# Make survey objects with in_primary
svy_b <- srvyr::as_survey_design(
  df_b %>% filter(!is.na(ind_wt), !is.na(province_num)),
  ids=psu_number, strata=province_num, weights=ind_wt, nest=TRUE)

svy_b1 <- srvyr::as_survey_design(
  df_b1 %>% filter(!is.na(ind_wt), !is.na(province_num)),
  ids=psu_number, strata=province_num, weights=ind_wt, nest=TRUE)

# Primary enrolment by province (all ages 15-24)
prim_prov <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct=survey_mean(in_primary==1, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

figPRIM <- ggplot(prim_prov,
                   aes(x=reorder(as.character(province_lbl), pct),
                       y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.72), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,0),"%"), y=pct+0.5),
            position=position_dodge(0.72), size=2.5, vjust=0) +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.2))) +
  coord_flip() +
  labs(title="Primary/Lower Secondary Enrolment by Province",
       subtitle="Youth 15-24 currently in Class 1-8 | Dark blue = Male, Light blue = Female",
       x=NULL, y="% Currently in Primary/Lower Secondary",
       caption=paste(src,"Low rates expected since most 15-24 year olds have passed primary level.")) +
  th()
sv(figPRIM, "figPRIM_primary_enrolment_province", w=9, h=5)

# Any enrolment by province - side by side comparison
any_prov <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct=survey_mean(attend_any==1, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

fig_compare <- bind_rows(
  any_prov %>% mutate(type="Any school (incl. HE)"),
  prim_prov %>% mutate(type="Primary/Lower sec (cl.1-8)")
)

figENROL_COMPARE <- ggplot(fig_compare %>% filter(sex_lbl=="Female"),
                             aes(x=reorder(as.character(province_lbl), pct),
                                 y=pct, fill=type)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  scale_fill_manual(values=c("Any school (incl. HE)"=ud,
                              "Primary/Lower sec (cl.1-8)"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  coord_flip() +
  labs(title="Girls Enrolment: Any School vs Primary/Lower Secondary",
       subtitle="Girls 15-24 | Most enrolment at this age is in secondary or higher education",
       x=NULL, y="% Currently Enrolled", caption=src) +
  th()
sv(figENROL_COMPARE, "figENROL_level_comparison", w=9, h=5)

cat("\nMigration/work sanity check (boys 19-24 vs girls 19-24):\n")
df %>% filter(age_num>=19, age_num<=24, !is.na(sex_lbl)) %>%
  group_by(sex_lbl) %>%
  summarise(
    pct_employed  = round(mean(employed_ilo,  na.rm=TRUE)*100,1),
    pct_wage      = round(mean(wage_emp_7d,    na.rm=TRUE)*100,1),
    pct_neet      = round(mean(neet,            na.rm=TRUE)*100,1),
    pct_enrolled  = round(mean(attend_any)*100,1),
    n=n(), .groups="drop"
  ) %>% print()

# =============================================================================
# EMPLOYMENT COMPARISON PLOT — boys vs girls, youth 15-24
# =============================================================================
cat("\n=== EMPLOYMENT COMPARISON ===\n")

svy_b <- srvyr::as_survey_design(
  df_b %>% filter(!is.na(ind_wt), !is.na(province_num)),
  ids=psu_number, strata=province_num, weights=ind_wt, nest=TRUE)

# Key labour market outcomes by sex and age group
emp_compare <- bind_rows(
  svy_b %>% group_by(sex_lbl, age_cat) %>%
    summarise(
      employed   = survey_mean(employed_ilo==1, na.rm=TRUE, vartype="ci")*100,
      wage_emp   = survey_mean(wage_emp_7d==1,  na.rm=TRUE, vartype="ci")*100,
      neet       = survey_mean(neet==1,          na.rm=TRUE, vartype="ci")*100,
      enrolled   = survey_mean(attend_any==1,               vartype="ci")*100,
      .groups="drop"
    ) %>% filter(!is.na(sex_lbl), age_cat %in% c("15-18","19-24"))
)

# Long format for faceted plot
emp_long <- emp_compare %>%
  select(sex_lbl, age_cat,
         employed, employed_low, employed_upp,
         neet, neet_low, neet_upp,
         enrolled, enrolled_low, enrolled_upp) %>%
  pivot_longer(
    cols = c(employed, neet, enrolled),
    names_to = "outcome", values_to = "pct"
  ) %>%
  mutate(
    lo = case_when(outcome=="employed"~employed_low, outcome=="neet"~neet_low,
                    outcome=="enrolled"~enrolled_low),
    hi = case_when(outcome=="employed"~employed_upp, outcome=="neet"~neet_upp,
                    outcome=="enrolled"~enrolled_upp),
    outcome_lbl = recode(outcome,
      "employed"="Employed (ILO 7-day)",
      "neet"="NEET",
      "enrolled"="Enrolled in school"),
    outcome_lbl = factor(outcome_lbl,
      c("Enrolled in school","Employed (ILO 7-day)","NEET"))
  ) %>% filter(!is.na(pct))

figEMP <- ggplot(emp_long,
                  aes(x=age_cat, y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=lo, ymax=hi),
                position=position_dodge(0.65), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,0),"%"), y=pct+2),
            position=position_dodge(0.65), size=2.5, colour="#222222") +
  scale_fill_manual(values=c("Male"="#006B9F","Female"="#1CABE2"), name=NULL) +
  scale_y_continuous(labels=scales::label_percent(scale=1),
                     expand=expansion(mult=c(0,0.18))) +
  facet_wrap(~outcome_lbl) +
  labs(title="School, Employment and NEET by Sex and Age Group",
       subtitle="Youth 15-24 | Dark blue = Male, Light blue = Female | Survey-weighted with 95% CI",
       x="Age group", y="%",
       caption="Source: NLSS IV 2022/23. Survey weights applied. Key: boys leave school for employment; girls leave school into NEET.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_blank(),
        plot.subtitle=element_text(face="bold",size=10,colour="#222222"),
        plot.caption=element_text(size=6.5,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        strip.text=element_text(face="bold"))

ggsave("outputs/figures/figEMP_school_employment_neet.png",
       figEMP, width=11, height=4.5, dpi=300)
cat("  ✓ figEMP_school_employment_neet\n")
