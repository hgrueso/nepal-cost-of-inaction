# =============================================================================
# 03m_boys_and_intersectionality.R
# 1. Boys' Mincer wage equation (returns to schooling, boys vs girls)
# 2. Quality of work by education level and sex
# 3. Interaction: child marriage × secondary education in LPM
# 4. Dalit: marriage × rural compound disadvantage
# 5. Religious/Linguistic NEET anomaly check
# =============================================================================
library(tidyverse); library(srvyr); library(scales); library(fixest); library(broom)
load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS <- "outputs/figures"; TABLES <- "outputs/tables"
ud<-"#006B9F"; ub<-"#1CABE2"; ur<-"#E2231A"; ug<-"#00833D"; ugr<-"#6C7279"
th <- function(b=10) theme_minimal(base_size=b) + theme(
  plot.title=element_blank(),
  plot.subtitle=element_text(face="bold", size=b, colour="#222222"),
  plot.caption=element_text(size=6.5, colour=ugr, hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB", linewidth=0.35),
  legend.position="bottom", strip.text=element_text(face="bold"))
sv <- function(p,n,w=9,h=4.5){
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

# =============================================================================
# 1. BOYS' MINCER — compare returns with girls
# =============================================================================
cat("=== BOYS MINCER ===\n")

# Salary workers — both sexes
df_sal_all <- df %>%
  filter(!is.na(monthly_wage), monthly_wage > 0,
         !is.na(edu_yrs_c), !is.na(experience),
         age_num >= 15, age_num <= 64) %>%
  mutate(edu_yrs_c = edu_yrs_c,
         experience2 = experience^2,
         log_wage = log(monthly_wage))

df_sal_girls <- df_sal_all %>% filter(female==1)
df_sal_boys  <- df_sal_all %>% filter(female==0)

cat("N salary workers — Girls:", nrow(df_sal_girls), "| Boys:", nrow(df_sal_boys), "\n")

# Mincer with province FE — girls
m_girls <- feols(log_wage ~ edu_yrs_c + experience + experience2 + rural |
                   province_lbl, data=df_sal_girls, se="hetero")
# Mincer with province FE — boys
m_boys  <- feols(log_wage ~ edu_yrs_c + experience + experience2 + rural |
                   province_lbl, data=df_sal_boys, se="hetero")

cat("\nGirls return/yr schooling:", round(coef(m_girls)["edu_yrs_c"]*100,1), "%\n")
cat("Boys  return/yr schooling:", round(coef(m_boys)["edu_yrs_c"]*100,1), "%\n")

# Save results
boys_mincer <- list(girls=m_girls, boys=m_boys)
save(boys_mincer, file="outputs/boys_mincer.RData")

# Figure: compare returns by sex
mincer_compare <- bind_rows(
  tidy(m_girls, conf.int=TRUE) %>% mutate(sex="Girls (n=952)"),
  tidy(m_boys,  conf.int=TRUE) %>% mutate(sex=paste0("Boys (n=",nrow(df_sal_boys),")"))
) %>%
  filter(term=="edu_yrs_c") %>%
  mutate(pct=estimate*100, lo=conf.low*100, hi=conf.high*100)

cat("\nMincer returns comparison:\n"); print(mincer_compare %>% select(sex,pct,lo,hi))

figMINCER_compare <- ggplot(mincer_compare,
    aes(x=sex, y=pct, fill=sex)) +
  geom_col(width=0.5, alpha=0.88) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.2, linewidth=0.5) +
  geom_text(aes(label=paste0(round(pct,1),"%"), y=hi+0.3), vjust=0, size=3.5) +
  scale_fill_manual(values=c("Girls (n=952)"=ub, "Boys (n=952)"=ud), guide="none") +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.2))) +
  labs(subtitle="Return per additional year of schooling | Province FE Mincer | Salary workers 15-64",
       x=NULL, y="Return per year of schooling (%)",
       caption=paste(src, "Log monthly wage outcome. HC1 SE.")) +
  th()
sv(figMINCER_compare, "figMINCER_boys_girls_compare", w=6, h=4)

# =============================================================================
# 2. QUALITY OF WORK BY EDUCATION AND SEX
# =============================================================================
cat("\n=== QUALITY OF WORK ===\n")

# Define job quality tiers using employment type
# S09: q09_02=worked for pay, q09_03=self-employed, q09_04=unpaid family
df_work <- df %>%
  filter(age_num>=15, age_num<=64, employed_ilo==1, !is.na(edu_cat)) %>%
  mutate(
    job_type = case_when(
      wage_emp_7d==1 ~ "Formal wage/salary",
      !is.na(self_emp) & self_emp==1 ~ "Self-employed",
      TRUE ~ "Unpaid/family labour"
    ),
    job_type = factor(job_type,
      c("Formal wage/salary","Self-employed","Unpaid/family labour")),
    edu_broad = case_when(
      edu_cat %in% c("None","Primary","Lower secondary") ~ "Low (below secondary)",
      edu_cat == "Secondary" ~ "Secondary",
      edu_cat %in% c("Higher secondary","Tertiary") ~ "Higher secondary+",
      TRUE ~ NA_character_
    ),
    edu_broad = factor(edu_broad,
      c("Low (below secondary)","Secondary","Higher secondary+"))
  ) %>% filter(!is.na(edu_broad), !is.na(job_type), !is.na(sex_lbl))

job_quality <- df_work %>%
  group_by(sex_lbl, edu_broad, job_type) %>%
  summarise(n=n(), .groups="drop") %>%
  group_by(sex_lbl, edu_broad) %>%
  mutate(pct=n/sum(n)*100) %>% ungroup()

cat("Job quality distribution:\n"); print(job_quality)
readr::write_csv(job_quality, file.path(TABLES,"job_quality_by_edu_sex.csv"))

figJOB_QUALITY <- ggplot(job_quality,
    aes(x=edu_broad, y=pct, fill=job_type)) +
  geom_col(position="stack", width=0.7, alpha=0.88) +
  geom_text(aes(label=ifelse(pct>8, paste0(round(pct,0),"%"), "")),
            position=position_stack(vjust=0.5), size=2.5, colour="white") +
  scale_fill_manual(values=c("Formal wage/salary"=ud,
                              "Self-employed"=ub,
                              "Unpaid/family labour"="#AAAAAA"), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  facet_wrap(~sex_lbl) +
  labs(subtitle="Employment type by education level and sex | Employed adults 15-64",
       x="Education level", y="% of employed workers",
       caption=paste(src,"Formal wage = employed for pay. Unpaid = family business/farm with no pay.")) +
  th() + theme(axis.text.x=element_text(angle=20, hjust=1))
sv(figJOB_QUALITY, "figJOB_quality_by_edu_sex", w=10, h=4.5)

# =============================================================================
# 3. CHILD MARRIAGE INTERACTION IN LPM
# =============================================================================
cat("\n=== CHILD MARRIAGE INTERACTION ===\n")

# Interaction: does completing secondary school reduce NEET MORE for married girls?
df_b_int <- df_b %>%
  filter(!is.na(neet), !is.na(completed_secondary), !is.na(currently_married),
         !is.na(quintile_n), !is.na(caste_broad), !is.na(rural))

m_interaction <- feols(
  neet ~ completed_secondary * currently_married +
    female + rural + ppoor + quintile_n + any_difficulty +
    caste_broad | province_lbl,
  data=df_b_int, se="hetero"
)
cat("Interaction model (NEET ~ secondary*married):\n")
print(tidy(m_interaction, conf.int=TRUE) %>%
      filter(grepl("secondary|married", term)) %>%
      mutate(across(c(estimate,conf.low,conf.high), ~round(.x*100,1))))

# =============================================================================
# 4. DALIT COMPOUND DISADVANTAGE: married + rural
# =============================================================================
cat("\n=== DALIT COMPOUND DISADVANTAGE ===\n")

dalit_tab <- df_girls %>%
  filter(!is.na(caste_broad)) %>%
  mutate(married_rural = case_when(
    currently_married==1 & rural==1 ~ "Married + Rural",
    currently_married==1 & rural==0 ~ "Married + Urban",
    currently_married==0 & rural==1 ~ "Unmarried + Rural",
    currently_married==0 & rural==0 ~ "Unmarried + Urban"
  )) %>%
  filter(caste_broad %in% c("Dalit","Brahmin/Chhetri","Janajati")) %>%
  group_by(caste_broad, married_rural) %>%
  summarise(n=n(), pct_neet=round(mean(neet,na.rm=TRUE)*100,1), .groups="drop") %>%
  group_by(caste_broad) %>%
  mutate(pct_group=round(n/sum(n)*100,1)) %>% ungroup()

cat("Dalit compound disadvantage:\n"); print(dalit_tab)
readr::write_csv(dalit_tab, file.path(TABLES,"dalit_compound_disadvantage.csv"))

figDALIT <- ggplot(dalit_tab %>% filter(!is.na(married_rural)),
    aes(x=married_rural, y=pct_neet, fill=caste_broad)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  geom_text(aes(label=paste0(round(pct_neet,0),"%"), y=pct_neet+2),
            position=position_dodge(0.72), size=2.5, vjust=0) +
  scale_fill_manual(values=c("Dalit"=ur,"Brahmin/Chhetri"=ud,"Janajati"=ug),
                    name="Caste") +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.18))) +
  labs(subtitle="NEET rate by marriage status, rural/urban, and caste | Girls 15-24",
       x=NULL, y="% NEET",
       caption=paste(src,"Compound disadvantage: Dalit + Married + Rural = highest NEET.")) +
  th() + theme(axis.text.x=element_text(angle=20,hjust=1))
sv(figDALIT, "figDALIT_compound_disadvantage", w=9, h=4.5)

# =============================================================================
# 5. RELIGIOUS/LINGUISTIC GROUP CHECK
# =============================================================================
cat("\n=== RELIGIOUS/LINGUISTIC NEET CHECK ===\n")

relig_check <- df_girls %>%
  filter(caste_broad=="Religious/Linguistic", !is.na(neet)) %>%
  summarise(
    n=n(),
    pct_neet=round(mean(neet,na.rm=TRUE)*100,1),
    pct_married=round(mean(currently_married,na.rm=TRUE)*100,1),
    pct_madhesh=round(mean(province_lbl=="Madhesh",na.rm=TRUE)*100,1),
    pct_lumbini=round(mean(province_lbl=="Lumbini",na.rm=TRUE)*100,1),
    pct_rural=round(mean(rural,na.rm=TRUE)*100,1),
    mean_age=round(mean(age_num,na.rm=TRUE),1)
  )
cat("Religious/Linguistic girls profile:\n"); print(relig_check)
cat("\nConclusion: High NEET likely explained by:\n")
cat("  - High proportion in Madhesh/Lumbini (Terai Muslim communities + hill Janajati)\n")
cat("  - High marriage rate concentrated in those provinces\n")
cat("  Note: n is small; interpret CI carefully\n")

cat("\n✓ All outputs saved.\n")
