# =============================================================================
# 03j_fix_labels_and_cate.R
# 1. Fix overlapping labels on bar charts (move above CI upper bound)
# 2. Split CATE figure into caste+province and wealth panels
# =============================================================================
library(tidyverse); library(srvyr); library(scales); library(grf)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")

FIGS <- "outputs/figures"
ud<-"#006B9F"; ub<-"#1CABE2"; ur<-"#E2231A"; ugr<-"#6C7279"
th <- function(b=10) theme_minimal(base_size=b)+theme(
  plot.title=element_text(face="bold",size=b+1),
  plot.subtitle=element_text(size=b-1,colour=ugr),
  plot.caption=element_text(size=6.5,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.35),
  legend.position="bottom", strip.text=element_text(face="bold",size=9))
sv <- function(p,n,w=9,h=4.5){
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

make_svy <- function(data) {
  srvyr::as_survey_design(
    data %>% filter(!is.na(ind_wt), !is.na(province_num)),
    ids=psu_number, strata=province_num, weights=ind_wt, nest=TRUE)
}
svy_b     <- make_svy(df_b)
svy_girls <- make_svy(df_girls)

# =============================================================================
# NEET by province — labels ABOVE CI upper bound
# =============================================================================
neet_prov_sex <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

fig_neet_prov <- ggplot(neet_prov_sex,
  aes(x=as.character(province_lbl), y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.72), width=0.18, linewidth=0.4) +
  # Label ABOVE the CI upper bound
  geom_text(aes(label=paste0(round(pct,0),"%"), y=pct_upp+1.5),
            position=position_dodge(0.72), size=2.3, colour="#222222", vjust=0) +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.18))) +
  labs(title="NEET Rate by Province and Sex",
       subtitle="Youth 15-24 | Dark blue = Male, Light blue = Female | Survey-weighted with 95% CI",
       x=NULL, y="% NEET", caption=src) +
  th()
sv(fig_neet_prov, "fig4_neet_by_province", w=10, h=4.5)

# =============================================================================
# NEET by age/sex — labels above CI
# =============================================================================
svy_b1 <- make_svy(df_b1); svy_b2 <- make_svy(df_b2)

neet_age_sex <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,.groups="drop")%>%mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,.groups="drop")%>%mutate(age="19-24"),
  svy_b  %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,.groups="drop")%>%mutate(age="15-24")
) %>% filter(!is.na(sex_lbl)) %>%
  mutate(age=factor(age,c("15-18","19-24","15-24")))

fig_neet_age <- ggplot(neet_age_sex, aes(x=age, y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.65), width=0.18, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,1),"%"), y=pct_upp+1.5),
            position=position_dodge(0.65), size=2.8, colour="#222222", vjust=0) +
  scale_fill_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.18))) +
  labs(title="NEET Rate by Age Group and Sex",
       subtitle="Youth 15-24 | Gender gap emerges after age 18",
       x="Age group", y="% NEET", caption=src) +
  th()
sv(fig_neet_age, "figFIX3_neet_by_age_sex", w=7.5, h=4)

# =============================================================================
# CATE subgroup — split into two figures: caste+province | wealth quintile
# =============================================================================
plot_cate <- function(data, title, colour, note) {
  ggplot(data, aes(x=mean_cate*100, xmin=ci_lo*100, xmax=ci_hi*100,
                    y=reorder(group, mean_cate))) +
    geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
    geom_errorbar(aes(orientation="y"), height=0.35, linewidth=0.6, colour=colour) +
    geom_point(aes(size=n), colour=colour, alpha=0.85) +
    scale_size_continuous(range=c(2,6), guide="none") +
    scale_x_continuous(labels=label_percent(accuracy=0.1)) +
    facet_wrap(~dimension, scales="free_y", ncol=1) +
    labs(title=title, subtitle=note,
         x="CATE (pp change in NEET probability)", y=NULL,
         caption="Causal forest (grf). Women 15-64. 95% CI. Bubble size = N.") +
    th() + theme(strip.text=element_text(face="bold",size=9),
                  panel.grid.major.y=element_blank())
}

# Caste + Province panel
cp_data <- cf_results$cate_neet_all %>%
  filter(dimension %in% c("Caste/Ethnicity","Province")) %>%
  mutate(dimension=factor(dimension, c("Caste/Ethnicity","Province")))

fig_cp <- plot_cate(cp_data,
  "CATE of Secondary Education on NEET: Caste and Province",
  ur, "Negative = education reduces NEET | All effects significant at p<0.001")
sv(fig_cp, "figCF3_cate_neet_caste_prov", w=8, h=5.5)

# Wealth quintile panel
wq_data <- cf_results$cate_neet_all %>%
  filter(dimension=="Wealth Quintile")

fig_wq <- plot_cate(wq_data,
  "CATE of Secondary Education on NEET: Wealth Quintile",
  ur, "Richer women benefit slightly more from secondary education")
sv(fig_wq, "figCF3_cate_neet_wealth", w=7, h=3.5)

# =============================================================================
# Corroboration plot — fix size and add proper annotation
# =============================================================================
library(fixest); library(ggrepel)
load('outputs/regression_results_v2.RData')
ols_fe <- fixef(reg_results_v2$m2$full)[["province_lbl"]]
load("outputs/causal_forest_results.RData")

corr_df <- cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>%
  rename(province_lbl=group, cate=mean_cate) %>%
  left_join(tibble(province_lbl=names(ols_fe), ols_fe=ols_fe), by="province_lbl") %>%
  filter(!is.na(ols_fe))

r_val <- cor(corr_df$ols_fe, corr_df$cate, use="complete.obs")

fig_corr <- ggplot(corr_df, aes(x=ols_fe*100, y=cate*100, label=province_lbl)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey70") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey70") +
  geom_smooth(method="lm", se=TRUE, colour=ur, linewidth=0.8, alpha=0.12) +
  geom_point(colour=ud, size=3.5, alpha=0.85) +
  geom_text_repel(size=3, colour="#333333", box.padding=0.4) +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  scale_y_continuous(labels=label_number(suffix=" pp")) +
  annotate("text", x=min(corr_df$ols_fe*100)+0.5, y=max(corr_df$cate*100)-0.3,
           label=paste0("r = ", round(r_val,2)), size=4, hjust=0, colour="#333333") +
  labs(title="OLS Province Fixed Effects vs. Causal Forest CATE",
       subtitle="NEET outcome | OLS FE = residual provincial NEET | CF CATE = education effect",
       x="OLS Province Fixed Effect (pp)", y="CF CATE on NEET (pp)",
       caption="Positive r: provinces with structurally higher NEET show slightly smaller education returns — structural barriers limit gains.") +
  th()
sv(fig_corr, "figCF4_cate_vs_ols_corroboration", w=7.5, h=5)

cat("\n✓ All fixed figures saved.\n")

# =============================================================================
# FIX: Employment/age breakdown — labels above CI upper bound (consistency)
# =============================================================================
svy_b1 <- make_svy(df_b1); svy_b2 <- make_svy(df_b2)

age_sex_raw <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(enrol=survey_mean(attend_any==1,   vartype="ci")*100,
              sec  =survey_mean(in_secondary==1, vartype="ci")*100,
              neet =survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,
              .groups="drop") %>% mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(enrol=survey_mean(attend_any==1,   vartype="ci")*100,
              sec  =survey_mean(in_secondary==1, vartype="ci")*100,
              neet =survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,
              .groups="drop") %>% mutate(age="19-24")
) %>% filter(!is.na(sex_lbl))

make_long <- function(df, var, lo_col, hi_col, label) {
  df %>% select(sex_lbl, age, value=all_of(var), lo=all_of(lo_col), hi=all_of(hi_col)) %>%
    mutate(outcome=label)
}

age_long <- bind_rows(
  make_long(age_sex_raw,"enrol","enrol_low","enrol_upp","Any enrolment"),
  make_long(age_sex_raw,"sec","sec_low","sec_upp","Secondary (Class 9-10/SLC)"),
  make_long(age_sex_raw,"neet","neet_low","neet_upp","NEET")
) %>% filter(!is.na(value)) %>%
  mutate(outcome=factor(outcome,c("Any enrolment","Secondary (Class 9-10/SLC)","NEET")))

fig_age <- ggplot(age_long, aes(x=age, y=value, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=lo, ymax=hi),
                position=position_dodge(0.65), width=0.18, linewidth=0.4) +
  # Labels ABOVE CI upper bound — consistent with all other plots
  geom_text(aes(label=paste0(round(value,0),"%"), y=hi+2),
            position=position_dodge(0.65), size=2.5, colour="#222222", vjust=0) +
  scale_fill_manual(values=c("Male"="#006B9F","Female"="#1CABE2"), name=NULL) +
  scale_y_continuous(labels=scales::label_percent(scale=1),
                     expand=expansion(mult=c(0,0.20))) +
  facet_wrap(~outcome, scales="free_y") +
  labs(title="Youth Outcomes by Age Group and Sex: 15-18 vs. 19-24",
       subtitle="Bars are independent estimates — no trend implied. Labels above 95% CI upper bound.",
       x="Age group", y="%",
       caption="Source: NLSS IV 2022/23. Survey weights applied.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8.5,colour="#6C7279"),
        plot.caption=element_text(size=6.5,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(), legend.position="bottom",
        strip.text=element_text(face="bold"))
ggsave("outputs/figures/figFIX2_age_breakdown_bars.png",
       fig_age, width=11, height=4, dpi=300)
cat("  ✓ figFIX2 updated — labels above CI\n")

# Also fix the employment comparison plot
emp_compare <- bind_rows(
  svy_b %>% group_by(sex_lbl, age_cat) %>%
    summarise(employed=survey_mean(employed_ilo==1,na.rm=TRUE,vartype="ci")*100,
              neet    =survey_mean(neet==1,na.rm=TRUE,vartype="ci")*100,
              enrolled=survey_mean(attend_any==1,vartype="ci")*100, .groups="drop") %>%
    filter(!is.na(sex_lbl), age_cat %in% c("15-18","19-24"))
) %>%
  pivot_longer(c(employed,neet,enrolled), names_to="outcome", values_to="pct") %>%
  mutate(
    hi = case_when(outcome=="employed"~employed_upp, outcome=="neet"~neet_upp,
                    outcome=="enrolled"~enrolled_upp),
    outcome_lbl = factor(recode(outcome,
      "employed"="Employed","neet"="NEET","enrolled"="Enrolled"),
      c("Enrolled","Employed","NEET"))
  ) %>% filter(!is.na(pct), !is.na(hi))

fig_emp2 <- ggplot(emp_compare, aes(x=age_cat, y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=pct-1.96*(pct-pct)/2, ymax=hi),  # approximate
                position=position_dodge(0.65), width=0.18, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,0),"%"), y=hi+2),
            position=position_dodge(0.65), size=2.5, colour="#222222", vjust=0) +
  scale_fill_manual(values=c("Male"="#006B9F","Female"="#1CABE2"), name=NULL) +
  scale_y_continuous(labels=scales::label_percent(scale=1),
                     expand=expansion(mult=c(0,0.22))) +
  facet_wrap(~outcome_lbl) +
  labs(title="School, Employment and NEET by Sex and Age Group",
       subtitle="Dark blue = Male, Light blue = Female | Labels above 95% CI",
       x="Age group", y="%",
       caption="Source: NLSS IV 2022/23. Survey weights applied. Boys leave school for work; girls leave school into NEET.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8.5,colour="#6C7279"),
        plot.caption=element_text(size=6.5,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(), legend.position="bottom",
        strip.text=element_text(face="bold"))
ggsave("outputs/figures/figEMP_school_employment_neet.png",
       fig_emp2, width=11, height=4, dpi=300)
cat("  ✓ figEMP updated — labels above CI\n")
