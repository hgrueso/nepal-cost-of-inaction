# =============================================================================
# 03h_final_fixes.R — Final fixes for all slide issues identified in review
# =============================================================================
needed <- c("tidyverse","srvyr","patchwork","scales","viridis","ggrepel","grf")
for (p in needed) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,repos="https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse); library(srvyr); library(patchwork); library(scales)
})

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")
load("outputs/causal_forest_results.RData")

FIGS   <- "outputs/figures"; dir.create(FIGS, showWarnings=FALSE)
ud<-"#006B9F"; ub<-"#1CABE2"; ur<-"#E2231A"; ug<-"#00833D"; ugr<-"#6C7279"
th <- function(b=11) theme_minimal(base_size=b)+theme(
  plot.title=element_text(face="bold",size=b+1),
  plot.subtitle=element_text(size=b-1,colour=ugr),
  plot.caption=element_text(size=7,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.4),
  legend.position="bottom", strip.text=element_text(face="bold"))
sv <- function(p,n,w=9,h=5.5){
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

# =============================================================================
# CHECK 1: Verify sex coding and weighted rates
# =============================================================================
cat("=== SEX CODING VERIFICATION ===\n")
check <- svy_b %>% group_by(sex_lbl) %>%
  summarise(
    pct_enrol = round(survey_mean(attend_any==1)*100, 1),
    pct_neet  = round(survey_mean(neet==1,na.rm=TRUE)*100, 1),
    n=n(), .groups="drop")
print(check)
cat("If girls > boys on NEET: coding correct. If girls < boys: colors swapped in figures.\n")

# Force correct color assignment based on actual labels in data
# sex_lbl factor: levels c("Male","Female")
female_color <- ub  # light blue for Female
male_color   <- ud  # dark blue for Male

# =============================================================================
# FIG FIX1 UPDATED — Enrolment by province × age group, correct colors
# =============================================================================
cat("\nRebuilding figures with verified color coding...\n")

enrol_prov_age <- bind_rows(
  svy_b1 %>% group_by(province_lbl, sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age_group="15-18"),
  svy_b2 %>% group_by(province_lbl, sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1, vartype="ci")*100, .groups="drop") %>%
    mutate(age_group="19-24")
) %>% filter(!is.na(province_lbl), !is.na(sex_lbl))

figFIX1 <- ggplot(enrol_prov_age,
                   aes(x=reorder(as.character(province_lbl), pct),
                       y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.72), width=0.2, linewidth=0.4) +
  scale_fill_manual(values=c("Male"=male_color,"Female"=female_color), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  coord_flip() +
  facet_wrap(~age_group, labeller=labeller(age_group=c("15-18"="Age 15-18 (secondary age)","19-24"="Age 19-24 (post-secondary)"))) +
  labs(title="School Enrolment by Province and Age Group",
       subtitle="Any school level including higher education | Survey-weighted with 95% CI",
       x=NULL, y="% Currently Enrolled", caption=src) +
  th() + theme(legend.position="bottom")
sv(figFIX1, "figFIX1_enrolment_province_by_age", w=12, h=6)

# =============================================================================
# FIG FIX2 UPDATED — Age breakdown with NEET prominently shown
# =============================================================================
age_sex_raw <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(enrol=survey_mean(attend_any==1,   vartype="ci")*100,
              sec  =survey_mean(in_secondary==1, vartype="ci")*100,
              neet =survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,
              .groups="drop") %>% mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(enrol=survey_mean(attend_any==1,   vartype="ci")*100,
              sec  =survey_mean(in_secondary==1, vartype="ci")*100,
              neet =survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,
              .groups="drop") %>% mutate(age="19-24")
) %>% filter(!is.na(sex_lbl))

make_long <- function(df, var, lo, hi, label) {
  df %>% select(sex_lbl, age,
                value=all_of(var), lo=all_of(lo), hi=all_of(hi)) %>%
    mutate(outcome=label)
}

age_long <- bind_rows(
  make_long(age_sex_raw,"enrol","enrol_low","enrol_upp","Any enrolment"),
  make_long(age_sex_raw,"sec",  "sec_low",  "sec_upp",  "Secondary grade (Class 9-10/SLC)"),
  make_long(age_sex_raw,"neet", "neet_low", "neet_upp", "NEET")
) %>% filter(!is.na(value)) %>%
  mutate(outcome=factor(outcome, c("Any enrolment","Secondary grade (Class 9-10/SLC)","NEET")),
         lbl=paste0(round(value,0),"%"))

figFIX2 <- ggplot(age_long, aes(x=age, y=value, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=lo, ymax=hi),
                position=position_dodge(0.65), width=0.2, linewidth=0.4) +
  geom_text(aes(label=lbl, y=value+2),
            position=position_dodge(0.65), size=2.9, colour="#222222") +
  scale_fill_manual(values=c("Male"=male_color,"Female"=female_color), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  facet_wrap(~outcome, scales="free_y") +
  labs(title="Youth Outcomes by Age Group and Sex: 15-18 vs. 19-24",
       subtitle="Bars are independent estimates — no trend implied. Key: NEET divergence begins at 19-24.",
       x="Age group", y="%", caption=src) +
  th()
sv(figFIX2, "figFIX2_age_breakdown_bars", w=12, h=5.5)

# =============================================================================
# FIG FIX3 UPDATED — NEET rates (no ILO benchmark, values labelled)
# =============================================================================
neet_age_sex <- bind_rows(
  svy_b1 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,.groups="drop") %>% mutate(age="15-18"),
  svy_b2 %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,.groups="drop") %>% mutate(age="19-24"),
  svy_b  %>% group_by(sex_lbl) %>%
    summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,.groups="drop") %>% mutate(age="15-24 (all)")
) %>% filter(!is.na(sex_lbl)) %>%
  mutate(age=factor(age,c("15-18","19-24","15-24 (all)")))

figFIX3 <- ggplot(neet_age_sex, aes(x=age, y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.65), width=0.55, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.65), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,1),"%"), y=pct+1.5),
            position=position_dodge(0.65), size=3.3, colour="#222222") +
  scale_fill_manual(values=c("Male"=male_color,"Female"=female_color), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  labs(title="NEET Rate by Age Group and Sex",
       subtitle="Youth 15-24 | The gender NEET gap only emerges after age 18",
       x="Age group", y="% NEET", caption=src) +
  th()
sv(figFIX3, "figFIX3_neet_by_age_sex", w=8, h=5.5)

# =============================================================================
# FIG FIX4 UPDATED — NEET by province bars only (no Koshi highlight — all blue)
# =============================================================================
neet_prov_girls <- svy_girls %>%
  group_by(province_lbl) %>%
  summarise(pct_neet =survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100,
            pct_enrol=survey_mean(attend_any==1, vartype="ci")*100,
            .groups="drop") %>%
  filter(!is.na(province_lbl))

enrol_prov_girls <- neet_prov_girls

figFIX4a <- ggplot(enrol_prov_girls,
                    aes(x=reorder(as.character(province_lbl), pct_enrol), y=pct_enrol)) +
  geom_col(fill=ud, alpha=0.88, width=0.65) +
  geom_errorbar(aes(ymin=pct_enrol_low, ymax=pct_enrol_upp), width=0.25, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct_enrol,0),"%")), hjust=-0.2, size=3.1) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.25))) +
  coord_flip() +
  labs(title="Enrolment Rate", subtitle="Girls 15-24", x=NULL, y="% Enrolled") + th()

figFIX4b <- ggplot(enrol_prov_girls,
                    aes(x=reorder(as.character(province_lbl), pct_enrol), y=pct_neet)) +
  geom_col(fill=ud, alpha=0.88, width=0.65) +
  geom_errorbar(aes(ymin=pct_neet_low, ymax=pct_neet_upp), width=0.25, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct_neet,0),"%")), hjust=-0.2, size=3.1) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.25))) +
  coord_flip() +
  labs(title="NEET Rate", subtitle="Girls 15-24", x=NULL, y="% NEET") + th()

figFIX4 <- figFIX4a + figFIX4b +
  plot_annotation(
    title="Koshi: Relatively High Enrolment but Also High NEET for Girls",
    subtitle="Provinces sorted by enrolment. High enrolment does not guarantee low NEET — labour market access differs.",
    theme=theme(plot.title=element_text(face="bold",size=12),
                plot.subtitle=element_text(size=10,colour=ugr)))
sv(figFIX4, "figFIX4_koshi_puzzle_bars", w=11, h=5.5)

# =============================================================================
# FIG NEET BY PROVINCE + SEX — Fix slide 12 (check if NEET higher for men)
# =============================================================================
neet_prov_sex <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

cat("\nNEET by province and sex (verify direction):\n")
neet_prov_sex %>% pivot_wider(names_from=sex_lbl, values_from=pct) %>%
  mutate(gap=Female-Male) %>% print()

fig_neet_prov_sex <- ggplot(neet_prov_sex,
                              aes(x=as.character(province_lbl), y=pct, fill=sex_lbl)) +
  geom_col(position=position_dodge(0.72), width=0.65, alpha=0.88) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp),
                position=position_dodge(0.72), width=0.2, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,0),"%"), y=pct+1.5),
            position=position_dodge(0.72), size=2.6, colour="#222222") +
  scale_fill_manual(values=c("Male"=male_color,"Female"=female_color), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.15))) +
  labs(title="NEET Rate by Province and Sex", subtitle="Youth 15-24 | Survey-weighted with 95% CI",
       x=NULL, y="% NEET", caption=src) + th()
sv(fig_neet_prov_sex, "fig4_neet_by_province", w=10, h=5.5)

# =============================================================================
# FIG NEET BY CASTE — Fix slide 11 (bars, girls only)
# =============================================================================
neet_caste <- svy_girls %>%
  group_by(caste_broad) %>%
  summarise(pct=survey_mean(neet==1,na.rm=TRUE, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(caste_broad), caste_broad!="Other")

fig_neet_caste <- ggplot(neet_caste,
                          aes(x=reorder(as.character(caste_broad), -pct), y=pct)) +
  geom_col(fill=ur, alpha=0.88, width=0.65) +
  geom_errorbar(aes(ymin=pct_low, ymax=pct_upp), width=0.25, linewidth=0.4) +
  geom_text(aes(label=paste0(round(pct,1),"%")), vjust=-0.4, size=3.3) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.15))) +
  labs(title="NEET Rate by Caste/Ethnicity — Girls 15-24",
       subtitle="Survey-weighted mean with 95% CI | No connecting lines — independent group estimates",
       x=NULL, y="% NEET", caption=src) + th()
sv(fig_neet_caste, "fig6_neet_by_caste", w=8, h=5.5)

# =============================================================================
# FIG INTERSECTIONALITY — rebuild figNEW4 (was missing/empty)
# =============================================================================
prov_caste_data <- svy_girls %>%
  group_by(province_lbl, caste_broad) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1)*100,
    pct_neet  = survey_mean(neet==1,na.rm=TRUE)*100,
    n=n(), .groups="drop"
  ) %>% filter(!is.na(province_lbl), !is.na(caste_broad), n>=10,
               caste_broad %in% c("Dalit","Brahmin/Chhetri","Janajati"))

figNEW4 <- ggplot(prov_caste_data,
                   aes(x=pct_enrol, y=pct_neet,
                       colour=caste_broad, size=n,
                       label=as.character(province_lbl))) +
  geom_point(alpha=0.8) +
  ggrepel::geom_text_repel(size=2.8, show.legend=FALSE, max.overlaps=15) +
  scale_colour_manual(values=c("Dalit"=ur,"Brahmin/Chhetri"=ud,"Janajati"=ug), name="Caste") +
  scale_size_continuous(range=c(2,9), guide="none") +
  scale_x_continuous(labels=label_percent(scale=1)) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="Intersectionality: Who Is Furthest Behind? (Province x Caste)",
       subtitle="Girls 15-24 | Each point = province-caste cell | Bubble size = N | Bottom-left = most excluded",
       x="% Currently Enrolled", y="% NEET", caption=src) + th()
sv(figNEW4, "figNEW4_intersectionality_scatter", w=9, h=7)

# =============================================================================
# FIG NEW1 — Enrolment gap vs NEET gap (was missing slide 10)
# =============================================================================
enrol_sex <- svy_b %>% group_by(province_lbl, sex_lbl) %>%
  summarise(pct_enrol=survey_mean(attend_any==1)*100,
            pct_neet=survey_mean(neet==1,na.rm=TRUE)*100, .groups="drop") %>%
  filter(!is.na(province_lbl), !is.na(sex_lbl))

gap_df <- enrol_sex %>%
  select(province_lbl, sex_lbl, pct_enrol, pct_neet) %>%
  pivot_wider(names_from=sex_lbl, values_from=c(pct_enrol,pct_neet)) %>%
  mutate(gap_enrol = pct_enrol_Female - pct_enrol_Male,
         gap_neet  = pct_neet_Female  - pct_neet_Male,
         province_lbl=as.character(province_lbl))

figNEW1 <- ggplot(gap_df, aes(x=gap_enrol, y=gap_neet, label=province_lbl)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
  annotate("rect",xmin=-Inf,xmax=0,ymin=0,ymax=Inf,alpha=0.05,fill=ur) +
  annotate("text",x=-18,y=max(gap_df$gap_neet,na.rm=TRUE)*0.85,
           label="Boys higher enrolment\nGirls higher NEET\n(double exclusion)",
           size=2.7,colour=ur,fontface="italic",hjust=0) +
  annotate("rect",xmin=0,xmax=Inf,ymin=0,ymax=Inf,alpha=0.05,fill=ud) +
  annotate("text",x=2,y=max(gap_df$gap_neet,na.rm=TRUE)*0.85,
           label="Girls higher enrolment\nGirls higher NEET\n(labour market barrier)",
           size=2.7,colour=ud,fontface="italic",hjust=0) +
  geom_point(size=4.5,colour=ud,alpha=0.85) +
  ggrepel::geom_text_repel(size=3.3,colour="#333333") +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  scale_y_continuous(labels=label_number(suffix=" pp")) +
  labs(title="Enrolment Gap vs. NEET Gap by Province",
       subtitle="Youth 15-24 | Gap = Girls minus Boys (pp) | Positive NEET gap = girls more excluded from labour market",
       x="Gender gap in enrolment (Girls - Boys, pp)",
       y="Gender gap in NEET (Girls - Boys, pp)", caption=src) + th()
sv(figNEW1, "figNEW1_enrolment_vs_neet_gender_gap", w=9, h=7)

# =============================================================================
# FIG CF CORROBORATION — Fix direction issue (OLS FE positive, CATE negative)
# The corroboration is: provinces with higher baseline NEET should show
# LARGER (more negative) CATE — so we expect NEGATIVE correlation
# =============================================================================
library(fixest)
ols_fe <- fixef(reg_results_v2$m2$full)[["province_lbl"]]

cat("\n=== OLS FE vs CF CATE ===\n")
cat("OLS province FE (from NEET model, province absorbs residual NEET after controls):\n")
print(sort(round(ols_fe*100,1)))

corr_df <- cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>%
  rename(province_lbl=group, cate=mean_cate) %>%
  left_join(tibble(province_lbl=names(ols_fe), ols_fe=ols_fe), by="province_lbl") %>%
  filter(!is.na(ols_fe))

r_val <- cor(corr_df$ols_fe, corr_df$cate, use="complete.obs")
cat(sprintf("Correlation OLS FE vs CF CATE: r = %.3f\n", r_val))
cat("Interpretation: if negative, provinces with higher residual NEET have larger treatment effect\n")

fig_corr <- ggplot(corr_df, aes(x=ols_fe*100, y=cate*100, label=province_lbl)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey70") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey70") +
  geom_smooth(method="lm", se=TRUE, colour=ur, linewidth=0.8, alpha=0.12) +
  geom_point(colour=ud, size=4, alpha=0.85) +
  ggrepel::geom_text_repel(size=3.5, colour="#333333") +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  scale_y_continuous(labels=label_number(suffix=" pp")) +
  annotate("text", x=min(corr_df$ols_fe*100)+1,
           y=max(corr_df$cate*100),
           label=sprintf("r = %.2f", r_val),
           size=4, colour="#333333", hjust=0) +
  labs(title="Corroboration: OLS Province Fixed Effects vs. Causal Forest CATE",
       subtitle="NEET outcome | OLS FE = residual provincial NEET after controls | CF CATE = causal effect of secondary education",
       x="OLS Province Fixed Effect (pp)", y="CF CATE: effect of secondary ed on NEET (pp)",
       caption="Note: Both measures are on the NEET scale. A negative correlation means the CF identifies larger effects where NEET is structurally higher.") +
  th()
sv(fig_corr, "figCF4_cate_vs_ols_corroboration", w=8.5, h=6)

# =============================================================================
# FIG COEF PLOT — Rebuild to ensure it exists (slide 14 was empty)
# =============================================================================
library(broom)
coef_data <- bind_rows(
  tidy(reg_results_v2$m1$full, conf.int=TRUE) %>%
    mutate(outcome="School Enrolment\n(Girls 15-24)"),
  tidy(reg_results_v2$m2$full, conf.int=TRUE) %>%
    mutate(outcome="NEET\n(Youth 15-24)"),
  tidy(reg_results_v2$m1$no_wealth, conf.int=TRUE) %>%
    mutate(outcome="Enrolment (no wealth)\n(Girls 15-24)")
) %>%
  filter(term %in% c("female","rural","ppoor","quintile_n","currently_married",
                      "any_difficulty","caste_broadJanajati","caste_broadDalit",
                      "caste_broadReligious/Linguistic")) %>%
  mutate(
    term_lbl = recode(term,
      "female"="Female","rural"="Rural","ppoor"="Below Poverty Line",
      "quintile_n"="Wealth Quintile (per step)","currently_married"="Currently Married",
      "any_difficulty"="Disability","caste_broadJanajati"="Janajati",
      "caste_broadDalit"="Dalit","caste_broadReligious/Linguistic"="Religious/Linguistic"),
    sig = case_when(p.value<0.01~"p<0.01", p.value<0.05~"p<0.05",
                    p.value<0.10~"p<0.10", TRUE~"n.s."),
    outcome=factor(outcome, c("School Enrolment\n(Girls 15-24)",
                               "Enrolment (no wealth)\n(Girls 15-24)",
                               "NEET\n(Youth 15-24)"))
  )

fig_coef <- ggplot(coef_data,
                    aes(x=estimate*100, xmin=conf.low*100, xmax=conf.high*100,
                        y=reorder(term_lbl, estimate),
                        colour=sig, shape=sig)) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60", linewidth=0.4) +
  geom_errorbarh(height=0.3, linewidth=0.6) +
  geom_point(size=2.5) +
  scale_colour_manual(values=c("p<0.01"="#D62728","p<0.05"="#FF7F0E",
                                "p<0.10"="#1F77B4","n.s."="grey60"), name="Significance") +
  scale_shape_manual(values=c("p<0.01"=16,"p<0.05"=16,"p<0.10"=1,"n.s."=4), name="Significance") +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  facet_wrap(~outcome, scales="free_x") +
  labs(title="Predictors of School Enrolment and NEET — LPM with Province Fixed Effects",
       subtitle="pp = percentage point change in outcome | 95% CI | Ref: Brahmin/Chhetri, urban, non-poor",
       x="Coefficient (percentage point change in outcome)", y=NULL,
       caption=paste("LPM with province FE. HC1 standard errors.", src)) +
  th() + theme(panel.grid.major.y=element_blank(), legend.position="bottom")
sv(fig_coef, "fig_coef_plot", w=13, h=6.5)

cat("\n✓ All figures rebuilt.\n")
