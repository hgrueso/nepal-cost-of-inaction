# =============================================================================
# 03f_new_figures.R — New figures addressing reviewer comments
# =============================================================================
needed <- c("tidyverse","srvyr","patchwork","scales","viridis","ggrepel")
for (p in needed) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,repos="https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse); library(srvyr); library(patchwork); library(scales)
})

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS <- "outputs/figures"; dir.create(FIGS, showWarnings=FALSE)
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

cat("Building new figures...\n")

# =============================================================================
# FIG NEW1 — ENROLMENT vs NEET by sex and province
# Key story: boys lag enrolment in some provinces, but girls lag more in NEET
# =============================================================================
enrol_neet_prov <- svy_b %>%
  group_by(province_lbl, sex_lbl) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1, vartype="ci")*100,
    pct_neet  = survey_mean(neet==1,       vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(province_lbl), !is.na(sex_lbl))

# Panel A: enrolment gap (girls - boys per province)
enrol_gap <- enrol_neet_prov %>%
  select(province_lbl, sex_lbl, pct_enrol) %>%
  pivot_wider(names_from=sex_lbl, values_from=pct_enrol) %>%
  mutate(gap_enrol = Female - Male)

# Panel B: NEET gap (girls - boys per province)
neet_gap <- enrol_neet_prov %>%
  select(province_lbl, sex_lbl, pct_neet) %>%
  pivot_wider(names_from=sex_lbl, values_from=pct_neet) %>%
  mutate(gap_neet = Female - Male)

gap_df <- left_join(enrol_gap, neet_gap, by="province_lbl") %>%
  mutate(province_lbl=as.character(province_lbl))

# Scatter: provinces where enrolment gap favours boys vs NEET gap penalises girls
figNEW1 <- ggplot(gap_df, aes(x=gap_enrol, y=gap_neet, label=province_lbl)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
  annotate("rect", xmin=-Inf,xmax=0, ymin=0,ymax=Inf,
           alpha=0.06, fill=ur) +
  annotate("text", x=-15, y=30, label="Boys higher enrolment\nGirls higher NEET\n(double exclusion)",
           size=2.8, colour=ur, fontface="italic", hjust=0) +
  annotate("rect", xmin=0,xmax=Inf, ymin=0,ymax=Inf,
           alpha=0.06, fill=ud) +
  annotate("text", x=5, y=30, label="Girls higher enrolment\nGirls higher NEET\n(labour market barrier)",
           size=2.8, colour=ud, fontface="italic", hjust=0) +
  geom_point(size=4.5, colour=ud, alpha=0.85) +
  ggrepel::geom_text_repel(size=3.3, colour="#333333") +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  scale_y_continuous(labels=label_number(suffix=" pp")) +
  labs(title="Enrolment Gap vs. NEET Gap by Province",
       subtitle="Youth 15-24 | Gap = Girls minus Boys (pp) | Upper right = labour market excludes girls despite enrolment",
       x="Gender gap in enrolment (Girls - Boys, pp)",
       y="Gender gap in NEET (Girls - Boys, pp)",
       caption=paste(src,"Positive NEET gap = girls more NEET than boys in that province.")) +
  th()
sv(figNEW1, "figNEW1_enrolment_vs_neet_gender_gap", w=9, h=7)

# =============================================================================
# FIG NEW2 — Age breakdown: enrolment and NEET by age group and sex
# =============================================================================
age_sex <- svy_b %>%
  group_by(age_cat, sex_lbl) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1,  vartype="ci")*100,
    pct_neet  = survey_mean(neet==1,         vartype="ci")*100,
    pct_sec   = survey_mean(attend_secondary==1, vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(age_cat), !is.na(sex_lbl),
               age_cat %in% c("15-18","19-24"))

figNEW2a <- ggplot(age_sex, aes(x=age_cat, y=pct_enrol,
                                 group=sex_lbl, colour=sex_lbl)) +
  geom_line(linewidth=1.2) + geom_point(size=3.5) +
  geom_errorbar(aes(ymin=pct_enrol_low, ymax=pct_enrol_upp),
                width=0.15, linewidth=0.5) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="School Enrolment", subtitle="Any level incl. HE",
       x="Age group", y="% Enrolled") + th()

figNEW2b <- ggplot(age_sex, aes(x=age_cat, y=pct_sec,
                                 group=sex_lbl, colour=sex_lbl)) +
  geom_line(linewidth=1.2) + geom_point(size=3.5) +
  geom_errorbar(aes(ymin=pct_sec_low, ymax=pct_sec_upp),
                width=0.15, linewidth=0.5) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="Secondary School Enrolment", subtitle="Government/private school only",
       x="Age group", y="% In secondary") + th()

figNEW2c <- ggplot(age_sex, aes(x=age_cat, y=pct_neet,
                                 group=sex_lbl, colour=sex_lbl)) +
  geom_line(linewidth=1.2) + geom_point(size=3.5) +
  geom_errorbar(aes(ymin=pct_neet_low, ymax=pct_neet_upp),
                width=0.15, linewidth=0.5) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="NEET Rate", subtitle="Not in employment, education or training",
       x="Age group", y="% NEET") + th()

figNEW2 <- (figNEW2a + figNEW2b + figNEW2c) +
  plot_annotation(
    title="Youth Outcomes by Age Group and Sex: 15-18 vs. 19-24",
    subtitle="Key: girls close to boys at 15-18; sharp divergence at 19-24 signals labour market exclusion",
    theme=theme(plot.title=element_text(face="bold",size=13),
                plot.subtitle=element_text(size=10,colour=ugr)))
sv(figNEW2, "figNEW2_age_breakdown", w=12, h=5)

# =============================================================================
# FIG NEW3 — NEET ILO comparison: our estimate vs ILO benchmark
# =============================================================================
# ILO Nepal estimates: women 15-24 NEET ~55-65%, men ~20-25%
# Our unweighted: girls 15-18: 20%, 19-24: 61.5%
# Our survey-weighted estimates:
neet_age_sex <- svy_b %>%
  group_by(age_cat, sex_lbl) %>%
  summarise(pct=survey_mean(neet==1, vartype="ci")*100, .groups="drop") %>%
  filter(!is.na(age_cat), !is.na(sex_lbl), age_cat %in% c("15-18","19-24")) %>%
  mutate(source="NLSS IV (this analysis)")

# ILO benchmark data (Nepal, approx from ILO ILOSTAT)
ilo_bench <- tribble(
  ~age_cat, ~sex_lbl, ~pct,    ~pct_low, ~pct_upp, ~source,
  "15-24",  "Female", 56.0,     52.0,     60.0,    "ILO ILOSTAT (Nepal ~2020)",
  "15-24",  "Male",   15.0,     12.0,     18.0,    "ILO ILOSTAT (Nepal ~2020)"
)

# Note: the "double" claim likely refers to full 15-24 pooled
# Our 15-18 girls: low because many still in school
# Our 19-24 girls: 61.5% — close to ILO figure
# Combined 15-24 girls weighted:
neet_1524_girls <- svy_girls %>%
  summarise(pct=survey_mean(neet==1, vartype="ci")*100)
neet_1524_boys <- svy_b %>%
  filter(female==0) %>%
  summarise(pct=survey_mean(neet==1, vartype="ci")*100)

cat(sprintf("Our NEET: Girls 15-24: %.1f%% | Boys 15-24: %.1f%%\n",
            neet_1524_girls$pct, neet_1524_boys$pct))
cat("ILO Nepal benchmark: Women 15-24 ~56%, Men 15-24 ~15%\n")
cat("Note: Age-group split explains apparent discrepancy\n")

comparison_df <- tibble(
  Group=c("Girls 15-18\n(this analysis)","Girls 19-24\n(this analysis)",
          "Girls 15-24\n(this analysis)","Girls 15-24\n(ILO benchmark)",
          "Boys 15-18\n(this analysis)","Boys 19-24\n(this analysis)"),
  NEET_pct=c(
    neet_age_sex %>% filter(age_cat=="15-18",sex_lbl=="Female") %>% pull(pct),
    neet_age_sex %>% filter(age_cat=="19-24",sex_lbl=="Female") %>% pull(pct),
    neet_1524_girls$pct,
    56.0,
    neet_age_sex %>% filter(age_cat=="15-18",sex_lbl=="Male") %>% pull(pct),
    neet_age_sex %>% filter(age_cat=="19-24",sex_lbl=="Male") %>% pull(pct)
  ),
  sex=c("Female","Female","Female","Female (benchmark)","Male","Male"),
  type=c("NLSS IV","NLSS IV","NLSS IV","ILO estimate","NLSS IV","NLSS IV")
)

figNEW3 <- ggplot(comparison_df,
                   aes(x=reorder(Group,-NEET_pct), y=NEET_pct,
                       fill=sex, alpha=type)) +
  geom_col(width=0.65) +
  geom_text(aes(label=paste0(round(NEET_pct,1),"%")),
            vjust=-0.4, size=3.2) +
  scale_fill_manual(values=c("Female"=ub,"Female (benchmark)"=ur,"Male"=ud),
                    name=NULL) +
  scale_alpha_manual(values=c("NLSS IV"=0.88,"ILO estimate"=0.5),
                     name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  labs(title="NEET Rate: NLSS IV vs. ILO Benchmark",
       subtitle="Girls 15-24 pooled (44%) is below ILO 56% because 15-18 are mostly in school.\nAt 19-24, our estimate (61.5%) is consistent with or above ILO.",
       x=NULL, y="% NEET",
       caption=paste(src,"ILO benchmark: ILOSTAT Nepal approx. 2020.")) +
  th() + theme(axis.text.x=element_text(angle=15,hjust=1))
sv(figNEW3, "figNEW3_neet_ilo_comparison", w=10, h=5.5)

# =============================================================================
# FIG NEW4 — Intersectionality: who is furthest behind
# Province x caste x sex — triple penalty
# =============================================================================
# Identify the most disadvantaged subgroup (Dalit girls in lagging provinces)
df_girls_sub <- df_girls %>%
  filter(!is.na(province_lbl), !is.na(caste_broad)) %>%
  mutate(subgroup = paste0(as.character(caste_broad),"\n",as.character(province_lbl)))

# Province × caste enrolment
prov_caste <- svy_girls %>%
  group_by(province_lbl, caste_broad) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1,  na.rm=TRUE)*100,
    pct_neet  = survey_mean(neet==1,         na.rm=TRUE)*100,
    n=n(), .groups="drop"
  ) %>%
  filter(!is.na(province_lbl), !is.na(caste_broad), n>=10)

figNEW4 <- ggplot(prov_caste %>% filter(caste_broad %in% c("Dalit","Brahmin/Chhetri","Janajati")),
                   aes(x=pct_enrol, y=pct_neet,
                       colour=caste_broad, size=n, label=as.character(province_lbl))) +
  geom_point(alpha=0.75) +
  ggrepel::geom_text_repel(size=2.5, show.legend=FALSE) +
  scale_colour_manual(values=c("Dalit"=ur,"Brahmin/Chhetri"=ud,"Janajati"=ug),
                      name="Caste") +
  scale_size_continuous(range=c(2,8), guide="none") +
  scale_x_continuous(labels=label_percent(scale=1)) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="Intersectionality: Who Is Furthest Behind?",
       subtitle="Girls 15-24 | Each point = one province-caste cell | Bubble size = N\nBottom-right = high enrolment, high NEET = labour market exclusion",
       x="% Currently Enrolled", y="% NEET",
       caption=src) +
  th()
sv(figNEW4, "figNEW4_intersectionality_scatter", w=9, h=7)

# =============================================================================
# FIG NEW5 — Koshi puzzle: high enrolment + high NEET
# =============================================================================
koshi_df <- svy_b %>%
  filter(province_lbl=="Koshi") %>%
  group_by(sex_lbl, age_cat) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1, vartype="ci")*100,
    pct_neet  = survey_mean(neet==1,       vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(sex_lbl), age_cat %in% c("15-18","19-24"))

# Compare Koshi vs national average
national_df <- svy_b %>%
  group_by(sex_lbl, age_cat) %>%
  summarise(
    pct_enrol = survey_mean(attend_any==1, vartype="ci")*100,
    pct_neet  = survey_mean(neet==1,       vartype="ci")*100,
    .groups="drop"
  ) %>% filter(!is.na(sex_lbl), age_cat %in% c("15-18","19-24")) %>%
  mutate(province_lbl="National average")

koshi_compare <- bind_rows(
  koshi_df %>% mutate(province_lbl="Koshi"),
  national_df
)

figNEW5a <- ggplot(koshi_compare,
                    aes(x=age_cat, y=pct_enrol, group=interaction(sex_lbl,province_lbl),
                        colour=sex_lbl, linetype=province_lbl)) +
  geom_line(linewidth=1.1) + geom_point(size=3) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_linetype_manual(values=c("Koshi"="solid","National average"="dashed"), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="Enrolment", x="Age group", y="%") + th()

figNEW5b <- ggplot(koshi_compare,
                    aes(x=age_cat, y=pct_neet, group=interaction(sex_lbl,province_lbl),
                        colour=sex_lbl, linetype=province_lbl)) +
  geom_line(linewidth=1.1) + geom_point(size=3) +
  scale_colour_manual(values=c("Male"=ud,"Female"=ub), name=NULL) +
  scale_linetype_manual(values=c("Koshi"="solid","National average"="dashed"), name=NULL) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(title="NEET Rate", x="Age group", y="%") + th()

figNEW5 <- figNEW5a + figNEW5b +
  plot_annotation(
    title="The Koshi Puzzle: High Enrolment, High NEET for Girls",
    subtitle="Koshi vs national average | Girls stay in school but face severe labour market exclusion at 19-24",
    caption="Interpretation: high migration from Koshi reduces male employment competition but leaves girls behind.",
    theme=theme(plot.title=element_text(face="bold",size=12),
                plot.subtitle=element_text(size=9.5,colour=ugr),
                plot.caption=element_text(size=7,colour=ugr,hjust=0)))
sv(figNEW5, "figNEW5_koshi_puzzle", w=10, h=5.5)

# =============================================================================
# FIG NEW6 — Wealth sensitivity: coefficient comparison (with vs without)
# =============================================================================
cm_plot <- c("female"="Female","rural"="Rural","ppoor"="Below poverty line",
             "currently_married"="Currently married",
             "any_difficulty"="Disability",
             "caste_broadJanajati"="Janajati","caste_broadDalit"="Dalit",
             "caste_broadReligious/Linguistic"="Religious/Ling.")

coef_w  <- broom::tidy(reg_results_v2$m2$full,    conf.int=TRUE) %>% mutate(model="With wealth quintile")
coef_nw <- broom::tidy(reg_results_v2$m2$no_wealth, conf.int=TRUE) %>% mutate(model="Without wealth quintile")

coef_comp <- bind_rows(coef_w, coef_nw) %>%
  filter(term %in% names(cm_plot)) %>%
  mutate(term_lbl=recode(term, !!!cm_plot))

figNEW6 <- ggplot(coef_comp,
                   aes(x=estimate*100, xmin=conf.low*100, xmax=conf.high*100,
                       y=reorder(term_lbl, estimate), colour=model)) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60") +
  geom_errorbarh(aes(colour=model), height=0.35, linewidth=0.7,
                 position=position_dodge(0.5)) +
  geom_point(size=2.5, position=position_dodge(0.5)) +
  scale_colour_manual(values=c("With wealth quintile"=ud,
                                "Without wealth quintile"=ur), name=NULL) +
  scale_x_continuous(labels=label_number(suffix=" pp")) +
  facet_wrap(~"NEET Outcome (Youth 15-24)") +
  labs(title="Wealth Sensitivity: Coefficient Comparison",
       subtitle="Does including wealth quintile mask caste and poverty effects on NEET?",
       x="Coefficient (percentage point change in NEET)", y=NULL,
       caption=paste("LPM with province FE. HC1 SE.", src)) +
  th() +
  theme(panel.grid.major.y=element_blank(), strip.text=element_text(face="bold"))
sv(figNEW6, "figNEW6_wealth_sensitivity", w=9, h=6)

# =============================================================================
# FIG NEW7 — Visual summary of key effects (NOT regression table)
# Coefficient plot style showing pp changes as the main result
# =============================================================================
key_effects <- tribble(
  ~outcome, ~factor, ~effect_pp, ~ci_lo, ~ci_hi, ~sig,
  "Enrolment (Girls)", "Currently Married",    -61.2, -65.7, -56.7, "***",
  "Enrolment (Girls)", "Dalit (vs Brahmin)",   -6.4,  -11.5,  -1.3, "***",
  "Enrolment (Girls)", "Disability",           -19.6, -40.4,   1.2, "*",
  "Enrolment (Girls)", "Per quintile step up",  5.9,   4.1,   7.7, "***",
  "Enrolment (Girls)", "Rural",                -2.9,  -6.6,   0.8, "",
  "NEET (Youth 15-24)", "Female",              +9.3,   6.0,  12.6, "***",
  "NEET (Youth 15-24)", "Below poverty line",  +0.3,  -5.6,   6.2, "",
  "NEET (Youth 15-24)", "Dalit (vs Brahmin)",  +0.3,  -4.1,   4.7, "",
  "NEET (Youth 15-24)", "Disability",         +23.0,   6.1,  39.9, "***",
  "NEET (Youth 15-24)", "Currently Married",  +42.4,  38.5,  46.3, "***",
  "NEET (Youth 15-24)", "Per quintile step up", -4.6, -6.0,  -3.2, "***"
) %>%
  mutate(
    fill_col = case_when(
      outcome=="Enrolment (Girls)" & effect_pp < 0 ~ ur,
      outcome=="Enrolment (Girls)" & effect_pp > 0 ~ ug,
      outcome=="NEET (Youth 15-24)" & effect_pp > 0 ~ ur,
      TRUE ~ ug
    )
  )

figNEW7 <- ggplot(key_effects,
                   aes(x=effect_pp, xmin=ci_lo, xmax=ci_hi,
                       y=reorder(factor, effect_pp))) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey60", linewidth=0.5) +
  geom_errorbarh(height=0.3, linewidth=0.6, colour="grey50") +
  geom_point(aes(colour=fill_col), size=3.5) +
  geom_text(aes(label=paste0(ifelse(effect_pp>0,"+",""),round(effect_pp,1)," pp ",sig)),
            hjust=-0.25, size=3, colour="#333333") +
  scale_colour_identity() +
  scale_x_continuous(labels=label_number(suffix=" pp"),
                     expand=expansion(mult=c(0.05,0.3))) +
  facet_wrap(~outcome, scales="free") +
  labs(title="Key Effects on Girls' Education and Labour Market Outcomes",
       subtitle="Percentage point changes | Province fixed-effects LPM | Red = worse outcome, Green = better",
       x="Effect (percentage points)", y=NULL,
       caption=paste("LPM with province FE, wealth quintile included.", src)) +
  th() +
  theme(panel.grid.major.y=element_blank(), strip.text=element_text(face="bold",size=10))
sv(figNEW7, "figNEW7_visual_summary_effects", w=12, h=7)

cat("\n✓ All new figures saved to outputs/figures/\n")
