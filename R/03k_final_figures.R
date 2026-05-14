# =============================================================================
# 03k_final_figures.R — Final definitive figure fixes
# 1. Enrolment with percentage labels
# 2. Key effects figure smaller
# 3. CATE replaced by table
# 4. CoI labels fixed
# =============================================================================
library(tidyverse); library(srvyr); library(scales); library(patchwork)
load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")
load("outputs/causal_forest_results.RData")
library(fixest)

FIGS   <- "outputs/figures"
ud<-"#006B9F"; ub<-"#1CABE2"; ur<-"#E2231A"; ug<-"#00833D"; ugr<-"#6C7279"
th <- function(b=10) theme_minimal(base_size=b)+theme(
  plot.title=element_blank(),  # title shown in slide subtitle instead
  plot.subtitle=element_text(face="bold",size=b, colour="#222222"),
  plot.subtitle=element_text(size=b-1,colour=ugr),
  plot.caption=element_text(size=6.5,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(),
  panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.35),
  legend.position="bottom",strip.text=element_text(face="bold",size=9))
sv<-function(p,n,w=9,h=4.5){ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300);cat("  ✓",n,"\n")}
src<-"Source: NLSS IV 2022/23. Survey weights applied."

make_svy<-function(data) srvyr::as_survey_design(
  data %>% filter(!is.na(ind_wt),!is.na(province_num)),
  ids=psu_number,strata=province_num,weights=ind_wt,nest=TRUE)

svy_b  <-make_svy(df_b)
svy_b1 <-make_svy(df_b1)
svy_girls<-make_svy(df_girls)
svy_girls1<-make_svy(df_girls1)

# =============================================================================
# FIG 1 — Enrolment by province and age group WITH percentage labels
# =============================================================================
enrol_age <- bind_rows(
  svy_b1 %>% group_by(province_lbl,sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1,vartype="ci")*100,.groups="drop")%>%mutate(age="15-18"),
  svy_b %>% filter(age_num>=19,age_num<=24) %>%
    as_survey_design(ids=psu_number,strata=province_num,weights=ind_wt,nest=TRUE) %>%
    group_by(province_lbl,sex_lbl) %>%
    summarise(pct=survey_mean(attend_any==1,vartype="ci")*100,.groups="drop")%>%mutate(age="19-24")
) %>% filter(!is.na(province_lbl),!is.na(sex_lbl))

fig1<-ggplot(enrol_age,aes(x=reorder(as.character(province_lbl),pct),y=pct,fill=sex_lbl))+
  geom_col(position=position_dodge(0.72),width=0.65,alpha=0.88)+
  geom_errorbar(aes(ymin=pct_low,ymax=pct_upp),position=position_dodge(0.72),width=0.18,linewidth=0.4)+
  geom_text(aes(label=paste0(round(pct,0),"%"),y=pct_upp+2),
            position=position_dodge(0.72),size=2.2,colour="#222222",vjust=0)+
  scale_fill_manual(values=c("Male"=ud,"Female"=ub),name=NULL)+
  scale_y_continuous(labels=label_percent(scale=1),expand=expansion(mult=c(0,0.20)))+
  coord_flip()+facet_wrap(~age,labeller=labeller(age=c("15-18"="Age 15-18","19-24"="Age 19-24")))+
  labs(subtitle="Any school level | Dark blue=Male, Light blue=Female | Labels above 95% CI",
       x=NULL, y="% Currently Enrolled", caption=src)+th()
sv(fig1,"figFIX1_enrolment_province_by_age",w=11,h=4.5)

# FIG 3 — Secondary enrolment 15-18 with labels
sec_prov<-svy_girls1 %>% group_by(province_lbl) %>%
  summarise(pct=survey_mean(in_secondary==1,vartype="ci")*100,.groups="drop")%>%
  filter(!is.na(province_lbl))
fig3<-ggplot(sec_prov,aes(x=reorder(as.character(province_lbl),pct),y=pct))+
  geom_col(fill=ud,alpha=0.88,width=0.65)+
  geom_errorbar(aes(ymin=pct_low,ymax=pct_upp),width=0.25,linewidth=0.4)+
  geom_text(aes(label=paste0(round(pct,0),"%"),y=pct_upp+1),hjust=-0.1,size=2.8)+
  scale_y_continuous(labels=label_percent(scale=1),expand=expansion(mult=c(0,0.2)))+
  coord_flip()+
  labs(subtitle="Survey-weighted with 95% CI",
       x=NULL, y="% In Secondary", caption=src)+th()
sv(fig3,"figFIX5_secondary_enrolment_1518",w=8,h=4)

# =============================================================================
# FIG 14 — Key effects SMALLER (rebuild with compact layout)
# =============================================================================
key_effects <- tribble(
  ~outcome,~factor,~effect_pp,~ci_lo,~ci_hi,~sig,
  "Enrolment\n(Girls)","Currently Married",-61.2,-65.7,-56.7,"***",
  "Enrolment\n(Girls)","Dalit (vs Brahmin)",-6.4,-11.5,-1.3,"***",
  "Enrolment\n(Girls)","Disability",-19.6,-40.4,1.2,"*",
  "Enrolment\n(Girls)","Per quintile step up",5.9,4.1,7.7,"***",
  "Enrolment\n(Girls)","Rural",-2.9,-6.6,0.8,"",
  "NEET\n(Youth)","Female",9.3,6.0,12.6,"***",
  "NEET\n(Youth)","Currently Married",42.4,38.5,46.3,"***",
  "NEET\n(Youth)","Disability",23.0,6.1,39.9,"***",
  "NEET\n(Youth)","Per quintile step up",-4.6,-6.0,-3.2,"***",
  "NEET\n(Youth)","Below poverty line",0.3,-5.6,6.2,"",
  "NEET\n(Youth)","Dalit (vs Brahmin)",0.3,-4.1,4.7,""
) %>% mutate(
  dot_col=case_when(effect_pp<0&outcome=="Enrolment\n(Girls)"~ur,
                    effect_pp>0&outcome=="Enrolment\n(Girls)"~ug,
                    effect_pp>0&outcome=="NEET\n(Youth)"~ur,TRUE~ug),
  lbl=paste0(ifelse(effect_pp>0,"+",""),round(effect_pp,1)," pp",
             ifelse(sig!="","","")),
  outcome=factor(outcome,c("Enrolment\n(Girls)","NEET\n(Youth)"))
)

fig14<-ggplot(key_effects,
  aes(x=effect_pp,xmin=ci_lo,xmax=ci_hi,y=reorder(factor,effect_pp)))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey60",linewidth=0.4)+
  geom_errorbarh(height=0.3,linewidth=0.6,colour="grey50")+
  geom_point(aes(colour=dot_col),size=2.8)+
  geom_text(aes(label=paste0(ifelse(effect_pp>0,"+",""),round(effect_pp,1)," pp ",sig),
                x=ci_hi+1.5),hjust=0,size=2.5,colour="#333333")+
  scale_colour_identity()+
  scale_x_continuous(labels=label_number(suffix=" pp"),expand=expansion(mult=c(0.1,0.35)))+
  facet_wrap(~outcome,scales="free",nrow=1)+
  labs(x="Effect (percentage points)",y=NULL,
       caption="LPM with province FE. 95% CI. NLSS IV 2022/23.")+
  th()+theme(panel.grid.major.y=element_blank(),legend.position="none",
             strip.text=element_text(face="bold",size=10))
sv(fig14,"figNEW7_visual_summary_effects",w=11,h=4)

# =============================================================================
# FIG 22/23 — CATE as a clean table figure (replaces tall forest plot)
# =============================================================================
cate_tbl <- bind_rows(
  cf_results$cate_neet_all %>% filter(dimension=="Caste/Ethnicity") %>%
    mutate(dim="Caste/Ethnicity"),
  cf_results$cate_neet_all %>% filter(dimension=="Province") %>%
    mutate(dim="Province")
) %>%
  mutate(
    CATE_pp = round(mean_cate*100,1),
    CI = paste0("(",round(ci_lo*100,1)," to ",round(ci_hi*100,1),")"),
    col_fill = ifelse(CATE_pp < -13, "#D62728", ifelse(CATE_pp < -12, "#006B9F","#6C7279"))
  ) %>%
  arrange(dim, CATE_pp)

fig_cate<-ggplot(cate_tbl,
  aes(x=mean_cate*100,xmin=ci_lo*100,xmax=ci_hi*100,y=reorder(group,mean_cate)))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey60")+
  geom_vline(xintercept=-13.2,linetype="dotted",colour="#333333",linewidth=0.6)+
  geom_errorbarh(height=0.35,linewidth=0.6,colour=ur)+
  geom_point(aes(size=n),colour=ur,alpha=0.85)+
  geom_text(aes(label=paste0(round(mean_cate*100,1)," pp"),x=ci_hi*100+0.3),
            hjust=0,size=2.5,colour="#333333")+
  scale_size_continuous(range=c(2,5),guide="none")+
  scale_x_continuous(labels=label_number(suffix=" pp"),expand=expansion(mult=c(0.05,0.2)))+
  annotate("text",x=-13.2,y=0.3,label="ATE\n-13.2 pp",size=2.5,colour="#333333",vjust=0,fontface="italic")+
  facet_wrap(~dim,scales="free_y",ncol=2)+
  labs(subtitle="Negative = education reduces NEET | Dotted = ATE | Bubble = N",
       x="CATE (pp change in NEET)", y=NULL,
       caption="Causal forest (grf). Women 15-64. 95% CI.")+
  th()+theme(panel.grid.major.y=element_blank())
sv(fig_cate,"figCF3_cate_neet_caste_prov",w=10,h=4.5)

# Wealth quintile CATE
wq_cate<-cf_results$cate_neet_all %>% filter(dimension=="Wealth Quintile")
fig_wq<-ggplot(wq_cate,
  aes(x=mean_cate*100,xmin=ci_lo*100,xmax=ci_hi*100,y=reorder(group,mean_cate)))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey60")+
  geom_errorbarh(height=0.3,linewidth=0.6,colour=ur)+
  geom_point(aes(size=n),colour=ur,alpha=0.85)+
  geom_text(aes(label=paste0(round(mean_cate*100,1)," pp"),x=ci_hi*100+0.15),
            hjust=0,size=2.8,colour="#333333")+
  scale_size_continuous(range=c(2,5),guide="none")+
  scale_x_continuous(labels=label_number(suffix=" pp"),expand=expansion(mult=c(0.05,0.2)))+
  labs(subtitle="Richer women benefit slightly more | Dotted = ATE (-13.2 pp)",
       x="CATE (pp)", y=NULL, caption="Causal forest (grf). Women 15-64.")+
  th()+theme(panel.grid.major.y=element_blank())
sv(fig_wq,"figCF3_cate_neet_wealth",w=7,h=3)

# =============================================================================
# FIG 25 — CoI with labels AFTER CI bar (definitive fix)
# =============================================================================
prov_wages<-df_wage_w %>% filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>% summarise(base_wage=median(monthly_wage,na.rm=TRUE))

coi_prov<-cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>% mutate(province_lbl=as.character(group)) %>%
  left_join(prov_wages %>% mutate(province_lbl=as.character(province_lbl)),by="province_lbl") %>%
  mutate(
    coi_ilo=abs(mean_cate)*100*(base_wage*12*((1.09)^2-1)*(1-1.05^(-35))/0.05)/1000,
    coi_145=abs(mean_cate)*100*(base_wage*12*((1.145)^2-1)*(1-1.05^(-35))/0.05)/1000
  ) %>% filter(!is.na(coi_ilo))

# Fixed offset = 300k regardless of bar size (avoids proportional overlap issue)
fig25<-ggplot(coi_prov,aes(x=reorder(province_lbl,coi_ilo),y=coi_ilo))+
  geom_col(fill=ud,alpha=0.85,width=0.65)+
  geom_errorbar(aes(ymin=coi_ilo,ymax=coi_145),width=0.25,linewidth=0.7,colour="#555555")+
  geom_text(aes(y=coi_145 + pmax(coi_145*0.20, 300),
                label=paste0("NPR ",format(round(coi_ilo),big.mark=","),"k")),
            hjust=0, size=2.8, colour="#222222")+
  scale_y_continuous(labels=label_comma(suffix="k",scale=1),
                     expand=expansion(mult=c(0,0.35)))+
  coord_flip()+
  labs(
       subtitle="Bar = ILO 9% | Error bar = Mincer 14.5% upper bound | Label = ILO value",
       x=NULL, y="Estimated Cost (NPR '000 per 100 girls)",
       caption="Source: NLSS IV 2022/23. Causal forest CATE x Mincer projection.")+
  th()
sv(fig25,"figCF5_coi_by_province_cate",w=9,h=4.5)

readr::write_csv(coi_prov,"outputs/tables/coi_by_province.csv")
cat("\n✓ All final figures done.\n")
