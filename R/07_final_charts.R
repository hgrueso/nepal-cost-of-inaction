suppressPackageStartupMessages({ library(tidyverse); library(scales) })
load("outputs/cleaned_data_v2.RData")
res  <- readRDS("outputs/census_anchor_results.RData")
prov <- readr::read_csv("outputs/tables/coi_census_anchored_province.csv", show_col_types = FALSE)
FIGS <- "outputs/figures"
ub<-"#1CABE2"; ud<-"#00377C"; ur<-"#E2231A"; ugr<-"#6C7279"
th <- function(b=12) theme_minimal(base_size=b)+theme(
  plot.title=element_text(face="bold",size=b+2,colour="#222"),
  plot.subtitle=element_text(size=b,colour=ugr),
  plot.caption=element_text(size=b-3,colour=ugr,hjust=0),
  panel.grid.minor=element_blank(), panel.grid.major.y=element_blank(),
  panel.grid.major.x=element_line(colour="#EBEBEB",linewidth=.4),
  legend.position="bottom")
sv <- function(p,n,w,h) { ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300,bg="white"); cat("saved",n,"\n") }
coi <- res$coi_b_r09
d <- tibble(item=c("Education budget","Social security allowances","Health budget","Annual cost of inaction"),
            v=c(203.66,106,86.24,coi)) %>%
  mutate(lbl=ifelse(item=="Annual cost of inaction", sprintf("NPR %.1fB", v),
                    sprintf("NPR %.1fB  (CoI = %.1f%%)", v, 100*coi/v)),
         item=factor(item, rev(item)))
p <- ggplot(d,aes(v,item,fill=item=="Annual cost of inaction"))+
  geom_col(width=.62,alpha=.92)+
  geom_text(aes(label=lbl),hjust=-.03,size=3.4,fontface="bold")+
  scale_fill_manual(values=c(`TRUE`=ur,`FALSE`=ub),guide="none")+
  scale_x_continuous(expand=expansion(mult=c(0,.33)))+
  labs(title="The annual cost of inaction compared with national budget lines",
       subtitle="NPR billion | census-anchored estimate (r = 9%), FY 2024/25 budgets",
       x="NPR billion",y=NULL,
       caption="Author's calculations, NLSS IV 2022/23 and Census 2021; MoF Red Book FY 2024/25.")+th()
sv(p,"c3_coi_vs_budget_census",11,5)
pd <- prov %>% arrange(coi_b_r09) %>% mutate(province_lbl=factor(province_lbl,province_lbl))
p <- ggplot(pd,aes(coi_b_r09,province_lbl))+
  geom_col(fill=ud,width=.62,alpha=.92)+
  geom_text(aes(label=sprintf("%.2f",coi_b_r09)),hjust=-.15,size=3.4,fontface="bold")+
  scale_x_continuous(expand=expansion(mult=c(0,.15)))+
  labs(title="Provincial distribution of the annual cost of inaction",
       subtitle="NPR billion per year, r = 9% | census-anchored population",
       x="NPR billion",y=NULL,
       caption="Author's calculations, NLSS IV 2022/23 and Census 2021.")+th()
sv(p,"coi_provincial_census",10,5.2)
gg <- df_girls %>% filter(age_num>=15, age_num<=24, !is.na(ind_wt)) %>%
  mutate(agegrp=ifelse(age_num<=18,"Ages 15-18","Ages 19-24"),
         mcat=case_when(currently_married==1 & age_num<18 ~ "Married, currently under 18",
                        currently_married==1 ~ "Married, age 18 or older",
                        TRUE ~ "Not married"))
b1 <- gg %>% group_by(agegrp,mcat) %>% summarise(w=sum(ind_wt),.groups="drop") %>%
  group_by(agegrp) %>% mutate(pct=100*w/sum(w)) %>% ungroup() %>% filter(mcat!="Not married") %>%
  mutate(mcat=factor(mcat,c("Married, age 18 or older","Married, currently under 18")))
p <- ggplot(b1,aes(agegrp,pct,fill=mcat))+
  geom_col(width=.6,alpha=.92)+
  geom_text(aes(label=paste0(round(pct),"%")),position=position_stack(vjust=.5),colour="white",fontface="bold",size=3.6)+
  scale_fill_manual(values=c("Married, age 18 or older"=ub,"Married, currently under 18"=ur),name=NULL)+
  scale_y_continuous(labels=label_percent(scale=1),expand=expansion(mult=c(0,.12)))+
  labs(title="Marriage rises sharply across the adolescent transition",
       subtitle="Share of girls currently married, by age group | Girls 15-24",
       x=NULL,y="% currently married",
       caption="NLSS IV 2022/23, survey-weighted. Age at first marriage is not recorded in NLSS IV; categories reflect current age and status.")+
  theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold",size=14,colour="#222"),
    plot.subtitle=element_text(colour=ugr),plot.caption=element_text(size=9,colour=ugr,hjust=0),
    panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_line(colour="#EBEBEB",linewidth=.4),legend.position="bottom")
sv(p,"b1_marriage_by_age",9,5)
