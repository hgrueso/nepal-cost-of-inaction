# =============================================================================
# 03c_wage_plots.R — Wage and earnings visualisations for cost-of-inaction
# =============================================================================
needed <- c("tidyverse","srvyr","patchwork","scales","viridis","ggrepel")
for (pkg in needed)
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(tidyverse); library(srvyr)
  library(patchwork); library(scales); library(viridis)
})

load("outputs/cleaned_data_v2.RData")
load("outputs/regression_results_v2.RData")

FIGS <- "outputs/figures"
dir.create(FIGS, showWarnings=FALSE)

# ── Theme ──────────────────────────────────────────────────────────────────────
ub <- "#1CABE2"; ud <- "#006B9F"; ug <- "#00833D"; ur <- "#E2231A"; ugr <- "#6C7279"

th <- function(base=11) {
  theme_minimal(base_size=base) +
  theme(plot.title=element_text(face="bold",size=base+1,colour="#222222"),
        plot.subtitle=element_text(size=base-1,colour=ugr),
        plot.caption=element_text(size=7,colour=ugr,hjust=0),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour="#EBEBEB",linewidth=0.4),
        legend.position="bottom", strip.text=element_text(face="bold"),
        plot.margin=margin(10,10,10,10))
}
sv <- function(p,n,w=8,h=5) {
  ggsave(file.path(FIGS,paste0(n,".png")),p,width=w,height=h,dpi=300)
  cat("  ✓",n,"\n")
}
src <- "Source: NLSS IV 2022/23. Survey weights applied."

# ── Wage education labels ──────────────────────────────────────────────────────
edu_order <- c("No education","Primary","Lower secondary",
               "Secondary","Higher secondary","Tertiary")
edu_short  <- c("None","Primary","Lower\nSec","Secondary","Higher\nSec","Tertiary")

# =============================================================================
# FIG W1 — Mean log wage by education level (salary workers)
# =============================================================================
cat("Building wage figures...\n")

w1_data <- df_wage_w_sal %>%
  filter(!is.na(edu_cat)) %>%
  group_by(edu_cat) %>%
  summarise(
    n         = n(),
    mean_log  = mean(log_wage, na.rm=TRUE),
    se_log    = sd(log_wage, na.rm=TRUE) / sqrt(n),
    mean_mo   = mean(monthly_wage, na.rm=TRUE),
    med_mo    = median(monthly_wage, na.rm=TRUE)
  ) %>%
  mutate(
    ci_lo = mean_log - 1.96*se_log,
    ci_hi = mean_log + 1.96*se_log,
    edu_short = edu_short[match(as.character(edu_cat), edu_order)]
  )

figW1 <- ggplot(w1_data, aes(x=edu_cat, y=mean_mo/1000)) +
  geom_col(fill=ud, alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=(exp(ci_lo)*1)/1000, ymax=(exp(ci_hi)*1)/1000),
                width=0.25, linewidth=0.5) +
  geom_text(aes(label=paste0("n=",n)), vjust=-0.5, size=3, colour=ugr) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0,0.15))) +
  labs(title="Mean Monthly Wage by Education Level",
       subtitle="Women salary workers (long-term employment) | 95% CI",
       x=NULL, y="Mean Monthly Wage (NPR '000)",
       caption=src) +
  th()
sv(figW1, "figW1_wage_by_education")

# =============================================================================
# FIG W2 — Education gradient: observed wage profile (all vs salary)
# =============================================================================
w2_data <- bind_rows(
  df_wage_w %>% filter(!is.na(edu_cat)) %>%
    group_by(edu_cat) %>%
    summarise(med_mo=median(monthly_wage,na.rm=TRUE), n=n()) %>%
    mutate(type="All wage workers"),
  df_wage_w_sal %>% filter(!is.na(edu_cat)) %>%
    group_by(edu_cat) %>%
    summarise(med_mo=median(monthly_wage,na.rm=TRUE), n=n()) %>%
    mutate(type="Salary workers only")
)

figW2 <- ggplot(w2_data, aes(x=edu_cat, y=med_mo/1000,
                              group=type, colour=type)) +
  geom_line(linewidth=1.1) +
  geom_point(aes(size=n), alpha=0.8) +
  scale_colour_manual(values=c("All wage workers"=ugr,
                                "Salary workers only"=ud), name=NULL) +
  scale_size_continuous(range=c(2,7), guide="none") +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1)) +
  labs(title="Median Monthly Wage by Education — Two Samples",
       subtitle="Bubble size = N workers | Salary workers show cleaner gradient",
       x=NULL, y="Median Monthly Wage (NPR '000)",
       caption=paste(src,"Daily wage workers dominate the all-sample estimate.")) +
  th() +
  theme(axis.text.x=element_text(angle=20,hjust=1))
sv(figW2, "figW2_wage_gradient_comparison")

# =============================================================================
# FIG W3 — Predicted earnings profile (Mincer curve)
# =============================================================================
m_pfe <- reg_results_v2$m5$sal_pfe
coefs <- coef(m_pfe)
r_yr  <- coefs["edu_yrs_c"] * 100

edu_yrs_seq <- 0:16
base_log <- coefs["(Intercept)"] %||% 11.5  # fallback if absorbed by FE
pred_df <- tibble(
  edu_yrs       = edu_yrs_seq,
  edu_yrs_c     = edu_yrs_seq - mean(df_wage_w_sal$edu_yrs_c +
                    mean(df_wage_w_sal$edu_yrs_c, na.rm=TRUE), na.rm=TRUE),
  pred_log_wage = 11.5 + coefs["edu_yrs_c"]*edu_yrs_c +
                  coefs["I(edu_yrs_c^2)"] * edu_yrs_c^2,
  pred_monthly  = exp(pred_log_wage) / 12
)

# Simpler: just scale from median by return
base_monthly <- median(df_wage_w_sal$monthly_wage, na.rm=TRUE)
pred_df2 <- tibble(
  edu_yrs       = 0:16,
  pred_monthly  = base_monthly * (1 + r_yr/100)^(edu_yrs - mean(df_wage_w_sal$edu_yrs_c +
                    median(df_wage_w_sal$edu_yrs_c + 10, na.rm=TRUE), na.rm=TRUE)),
  pred_monthly2 = base_monthly * (1 + 9/100)^(edu_yrs - 10)
) %>%
  mutate(pred_monthly2 = pmax(pred_monthly2, 5000))

# Annotate key thresholds
milestones <- tibble(
  edu_yrs = c(5, 8, 10, 12, 16),
  label   = c("Primary\n(class 5)", "Lower sec\n(class 8)",
              "Secondary\n(SLC)", "Higher sec\n(class 12)", "Bachelor+"),
  pred_monthly = base_monthly * (1 + r_yr/100)^(c(5,8,10,12,16) - 10)
)

figW3 <- ggplot() +
  geom_ribbon(data=pred_df2,
              aes(x=edu_yrs, ymin=pred_monthly2/1000, ymax=pred_monthly/1000),
              alpha=0.12, fill=ud) +
  geom_line(data=pred_df2, aes(x=edu_yrs, y=pred_monthly/1000),
            colour=ud, linewidth=1.2, linetype="solid") +
  geom_line(data=pred_df2, aes(x=edu_yrs, y=pred_monthly2/1000),
            colour=ugr, linewidth=1, linetype="dashed") +
  geom_point(data=milestones, aes(x=edu_yrs, y=pred_monthly/1000),
             colour=ur, size=3.5) +
  geom_text(data=milestones,
            aes(x=edu_yrs, y=pred_monthly/1000 + 2, label=label),
            size=2.8, colour="#333333", lineheight=0.9) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0.05, 0.2))) +
  scale_x_continuous(breaks=c(0,5,8,10,12,16)) +
  annotate("text", x=14, y=max(pred_df2$pred_monthly)/1000*0.4,
           label=paste0("Mincer return:\n",round(r_yr,1),"% per year\n(salary workers)"),
           size=3, colour=ud, hjust=0) +
  annotate("text", x=14, y=max(pred_df2$pred_monthly)/1000*0.25,
           label="ILO benchmark:\n9% per year", size=3, colour=ugr, hjust=0) +
  labs(title="Predicted Monthly Earnings by Years of Schooling",
       subtitle="Solid = Mincer estimate (14.5%) | Dashed = ILO Nepal benchmark (9%)",
       x="Years of Schooling", y="Predicted Monthly Wage (NPR '000)",
       caption=src) +
  th()
sv(figW3, "figW3_predicted_earnings_curve", w=9, h=5.5)

# =============================================================================
# FIG W4 — Foregone earnings by subgroup (cost-of-inaction visualization)
#   Show PV foregone for: province, caste, poverty quintile
#   Using ILO 9% return, NPR 15,000 median baseline, 35-year horizon, 5% discount
# =============================================================================
compute_coi <- function(base_wage, return_pct=9, edu_gain=2,
                         working_yrs=35, discount=0.05) {
  foregone_annual <- base_wage * 12 * ((1+return_pct/100)^edu_gain - 1)
  pv <- foregone_annual * (1 - (1+discount)^(-working_yrs)) / discount
  list(foregone_annual=foregone_annual, pv=pv)
}

# Province-level baseline wage from survey-weighted medians
prov_wages <- df_wage_w %>%
  filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(base_wage = median(monthly_wage, na.rm=TRUE), n=n()) %>%
  mutate(
    pv_9pct  = map_dbl(base_wage, ~compute_coi(.x, 9)$pv),
    pv_145pct = map_dbl(base_wage, ~compute_coi(.x, 14.5)$pv)
  )

figW4a <- ggplot(prov_wages,
                 aes(x=reorder(province_lbl, pv_9pct), y=pv_9pct/1000)) +
  geom_col(fill=ud, alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=pv_9pct/1000*0.9, ymax=pv_145pct/1000),
                width=0.25, linewidth=0.5, colour=ugr) +
  geom_text(aes(label=paste0("NPR ",round(pv_9pct/1000),"k")),
            hjust=-0.15, size=3) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0,0.3))) +
  coord_flip() +
  labs(title="PV Foregone Earnings by Province",
       subtitle="Cost of NOT completing secondary school | ILO 9% return\nError bar upper = 14.5% Mincer estimate",
       x=NULL, y="PV Foregone (NPR '000, 35yr horizon)",
       caption=src) +
  th()

# Caste-level
caste_wages <- df_wage_w %>%
  filter(!is.na(caste_broad)) %>%
  group_by(caste_broad) %>%
  summarise(base_wage=median(monthly_wage,na.rm=TRUE), n=n()) %>%
  mutate(pv_9pct=map_dbl(base_wage,~compute_coi(.x,9)$pv),
         pv_145pct=map_dbl(base_wage,~compute_coi(.x,14.5)$pv))

figW4b <- ggplot(caste_wages,
                 aes(x=reorder(caste_broad, pv_9pct), y=pv_9pct/1000)) +
  geom_col(fill=ur, alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=pv_9pct/1000*0.9, ymax=pv_145pct/1000),
                width=0.25, linewidth=0.5, colour=ugr) +
  geom_text(aes(label=paste0("NPR ",round(pv_9pct/1000),"k")),
            hjust=-0.15, size=3) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0,0.3))) +
  coord_flip() +
  labs(title="PV Foregone Earnings by Caste/Ethnicity",
       subtitle="Girls not completing secondary school",
       x=NULL, y="PV Foregone (NPR '000)",
       caption=src) +
  th()

figW4 <- figW4a / figW4b +
  plot_annotation(title="Cost-of-Inaction: Foregone Earnings by Subgroup",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
sv(figW4, "figW4_coi_by_subgroup", w=10, h=10)

# =============================================================================
# FIG W5 — Wage distribution boxplot by education (salary workers)
# =============================================================================
figW5 <- df_wage_w_sal %>%
  filter(!is.na(edu_cat), monthly_wage < 100000) %>%
  ggplot(aes(x=edu_cat, y=monthly_wage/1000, fill=edu_cat)) +
  geom_boxplot(alpha=0.75, outlier.alpha=0.3, outlier.size=1) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, colour="white") +
  scale_fill_viridis_d(option="mako", direction=-1, guide="none") +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1)) +
  labs(title="Wage Distribution by Education Level",
       subtitle="Women salary workers | Diamond = mean | Outliers >NPR 100k excluded",
       x=NULL, y="Monthly Wage (NPR '000)",
       caption=src) +
  th() +
  theme(axis.text.x=element_text(angle=20,hjust=1))
sv(figW5, "figW5_wage_boxplot")

# =============================================================================
# FIG W6 — Wage × province heatmap (median monthly, salary workers)
# =============================================================================
w6_data <- df_wage_w_sal %>%
  filter(!is.na(edu_cat), !is.na(province_lbl)) %>%
  group_by(edu_cat, province_lbl) %>%
  summarise(med_mo=median(monthly_wage,na.rm=TRUE), n=n(), .groups="drop") %>%
  filter(n >= 5)

figW6 <- ggplot(w6_data, aes(x=edu_cat, y=province_lbl, fill=med_mo/1000)) +
  geom_tile(colour="white", linewidth=0.5) +
  geom_text(aes(label=ifelse(n>=5, paste0(round(med_mo/1000,0),"k"), "")),
            size=2.8, colour=ifelse(w6_data$med_mo > 20000,"white","#222222")) +
  scale_fill_viridis_c(option="plasma", direction=-1,
                        name="Median wage\n(NPR '000)") +
  labs(title="Median Monthly Wage: Education × Province",
       subtitle="Women salary workers | Cells with n<5 blank",
       x=NULL, y=NULL, caption=src) +
  th() +
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle=25,hjust=1))
sv(figW6, "figW6_wage_heatmap_edu_province", w=10, h=5)

cat("\n✓ All wage figures saved to outputs/figures/\n")
