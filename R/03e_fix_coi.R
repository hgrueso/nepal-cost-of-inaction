# Fix CoI table and produce final summary
library(tidyverse); library(scales)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")

FIGS   <- "outputs/figures"
TABLES <- "outputs/tables"

th <- function(b=11) theme_minimal(base_size=b) +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(colour="#6C7279"),
        plot.caption=element_text(size=7,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(), legend.position="bottom")

# Province-level wage medians
prov_wages <- df_wage_w %>%
  filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(base_wage=median(monthly_wage, na.rm=TRUE), n_wage=n())

# Fix: use character matching for join
cate_prov_fixed <- cf_results$cate_neet_all %>%
  filter(dimension == "Province") %>%
  mutate(province_lbl = as.character(group)) %>%
  left_join(prov_wages %>% mutate(province_lbl=as.character(province_lbl)),
            by="province_lbl") %>%
  mutate(
    cate_neet       = mean_cate,                          # already in pp (fraction)
    pv_per_girl_ilo = base_wage * 12 * ((1.09)^2 - 1) *  # ILO 9%, 2 yrs gained
                      (1 - 1.05^(-35)) / 0.05,
    pv_per_girl_145 = base_wage * 12 * ((1.145)^2 - 1) * # Mincer 14.5%
                      (1 - 1.05^(-35)) / 0.05,
    # Cost per 100 girls = CATE × 100 × PV (CATE is negative for NEET)
    # abs(CATE) = fraction of girls who would NOT be NEET if secondary completed
    coi_100_ilo     = abs(cate_neet) * 100 * pv_per_girl_ilo / 1000,
    coi_100_145     = abs(cate_neet) * 100 * pv_per_girl_145 / 1000
  )

cat("=== COST-OF-INACTION BY PROVINCE (CORRECTED) ===\n")
cat("ILO 9% return | 35yr horizon | 5% discount | per 100 girls\n\n")
cate_prov_fixed %>%
  select(province_lbl, cate_neet, base_wage, pv_per_girl_ilo, coi_100_ilo, coi_100_145) %>%
  mutate(
    cate_neet_pp    = round(cate_neet * 100, 1),
    base_wage_mo    = round(base_wage),
    pv_ilo_kNPR     = round(pv_per_girl_ilo / 1000),
    coi_ilo_kNPR    = round(coi_100_ilo),
    coi_145_kNPR    = round(coi_100_145)
  ) %>%
  select(province_lbl, cate_neet_pp, base_wage_mo, pv_ilo_kNPR,
         coi_ilo_kNPR, coi_145_kNPR) %>%
  arrange(desc(abs(cate_neet_pp))) %>%
  print()

# Caste CoI
caste_wages <- df_wage_w %>%
  filter(!is.na(caste_broad)) %>%
  group_by(caste_broad) %>%
  summarise(base_wage=median(monthly_wage, na.rm=TRUE))

cate_caste_fixed <- cf_results$cate_neet_all %>%
  filter(dimension == "Caste/Ethnicity") %>%
  mutate(caste_broad = as.character(group)) %>%
  left_join(caste_wages %>% mutate(caste_broad=as.character(caste_broad)),
            by="caste_broad") %>%
  mutate(
    cate_neet       = mean_cate,
    pv_per_girl_ilo = base_wage * 12 * ((1.09)^2-1) * (1-1.05^(-35))/0.05,
    coi_100_ilo     = abs(cate_neet) * 100 * pv_per_girl_ilo / 1000
  )

cat("\n=== COST-OF-INACTION BY CASTE ===\n")
cate_caste_fixed %>%
  mutate(cate_pp=round(cate_neet*100,1),
         base_wage_mo=round(base_wage),
         pv_kNPR=round(pv_per_girl_ilo/1000),
         coi_kNPR=round(coi_100_ilo)) %>%
  select(caste_broad, cate_pp, base_wage_mo, pv_kNPR, coi_kNPR) %>%
  arrange(cate_pp) %>% print()

# Save
readr::write_csv(cate_prov_fixed, file.path(TABLES, "coi_by_province.csv"))
readr::write_csv(cate_caste_fixed, file.path(TABLES, "coi_by_caste.csv"))

# Better CoI figure
figCF5_fixed <- ggplot(
  cate_prov_fixed %>% filter(!is.na(coi_100_ilo)),
  aes(x=reorder(province_lbl, coi_100_ilo), y=coi_100_ilo)) +
  geom_col(fill="#006B9F", alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=coi_100_ilo*0.85, ymax=coi_100_145),
                width=0.25, linewidth=0.5, colour="#6C7279") +
  geom_text(aes(label=paste0("NPR ",round(coi_100_ilo),"k")),
            hjust=-0.15, size=3.2) +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0, 0.3))) +
  coord_flip() +
  labs(title="Cost of Inaction per 100 Girls — By Province",
       subtitle="CATE of secondary education on NEET × PV foregone earnings\nBar = ILO 9% | Error bar upper = Mincer 14.5%",
       x=NULL, y="Estimated Cost (NPR '000 per 100 girls)",
       caption="Source: NLSS IV 2022/23. Causal forest CATE × Mincer earnings projection.") +
  th()
ggsave(file.path(FIGS,"figCF5_coi_by_province_cate.png"),
       figCF5_fixed, width=9, height=5, dpi=300)
cat("\n✓ figCF5 updated\n")

# Summary table for report
cat("\n=== HEADLINE NUMBERS FOR REPORT ===\n")
cat(sprintf("ATE of secondary education on NEET (all women): %.1f pp\n",
            cf_results$ate_neet[1]*100))
cat(sprintf("Range across provinces: %.1f to %.1f pp\n",
            min(cate_prov_fixed$cate_neet*100),
            max(cate_prov_fixed$cate_neet*100)))
cat(sprintf("Dalit vs Brahmin/Chhetri gap in CATE: %.1f pp\n",
            (cate_caste_fixed %>% filter(caste_broad=="Dalit") %>% pull(cate_neet) -
             cate_caste_fixed %>% filter(caste_broad=="Brahmin/Chhetri") %>% pull(cate_neet)) * 100))
cat(sprintf("\nCost of inaction (Karnali, ILO 9%%): NPR %.0fk per 100 girls\n",
            cate_prov_fixed %>% filter(province_lbl=="Karnali") %>% pull(coi_100_ilo)))
cat(sprintf("Cost of inaction (Bagmati, ILO 9%%): NPR %.0fk per 100 girls\n",
            cate_prov_fixed %>% filter(province_lbl=="Bagmati") %>% pull(coi_100_ilo)))

# =============================================================================
# Rebuild figCF5 with NPR labels to the RIGHT of the error bar
# =============================================================================
library(tidyverse); library(scales)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")
FIGS <- "outputs/figures"

prov_wages <- df_wage_w %>% filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(base_wage=median(monthly_wage,na.rm=TRUE), .groups="drop")

coi_prov <- cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>%
  mutate(province_lbl=as.character(group)) %>%
  left_join(prov_wages %>% mutate(province_lbl=as.character(province_lbl)),
            by="province_lbl") %>%
  mutate(
    pv_ilo  = base_wage*12*((1.09)^2-1)*(1-1.05^(-35))/0.05,
    pv_145  = base_wage*12*((1.145)^2-1)*(1-1.05^(-35))/0.05,
    coi_ilo = abs(mean_cate)*100*pv_ilo/1000,
    coi_145 = abs(mean_cate)*100*pv_145/1000,
    # label positioned after the upper CI bar
    label_x = coi_145 + max(coi_ilo, na.rm=TRUE)*0.05
  ) %>% filter(!is.na(coi_ilo))

figCF5 <- ggplot(coi_prov,
                  aes(x=reorder(province_lbl, coi_ilo), y=coi_ilo)) +
  geom_col(fill="#006B9F", alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=coi_ilo*0.9, ymax=coi_145),
                width=0.25, linewidth=0.5, colour="#6C7279") +
  # Labels placed to the RIGHT of the 14.5% error bar
  geom_text(aes(y=label_x, label=paste0("NPR ",round(coi_ilo),"k")),
            hjust=0, size=2.8, colour="#333333") +
  scale_y_continuous(labels=label_comma(suffix="k",scale=1),
                     expand=expansion(mult=c(0,0.3))) +
  coord_flip() +
  labs(title="Cost of Inaction per 100 Girls --- By Province",
       subtitle="CATE x PV foregone earnings | Bar=ILO 9% | Error bar=Mincer 14.5% | Label=ILO estimate",
       x=NULL, y="Estimated Cost (NPR '000 per 100 girls)",
       caption="Source: NLSS IV 2022/23. Causal forest CATE x Mincer earnings projection.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8.5,colour="#6C7279"),
        plot.caption=element_text(size=6.5,colour="#6C7279",hjust=0),
        panel.grid.minor=element_blank(), legend.position="bottom")

ggsave(file.path(FIGS,"figCF5_coi_by_province_cate.png"),
       figCF5, width=9, height=4.5, dpi=300)
cat("  ✓ figCF5 rebuilt with labels right of CI bar\n")

readr::write_csv(coi_prov, "outputs/tables/coi_by_province.csv")

# =============================================================================
# Rebuild figCF5 — labels CLEARLY to the right of CI, no overlap
# =============================================================================
library(tidyverse); library(scales)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")
FIGS <- "outputs/figures"

prov_wages <- df_wage_w %>% filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(base_wage=median(monthly_wage,na.rm=TRUE), .groups="drop")

coi_prov <- cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>%
  mutate(province_lbl=as.character(group)) %>%
  left_join(prov_wages %>% mutate(province_lbl=as.character(province_lbl)),
            by="province_lbl") %>%
  mutate(
    pv_ilo  = base_wage*12*((1.09)^2-1)*(1-1.05^(-35))/0.05,
    pv_145  = base_wage*12*((1.145)^2-1)*(1-1.05^(-35))/0.05,
    coi_ilo = abs(mean_cate)*100*pv_ilo/1000,
    coi_145 = abs(mean_cate)*100*pv_145/1000
  ) %>% filter(!is.na(coi_ilo))

# X-axis max for positioning
x_max <- max(coi_prov$coi_145, na.rm=TRUE)

figCF5 <- ggplot(coi_prov,
                  aes(x=reorder(province_lbl, coi_ilo), y=coi_ilo)) +
  geom_col(fill="#006B9F", alpha=0.85, width=0.65) +
  # Error bar from ILO to Mincer estimate
  geom_errorbar(aes(ymin=coi_ilo, ymax=coi_145),
                width=0.25, linewidth=0.7, colour="#555555") +
  # Label at fixed distance from RIGHT end of CI bar
  geom_text(aes(y=coi_145 + x_max*0.06,
                label=paste0("NPR ", format(round(coi_ilo), big.mark=","), "k")),
            hjust=0, size=3.0, colour="#222222") +
  scale_y_continuous(labels=label_comma(suffix="k", scale=1),
                     expand=expansion(mult=c(0, 0.30))) +
  coord_flip() +
  labs(title="Cost of Inaction per 100 Girls --- By Province",
       subtitle="Bar = ILO 9% estimate | Error bar right end = Mincer 14.5% upper bound | Label = ILO 9% value",
       x=NULL, y="Estimated Cost (NPR '000 per 100 girls)",
       caption="Source: NLSS IV 2022/23. Causal forest CATE x Mincer earnings projection.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8.5, colour="#6C7279"),
        plot.caption=element_text(size=6.5, colour="#6C7279", hjust=0),
        panel.grid.minor=element_blank(), legend.position="bottom")

ggsave(file.path(FIGS,"figCF5_coi_by_province_cate.png"),
       figCF5, width=9, height=4.5, dpi=300)
cat("  ✓ figCF5 final version — labels clearly right of CI\n")
readr::write_csv(coi_prov, "outputs/tables/coi_by_province.csv")
