# =============================================================================
# 03l_coi_scaled.R — Scale CoI to population, express as % of provincial GDP
# Sources: NSO Nepal provincial GDP 2023/24, NLSS IV population weights
# =============================================================================
library(tidyverse); library(scales)
load("outputs/cleaned_data_v2.RData")
load("outputs/causal_forest_results.RData")

FIGS   <- "outputs/figures"; TABLES <- "outputs/tables"
ud<-"#006B9F"; ub<-"#1CABE2"; ur<-"#E2231A"; ugr<-"#6C7279"

# ── Provincial GDP (NPR billion, NSO 2023/24, consumer prices) ─────────────
# Source: NSO Nepal, Rising Nepal Daily (May 2024), Grokipedia
prov_gdp <- tribble(
  ~province_lbl,           ~gdp_nprb,  # NPR billion 2023/24
  "Bagmati",                2074,
  "Koshi",                   903,
  "Lumbini",                 814,
  "Madhesh",                 748,
  "Gandaki",                 517,
  "Sudurpashchim",           404,
  "Karnali",                 244
)

# ── Estimate girls 15-24 NOT completing secondary, per province ──────────────
# From NLSS IV survey weights: scale up to province populations
# Secondary completion rate from data; complement = not completed
# Population of girls 15-24 per province estimated from survey weights
pop_girls_prov <- df_girls %>%
  filter(!is.na(province_lbl)) %>%
  group_by(province_lbl) %>%
  summarise(
    n_sample          = n(),
    total_weight      = sum(ind_wt, na.rm=TRUE),
    pct_no_secondary  = mean(completed_secondary==0, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    province_lbl = as.character(province_lbl),
    # Scale weights to population (NLSS weights sum to ~population)
    # Girls 15-24 national population ~1.4M; distribute by weight share
    nat_pop_girls_1524 = 1400000,
    pop_share         = total_weight / sum(total_weight),
    pop_girls_prov    = round(nat_pop_girls_1524 * pop_share),
    n_excluded_girls  = round(pop_girls_prov * pct_no_secondary)
  )

cat("Girls 15-24 population estimate by province:\n")
print(pop_girls_prov %>% select(province_lbl, pop_girls_prov, pct_no_secondary, n_excluded_girls))

# ── Median wages per province (from salary workers) ─────────────────────────
# Median wages by province from 03e_fix_coi.R results (salary workers)
# These match the CoI table in the slides
prov_wages <- tribble(
  ~province_lbl,    ~base_wage,
  "Bagmati",         7000,
  "Koshi",           2500,
  "Gandaki",         2292,
  "Madhesh",         2000,
  "Lumbini",         1500,
  "Karnali",         1066,
  "Sudurpashchim",   1042
)
cat("Using hardcoded median wages from salary worker analysis\n")

# ── CATE per province ────────────────────────────────────────────────────────
cate_prov <- cf_results$cate_neet_all %>%
  filter(dimension=="Province") %>%
  mutate(province_lbl = as.character(group),
         cate_abs = abs(mean_cate))

# ── Full CoI calculation ──────────────────────────────────────────────────────
coi_scaled <- pop_girls_prov %>%
  left_join(prov_wages, by="province_lbl") %>%
  left_join(cate_prov %>% select(province_lbl, cate_abs), by="province_lbl") %>%
  left_join(prov_gdp, by="province_lbl") %>%
  mutate(
    # PV per girl (ILO 9%, 2 years secondary, 35yr horizon, 5% discount)
    pv_per_girl_ilo = base_wage * 12 * ((1.09)^2 - 1) * (1 - 1.05^(-35)) / 0.05,
    # CoI per 100 girls (NPR '000)
    coi_per_100_ilo = cate_abs * 100 * pv_per_girl_ilo / 1000,
    # Total provincial CoI (NPR million) = CoI per girl × N excluded girls
    pv_per_girl_m   = pv_per_girl_ilo / 1e6,  # NPR million per girl
    total_coi_m     = cate_abs * n_excluded_girls * pv_per_girl_ilo / 1e6,
    # As % of provincial GDP (GDP in NPR billion → convert CoI to billion)
    total_coi_b     = total_coi_m / 1000,
    coi_pct_gdp     = total_coi_b / gdp_nprb * 100
  ) %>%
  arrange(desc(total_coi_b))

cat("\nScaled CoI by province:\n")
coi_scaled %>%
  select(province_lbl, n_excluded_girls, total_coi_b, gdp_nprb, coi_pct_gdp) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  print()

cat(sprintf("\nNATIONAL TOTAL CoI: NPR %.1f billion\n", sum(coi_scaled$total_coi_b, na.rm=TRUE)))
readr::write_csv(coi_scaled, file.path(TABLES, "coi_scaled_province.csv"))

# ── Figure: total CoI as % of provincial GDP ────────────────────────────────
fig_gdp_pct <- ggplot(coi_scaled %>% filter(!is.na(coi_pct_gdp)),
  aes(x=reorder(province_lbl, coi_pct_gdp), y=coi_pct_gdp)) +
  geom_col(fill=ud, alpha=0.85, width=0.65) +
  geom_text(aes(label=paste0(round(coi_pct_gdp,1),"%"), y=coi_pct_gdp+0.05),
            hjust=0, size=3.2, colour="#222222") +
  scale_y_continuous(labels=label_percent(scale=1),
                     expand=expansion(mult=c(0, 0.25))) +
  coord_flip() +
  labs(subtitle="Total lifetime earnings loss from girls not completing secondary school\nas a share of provincial GDP | ILO 9% return | 35-year horizon",
       x=NULL, y="Cost of Inaction as % of Provincial GDP",
       caption="Sources: NLSS IV 2022/23 (CATE, wages); NSO Nepal (provincial GDP 2023/24); population scaled from survey weights.") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_blank(),
        plot.subtitle=element_text(size=9, colour="#222222", face="bold"),
        plot.caption=element_text(size=6.5, colour=ugr, hjust=0),
        panel.grid.minor=element_blank())
ggsave(file.path(FIGS,"figCOI_gdp_pct.png"), fig_gdp_pct, width=9, height=4.5, dpi=300)
cat("  ✓ figCOI_gdp_pct\n")

# ── Figure: absolute total CoI in NPR billion ────────────────────────────────
fig_total_coi <- ggplot(coi_scaled %>% filter(!is.na(total_coi_b)),
  aes(x=reorder(province_lbl, total_coi_b), y=total_coi_b)) +
  geom_col(aes(fill=ifelse(province_lbl %in% c("Karnali","Sudurpashchim"), ur, ud)),
           alpha=0.85, width=0.65) +
  geom_text(aes(label=paste0("NPR ", round(total_coi_b,1),"B"), y=total_coi_b+0.5),
            hjust=0, size=3.0, colour="#222222") +
  scale_fill_identity() +
  scale_y_continuous(labels=label_number(suffix="B", scale=1),
                     expand=expansion(mult=c(0, 0.25))) +
  coord_flip() +
  labs(subtitle="Total estimated lifetime productivity loss per province (NPR billion)\nScaled to full population of girls 15-24 not completing secondary | ILO 9% return",
       x=NULL, y="Total Cost of Inaction (NPR Billion)",
       caption=paste("Sources: NLSS IV 2022/23; NSO Nepal provincial GDP 2023/24; population estimates from survey weights.",
                     "Red = equity-priority provinces (highest CATE, lowest wages). National total:",
                     paste0("NPR ", round(sum(coi_scaled$total_coi_b, na.rm=TRUE),1),"B."))) +
  theme_minimal(base_size=10) +
  theme(plot.title=element_blank(),
        plot.subtitle=element_text(size=9, colour="#222222", face="bold"),
        plot.caption=element_text(size=6.5, colour=ugr, hjust=0),
        panel.grid.minor=element_blank(), legend.position="none")
ggsave(file.path(FIGS,"figCOI_total_province.png"), fig_total_coi, width=9, height=4.5, dpi=300)
cat("  ✓ figCOI_total_province\n")

cat("\n✓ Scaled CoI analysis complete.\n")
