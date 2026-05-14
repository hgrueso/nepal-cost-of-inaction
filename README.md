# Nepal Cost-of-Inaction — Adolescent Girls

This repository contains the R code and methodology to estimate the **economic cost of inaction** on adolescent girls' secondary education in Nepal. The analysis combines descriptive evidence on gender gaps in education and employment from the Nepal Living Standards Survey (NLSS IV 2022/23), multivariate regression analysis with intersectional cuts, causal forest treatment-effect estimation, and a scaled Mincer-based projection of forgone lifetime earnings.

The output is a policy-ready investment case for the new government of Nepal, quantifying the annual macroeconomic loss from girls' educational exclusion and identifying the highest-return policy levers.

---

## Repository structure

```
analysis/
├── .here                            # project root sentinel
├── README.md                        # this file
├── REPLICATE.sh                     # full pipeline driver
│
├── R/                               # numbered analytical stages
│   ├── 00d_fix_wages.R              # wage construction from S10
│   ├── 00e_diagnostics.R            # sex coding + survey weights checks
│   ├── 01_load_clean_v2.R           # data cleaning + module merge
│   ├── 02_descriptives.R            # weighted descriptive statistics
│   ├── 03_regressions_v2.R          # LPM enrolment, NEET, employment
│   ├── 03b_mincer.R                 # Mincer wage equation
│   ├── 03c_wage_plots.R             # wage distribution figures
│   ├── 03d_causal_forest.R          # grf treatment effects
│   ├── 03e_fix_coi.R                # cost-of-inaction calculation
│   ├── 03i_descriptives_and_primary.R   # descriptive table + primary enrolment
│   ├── 03j_fix_labels_and_cate.R    # CATE figures with corrected labels
│   ├── 03k_final_figures.R          # consolidated final figures
│   ├── 03l_coi_scaled.R             # CoI scaled to population + provincial GDP
│   ├── 03m_boys_and_intersectionality.R  # boys' analysis + intersectionality
│   └── 04_slides_figures.R          # policy-ready figures for revealjs deck
│
├── data/                            # input data (raw microdata gitignored)
│   └── NLSSIV_microdata/             # raw NLSS IV 2022/23 microdata (NOT committed)
│       └── data/stata format/        # S01, S07, S08, S09, S10, poverty
│
├── reports/                         # slide decks
│   ├── nepal_slides_en.qmd          # policy-ready revealjs + Beamer deck
│   ├── slides_theme.scss            # revealjs UNICEF theme
│   └── nepal_nlss_slides_v3.qmd     # earlier Beamer slide deck (legacy)
│
├── slides/                          # rendered deliverables (committed)
│   ├── nepal_slides_en.html         # interactive slide deck
│   └── nepal_slides_en.pdf          # PDF version for distribution
│
└── outputs/                         # generated artefacts (most gitignored)
    ├── cleaned_data_v2.RData        # cleaned analytical dataset
    ├── regression_results_v2.RData  # LPM model objects
    ├── causal_forest_results.RData  # grf results
    ├── coi_summary_numbers.RData    # headline CoI figures
    ├── figures/                     # PNG figures used in the deck
    └── tables/                      # CSV tables (CoI, descriptive, regressions)
```

---

## Methodology summary

### Data

**Nepal Living Standards Survey IV (NLSS IV) 2022/23 — National Statistics Office.** Nationally representative household survey with 46,870 individuals across 7 provinces. Survey design implemented with `survey` and `srvyr`, using primary sampling units, province strata, and individual sampling weights (`ind_wt`).

### Descriptive layer

Gender gaps in core education and labour-market outcomes — **school enrolment**, **secondary enrolment**, **employment**, and **NEET (not in education, employment, or training)** — disaggregated by sex, then intersectionally by province, caste, marital status, rural/urban, wealth quintile, and disability (Washington Group). Outcomes broken down by age (15–18 vs 19–24) to identify the post-secondary transition gap.

### Multivariate analysis

Linear probability models with province fixed effects, sampling weights, and HC1 standard errors (`fixest::feols`). Reported in percentage points throughout for policy readability. Reference categories: Brahmin/Chhetri, urban, non-poor.

### Causal effect

Generalised Random Forest (Wager & Athey 2018) with honest splitting (`grf::causal_forest`). Treatment = completion of secondary education. Outcome = NEET. Sample = women 15–64 (n = 13,586). Conditional Average Treatment Effects extracted by caste, province, and wealth quintile.

### Wage projection

Mincer equation on 952 women in long-term salaried employment, controlling for years of schooling, experience, experience², rural and caste, with province fixed effects. Returns expressed as percentage gain per year of schooling. Robustness with ILO Nepal benchmark of 9% per year of schooling.

### Cost of inaction

Three-step calculation per province *p*:

```
CATE_p × N_excluded_p × PV_p

where PV_p = base_wage_p × 12 × ((1 + r)^Δs − 1) × annuity(d, T)
            r = 0.09 (ILO benchmark)
            Δs = 2 (years of secondary education)
            d = 0.05 (discount rate)
            T = 35 (working horizon)
```

Aggregated to national total, then expressed as: NPR billion, share of national sector budgets, and wage-months equivalent per girl.

---

## Reproducing the analysis

### Prerequisites

```r
install.packages(c(
  "haven", "labelled", "dplyr", "tidyr", "purrr", "stringr",
  "readr", "tibble", "here", "fs", "glue",
  "survey", "srvyr", "fixest", "broom", "modelsummary",
  "Hmisc", "ggplot2", "scales", "ggtext", "patchwork",
  "openxlsx", "grf", "kableExtra"
))
```

[Quarto](https://quarto.org) and a recent LaTeX/pandoc installation are required to render the slide decks. To render Beamer PDF you will need `lualatex` available (TeX Live 2023 or later).

### Run the full pipeline

```bash
# From the analysis/ root:
bash REPLICATE.sh
```

Or run individual stages — see the `R/` folder for the numbered sequence.

### Render the slide decks

```bash
# Both HTML (revealjs) and PDF (Beamer) versions:
quarto render reports/nepal_slides_en.qmd
```

Output files: `reports/nepal_slides_en.html` and `reports/nepal_slides_en.pdf`.

---

## Data access

Raw NLSS IV 2022/23 microdata is distributed by the National Statistics Office of Nepal and is **not redistributed in this repository** (see `.gitignore`). To reproduce the analysis end-to-end, place the .dta files in `data/NLSSIV_microdata/data/stata format/`. The pipeline reads from there.

---

## Contact

**Hernando Grueso, PhD**
AImpact Lab · Tech-enabled impact evaluation
📧 `hgrueso@unicef.org`
🌐 [aimpactlab.com](https://aimpactlab.com)
