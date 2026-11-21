# Nepal: the cost of inaction on adolescent girls' education

R code behind the UNICEF analysis of the economic cost of inaction on adolescent girls' education and school-to-work transition in Nepal, using the Nepal Living Standards Survey IV (NLSS IV 2022/23).

The analysis documents gender gaps in enrolment, employment and NEET (not in education, employment or training), identifies the factors associated with them (marriage above all), estimates the effect of schooling beyond grade 8 on NEET with a causal forest, estimates returns to schooling with a Mincer wage equation, and values the lifetime earnings forgone by girls who do not progress beyond grade 8.

## Main results

| Measure | Result |
|---|---|
| Lifetime earnings forgone by girls aged 15-24 who have not completed grade 9 | NPR 10.8 billion (USD 81 million) |
| Girls aged 15-24 who have not completed grade 9 | 1.0 million (743,000 out of school, 257,000 still enrolled) |
| Added by each new yearly cohort | about NPR 1 billion |
| NEET, ages 15-24 (girls / boys) | 44% / 25% |
| NEET, ages 19-24 (women / men) | 61% / 35% |
| Enrolment, ages 15-18 (girls / boys) | 77% / 76% |
| Marriage and NEET, girls 15-24 (linear probability model) | +57 percentage points |
| Schooling beyond grade 8 and NEET, women 15-64 (causal forest) | -15 percentage points (95% interval -21 to -9) |
| Return per year of schooling (women / men) | 14.3% / 9.3% |

Every number cited in the report, slides and two-pager is written to `outputs/tables/key_numbers.csv` by `R/12_key_numbers.R`.

## Data

NLSS IV 2022/23, National Statistics Office of Nepal: 9,600 households and 38,101 household members. The household roster also lists 8,769 people who are not household members; they were not asked about education or work and are excluded. Estimates use the person-level sampling weight (`hhs_wt`), primary sampling units and provincial strata. Weighted, household members add up to 28.7 million people (2021 Census: 29.2 million). National counts apply survey shares to the 2021 Census female population.

The raw microdata are distributed by the National Statistics Office and are not included in this repository. To reproduce the analysis, place the .dta files in `data/NLSSIV_microdata/data/stata format/`.

## Reproducing the analysis

```bash
bash REPLICATE.sh      # from the raw microdata: cleaning, analysis, figures, tables, slide decks
bash run_pipeline.sh   # analysis only, from an existing outputs/cleaned_data_v2.RData
```

Each script writes a log to `outputs/logs/`; `outputs/logs/_status.txt` lists OK or FAILED per script.

Required R packages: tidyverse, haven, srvyr, survey, fixest, modelsummary, broom, sandwich, sampleSelection, grf, patchwork, scales, knitr. The slide decks need Quarto and a LaTeX installation.

## Pipeline

| Script | What it does |
|---|---|
| `R/01_load_clean_v2.R` | Merges the NLSS IV modules and builds the analysis variables |
| `R/00d_fix_wages.R` | Wage earnings from S10 (salaried, daily and contract work) |
| `R/00f_secondary_flag.R` | Current-grade flags used in the descriptive figures |
| `R/01b_fix_sample_and_weights.R` | Keeps household members only, sets the person-level weight, rebuilds the analysis samples |
| `R/03_regressions_v2.R` | Linear probability models for enrolment and NEET |
| `R/03b_mincer.R` | Earlier Mincer specifications (kept for reference) |
| `R/03d_causal_forest.R` | Causal forest: effect of grade 9 or above on NEET, women 15-64 |
| `R/04_slides_figures.R` | Descriptive and regression figures |
| `R/03p_mincer_main.R` | Returns to schooling (one specification used throughout) and Heckman selection test |
| `R/05_census_anchor_coi.R` | Census-anchored cost of inaction by province |
| `R/06_age_disagg_figures.R` | Outcomes and regressions by age group (15-18, 19-24) |
| `R/07_final_charts.R` | Cost charts and marriage by age |
| `R/09_causal_forest_figure.R` | Causal forest effects by province and wealth |
| `R/08_slide_figures.R` | Slide versions of the figures (without titles) |
| `R/10_v5_comment_checks.R` | Figures behind replies to reviewer comments |
| `R/11_sample_and_weight_check.R` | Diagnostic comparing results before and after the sample and weight correction |
| `R/12_key_numbers.R` | Every number cited in the deliverables |

`R/02_descriptives.R`, `R/03e_fix_coi.R`, `R/03l_coi_scaled.R`, `R/03m_boys_and_intersectionality.R` and `R/03n_heckman_mincer.R` are earlier stages kept for reference and are not part of the pipeline.

## Deliverables

| File | Content |
|---|---|
| `reports/Nepal_CoI_Policy_Report_V7.docx` | Policy report (tracked changes and comments for review) |
| `reports/Nepal_CoI_Two_Pager.pdf` | Two-page summary of key takeaways |
| `reports/nepal_coi_slides_V6.pdf` / `.pptx` | Full slide deck |
| `reports/nepal_coi_slides_short_V6.pdf` / `.pptx` | Short slide deck |

## Method notes

- **Cost of inaction.** For each province: effect of schooling on NEET x girls aged 15-24 who have not completed grade 9 x present value of the earnings gain from two more years of schooling. Return 9% per year (ILO benchmark, below the 14.3% estimated here), 5% discount rate, 35 working years, provincial median wages. The total is a lifetime figure for the girls aged 15-24 today (ten yearly cohorts), not a yearly figure.
- **Correction (November 2026).** Earlier versions included roster members who are not household members and used the household-level weight (`hhs_wt` x household size) for individuals. Correcting both lowered the cost estimate from NPR 16.3 to 10.8 billion; the main findings hold.
- **Caveats.** The cost assumes jobs are available at current wages for girls who stay in school longer. Results on marriage, caste and disability are associations.

## Contact

Hernando Grueso-Hurtado, UNICEF (hgrueso@unicef.org)
