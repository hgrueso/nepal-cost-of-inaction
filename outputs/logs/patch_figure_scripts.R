# One-off: makes figure titles and captions follow the data (no hard-coded numbers or claims)
# and points the slide figures to the single wage specification. Originals are kept in archive/.
P <- list(
 list("R/04_slides_figures.R",
  '  labs(title = "Girls enrol more, work less, and end up NEET twice as often",',
  '  labs(title = "Young women are far more likely than young men to be NEET",'),
 list("R/04_slides_figures.R",
  '       caption = "The gender gap is not in schools - it is in labour markets after schools.") +',
  '       caption = sprintf("Enrolled: girls %.0f%%, boys %.0f%% | NEET: girls %.0f%%, boys %.0f%%.",\n                         gap_overall$Enrolled[gap_overall$sex_lbl == "Female"], gap_overall$Enrolled[gap_overall$sex_lbl == "Male"],\n                         gap_overall$NEET[gap_overall$sex_lbl == "Female"], gap_overall$NEET[gap_overall$sex_lbl == "Male"])) +'),
 list("R/04_slides_figures.R",
  'figD2 <- ggplot(neet_prov,',
  'd2_gap <- neet_prov %>% select(province_lbl, sex_lbl, pct) %>%\n  tidyr::pivot_wider(names_from = sex_lbl, values_from = pct) %>% mutate(gap = Female - Male) %>% arrange(desc(gap))\nfigD2 <- ggplot(neet_prov,'),
 list("R/04_slides_figures.R",
  '  labs(title = "Madhesh and Lumbini show the widest female NEET gap",\n       subtitle = "Female NEET rate exceeds male in every province | Youth 15-24",',
  '  labs(title = paste(d2_gap$province_lbl[1], "and", d2_gap$province_lbl[2], "show the widest female NEET gap"),\n       subtitle = paste0(ifelse(all(d2_gap$gap > 0), "Female NEET rate exceeds male in every province", "NEET rate by province and sex"), " | Youth 15-24"),'),
 list("R/04_slides_figures.R",
  'figR2 <- ggplot(cate_caste, aes(x = mean_cate * 100, y = group)) +',
  'ate_pp <- 100 * unname(cf_results$ate_neet[1])\nfigR2 <- ggplot(cate_caste, aes(x = mean_cate * 100, y = group)) +'),
 list("R/04_slides_figures.R",
  '  geom_vline(xintercept = -13.2, linetype = "dotted", colour = "#333") +',
  '  geom_vline(xintercept = ate_pp, linetype = "dotted", colour = "#333") +'),
 list("R/04_slides_figures.R",
  '  annotate("text", x = -13.2, y = 0.5, label = "ATE = -13.2 pp",',
  '  annotate("text", x = ate_pp, y = 0.5, label = sprintf("Average effect = %.1f pp", ate_pp),'),
 list("R/04_slides_figures.R",
  '  labs(title = "Secondary education reduces NEET by ~13 pp - for every caste",\n       subtitle = "Causal forest CATE | Women 15-64 | Negative = education works",',
  '  labs(title = paste0("Schooling beyond grade 8 lowers NEET by about ", round(abs(ate_pp)), " pp, across caste groups"),\n       subtitle = "Causal forest estimates | Women 15-64 | Negative = lower NEET",'),
 list("R/04_slides_figures.R",
  '       caption = "Effects stable across groups. Dalit women benefit ~2 pp less.") +',
  '       caption = "Generalised random forest. Bars: 95% interval of the mean predicted effect in each group.") +'),
 list("R/04_slides_figures.R",
  'sv(figMINCER_lifetime, "m1_mincer_lifetime", w = 11, h = 5.5)',
  '# m1_mincer_lifetime is produced by R/03p_mincer_main.R (single wage specification)'),
 list("R/04_slides_figures.R",
  '  labs(title = "Secondary education protects unmarried girls more strongly",',
  '  labs(title = "NEET reduction associated with secondary schooling, by marital status",'),
 list("R/04_slides_figures.R",
  '       caption = "Interaction term in LPM with province FE. Marriage erodes much of the education return.") +',
  '       caption = "Linear probability model with province fixed effects; schooling (grade 9 or above) interacted with marital status.") +'),
 list("R/04_slides_figures.R",
  '  labs(title = "The gender NEET gap explodes after age 18",',
  '  labs(title = "The gender NEET gap widens after age 18",'),
 list("R/04_slides_figures.R",
  '       caption = "At 15-18 the gap is +9 pp; at 19-24 it reaches +25 pp.") +',
  '       caption = sprintf("NEET gap between girls and boys: %+.0f pp at ages 15-18 and %+.0f pp at ages 19-24.",\n                         with(gap_age, NEET[sex_lbl == "Female" & age == "15-18"] - NEET[sex_lbl == "Male" & age == "15-18"]),\n                         with(gap_age, NEET[sex_lbl == "Female" & age == "19-24"] - NEET[sex_lbl == "Male" & age == "19-24"]))) +'),
 list("R/04_slides_figures.R",
  'readr::write_csv(mincer_girls_tbl, file.path(TABLES, "mincer_girls_full.csv"))\nreadr::write_csv(mincer_boys_tbl,  file.path(TABLES, "mincer_boys_full.csv"))',
  '# mincer_girls_full.csv and mincer_boys_full.csv are written by R/03p_mincer_main.R'),
 list("R/08_slide_figures.R",
  'scripts <- c("R/03n_heckman_mincer.R", "R/04_slides_figures.R", "R/06_age_disagg_figures.R", "R/07_final_charts.R")',
  'scripts <- c("R/04_slides_figures.R", "R/03p_mincer_main.R", "R/06_age_disagg_figures.R", "R/07_final_charts.R")'),
 list("R/03n_heckman_mincer.R",
  '# =============================================================================\n# STAGE 1',
  '# SUPERSEDED by R/03p_mincer_main.R (household composition is now built per household,\n# and one wage specification is used throughout). Kept for the record; not part of the pipeline.\n# =============================================================================\n# STAGE 1'))
bk <- file.path("archive", "scripts_before_sample_weight_fix"); dir.create(bk, recursive = TRUE, showWarnings = FALSE)
ok <- TRUE
for (f in unique(vapply(P, `[[`, "", 1))) {
  x <- paste(readLines(f, warn = FALSE), collapse = "\n"); x0 <- x
  for (p in Filter(function(p) p[[1]] == f, P)) {
    n_old <- lengths(regmatches(x, gregexpr(p[[2]], x, fixed = TRUE))); done <- grepl(p[[3]], x, fixed = TRUE)
    if (done) next
    if (n_old == 1) x <- sub(p[[2]], p[[3]], x, fixed = TRUE)
    else { ok <- FALSE; cat("  NOT APPLIED in", f, ":", substr(p[[2]], 1, 70), "\n") }
  }
  if (!identical(x, x0)) {
    if (!file.exists(file.path(bk, basename(f)))) file.copy(f, file.path(bk, basename(f)))
    writeLines(x, f); invisible(parse(f)); cat("  patched", f, "\n")
  } else cat("  unchanged", f, "\n")
}
cat(if (ok) "All figure-text patches in place.\n" else "Some patches were not applied (see above).\n")
