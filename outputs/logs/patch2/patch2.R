# Slide review fixes: relative-earnings chart, weighted figure models, corrected figure titles.
args <- commandArgs(TRUE); blk <- args[1]
rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
P <- list(
  list("R/03p_mincer_main.R", rd(file.path(blk, "old_m1_block.txt")), rd(file.path(blk, "new_m1_block.txt"))),
  list("R/06_age_disagg_figures.R", 'labs(title = "The marriage association strengthens with age; most other factors are stable",',
       'labs(title = "The marriage association is strongest at ages 15-18; most other factors are stable",'),
  list("R/04_slides_figures.R", 'province_lbl, data = df_int, se = "hetero")', 'province_lbl, data = df_int, weights = ~ind_wt, se = "hetero")'),
  list("R/04_slides_figures.R", 'subtitle = "Effect of secondary completion on NEET probability, by marital status",',
       'subtitle = "Difference in NEET associated with schooling beyond grade 8, by marital status | Girls 15-24, survey-weighted",'),
  list("R/04_slides_figures.R", 'data = df_boys_reg, se = "hetero")', 'data = df_boys_reg, weights = ~ind_wt, se = "hetero")'),
  list("R/04_slides_figures.R", 'subtitle = "Marriage matters much less for boys; poverty matters more",',
       'subtitle = "Marriage matters much less for boys than for girls | survey-weighted",'),
  list("R/04_slides_figures.R", 'caption = "LPM with province FE | HC1 SE | Ref: Brahmin/Chhetri, urban, non-poor.") +',
       'caption = "LPM with province FE, survey-weighted | HC1 SE | Ref: Brahmin/Chhetri, urban, non-poor.") +'))
bk <- "archive/scripts_before_slide_review"; dir.create(bk, recursive = TRUE, showWarnings = FALSE); ok <- TRUE
for (p in P) {
  x <- rd(p[[1]])
  if (grepl(p[[3]], x, fixed = TRUE)) { cat("  already done:", p[[1]], "\n"); next }
  n <- lengths(regmatches(x, gregexpr(p[[2]], x, fixed = TRUE)))
  if (n != 1) { ok <- FALSE; cat("  NOT APPLIED:", p[[1]], substr(p[[2]], 1, 60), "\n"); next }
  if (!file.exists(file.path(bk, basename(p[[1]])))) file.copy(p[[1]], file.path(bk, basename(p[[1]])))
  writeLines(sub(p[[2]], p[[3]], x, fixed = TRUE), p[[1]]); invisible(parse(p[[1]])); cat("  patched:", p[[1]], "\n")
}
cat(if (ok) "All script patches in place.\n" else "SOME PATCHES FAILED\n"); if (!ok) quit(status = 1)
