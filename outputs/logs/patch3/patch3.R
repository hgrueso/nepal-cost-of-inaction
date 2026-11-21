# Consistency fixes: clearer chart labels, budget years, one source for the marriage-by-age chart.
rd <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
P <- list(
  list("R/04_slides_figures.R", 'figD1 <- ggplot(g_long, aes(x = outcome, y = value, fill = sex_lbl)) +',
       'figD1 <- ggplot(g_long, aes(x = outcome, y = value, fill = sex_lbl)) +\n  scale_x_discrete(labels = function(x) sub("^Secondary$", "In secondary school", x)) +'),
  list("R/04_slides_figures.R", 'figD4 <- ggplot(ga_long, aes(x = outcome, y = value, fill = sex_lbl)) +',
       'figD4 <- ggplot(ga_long, aes(x = outcome, y = value, fill = sex_lbl)) +\n  scale_x_discrete(labels = function(x) sub("^Secondary$", "In secondary\\nschool", x)) +'),
  list("R/06_age_disagg_figures.R", 'sv(figB1, "b1_marriage_by_age", w = 9, h = 5)',
       '# b1_marriage_by_age is produced by R/07_final_charts.R (labels by current age and status)'),
  list("R/07_final_charts.R", 'subtitle="NPR billion | census-anchored estimate (r = 9%), FY 2024/25 budgets",',
       'subtitle="NPR billion | health and education budgets FY 2024/25, social security allowances FY 2022/23",'))
ok <- TRUE
for (p in P) { x <- rd(p[[1]])
  if (grepl(p[[3]], x, fixed = TRUE)) { cat("  already done:", p[[1]], "\n"); next }
  if (lengths(regmatches(x, gregexpr(p[[2]], x, fixed = TRUE))) != 1) { ok <- FALSE; cat("  NOT APPLIED:", p[[1]], "\n"); next }
  writeLines(sub(p[[2]], p[[3]], x, fixed = TRUE), p[[1]]); invisible(parse(p[[1]])); cat("  patched:", p[[1]], "\n") }
cat(if (ok) "All script patches in place.\n" else "SOME PATCHES FAILED\n"); if (!ok) quit(status = 1)
