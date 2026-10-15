# 08_slide_figures.R
# Slide versions of the figures: same as the report figures but without the
# ggplot title, since each slide already carries it in its header. Subtitles,
# captions and axes are kept. Existing scripts are re-run with ggsave()
# redirected to outputs/figures/slides/, so report figures are left untouched.
suppressPackageStartupMessages({ library(ggplot2) })
SLIDES_DIR <- "outputs/figures/slides"
dir.create(SLIDES_DIR, showWarnings = FALSE, recursive = TRUE)
slide_ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  if (inherits(plot, "patchwork")) {
    plot <- plot + patchwork::plot_annotation(theme = ggplot2::theme(plot.title = ggplot2::element_blank()))
  } else if (inherits(plot, "ggplot")) {
    plot <- plot + ggplot2::labs(title = NULL)
  }
  ggplot2::ggsave(file.path(SLIDES_DIR, basename(filename)), plot, ...)
}
scripts <- c("R/03n_heckman_mincer.R", "R/04_slides_figures.R", "R/06_age_disagg_figures.R", "R/07_final_charts.R")
for (s in scripts) {
  cat("\n>>> slide versions from", s, "\n")
  env <- new.env(parent = globalenv()); env$ggsave <- slide_ggsave
  tryCatch(sys.source(s, envir = env, keep.source = FALSE),
           error = function(e) cat("   WARNING:", conditionMessage(e), "\n"))
}
needed <- c("d1_overall_gap","d2_neet_by_province","b1_marriage_by_age","b3_neet_marriage_caste_age",
            "b4_enrol_marriage_caste_age","r1a_coef_enrolment","r1b_coef_neet","b5_coef_by_age",
            "r2_cate_caste","m1_mincer_lifetime","m2_marriage_interaction","c3_coi_vs_budget_census",
            "coi_provincial_census","d4_gap_by_age","a1_boys_coef","n1_heckman_returns")
cat("\n=== Slide figure check ===\n")
for (n in needed) {
  f <- file.path(SLIDES_DIR, paste0(n, ".png"))
  if (file.exists(f)) cat("  ok      ", n, "\n") else {
    file.copy(file.path("outputs/figures", paste0(n, ".png")), f)
    cat("  FALLBACK", n, "(report version copied, title kept)\n")
  }
}
cat("=== DONE ===\n")
