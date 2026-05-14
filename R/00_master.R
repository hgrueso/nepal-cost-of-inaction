# =============================================================================
# Nepal NLSS — Cost-of-Inaction Analysis
# Master script: runs the full pipeline in order
# Author: AImpact Lab (Hernando Grueso) | April 2026
# =============================================================================
#
# HOW TO RUN FROM TERMINAL:
#   cd /Users/hgrueso/Dropbox/Documents/Laboral/AImpact\ Lab/Clients/UNICEF/\
#       Deliverables/Investment\ Cases/Nepal/analysis
#   Rscript R/00_master.R
#
# To render the Quarto report after running the pipeline:
#   quarto render reports/nepal_nlss_analysis.qmd
# =============================================================================

# ── 0. Paths (edit PROJECT_ROOT if not using here::) ──────────────────────────
PROJECT_ROOT <- tryCatch(here::here(), error = function(e) getwd())
DATA_PATH    <- file.path(PROJECT_ROOT, "data", "NLSS oct 20.dta")
OUTPUT_PATH  <- file.path(PROJECT_ROOT, "outputs")
FIGURES_PATH <- file.path(OUTPUT_PATH, "figures")
TABLES_PATH  <- file.path(OUTPUT_PATH, "tables")
dir.create(FIGURES_PATH, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_PATH,  showWarnings = FALSE, recursive = TRUE)

# ── 1. Install missing packages ───────────────────────────────────────────────
required_pkgs <- c(
  "haven", "tidyverse", "survey", "srvyr", "modelsummary",
  "kableExtra", "patchwork", "scales", "viridis", "broom",
  "marginaleffects", "fixest", "here", "RColorBrewer"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# ── 2. Run pipeline ────────────────────────────────────────────────────────────
cat("\n=== STEP 1: Load & clean ===\n")
source(file.path(PROJECT_ROOT, "R", "01_load_clean.R"))
cat("\n=== STEP 2: Descriptives ===\n")
source(file.path(PROJECT_ROOT, "R", "02_descriptives.R"))
cat("\n=== STEP 3: Regressions ===\n")
source(file.path(PROJECT_ROOT, "R", "03_regressions.R"))

# ── 3. Save results object for Quarto ─────────────────────────────────────────
save(results, file = file.path(OUTPUT_PATH, "analysis_results.RData"))
cat("\n✓ Done. results saved to outputs/analysis_results.RData\n")
cat("  Run: quarto render reports/nepal_nlss_analysis.qmd\n\n")
