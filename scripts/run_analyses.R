# ==============================================================================
# RUN ANALYSES - INDIVIDUAL OR COMBINED
# ==============================================================================
#
# Simple orchestration script to run research question analyses individually
# or in combination.
#
# USAGE:
#   - Set flags below to TRUE/FALSE to control which analyses run
#   - Source this file: source("run_analyses.R")
#   - Or run from command line: Rscript run_analyses.R
#
# PREREQUISITES:
#   - Data and configuration must be loaded first via publication_analysis.R
#   - Or set load_data = TRUE below to load automatically
# ==============================================================================

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Load data automatically? (FALSE if already loaded in environment)
load_data <- TRUE

# Which analyses to run?
run_rq1 <- TRUE   # RQ1: Structure of nuclear winter awareness
run_rq2 <- TRUE   # RQ2: Awareness as associational predictor
run_rq3 <- TRUE   # RQ3: Treatment effects
run_rq4 <- TRUE   # RQ4: Decision factors structure
run_rq5 <- FALSE  # RQ5: Exploratory integration (not yet implemented)

# ==============================================================================
# LOAD DATA (if requested)
# ==============================================================================

if (load_data) {
  cat("\n")
  cat("==============================================================================\n")
  cat("LOADING DATA AND CONFIGURATION\n")
  cat("==============================================================================\n\n")

  # Source helper functions
  source("scripts/c.helper_functions/00_config.R")
  source("scripts/c.helper_functions/01_validation.R")
  source("scripts/c.helper_functions/02_data_processing.R")
  source("scripts/c.helper_functions/03_regression_core.R")
  source("scripts/c.helper_functions/04_regression_diagnostics.R")
  source("scripts/c.helper_functions/05_plotting.R")
  source("scripts/c.helper_functions/06_pdf_export.R")

  # Load packages
  suppressMessages({
    library(tidyverse)
    library(googlesheets4)
    library(psych)
    library(gridExtra)
  })

  # Authenticate and load data
  gs4_auth(email = "w@wbnicholson.com")

  data.tb <- read_sheet(sheets.id, sheet = "data")
  questions.tb <- read_sheet(sheets.id, sheet = "questions")
  response.options.tb <- read_sheet(sheets.id, sheet = "response.options")

  # Process data
  data.tb <- prepare_data_for_analysis(data.tb)

  cat("\nData loaded and processed successfully.\n")
  cat("Sample size:", nrow(data.tb), "\n\n")
}

# ------------------------------------------------------------------------------
# RQ1: Structure of Nuclear Winter Awareness
# ------------------------------------------------------------------------------

if (run_rq1) {
  cat("Starting RQ1: Structure of Nuclear-Winter Awareness...\n\n")
  source("scripts/b.analysis/RQ1_awareness_structure.R")
  cat("\n")
}

# ------------------------------------------------------------------------------
# RQ2: Using Awareness as Associational Predictor
# ------------------------------------------------------------------------------

if (run_rq2) {
  cat("Starting RQ2: Awareness as Associational Predictor...\n\n")
  source("scripts/b.analysis/RQ2_awareness_support.R")
  cat("\n")
}

# ------------------------------------------------------------------------------
# RQ3: Treatment Effects
# ------------------------------------------------------------------------------

if (run_rq3) {
  cat("Starting RQ3: Treatment Effects...\n\n")
  source("scripts/b.analysis/RQ3_treatment_effects.R")
  cat("\n")
}

# ------------------------------------------------------------------------------
# RQ4: Decision Factors Structure
# ------------------------------------------------------------------------------

if (run_rq4) {
  cat("Starting RQ4: Decision Factors Structure...\n\n")
  source("scripts/b.analysis/RQ4_decision_factors_structure.R")
  cat("\n")
}

# ------------------------------------------------------------------------------
# RQ5: Exploratory Integration (if implemented)
# ------------------------------------------------------------------------------

if (run_rq5) {
  rq5_script <- "scripts/b.analysis/RQ5_integration_exploratory.R"
  if (file.exists(rq5_script)) {
    cat("Starting RQ5: Exploratory Integration...\n\n")
    source(rq5_script)
    cat("\n")
  } else {
    cat("RQ5 script not found. Skipping.\n\n")
  }
}