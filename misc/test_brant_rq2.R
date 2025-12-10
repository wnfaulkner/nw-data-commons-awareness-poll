# ==============================================================================
# Test perform_brant_test() on RQ2 Model 1 with verbose output
# ==============================================================================

# Load necessary packages and functions
source("scripts/c.helper_functions/00_config.R")
source("scripts/c.helper_functions/01_validation.R")
source("scripts/c.helper_functions/02_data_processing.R")
source("scripts/c.helper_functions/03_regression_core.R")
source("scripts/c.helper_functions/04_regression_diagnostics.R")

# Suppress messages for cleaner output
suppressMessages({
  library(tidyverse)
  library(MASS)
  library(ordinal)
  library(googlesheets4)
})

cat("\n==============================================================================\n")
cat("TESTING perform_brant_test() ON RQ2 MODEL 1\n")
cat("==============================================================================\n\n")

# Load data
cat("Loading data from Google Sheets...\n")
gs4_auth(email = "wfrierson@uw.edu")
data.tb <- read_sheet(sheets.id, sheet = "data")

# Create awareness variables (simplified from RQ1)
cat("Creating awareness variables...\n")
rq1_awareness_mean <- data.tb %>%
  filter(shown.infographic == "Shown NW Iinfographic") %>%
  select(
    participant.id,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric
  ) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(
    awareness_mean = mean(c(nw.awareness.1980s_numeric,
                            nw.awareness.recent.academic_numeric,
                            nw.awareness.recent.media_numeric), na.rm = TRUE)
  ) %>%
  ungroup()

# Prepare RQ2 data (treatment group only)
cat("Preparing RQ2 data...\n")
rq2_data <- data.tb %>%
  left_join(rq1_awareness_mean, by = "participant.id") %>%
  filter(
    shown.infographic == "Shown NW Iinfographic",
    !is.na(support.nuclear.strike.on.russia_numeric),
    !is.na(awareness_mean)
  ) %>%
  select(
    participant.id,
    support.nuclear.strike.on.russia_numeric,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric,
    awareness_mean,
    age,
    sex,
    ethnicity,
    political.affiliation,
    employment.status,
    student.status
  ) %>%
  drop_na()

cat("Sample size:", nrow(rq2_data), "\n\n")

# Fit Model 1 (separate items)
cat("Fitting Model 1 (separate awareness items)...\n")
covariates <- c("age", "sex", "ethnicity", "political.affiliation", "employment.status", "student.status")

formula_items <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~",
  "nw.awareness.1980s_numeric +",
  "nw.awareness.recent.academic_numeric +",
  "nw.awareness.recent.media_numeric +",
  paste(covariates, collapse = " + ")
))

model1_items <- fit_pom(
  formula = formula_items,
  data = rq2_data,
  link = "logit",
  verbose = FALSE
)

cat("Model fitted successfully.\n")
cat("AIC:", round(model1_items$model_stats$aic, 2), "\n\n")

# Test proportional odds assumption with verbose output
cat("==============================================================================\n")
cat("RUNNING perform_brant_test() WITH VERBOSE=TRUE\n")
cat("==============================================================================\n\n")

brant_result <- perform_brant_test(model1_items$model, verbose = TRUE)

cat("\n==============================================================================\n")
cat("EXAMINING DETAILED RESULTS\n")
cat("==============================================================================\n\n")

cat("Omnibus p-value:", brant_result$omnibus_p_value, "\n")
cat("Has violations:", brant_result$has_violations, "\n")
cat("Number of individual tests:", nrow(brant_result$test_statistics) - 1, "\n\n")

cat("Individual test results:\n")
print(brant_result$test_statistics)

cat("\n==============================================================================\n")
cat("MANUAL INSPECTION OF nominal_test OUTPUT\n")
cat("==============================================================================\n\n")

# Manually refit and run nominal_test to see raw output
cat("Refitting model as clm...\n")
model_data <- model1_items$model$model
outcome_col <- names(model_data)[1]
predictor_cols <- names(model_data)[-1]
formula_str <- paste("`", outcome_col, "` ~ ", paste("`", predictor_cols, "`", sep = "", collapse = " + "), sep = "")
formula_obj <- as.formula(formula_str)

clm_model <- ordinal::clm(formula_obj, data = model_data, link = "logit")

cat("Running nominal_test...\n\n")
nom_test_result <- ordinal::nominal_test(clm_model)

cat("Raw nominal_test output:\n")
print(nom_test_result)

cat("\n\nRow names:\n")
print(rownames(nom_test_result))

cat("\n\nAfter removing <none> row:\n")
test_results <- nom_test_result[rownames(nom_test_result) != "<none>", , drop = FALSE]
print(test_results)

cat("\n\nDimensions after filtering:\n")
cat("Rows:", nrow(test_results), "\n")
cat("Columns:", ncol(test_results), "\n")

cat("\n\nColumn names:\n")
print(colnames(test_results))

cat("\n\nPr(>Chi) values:\n")
print(test_results$`Pr(>Chi)`)

cat("\n==============================================================================\n")
cat("TEST COMPLETE\n")
cat("==============================================================================\n")
