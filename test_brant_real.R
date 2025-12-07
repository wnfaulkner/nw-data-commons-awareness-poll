# Test brant with actual RQ3 model
library(MASS)
library(brant)
library(tidyverse)

# Source the helper functions
source("R/00_config.R")
source("R/01_validation.R")
source("R/02_data_processing.R")
source("R/03_regression_core.R")
source("R/04_regression_diagnostics.R")

# Load data
load_google_sheets_data()

# Get data.tb
data.tb <- raw.data.tb %>%
  recode_all_columns()

# Prepare RQ3 data (simplified)
rq3_test <- data.tb %>%
  filter(
    !is.na(shown.infographic),
    !is.na(support.nuclear.strike.on.russia_numeric)
  ) %>%
  select(
    shown.infographic,
    support.nuclear.strike.on.russia_numeric,
    age,
    sex,
    ethnicity,
    political.affiliation,
    employment.status,
    student.status
  ) %>%
  drop_na()

cat("Sample size:", nrow(rq3_test), "\n")
cat("Outcome levels:\n")
print(table(rq3_test$support.nuclear.strike.on.russia_numeric))
cat("\nTreatment levels:\n")
print(table(rq3_test$shown.infographic))

# Fit simple model first
cat("\n\nFitting simple unadjusted model...\n")
formula_simple <- as.formula("support.nuclear.strike.on.russia_numeric ~ shown.infographic")
model_simple <- polr(formula_simple, data = rq3_test, method = "logistic")
cat("Simple model fitted successfully\n")

# Try brant on simple model
cat("\nTesting brant on simple model...\n")
tryCatch({
  brant_simple <- brant(model_simple)
  cat("SUCCESS: Brant test worked on simple model\n")
  print(brant_simple)
}, error = function(e) {
  cat("FAILED: Brant test failed on simple model\n")
  cat("Error:", e$message, "\n")
})

# Fit adjusted model
cat("\n\nFitting adjusted model with all covariates...\n")
formula_adj <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  "age + sex + ethnicity + political.affiliation + employment.status + student.status"
))

model_adj <- polr(formula_adj, data = rq3_test, method = "logistic")
cat("Adjusted model fitted successfully\n")
cat("Number of coefficients:", length(coef(model_adj)), "\n")

# Try brant on adjusted model
cat("\nTesting brant on adjusted model...\n")
tryCatch({
  brant_adj <- brant(model_adj)
  cat("SUCCESS: Brant test worked on adjusted model\n")
  print(brant_adj)
}, error = function(e) {
  cat("FAILED: Brant test failed on adjusted model\n")
  cat("Error:", e$message, "\n")
  cat("\nDebugging info:\n")
  cat("Model names:\n")
  print(names(model_adj))
  cat("\nCoefficient names:\n")
  print(names(coef(model_adj)))
})
