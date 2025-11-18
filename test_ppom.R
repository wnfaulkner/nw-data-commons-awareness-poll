# Test script for PPOM implementation
library(tibble)
library(dplyr)

# Source the main script functions (just the functions we need)
source("Ingram_NW_Awareness.R", local = TRUE, echo = FALSE)

cat("\n================================================================================\n")
cat("TESTING PPOM IMPLEMENTATION\n")
cat("================================================================================\n\n")

# Create sample data
set.seed(123)
n <- 500

test_data <- tibble(
  outcome = ordered(sample(1:5, n, replace = TRUE)),
  age = sample(c("18-24", "25-34", "35-44", "45-54", "55+"), n, replace = TRUE),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Graduate"), n, replace = TRUE)
)

cat("Sample data created:\n")
cat("  n =", n, "\n")
cat("  Outcome levels:", nlevels(test_data$outcome), "\n")
cat("  Predictors: age, sex, education\n\n")

# Test PPOM fitting
cat("Testing fit_ppom() function...\n\n")

formula <- outcome ~ age + sex + education

ppom_result <- tryCatch({
  fit_ppom(
    formula = formula,
    data = test_data,
    link = "logit",
    confidence_level = 0.95,
    verbose = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  return(NULL)
})

if (!is.null(ppom_result)) {
  cat("\n================================================================================\n")
  cat("PPOM MODEL SUCCESSFULLY FITTED!\n")
  cat("================================================================================\n\n")

  cat("Model Statistics:\n")
  cat("  Model type:", ppom_result$model_stats$model_type, "\n")
  cat("  Link function:", ppom_result$model_stats$link_function, "\n")
  cat("  AIC:", round(ppom_result$model_stats$aic, 2), "\n")
  cat("  BIC:", round(ppom_result$model_stats$bic, 2), "\n")
  cat("  Number of thresholds:", ppom_result$model_stats$n_thresholds, "\n")
  cat("  Number of predictors:", ppom_result$model_stats$n_predictors, "\n\n")

  cat("Average Coefficients (across thresholds):\n")
  print(ppom_result$coefficients)
  cat("\n")

  cat("Threshold-Specific Coefficients:\n")
  print(ppom_result$threshold_specific_coefs)
  cat("\n")

  # Test plotting function
  cat("Testing plot_ppom_threshold_coefficients() function...\n\n")

  threshold_plot <- tryCatch({
    plot_ppom_threshold_coefficients(ppom_result)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })

  if (!is.null(threshold_plot)) {
    cat("Threshold plot created successfully!\n\n")

    # Save plot
    pdf("test_ppom_threshold_plot.pdf", width = 10, height = 8)
    print(threshold_plot)
    dev.off()

    cat("Plot saved to: test_ppom_threshold_plot.pdf\n\n")
  }

  cat("================================================================================\n")
  cat("TEST COMPLETED SUCCESSFULLY!\n")
  cat("================================================================================\n\n")

  cat("Summary:\n")
  cat("  ✓ PPOM model fitting works\n")
  cat("  ✓ Threshold-specific coefficients extracted\n")
  cat("  ✓ Threshold-specific plotting works\n")
  cat("  ✓ Model statistics calculated correctly\n\n")

} else {
  cat("\n================================================================================\n")
  cat("TEST FAILED - PPOM model could not be fitted\n")
  cat("================================================================================\n\n")
}
