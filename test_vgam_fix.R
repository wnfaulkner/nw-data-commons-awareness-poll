# Minimal test for VGAM coefficient extraction fix
library(VGAM)
library(tibble)

cat("\n================================================================================\n")
cat("TESTING VGAM COEFFICIENT EXTRACTION FIX\n")
cat("================================================================================\n\n")

# Create simple test data
set.seed(123)
n <- 200

test_data <- data.frame(
  outcome = ordered(sample(1:4, n, replace = TRUE)),
  x1 = rnorm(n),
  x2 = sample(c("A", "B", "C"), n, replace = TRUE)
)

cat("Test data created:\n")
cat("  n =", n, "\n")
cat("  Outcome levels:", nlevels(test_data$outcome), "\n")
cat("  Predictors: x1 (numeric), x2 (categorical)\n\n")

# Fit PPOM model
cat("Fitting PPOM model using VGAM::vglm...\n")
model <- VGAM::vglm(
  outcome ~ x1 + x2,
  family = VGAM::cumulative(link = "logitlink", parallel = FALSE),
  data = test_data
)
cat("Model fitted successfully!\n\n")

# Test the NEW coefficient extraction approach
cat("Testing NEW coefficient extraction method:\n")
cat("  Using: summary(model)@coef3\n\n")

tryCatch({
  coef_summary <- summary(model)@coef3

  cat("SUCCESS! Coefficient table extracted:\n")
  cat("  Class:", class(coef_summary), "\n")
  cat("  Dimensions:", nrow(coef_summary), "rows x", ncol(coef_summary), "columns\n")
  cat("  Column names:", colnames(coef_summary), "\n\n")

  cat("First few rows:\n")
  print(head(coef_summary, 10))
  cat("\n")

  cat("Coefficient names (rownames):\n")
  print(rownames(coef_summary))
  cat("\n")

  cat("================================================================================\n")
  cat("FIX VERIFIED - VGAM coefficient extraction working!\n")
  cat("================================================================================\n\n")

}, error = function(e) {
  cat("ERROR:", e$message, "\n\n")
  cat("================================================================================\n")
  cat("FIX FAILED\n")
  cat("================================================================================\n\n")
})
