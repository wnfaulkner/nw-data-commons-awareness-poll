# Test script to debug brant issue
library(MASS)
library(brant)

# Create simple test data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)

# Create ordinal outcome
y_continuous <- x1 + x2 + rnorm(n)
y_ordinal <- cut(y_continuous, breaks = c(-Inf, -1, 0, 1, Inf), labels = c("1", "2", "3", "4"))
y_factor <- as.factor(y_ordinal)

# Fit polr model
test_model <- polr(y_factor ~ x1 + x2, method = "logistic")

cat("Model fitted successfully\n")
cat("Model class:", class(test_model), "\n")
cat("Model method:", test_model$method, "\n\n")

# Try brant test
cat("Attempting Brant test...\n")
tryCatch({
  brant_result <- brant(test_model)
  cat("Brant test succeeded!\n")
  print(brant_result)
}, error = function(e) {
  cat("Brant test FAILED with error:\n")
  cat("  Error message:", e$message, "\n")
  cat("  Error class:", class(e), "\n")

  # Try to get more debugging info
  cat("\nModel structure:\n")
  print(str(test_model, max.level = 1))
})
