# Minimal test to isolate brant issue
library(MASS)
library(brant)

# Test if df variable name causes issues
set.seed(123)
n <- 200

# Create data with a variable that might conflict
test_data <- data.frame(
  y = ordered(sample(1:5, n, replace = TRUE)),
  treatment = factor(sample(c("Treatment", "Control"), n, replace = TRUE)),
  age = rnorm(n, 50, 15),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  df_test = rnorm(n)  # Variable with 'df' in name
)

# Try simple model first
cat("Test 1: Simple model\n")
m1 <- polr(y ~ treatment, data = test_data, method = "logistic")
tryCatch({
  b1 <- brant(m1)
  cat("SUCCESS\n\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n\n")
})

# Try with multiple predictors
cat("Test 2: Multiple predictors\n")
m2 <- polr(y ~ treatment + age + sex, data = test_data, method = "logistic")
tryCatch({
  b2 <- brant(m2)
  cat("SUCCESS\n\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n\n")
})

# Try with df variable
cat("Test 3: With df_test variable\n")
m3 <- polr(y ~ treatment + df_test, data = test_data, method = "logistic")
tryCatch({
  b3 <- brant(m3)
  cat("SUCCESS\n\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n\n")
})

# Try after detaching and reattaching brant
cat("Test 4: After detaching/reattaching brant\n")
detach("package:brant", unload = TRUE)
library(brant)
tryCatch({
  b4 <- brant(m2)
  cat("SUCCESS\n\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n\n")
})
