# Test nominal_test with complex factor structure similar to RQ2

library(ordinal)
library(MASS)

set.seed(123)
n <- 300

# Create data similar to RQ2 structure
df <- data.frame(
  y = ordered(sample(1:5, n, replace = TRUE)),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  age = sample(18:80, n, replace = TRUE),
  sex = factor(sample(c("Female", "Male", "Prefer not to say"), n, replace = TRUE, prob = c(0.5, 0.45, 0.05))),
  ethnicity = factor(sample(c("White", "Black", "Asian", "Mixed", "Other"), n, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.05, 0.05))),
  political = factor(sample(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), n, replace = TRUE))
)

cat("Data structure:\n")
str(df)

# Fit polr model first (like RQ2 does)
cat("\n\nFitting polr model...\n")
polr_model <- MASS::polr(y ~ x1 + x2 + x3 + age + sex + ethnicity + political, data = df)

cat("polr model fitted.\n")

# Extract data and formula from polr (mimicking perform_brant_test)
cat("\n\nExtracting model data...\n")
model_data <- polr_model$model
outcome_col <- names(model_data)[1]
predictor_cols <- names(model_data)[-1]

cat("Outcome column:", outcome_col, "\n")
cat("Predictor columns:", paste(predictor_cols, collapse = ", "), "\n")

# Reconstruct formula using backticks
formula_str <- paste("`", outcome_col, "` ~ ", paste("`", predictor_cols, "`", sep = "", collapse = " + "), sep = "")
cat("\nReconstructed formula string:", formula_str, "\n")

formula_obj <- as.formula(formula_str)
cat("Formula object created.\n")

# Refit as clm
cat("\nRefitting as clm...\n")
clm_model <- ordinal::clm(formula_obj, data = model_data, link = "logit")
cat("clm model fitted.\n")

cat("\nclm model summary:\n")
print(summary(clm_model))

# Run nominal_test
cat("\n\nRunning nominal_test...\n")
nom_test_result <- ordinal::nominal_test(clm_model)

cat("\nRaw nominal_test output:\n")
print(nom_test_result)

cat("\n\nDetailed inspection:\n")
cat("Row names:", rownames(nom_test_result), "\n")
cat("Number of rows:", nrow(nom_test_result), "\n")
cat("\nLRT values:\n")
print(nom_test_result$LRT)
cat("\nP-values:\n")
print(nom_test_result$`Pr(>Chi)`)

# Check for NAs
cat("\n\nNA check:\n")
cat("Number of NA LRT values:", sum(is.na(nom_test_result$LRT)), "\n")
cat("Number of NA p-values:", sum(is.na(nom_test_result$`Pr(>Chi)`)), "\n")

# Test if it's a factor issue
cat("\n\n======================\n")
cat("Testing without factors:\n")
cat("======================\n")

df_numeric <- data.frame(
  y = ordered(sample(1:5, n, replace = TRUE)),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  age = sample(18:80, n, replace = TRUE)
)

polr_model2 <- MASS::polr(y ~ x1 + x2 + x3 + age, data = df_numeric)
model_data2 <- polr_model2$model
outcome_col2 <- names(model_data2)[1]
predictor_cols2 <- names(model_data2)[-1]
formula_str2 <- paste("`", outcome_col2, "` ~ ", paste("`", predictor_cols2, "`", sep = "", collapse = " + "), sep = "")
formula_obj2 <- as.formula(formula_str2)
clm_model2 <- ordinal::clm(formula_obj2, data = model_data2, link = "logit")

cat("\nRunning nominal_test on numeric-only model...\n")
nom_test_result2 <- ordinal::nominal_test(clm_model2)
print(nom_test_result2)
