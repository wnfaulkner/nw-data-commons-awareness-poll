# ==============================================================================
# 04_regression_diagnostics.R
# Regression diagnostic functions (VIF, Brant test, residuals)
# ==============================================================================

# CALCULATE VIF (Variance Inflation Factor) FOR MULTICOLLINEARITY DETECTION
calculate_vif <- function(model) {
  # Extract model matrix (without intercept)
  X <- model.matrix(model)[, -1, drop = FALSE]

  # Handle case where there's only one predictor
  if (ncol(X) == 1) {
    vif_results <- data.frame(
      variable = colnames(X),
      vif = 1,
      tolerance = 1
    )
    return(vif_results)
  }

  # Calculate VIF for each predictor
  vif_values <- numeric(ncol(X))
  names(vif_values) <- colnames(X)

  for (i in seq_len(ncol(X))) {
    # Regress each predictor on all other predictors
    # Wrap variable names in backticks to handle spaces and special characters
    lhs <- paste0("`", colnames(X)[i], "`")
    rhs <- paste(paste0("`", colnames(X)[-i], "`"), collapse = " + ")
    formula_str <- paste(lhs, "~", rhs)
    r_squared <- summary(lm(as.formula(formula_str), data = as.data.frame(X)))$r.squared

    # VIF = 1 / (1 - R²)
    vif_values[i] <- 1 / (1 - r_squared)
  }

  # Create results dataframe
  vif_results <- data.frame(
    variable = names(vif_values),
    vif = vif_values,
    tolerance = 1 / vif_values,
    row.names = NULL
  )

  return(vif_results)
}

# BRANT TEST FOR PROPORTIONAL ODDS ASSUMPTION
perform_brant_test <- function(pom_model, verbose = TRUE) {
  # Load brant package
  if (!requireNamespace("brant", quietly = TRUE)) {
    stop("Package 'brant' is required for Brant test. Please install it with: install.packages('brant')")
  }

  # Load the package into the namespace
  library(brant)

  if (verbose) {
    cat("  Running Brant test for proportional odds assumption...\n")
  }

  # Run Brant test
  brant_result <- tryCatch({
    brant(pom_model)
  }, error = function(e) {
    cat("\nDetailed error information:\n")
    cat("Error message:", e$message, "\n")
    cat("Model class:", class(pom_model), "\n")
    cat("Model method:", pom_model$method, "\n")
    stop(paste("Brant test failed:", e$message))
  })

  if (verbose) {
    cat("  Brant test completed. Processing results...\n")
  }

  # Extract results
  # The brant test returns a matrix with columns: X2, df, probability
  # Rows include: Omnibus test + individual predictor tests
  test_stats <- tryCatch({
    as.data.frame(brant_result, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat("Error details:\n")
    cat("  brant_result class:", class(brant_result), "\n")
    cat("  brant_result type:", typeof(brant_result), "\n")
    stop(paste("Failed to convert brant result to data frame:", e$message))
  })

  # Rename columns for clarity (avoid name conflict with df() function)
  colnames(test_stats) <- c("chi_squared", "degrees_of_freedom", "p_value")

  # Add interpretation column
  test_stats$interpretation <- ifelse(
    test_stats$p_value < 0.05,
    "VIOLATION: Proportional odds assumption violated",
    "OK: Proportional odds assumption holds"
  )

  # Create summary
  # Note: rownames should have "Omnibus" (capital O) based on brant output
  omnibus_row <- which(rownames(test_stats) == "Omnibus")
  if (length(omnibus_row) == 0) {
    # Try lowercase if uppercase not found
    omnibus_row <- which(tolower(rownames(test_stats)) == "omnibus")
  }

  summary_result <- list(
    test_statistics = test_stats,
    omnibus_statistic = test_stats[omnibus_row, "chi_squared"],
    omnibus_df = test_stats[omnibus_row, "degrees_of_freedom"],
    omnibus_p_value = test_stats[omnibus_row, "p_value"],
    omnibus_interpretation = ifelse(
      test_stats[omnibus_row, "p_value"] < 0.05,
      "VIOLATION: Overall proportional odds assumption violated",
      "OK: Overall proportional odds assumption holds"
    ),
    violations = test_stats[test_stats$p_value < 0.05 & seq_len(nrow(test_stats)) != omnibus_row, , drop = FALSE],
    has_violations = any(test_stats$p_value[seq_len(nrow(test_stats)) != omnibus_row] < 0.05)
  )

  if (verbose) {
    cat("\n  Brant Test Results:\n")
    cat("  ==================\n\n")
    cat("  Omnibus Test:\n")
    cat("    Chi-squared =", round(summary_result$omnibus_statistic, 4), "\n")
    cat("    df =", summary_result$omnibus_df, "\n")
    cat("    p-value =", format.pval(summary_result$omnibus_p_value, digits = 4), "\n")
    cat("    Result:", summary_result$omnibus_interpretation, "\n\n")

    cat("  Individual Variable Tests:\n")
    individual_tests <- test_stats[seq_len(nrow(test_stats)) != omnibus_row, , drop = FALSE]
    for (i in seq_len(nrow(individual_tests))) {
      var_name <- rownames(individual_tests)[i]
      cat("    ", var_name, ":\n", sep = "")
      cat("      Chi-squared =", round(individual_tests[i, "chi_squared"], 4), "\n")
      cat("      df =", individual_tests[i, "degrees_of_freedom"], "\n")
      cat("      p-value =", format.pval(individual_tests[i, "p_value"], digits = 4), "\n")
      cat("      Result:", individual_tests[i, "interpretation"], "\n")
    }
    cat("\n")

    if (summary_result$has_violations) {
      cat("  WARNING: Proportional odds assumption violated for one or more predictors.\n")
      cat("  Consider:\n")
      cat("    1. Partial Proportional Odds Model (PPOM)\n")
      cat("    2. Multinomial Logistic Regression\n")
      cat("    3. Examining/transforming problematic predictors\n\n")
    } else {
      cat("  GOOD NEWS: Proportional odds assumption holds for all predictors.\n")
      cat("  The POM model is appropriate for this data.\n\n")
    }
  }

  return(summary_result)
}

# CALCULATE RESIDUALS FOR PROPORTIONAL ODDS MODEL
calculate_pom_residuals <- function(pom_result, verbose = TRUE) {
  # Extract model object
  model <- pom_result$model

  if (verbose) {
    cat("  Calculating residuals for POM...\n")
  }

  # Get model data
  model_data <- model$model
  n_obs <- nrow(model_data)

  # Extract outcome variable
  outcome_var <- pom_result$outcome_var
  y_observed <- as.numeric(model_data[[outcome_var]])

  # Calculate linear predictor (X * beta)
  # For polr, the linear predictor is: X %*% beta (without intercepts/thresholds)
  X <- model.matrix(model)[, -1, drop = FALSE]  # Remove intercept column if present
  beta <- coef(model)

  # Handle case where model.matrix includes factor levels
  # polr coefficients don't include intercept, so align them
  if (ncol(X) == length(beta)) {
    linear_predictor <- as.vector(X %*% beta)
  } else {
    # If dimensions don't match, reconstruct more carefully
    linear_predictor <- predict(model, type = "linear")
  }

  # Get predicted probabilities for each category
  pred_probs <- predict(model, type = "probs")

  # If pred_probs is a vector (single observation), convert to matrix
  if (is.vector(pred_probs)) {
    pred_probs <- matrix(pred_probs, nrow = 1)
  }

  # Get predicted class (mode of predicted probabilities)
  pred_class_num <- apply(pred_probs, 1, which.max)

  # Calculate Pearson residuals
  # For ordinal regression: (observed indicator - predicted prob) / sqrt(predicted prob * (1 - predicted prob))
  # We'll calculate this for the observed category
  pearson_residuals <- numeric(n_obs)

  for (i in seq_len(n_obs)) {
    obs_category <- y_observed[i]
    p_obs <- pred_probs[i, obs_category]

    # Pearson residual for observed category
    pearson_residuals[i] <- (1 - p_obs) / sqrt(p_obs * (1 - p_obs))
  }

  # Calculate deviance residuals
  # Deviance residual = sqrt(-2 * log(predicted probability of observed category))
  # Sign matches whether observed is above or below predicted mode
  deviance_residuals <- numeric(n_obs)

  for (i in seq_len(n_obs)) {
    obs_category <- y_observed[i]
    p_obs <- pred_probs[i, obs_category]

    # Deviance component
    dev_component <- sqrt(-2 * log(p_obs))

    # Add sign based on observed vs predicted
    sign_val <- ifelse(obs_category > pred_class_num[i], 1,
      ifelse(obs_category < pred_class_num[i], -1, 0))

    deviance_residuals[i] <- sign_val * dev_component
  }

  # Create result tibble with observation-level data
  residuals_tb <- tibble(
    obs_index = seq_len(n_obs),
    observed = y_observed,
    predicted_class = pred_class_num,
    linear_predictor = linear_predictor,
    pearson_residual = pearson_residuals,
    deviance_residual = deviance_residuals
  )

  # Add predicted probabilities as separate columns
  prob_cols <- as_tibble(pred_probs)
  colnames(prob_cols) <- paste0("prob_class_", seq_len(ncol(pred_probs)))

  residuals_tb <- bind_cols(residuals_tb, prob_cols)

  if (verbose) {
    cat("  Residuals calculated for", n_obs, "observations\n")
    cat("  Pearson residuals range: [",
      round(min(pearson_residuals), 3), ",",
      round(max(pearson_residuals), 3), "]\n", sep = "")
    cat("  Deviance residuals range: [",
      round(min(deviance_residuals), 3), ",",
      round(max(deviance_residuals), 3), "]\n\n", sep = "")
  }

  return(residuals_tb)
}

cat("Regression diagnostics functions loaded successfully.\n")
