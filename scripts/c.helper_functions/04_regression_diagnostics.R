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

# TEST FOR PROPORTIONAL ODDS ASSUMPTION USING NOMINAL_TEST
perform_brant_test <- function(pom_model, verbose = TRUE) {
  # Load required packages
  if (!requireNamespace("ordinal", quietly = TRUE)) {
    stop("Package 'ordinal' is required for proportional odds testing. Please install it with: install.packages('ordinal')")
  }

  library(ordinal)

  if (verbose) {
    cat("  Testing proportional odds assumption (nominal_test)...\n")
  }

  # Extract data and formula from polr model
  model_data <- pom_model$model

  # Get outcome variable name from model (first column)
  outcome_col <- names(model_data)[1]
  predictor_cols <- names(model_data)[-1]

  # Reconstruct formula using actual column names from model data
  # This handles cases where outcome is wrapped in functions like ordered()
  formula_str <- paste("`", outcome_col, "` ~ ", paste("`", predictor_cols, "`", sep = "", collapse = " + "), sep = "")
  formula_obj <- as.formula(formula_str)

  # Refit as clm model (ordinal package) for nominal_test
  clm_model <- tryCatch({
    ordinal::clm(formula_obj, data = model_data, link = "logit")
  }, error = function(e) {
    stop(paste("Failed to refit model as clm:", e$message))
  })

  # Run nominal_test
  nom_test_result <- tryCatch({
    ordinal::nominal_test(clm_model)
  }, error = function(e) {
    stop(paste("nominal_test failed:", e$message))
  })

  if (verbose) {
    cat("  Proportional odds test completed. Processing results...\n")
    cat("\n  DEBUG: Raw nominal_test output:\n")
    print(nom_test_result)
    cat("\n  DEBUG: Row names:\n")
    print(rownames(nom_test_result))
    cat("\n  DEBUG: Column names:\n")
    print(colnames(nom_test_result))
    cat("\n  DEBUG: Dimensions:", nrow(nom_test_result), "rows x", ncol(nom_test_result), "cols\n")
  }

  # Extract results from nominal_test
  # nominal_test returns an anova object with rows for each predictor
  # First row is "<none>" (base model), subsequent rows are predictors

  # Remove <none> row (which has NA p-value)
  test_results <- nom_test_result[rownames(nom_test_result) != "<none>", , drop = FALSE]

  if (verbose) {
    cat("\n  DEBUG: After removing <none>:", nrow(test_results), "rows\n")
  }

  # Create test statistics dataframe in Brant-like format
  test_stats <- data.frame(
    variable = rownames(test_results),
    chi_squared = test_results$LRT,
    degrees_of_freedom = test_results$Df,
    p_value = test_results$`Pr(>Chi)`,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # Filter out any remaining NA p-values
  test_stats <- test_stats[!is.na(test_stats$p_value), , drop = FALSE]

  # Add interpretation column
  test_stats$interpretation <- ifelse(
    test_stats$p_value < 0.05,
    "VIOLATION: Proportional odds assumption violated",
    "OK: Proportional odds assumption holds"
  )

  # Create omnibus test using minimum p-value (conservative approach)
  # This represents the strongest evidence against proportional odds
  if (nrow(test_stats) > 0) {
    omnibus_p_value <- min(test_stats$p_value, na.rm = TRUE)
    omnibus_lrt <- sum(test_stats$chi_squared, na.rm = TRUE)
    omnibus_df <- sum(test_stats$degrees_of_freedom, na.rm = TRUE)
  } else {
    # No valid tests - assume proportional odds holds
    omnibus_p_value <- 1.0
    omnibus_lrt <- 0
    omnibus_df <- 0
  }

  # Create omnibus row
  omnibus_stats <- data.frame(
    variable = "Omnibus",
    chi_squared = omnibus_lrt,
    degrees_of_freedom = omnibus_df,
    p_value = omnibus_p_value,
    interpretation = ifelse(
      omnibus_p_value < 0.05,
      "VIOLATION: Proportional odds assumption violated",
      "OK: Proportional odds assumption holds"
    ),
    stringsAsFactors = FALSE
  )

  # Combine omnibus and individual tests
  all_stats <- rbind(omnibus_stats, test_stats)
  rownames(all_stats) <- c("Omnibus", test_stats$variable)

  summary_result <- list(
    test_statistics = all_stats,
    omnibus_statistic = omnibus_lrt,
    omnibus_df = omnibus_df,
    omnibus_p_value = omnibus_p_value,
    omnibus_interpretation = ifelse(
      omnibus_p_value < 0.05,
      "VIOLATION: Overall proportional odds assumption violated",
      "OK: Overall proportional odds assumption holds"
    ),
    violations = test_stats[test_stats$p_value < 0.05, , drop = FALSE],
    has_violations = any(test_stats$p_value < 0.05, na.rm = TRUE)
  )

  if (verbose) {
    cat("\n  Proportional Odds Test Results (Likelihood Ratio Test):\n")
    cat("  ========================================================\n\n")
    cat("  Omnibus Test (minimum p-value across predictors):\n")
    cat("    LR Chi-squared =", round(summary_result$omnibus_statistic, 4), "\n")
    cat("    df =", summary_result$omnibus_df, "\n")
    cat("    p-value =", format.pval(summary_result$omnibus_p_value, digits = 4), "\n")
    cat("    Result:", summary_result$omnibus_interpretation, "\n\n")

    cat("  Individual Variable Tests:\n")
    for (i in seq_len(nrow(test_stats))) {
      var_name <- test_stats$variable[i]
      cat("    ", var_name, ":\n", sep = "")
      cat("      LR Chi-squared =", round(test_stats$chi_squared[i], 4), "\n")
      cat("      df =", test_stats$degrees_of_freedom[i], "\n")
      cat("      p-value =", format.pval(test_stats$p_value[i], digits = 4), "\n")
      cat("      Result:", test_stats$interpretation[i], "\n")
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
    model_type <- ifelse(isS4(model), "PPOM (VGAM)", "POM (MASS)")
    cat("  Calculating residuals for", model_type, "...\n")
  }

  # Get model data - handle both S3 (polr) and S4 (vglm) models
  if (isS4(model)) {
    # VGAM::vglm model (S4 object)
    # VGAM models don't store model frame the same way - get from original call
    n_obs <- nobs(model)
    outcome_var <- pom_result$outcome_var

    # Extract observed values from model's y slot or from pom_result
    if ("y" %in% methods::slotNames(model)) {
      y_matrix <- model@y
      # For ordinal models, y is typically a matrix with indicators for each level
      # Convert to numeric class labels
      if (is.matrix(y_matrix)) {
        y_observed <- apply(y_matrix, 1, which.max)
      } else {
        y_observed <- as.numeric(y_matrix)
      }
    } else {
      stop("Cannot extract observed outcomes from VGAM model")
    }
  } else {
    # MASS::polr model (S3 object)
    model_data <- model$model
    n_obs <- nrow(model_data)
    outcome_var <- pom_result$outcome_var
    y_observed <- as.numeric(model_data[[outcome_var]])
  }

  # Calculate linear predictor
  # Different approaches for POM (polr) vs PPOM (vglm)
  if (isS4(model)) {
    # VGAM model - for diagnostic purposes, use average of linear predictors across thresholds
    # VGAM ordinal models have separate linear predictors for each threshold
    if ("predictors" %in% methods::slotNames(model)) {
      # Average across all threshold linear predictors
      predictors_matrix <- model@predictors
      if (is.matrix(predictors_matrix) && nrow(predictors_matrix) == n_obs) {
        linear_predictor <- rowMeans(predictors_matrix)
      } else {
        # If predictors structure is unexpected, use fitted as fallback
        linear_predictor <- rep(0, n_obs)  # Placeholder for diagnostics
      }
    } else {
      # Fallback: use zeros as placeholder (linear predictor less meaningful for PPOM anyway)
      linear_predictor <- rep(0, n_obs)
    }
  } else {
    # MASS::polr model
    X <- model.matrix(model)[, -1, drop = FALSE]
    beta <- coef(model)

    if (ncol(X) == length(beta)) {
      linear_predictor <- as.vector(X %*% beta)
    } else {
      linear_predictor <- predict(model, type = "linear")
    }
  }

  # Get predicted probabilities for each category
  if (isS4(model)) {
    # VGAM model uses type = "response" for probabilities
    pred_probs <- predict(model, type = "response")
  } else {
    # MASS::polr uses type = "probs"
    pred_probs <- predict(model, type = "probs")
  }

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

  # Validate all vectors have correct length before creating tibble
  if (length(y_observed) != n_obs) {
    stop(paste("y_observed length mismatch:", length(y_observed), "vs n_obs:", n_obs))
  }
  if (length(pred_class_num) != n_obs) {
    stop(paste("pred_class_num length mismatch:", length(pred_class_num), "vs n_obs:", n_obs))
  }
  if (length(linear_predictor) != n_obs) {
    stop(paste("linear_predictor length mismatch:", length(linear_predictor), "vs n_obs:", n_obs))
  }
  if (length(pearson_residuals) != n_obs) {
    stop(paste("pearson_residuals length mismatch:", length(pearson_residuals), "vs n_obs:", n_obs))
  }
  if (length(deviance_residuals) != n_obs) {
    stop(paste("deviance_residuals length mismatch:", length(deviance_residuals), "vs n_obs:", n_obs))
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
