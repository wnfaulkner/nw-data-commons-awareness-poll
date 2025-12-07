# ==============================================================================
# 03_regression_core.R
# Core regression model fitting functions (POM/PPOM)
# ==============================================================================

fit_model <- function(model_spec, formula, data, confidence_level = 0.95,
                     verbose = TRUE) {
  # Unified model fitting function that routes to appropriate model type
  #
  # Args:
  #   model_spec: String specifying model type (e.g., "logit", "ppom_logit", "ols")
  #   formula: Model formula
  #   data: Data frame
  #   confidence_level: Confidence level for CIs
  #   verbose: Print progress messages
  #
  # Returns:
  #   Model results with standard structure

  # Parse model specification
  model_info <- get_model_type(model_spec)

  if (verbose) {
    cat("  Model type:", toupper(model_info$model_type), "\n")
    if (model_info$link_function != "identity") {
      cat("  Link function:", model_info$link_function, "\n")
    }
  }

  # Route to appropriate fitting function
  result <- switch(model_info$model_type,
    "pom" = fit_pom(
      formula = formula,
      data = data,
      link = model_info$link_function,
      confidence_level = confidence_level,
      verbose = verbose
    ),
    "ppom" = fit_ppom(
      formula = formula,
      data = data,
      link = model_info$link_function,
      confidence_level = confidence_level,
      verbose = verbose
    ),
    "ols" = not_implemented_model("ols"),
    "binary" = not_implemented_model("binary"),
    "multinomial" = not_implemented_model("multinomial"),
    "gam_ordinal" = not_implemented_model("gam_ordinal"),
    stop("Unknown model type: ", model_info$model_type)
  )

  # Add model type info to result
  if (!is.null(result)) {
    result$model_info <- model_info
  }

  return(result)
}

# Stub function for model types not yet implemented
not_implemented_model <- function(model_type) {
  messages <- list(
    "ols" = "OLS regression not yet implemented. Use 'pom_logit' for ordinal outcomes.",
    "binary" = "Binary logistic/probit not yet implemented. Use 'pom_logit' for ordinal outcomes.",
    "multinomial" = "Multinomial logistic regression not yet implemented. Coming soon!",
    "gam_ordinal" = "GAM-based ordinal regression not yet implemented. Coming soon!"
  )
  stop(messages[[model_type]])
}

prepare_regression_data <- function(config_row, data_tb, id.varnames) {
  # Get data source
  data_source_name <- config_row$data_source

  # Check if data source exists
  if (!exists(data_source_name)) {
    stop(paste("Data source", data_source_name, "not found"))
  }

  source_data <- get(data_source_name)

  # Apply filter conditions if specified
  if (!is.null(config_row$filter_conditions) &&
    !is.na(config_row$filter_conditions) &&
    is.character(config_row$filter_conditions) &&
    nchar(trimws(config_row$filter_conditions)) > 0) {
    source_data <- source_data %>%
      filter(eval(parse(text = config_row$filter_conditions)))
  }

  # Get outcome and predictor variables
  outcome_var <- config_row$outcome_var
  outcome_var <- map_column_names(outcome_var, data_source_name)

  # Parse predictors with type checking
  if (!is.null(config_row$predictors) &&
    !is.na(config_row$predictors) &&
    is.character(config_row$predictors) &&
    nchar(trimws(config_row$predictors)) > 0) {
    predictors <- trimws(strsplit(config_row$predictors, ",")[[1]])
    predictors <- map_column_names(predictors, data_source_name)
  } else {
    stop("predictors must be specified in config")
  }

  # Add control variables if specified
  if (!is.null(config_row$control_vars) &&
    !is.na(config_row$control_vars) &&
    is.character(config_row$control_vars) &&
    nchar(trimws(config_row$control_vars)) > 0) {
    control_vars <- trimws(strsplit(config_row$control_vars, ",")[[1]])
    predictors <- c(predictors, control_vars)
  }

  # Select relevant columns
  all_vars <- unique(c(outcome_var, predictors))

  # For wide data, ensure all variables exist
  missing_vars <- setdiff(all_vars, names(source_data))
  if (length(missing_vars) > 0) {
    error_msg <- paste0(
      "\n\n",
      "================================================================================\n",
      "CONFIGURATION ERROR: Missing Variables\n",
      "================================================================================\n\n",
      "The following variable(s) specified in your regression config do not exist\n",
      "in the data source '", data_source_name, "':\n\n",
      "  Missing: ", paste(missing_vars, collapse = ", "), "\n\n",
      "ACTION REQUIRED:\n",
      "  1. Open the Google Sheets regression config tab:\n",
      "     https://docs.google.com/spreadsheets/d/1dcMT2lv9TBz_MPeq6jEMGD7H2Qn3FX-fLMEjc9d5_9Y\n",
      "  2. Find model_id: ", config_row$model_id, "\n",
      "  3. Check the 'predictors' and 'control_vars' columns\n",
      "  4. Fix or remove: ", paste(missing_vars, collapse = ", "), "\n\n",
      "Available variables in '", data_source_name, "':\n",
      "  ", paste(head(sort(names(source_data)), 20), collapse = ", "),
      ifelse(ncol(source_data) > 20, paste0("\n  ... and ", ncol(source_data) - 20, " more"), ""),
      "\n\n",
      "TIP: Use RStudio's View() function to inspect the full data:\n",
      "  View(", data_source_name, ")\n\n",
      "================================================================================\n"
    )
    stop(error_msg)
  }

  # Create regression dataset
  regression_data <- source_data %>%
    select(all_of(all_vars)) %>%
    drop_na()  # Remove rows with any NA in the model variables

  # Extract outcome label for better reporting
  # If outcome is generic "value.num" and data has "category" column, use category name
  outcome_label <- outcome_var
  if (outcome_var == "value.num" && "category" %in% names(source_data)) {
    unique_categories <- unique(source_data$category)
    if (length(unique_categories) == 1) {
      outcome_label <- as.character(unique_categories[1])
    } else if (length(unique_categories) > 1) {
      # Multiple categories - use data source name as label
      outcome_label <- gsub("\\.tb$", "", data_source_name)
    }
  }

  return(list(
    data = regression_data,
    outcome_var = outcome_var,
    outcome_label = outcome_label,  # Human-readable label
    predictors = predictors,
    n_original = nrow(source_data),
    n_final = nrow(regression_data),
    n_dropped = nrow(source_data) - nrow(regression_data)
  ))
}

# BUILD FORMULA FROM PREDICTORS AND INTERACTIONS
build_formula <- function(outcome_var, predictors, interaction_terms = NA) {
  # Start with main effects
  formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))

  # Add interactions if specified
  if (!is.null(interaction_terms) &&
    !is.na(interaction_terms) &&
    is.character(interaction_terms) &&
    nchar(trimws(interaction_terms)) > 0) {
    interactions <- trimws(strsplit(interaction_terms, ",")[[1]])
    # Replace * with : for interaction notation in formula
    interactions <- gsub("\\*", ":", interactions)
    formula_str <- paste(formula_str, "+", paste(interactions, collapse = " + "))
  }

  return(as.formula(formula_str))
}

fit_pom <- function(
                    formula,
                    data,
                    link = "logit",
                    confidence_level = 0.95,
                    verbose = TRUE
) {
  # Load MASS for polr
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for ordinal regression. Please install it.")
  }

  # Validate and convert link function names to MASS::polr method names
  # User-friendly name -> polr method name
  link_map <- c(
    "logit" = "logistic",
    "probit" = "probit",
    "cloglog" = "cloglog",
    "loglog" = "loglog",
    "cauchit" = "cauchit"
  )

  if (!(link %in% names(link_map))) {
    stop(paste("Invalid link function. Must be one of:",
      paste(names(link_map), collapse = ", ")))
  }

  # Get the polr method name
  polr_method <- link_map[link]

  # Extract outcome variable name from formula
  outcome_var <- as.character(formula)[2]

  # Ensure outcome is an ordered factor
  if (!is.ordered(data[[outcome_var]])) {
    if (verbose) {
      cat("  Converting outcome variable to ordered factor...\n")
    }
    # If numeric, assume natural ordering
    if (is.numeric(data[[outcome_var]])) {
      data[[outcome_var]] <- factor(data[[outcome_var]], ordered = TRUE)
    } else {
      # If character/factor, preserve existing levels or sort
      existing_levels <- if (is.factor(data[[outcome_var]])) {
        levels(data[[outcome_var]])
      } else {
        sort(unique(data[[outcome_var]]))
      }
      data[[outcome_var]] <- factor(data[[outcome_var]],
        levels = existing_levels,
        ordered = TRUE)
    }
  }

  # Check that all predictor variables have at least 2 levels
  predictor_vars <- all.vars(formula[[3]])  # Get variable names from RHS
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      n_levels <- length(unique(data[[pred_var]]))
      if (n_levels < 2) {
        stop(paste("Predictor variable", pred_var, "has only", n_levels,
          "level(s). All predictors must have at least 2 levels."))
      }
    }
  }

  # Check cell counts
  cell_check <- check_cell_counts(data, outcome_var, min_count = 10)
  if (cell_check$has_issues && verbose) {
    cat("  WARNING: Some outcome categories have fewer than 10 observations:\n")
    print(cell_check$issues)
    cat("\n")
  }

  # Set base category to largest group for all categorical predictors
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      # Check if variable is categorical (character or factor)
      if (is.character(data[[pred_var]]) || is.factor(data[[pred_var]])) {
        # Count observations per level
        level_counts <- table(data[[pred_var]], useNA = "no")

        # Get levels ordered by count (descending)
        levels_by_count <- names(sort(level_counts, decreasing = TRUE))

        # Convert to factor with most common level first
        data[[pred_var]] <- factor(data[[pred_var]], levels = levels_by_count)

        if (verbose) {
          cat("  Set base category for", pred_var, "to:", levels_by_count[1],
            "(n =", level_counts[levels_by_count[1]], ")\n")
        }
      }
    }
  }

  if (verbose) {
    cat("\n")
  }

  # Fit the model
  if (verbose) {
    cat("  Fitting proportional odds model with", link, "link...\n")
  }

  model <- tryCatch({
    MASS::polr(formula, data = data, Hess = TRUE, method = polr_method)
  }, error = function(e) {
    stop(paste("Model fitting failed:", e$message))
  })

  # Extract coefficients and standard errors
  coef_table <- coef(summary(model))

  # Calculate z-critical value for confidence intervals
  alpha <- 1 - confidence_level
  z_crit <- qnorm(1 - alpha / 2)

  # Process coefficients (predictors only, not thresholds)
  n_predictors <- nrow(coef_table) - (nlevels(data[[outcome_var]]) - 1)

  if (n_predictors > 0) {
    predictor_coefs <- coef_table[1:n_predictors, , drop = FALSE]

    # Create results dataframe
    results <- tibble(
      variable = rownames(predictor_coefs),
      log_odds = predictor_coefs[, "Value"],
      std_error = predictor_coefs[, "Std. Error"],
      t_value = predictor_coefs[, "t value"],
      p_value = 2 * pnorm(abs(predictor_coefs[, "t value"]), lower.tail = FALSE),
      # Calculate confidence intervals
      ci_lower_log_odds = log_odds - z_crit * std_error,
      ci_upper_log_odds = log_odds + z_crit * std_error,
      # Convert to odds ratios
      odds_ratio = exp(log_odds),
      ci_lower_or = exp(ci_lower_log_odds),
      ci_upper_or = exp(ci_upper_log_odds)
    )
  } else {
    results <- tibble(
      variable = character(),
      log_odds = numeric(),
      std_error = numeric(),
      t_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
  }

  # Extract thresholds (intercepts)
  threshold_indices <- (n_predictors + 1):nrow(coef_table)
  thresholds <- tibble(
    threshold = rownames(coef_table)[threshold_indices],
    value = coef_table[threshold_indices, "Value"],
    std_error = coef_table[threshold_indices, "Std. Error"]
  )

  # Calculate model fit statistics
  model_stats <- list(
    aic = AIC(model),
    bic = BIC(model),
    log_likelihood = logLik(model)[1],
    df = attr(logLik(model), "df"),
    n_obs = nobs(model),
    n_predictors = n_predictors,
    link_function = link,
    confidence_level = confidence_level
  )

  if (verbose) {
    cat("  Model fitted successfully\n")
    cat("  AIC:", round(model_stats$aic, 2), "\n")
    cat("  BIC:", round(model_stats$bic, 2), "\n")
    cat("  Log-likelihood:", round(model_stats$log_likelihood, 2), "\n\n")
  }

  return(list(
    model = model,
    coefficients = results,
    thresholds = thresholds,
    model_stats = model_stats,
    cell_counts = cell_check$all_counts,
    formula = formula,
    outcome_var = outcome_var
  ))
}

# FIT PARTIAL PROPORTIONAL ODDS MODEL (PPOM) WITH SPECIFIED LINK FUNCTION
fit_ppom <- function(
                     formula,
                     data,
                     link = "logit",
                     confidence_level = 0.95,
                     verbose = TRUE
) {
  # Load VGAM for vglm (partial proportional odds model)
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' is required for partial proportional odds model. Please install it with: install.packages('VGAM')")
  }

  # Must explicitly load VGAM to ensure summary.vglm method is available
  library(VGAM)

  # Validate link function for PPOM
  # VGAM::cumulative supports: logitlink, probitlink, clogloglink, cauchitlink
  link_map <- c(
    "logit" = "logitlink",
    "probit" = "probitlink",
    "cloglog" = "clogloglink",
    "cauchit" = "cauchitlink"
  )

  if (!(link %in% names(link_map))) {
    stop(paste("Invalid link function for PPOM. Must be one of:",
      paste(names(link_map), collapse = ", ")))
  }

  vgam_link <- link_map[link]

  # Extract outcome variable name from formula
  outcome_var <- as.character(formula)[2]

  # Ensure outcome is an ordered factor
  if (!is.ordered(data[[outcome_var]])) {
    if (verbose) {
      cat("  Converting outcome variable to ordered factor...\n")
    }
    if (is.numeric(data[[outcome_var]])) {
      data[[outcome_var]] <- factor(data[[outcome_var]], ordered = TRUE)
    } else {
      existing_levels <- if (is.factor(data[[outcome_var]])) {
        levels(data[[outcome_var]])
      } else {
        sort(unique(data[[outcome_var]]))
      }
      data[[outcome_var]] <- factor(data[[outcome_var]],
        levels = existing_levels,
        ordered = TRUE)
    }
  }

  # Check that all predictor variables have at least 2 levels
  predictor_vars <- all.vars(formula[[3]])
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      n_levels <- length(unique(data[[pred_var]]))
      if (n_levels < 2) {
        stop(paste("Predictor variable", pred_var, "has only", n_levels,
          "level(s). All predictors must have at least 2 levels."))
      }
    }
  }

  # Check cell counts
  cell_check <- check_cell_counts(data, outcome_var, min_count = 10)
  if (cell_check$has_issues && verbose) {
    cat("  WARNING: Some outcome categories have fewer than 10 observations:\n")
    print(cell_check$issues)
    cat("\n")
  }

  # Set base category to largest group for all categorical predictors
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      if (is.character(data[[pred_var]]) || is.factor(data[[pred_var]])) {
        level_counts <- table(data[[pred_var]], useNA = "no")
        levels_by_count <- names(sort(level_counts, decreasing = TRUE))
        data[[pred_var]] <- factor(data[[pred_var]], levels = levels_by_count)

        if (verbose) {
          cat("  Set base category for", pred_var, "to:", levels_by_count[1],
            "(n =", level_counts[levels_by_count[1]], ")\n")
        }
      }
    }
  }

  if (verbose) {
    cat("\n")
  }

  # Fit the PPOM model
  if (verbose) {
    cat("  Fitting partial proportional odds model (PPOM) with", link, "link...\n")
  }

  model <- tryCatch({
    VGAM::vglm(formula,
      family = VGAM::cumulative(link = vgam_link, parallel = FALSE),
      data = data)
  }, error = function(e) {
    stop(paste("PPOM model fitting failed:", e$message))
  })

  # Extract coefficients - VGAM returns threshold-specific coefficients
  # summary(model) returns an S4 object with coefficient table in @coef3 slot
  coef_summary <- summary(model)@coef3

  # Get number of outcome levels and thresholds
  n_levels <- nlevels(data[[outcome_var]])
  n_thresholds <- n_levels - 1

  # Calculate z-critical value for confidence intervals
  alpha <- 1 - confidence_level
  z_crit <- qnorm(1 - alpha / 2)

  # Extract threshold names (intercepts)
  threshold_names <- paste0(levels(data[[outcome_var]])[1:(n_levels - 1)], "|",
    levels(data[[outcome_var]])[2:n_levels])

  # Parse coefficient names to separate predictors and thresholds
  # VGAM naming: "(Intercept):1", "(Intercept):2", "predictor:1", "predictor:2", etc.
  coef_names <- rownames(coef_summary)

  # Separate intercepts and predictor coefficients
  intercept_rows <- grep("^\\(Intercept\\):", coef_names)
  predictor_rows <- setdiff(1:nrow(coef_summary), intercept_rows)

  # Extract thresholds
  thresholds <- tibble(
    threshold = threshold_names,
    value = coef_summary[intercept_rows, "Estimate"],
    std_error = coef_summary[intercept_rows, "Std. Error"]
  )

  # Process predictor coefficients - they vary by threshold in PPOM
  if (length(predictor_rows) > 0) {
    # Get unique predictor names (without threshold suffix)
    predictor_coef_names <- coef_names[predictor_rows]
    unique_predictors <- unique(gsub(":[0-9]+$", "", predictor_coef_names))

    # Build threshold-specific coefficient dataframe
    threshold_coefs_list <- list()

    for (pred in unique_predictors) {
      pred_rows <- grep(paste0("^", pred, ":"), coef_names)

      for (i in seq_along(pred_rows)) {
        row_idx <- pred_rows[i]
        threshold_coefs_list[[length(threshold_coefs_list) + 1]] <- tibble(
          variable = pred,
          threshold = threshold_names[i],
          threshold_num = i,
          log_odds = coef_summary[row_idx, "Estimate"],
          std_error = coef_summary[row_idx, "Std. Error"],
          z_value = coef_summary[row_idx, "z value"],
          p_value = coef_summary[row_idx, "Pr(>|z|)"],
          ci_lower_log_odds = log_odds - z_crit * std_error,
          ci_upper_log_odds = log_odds + z_crit * std_error,
          odds_ratio = exp(log_odds),
          ci_lower_or = exp(ci_lower_log_odds),
          ci_upper_or = exp(ci_upper_log_odds)
        )
      }
    }

    threshold_specific_coefs <- bind_rows(threshold_coefs_list)

    # Also create average coefficients across thresholds for summary table
    avg_coefs <- threshold_specific_coefs %>%
      group_by(variable) %>%
      summarize(
        log_odds = mean(log_odds),
        std_error = mean(std_error),
        z_value = mean(z_value),
        p_value = mean(p_value),
        ci_lower_log_odds = mean(ci_lower_log_odds),
        ci_upper_log_odds = mean(ci_upper_log_odds),
        odds_ratio = exp(log_odds),
        ci_lower_or = exp(ci_lower_log_odds),
        ci_upper_or = exp(ci_upper_log_odds),
        .groups = "drop"
      )
  } else {
    threshold_specific_coefs <- tibble(
      variable = character(),
      threshold = character(),
      threshold_num = integer(),
      log_odds = numeric(),
      std_error = numeric(),
      z_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
    avg_coefs <- tibble(
      variable = character(),
      log_odds = numeric(),
      std_error = numeric(),
      z_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
  }

  # Calculate model fit statistics
  model_stats <- list(
    aic = AIC(model),
    bic = BIC(model),
    log_likelihood = logLik(model)[1],
    df = attr(logLik(model), "df"),
    n_obs = nobs(model),
    n_predictors = length(unique_predictors),
    n_thresholds = n_thresholds,
    link_function = link,
    confidence_level = confidence_level,
    model_type = "PPOM"
  )

  if (verbose) {
    cat("  PPOM model fitted successfully\n")
    cat("  AIC:", round(model_stats$aic, 2), "\n")
    cat("  BIC:", round(model_stats$bic, 2), "\n")
    cat("  Log-likelihood:", round(model_stats$log_likelihood, 2), "\n")
    cat("  Number of thresholds:", n_thresholds, "\n\n")
  }

  return(list(
    model = model,
    coefficients = avg_coefs,  # Average coefficients for summary table
    threshold_specific_coefs = threshold_specific_coefs,  # Full threshold-specific coefficients
    thresholds = thresholds,
    model_stats = model_stats,
    cell_counts = cell_check$all_counts,
    formula = formula,
    outcome_var = outcome_var,
    model_type = "PPOM"
  ))
}

# GET BASE CATEGORIES FOR FACTOR VARIABLES
get_base_categories <- function(pom_result) {
  # Extract base categories for all factor variables in a POM or PPOM model
  #
  # Args:
  #   pom_result: Output from fit_pom() or fit_ppom() containing model object
  #
  # Returns:
  #   Named list where names are variable names and values are base categories

  model <- pom_result$model

  # Get xlevels from the model (contains factor levels)
  # Handle both S3 (polr) and S4 (vglm) model objects
  xlevels <- if (isS4(model)) {
    # VGAM::vglm models are S4 objects
    if ("xlevels" %in% methods::slotNames(model)) {
      slot(model, "xlevels")
    } else {
      NULL
    }
  } else {
    # MASS::polr models are S3 objects
    model$xlevels
  }

  if (is.null(xlevels) || length(xlevels) == 0) {
    return(list())
  }

  # For each factor variable, the base category is the first level
  # (R uses treatment contrasts by default, where first level is reference)
  base_cats <- lapply(xlevels, function(levels) levels[1])

  return(base_cats)
}

# FORMAT BASE CATEGORIES FOR DISPLAY
format_base_categories_note <- function(base_categories) {
  # Create a formatted text note showing base categories
  #
  # Args:
  #   base_categories: Named list from get_base_categories()
  #
  # Returns:
  #   Character string with formatted note, or NULL if no categorical variables

  if (length(base_categories) == 0) {
    return(NULL)
  }

  # Format each variable's base category
  formatted_items <- sapply(names(base_categories), function(var_name) {
    paste0(var_name, " (base: ", base_categories[[var_name]], ")")
  })

  # Combine into a single note
  note <- paste("Base categories:", paste(formatted_items, collapse = ", "))

  return(note)
}

cat("Regression core functions loaded successfully.\n")
