# ==============================================================================
# 07_orchestration.R
# High-level workflow orchestration functions
# ==============================================================================

run_single_pom_analysis <- function(
                                    config_row,
                                    run_brant_test = FALSE,
                                    verbose = TRUE
) {
  # Orchestrate complete proportional odds model analysis for a single config
  #
  # Args:
  #   config_row: Single row from regression.configs.tb
  #   run_brant_test: Whether to attempt Brant test
  #   verbose: Print progress messages
  #
  # Returns:
  #   List with model_info, data_prep, pom_result, vif_results, etc.

  if (verbose) {
    cat("\n========================================\n")
    cat("RUNNING POM ANALYSIS:", config_row$model_id, "\n")
    cat("========================================\n\n")
    cat("Model:", config_row$model_label, "\n")
    cat("Outcome:", config_row$outcome_var, "\n")
    cat("Data source:", config_row$data_source, "\n\n")
  }

  # Initialize results
  results <- list(
    model_info = list(
      model_id = config_row$model_id,
      model_label = config_row$model_label,
      outcome_var = config_row$outcome_var,
      data_source = config_row$data_source,
      interaction_terms = config_row$interaction_terms
    ),
    warnings = list(),
    errors = list()
  )

  # STEP 1: Prepare data
  if (verbose) cat("Step 1: Preparing data...\n")

  data_prep <- tryCatch({
    prepare_regression_data(
      config_row = config_row,
      data_tb = NULL,
      id.varnames = id.varnames
    )
  }, error = function(e) {
    results$errors$data_prep <<- e$message
    if (verbose) {
      cat("\n")
      cat(e$message)
      cat("\n")
    }
    return(NULL)
  })

  if (is.null(data_prep)) {
    results$status <- "FAILED: Data preparation"
    return(results)
  }

  results$data_prep <- data_prep

  if (verbose) {
    cat("  n_before_filter:", data_prep$n_final, "\n")
  }

  # STEP 1.5: Apply filter_condition if specified
  filter_applied <- FALSE
  n_before_filter <- data_prep$n_final

  if (!is.null(config_row$filter_condition) &&
    !is.na(config_row$filter_condition) &&
    nchar(trimws(config_row$filter_condition)) > 0) {

    filter_condition <- trimws(config_row$filter_condition)

    if (verbose) {
      cat("  Applying filter:", filter_condition, "\n")
    }

    # Apply filter using dplyr::filter with rlang::parse_expr
    filtered_data <- tryCatch({
      data_prep$data %>%
        filter(!!rlang::parse_expr(filter_condition))
    }, error = function(e) {
      warning(paste("Filter condition failed:", e$message))
      if (verbose) {
        cat("  WARNING: Filter failed, using unfiltered data\n")
      }
      return(data_prep$data)
    })

    data_prep$data <- filtered_data
    data_prep$n_final <- nrow(filtered_data)
    filter_applied <- TRUE

    # Store filter info in results
    results$filter_info <- list(
      filter_applied = TRUE,
      filter_condition = filter_condition,
      n_before_filter = n_before_filter,
      n_after_filter = data_prep$n_final,
      n_excluded = n_before_filter - data_prep$n_final
    )

    if (verbose) {
      cat("  n_after_filter:", data_prep$n_final, "\n")
      cat("  n_excluded:", n_before_filter - data_prep$n_final, "\n")

      # Report sex distribution after filtering if sex is a predictor
      if ("sex" %in% data_prep$predictors && "sex" %in% names(filtered_data)) {
        sex_counts <- table(filtered_data$sex, useNA = "no")
        cat("  Sex distribution after filtering:\n")
        for (sex_level in names(sex_counts)) {
          cat("    ", sex_level, ": n =", sex_counts[sex_level], "\n")
        }
      }
      cat("\n")
    }
  } else {
    # Store that no filter was applied
    results$filter_info <- list(
      filter_applied = FALSE
    )

    if (verbose) {
      cat("  No filter applied\n\n")
    }
  }

  # STEP 2: Build formula
  if (verbose) cat("Step 2: Building formula...\n")

  formula <- tryCatch({
    build_formula(
      outcome_var = data_prep$outcome_var,
      predictors = data_prep$predictors,
      interaction_terms = config_row$interaction_terms
    )
  }, error = function(e) {
    results$errors$formula <<- e$message
    return(NULL)
  })

  if (is.null(formula)) {
    results$status <- "FAILED: Formula"
    return(results)
  }

  if (verbose) cat("  Formula:", deparse(formula), "\n\n")

  # STEP 3: Fit Model
  if (verbose) cat("Step 3: Fitting model...\n")

  # Parse regression_method with robust type checking
  regression_methods <- if (!is.null(config_row$regression_method) &&
    !is.na(config_row$regression_method) &&
    is.character(config_row$regression_method) &&
    nchar(trimws(config_row$regression_method)) > 0) {
    # Split comma-separated methods and trim whitespace
    trimws(strsplit(config_row$regression_method, ",")[[1]])
  } else {
    # Default to pom_logit if not specified
    c("pom_logit")
  }

  # Parse confidence level with type checking
  conf_level <- if (!is.null(config_row$confidence_level) &&
    !is.na(config_row$confidence_level) &&
    is.numeric(config_row$confidence_level)) {
    config_row$confidence_level
  } else {
    0.95
  }

  pom_results_by_link <- list()

  for (model_spec in regression_methods) {
    # Use unified model fitting framework
    pom_result <- tryCatch({
      fit_model(
        model_spec = model_spec,
        formula = formula,
        data = data_prep$data,
        confidence_level = conf_level,
        verbose = verbose
      )
    }, error = function(e) {
      # Parse model type for error reporting
      model_info <- get_model_type(model_spec)
      results$errors[[paste0(model_info$model_type, "_", model_spec)]] <<- e$message
      if (verbose) {
        cat("  ERROR fitting", toupper(model_info$model_type), ":", e$message, "\n")
      }
      return(NULL)
    })

    if (!is.null(pom_result)) {
      pom_results_by_link[[model_spec]] <- pom_result
    }
  }

  if (length(pom_results_by_link) == 0) {
    results$status <- "FAILED: POM fitting"
    if (verbose) {
      cat("\n")
      cat("================================================================================\n")
      cat("POM FITTING ERROR\n")
      cat("================================================================================\n\n")
      cat("All link functions failed to fit the proportional odds model.\n\n")

      # DIAGNOSTIC CHECKS
      cat("DIAGNOSTIC CHECKS:\n")
      cat(strrep("-", 80), "\n\n", sep = "")

      # Check 1: Sample size
      min_n_threshold <- 30  # Generally recommended minimum
      min_n_per_category <- 10

      outcome_counts <- data_prep$data %>%
        count(.data[[data_prep$outcome_var]]) %>%
        arrange(n)

      n_categories <- nrow(outcome_counts)
      min_category_n <- min(outcome_counts$n)

      cat("1. SAMPLE SIZE CHECK\n")
      cat("   Total observations: n =", data_prep$n_final, "\n")
      cat("   Minimum recommended: n ≥", min_n_threshold, "\n")
      if (data_prep$n_final < min_n_threshold) {
        cat("   ✗ INSUFFICIENT DATA - Sample size below recommended minimum\n")
      } else {
        cat("   ✓ Sample size adequate\n")
      }
      cat("\n")

      # Check 2: Outcome variation
      cat("2. OUTCOME VARIATION CHECK\n")
      cat("   Outcome variable:", data_prep$outcome_var, "\n")
      cat("   Number of categories:", n_categories, "\n")
      cat("   Category distribution:\n")
      for (i in seq_len(nrow(outcome_counts))) {
        pct <- round(100 * outcome_counts$n[i] / data_prep$n_final, 1)
        cat("     ", outcome_counts[[data_prep$outcome_var]][i], ": n =",
          outcome_counts$n[i], paste0("(", pct, "%)"), "\n")
      }
      cat("   Minimum per category: n =", min_category_n,
        "(threshold: ≥", min_n_per_category, ")\n")

      if (n_categories < 2) {
        cat("   ✗ NO VARIATION - All observations in one category\n")
      } else if (min_category_n < min_n_per_category) {
        cat("   ✗ INSUFFICIENT VARIATION - Some categories have too few observations\n")
      } else {
        cat("   ✓ Outcome variation adequate\n")
      }
      cat("\n")

      # Check 3: Predictor variance
      cat("3. PREDICTOR VARIANCE CHECK\n")
      zero_var_predictors <- c()
      low_var_predictors <- c()

      for (pred in data_prep$predictors) {
        # Check unique values (removing NA)
        unique_vals <- length(unique(na.omit(data_prep$data[[pred]])))

        if (unique_vals == 1) {
          zero_var_predictors <- c(zero_var_predictors, pred)
          # Show the single value for context
          single_val <- unique(na.omit(data_prep$data[[pred]]))
          cat("   ", pred, ": unique values =", unique_vals,
            "✗ ZERO VARIANCE (all =", single_val, ")\n")
        } else if (unique_vals == 2) {
          # Check if binary has extreme imbalance
          val_counts <- table(data_prep$data[[pred]])
          min_prop <- min(val_counts) / sum(val_counts)
          if (min_prop < 0.05) {
            low_var_predictors <- c(low_var_predictors, pred)
            cat("   ", pred, ": unique values =", unique_vals,
              "⚠ LOW VARIANCE (", round(min_prop * 100, 1),
              "% in minority)\n", sep = "")
          } else {
            cat("   ", pred, ": unique values =", unique_vals, "✓\n")
          }
        } else {
          cat("   ", pred, ": unique values =", unique_vals, "✓\n")
        }
      }

      if (length(zero_var_predictors) > 0) {
        cat("\n   ✗ ZERO VARIANCE PREDICTORS FOUND: ",
          paste(zero_var_predictors, collapse = ", "), "\n")
        cat("   EXPLANATION: These variables have only one unique value after\n")
        cat("                filtering and NA removal. This may occur because:\n")
        cat("                • Filter conditions excluded variation\n")
        cat("                • All observations in this category have same value\n")
        cat("                • Variable is constant for this subset of data\n")
        cat("   ACTION: Remove these variables from model or adjust filters\n")
      } else if (length(low_var_predictors) > 0) {
        cat("\n   ⚠ Low variance predictors: ",
          paste(low_var_predictors, collapse = ", "), "\n")
        cat("   WARNING: Extreme imbalance may cause convergence issues\n")
      } else {
        cat("   ✓ All predictors have sufficient variance\n")
      }
      cat("\n")

      # Check 4: Separation issues
      cat("4. SEPARATION CHECK\n")
      cat("   Testing for perfect/quasi-complete separation...\n")

      # Simple check: For each categorical predictor, see if any level
      # perfectly predicts the outcome
      separation_issues <- c()

      for (pred in data_prep$predictors) {
        if (!is.numeric(data_prep$data[[pred]]) ||
          length(unique(data_prep$data[[pred]])) <= 5) {
          # Treat as categorical
          cross_tab <- table(data_prep$data[[pred]],
            data_prep$data[[data_prep$outcome_var]])

          # Check if any row (predictor level) has all observations in one outcome category
          for (i in seq_len(nrow(cross_tab))) {
            if (sum(cross_tab[i, ] > 0) == 1) {
              separation_issues <- c(separation_issues,
                paste0(pred, " (level '", rownames(cross_tab)[i],
                  "' → outcome '", colnames(cross_tab)[which(cross_tab[i, ] > 0)], "')"))
            }
          }
        }
      }

      if (length(separation_issues) > 0) {
        cat("   ✗ PERFECT SEPARATION DETECTED:\n")
        for (issue in separation_issues) {
          cat("     •", issue, "\n")
        }
      } else {
        cat("   ✓ No obvious separation detected\n")
        cat("   Note: Quasi-complete separation may still exist\n")
      }
      cat("\n")

      # Original error messages
      cat(strrep("-", 80), "\n\n", sep = "")
      cat("UNDERLYING ERROR MESSAGES:\n")
      for (err_name in names(results$errors)) {
        if (grepl("^pom_", err_name)) {
          cat("  ", err_name, ":\n    ", results$errors[[err_name]], "\n")
        }
      }
      cat("\n")

      # Recommendations based on findings
      cat(strrep("-", 80), "\n\n", sep = "")
      cat("RECOMMENDED ACTIONS:\n")

      if (data_prep$n_final < min_n_threshold) {
        cat("  • Increase sample size or relax filter conditions\n")
      }

      if (n_categories < 2 || min_category_n < min_n_per_category) {
        cat("  • Collapse outcome categories with low counts\n")
        cat("  • Check filter conditions - may be excluding too much data\n")
      }

      if (length(zero_var_predictors) > 0) {
        cat("  • Remove zero-variance predictors:",
          paste(zero_var_predictors, collapse = ", "), "\n")
      }

      if (length(low_var_predictors) > 0) {
        cat("  • Consider removing highly imbalanced predictors:",
          paste(low_var_predictors, collapse = ", "), "\n")
      }

      if (length(separation_issues) > 0) {
        cat("  • Remove predictors causing perfect separation\n")
        cat("  • Or: Collapse predictor categories to reduce separation\n")
      }

      cat("  • Try alternative regression methods (multinomial logistic, etc.)\n")
      cat("  • Consult with a statistician about your specific data structure\n\n")
      cat("================================================================================\n\n")
    }
    return(results)
  }

  # Select best by AIC
  aic_values <- sapply(pom_results_by_link, function(x) x$model_stats$aic)
  best_link <- names(which.min(aic_values))

  results$pom_result <- pom_results_by_link[[best_link]]
  results$link_comparison <- tibble(
    link_function = names(aic_values),
    aic = aic_values,
    selected = names(aic_values) == best_link
  ) %>% arrange(aic)

  if (verbose) {
    cat("  Best link:", best_link, "\n")
    cat("  AIC:", round(results$pom_result$model_stats$aic, 2), "\n\n")
  }

  # STEP 4: VIF
  if (verbose) cat("Step 4: Calculating VIF...\n")

  vif_results <- tryCatch({
    calculate_vif(results$pom_result$model)
  }, error = function(e) {
    results$warnings$vif <<- e$message
    return(NULL)
  })

  results$vif_results <- vif_results

  if (!is.null(vif_results) && verbose) {
    high_vif <- vif_results[vif_results$vif > 5, ]
    if (nrow(high_vif) > 0) {
      cat("  WARNING: High VIF (>5) detected\n")
    } else {
      cat("  VIF check passed\n")
    }
    cat("\n")
  }

  # STEP 5: Brant test
  if (run_brant_test) {
    if (verbose) cat("Step 5: Running Brant test...\n")

    brant_results <- tryCatch({
      perform_brant_test(results$pom_result$model, verbose = FALSE)
    }, error = function(e) {
      results$warnings$brant <<- e$message
      return(NULL)
    })

    results$brant_results <- brant_results
  } else {
    if (verbose) cat("Step 5: Skipping Brant test\n\n")
  }

  # STEP 6: Residuals
  if (verbose) cat("Step 6: Calculating residuals...\n")

  residuals <- tryCatch({
    calculate_pom_residuals(results$pom_result, verbose = FALSE)
  }, error = function(e) {
    results$errors$residuals <<- e$message
    return(NULL)
  })

  results$residuals <- residuals

  if (verbose && !is.null(residuals)) {
    cat("  Residuals calculated (n =", nrow(residuals), ")\n\n")
  }

  # STEP 7: Diagnostic plots
  if (verbose) cat("Step 7: Creating diagnostic plots...\n")

  if (!is.null(residuals)) {
    diagnostic_plot <- tryCatch({
      plot_pom_diagnostics(
        pom_result = results$pom_result,
        residuals_tb = residuals,
        plot_title = paste(config_row$model_id, "-", config_row$model_label)
      )
    }, error = function(e) {
      results$warnings$diagnostic_plot <<- e$message
      return(NULL)
    })

    results$diagnostic_plot <- diagnostic_plot

    if (!is.null(diagnostic_plot) && verbose) {
      cat("  Diagnostic plots created\n\n")
    }
  }

  # STEP 8: Coefficient plot (placeholder)
  if (verbose) {
    cat("Step 8: Coefficient plot (TODO)\n")
    cat("  Placeholder - will implement later\n\n")
  }

  results$coefficient_plot <- NULL
  results$status <- "SUCCESS"

  if (verbose) {
    cat("========================================\n")
    cat("ANALYSIS COMPLETE\n")
    cat("========================================\n\n")
  }

  return(results)
}

# RUN POM WITH CATEGORY SPLITS
run_pom_with_splits <- function(
                                config_row,
                                run_brant_test = FALSE,
                                verbose = TRUE
) {
  # Wrapper that handles split_by_category logic
  # If split_by_category=TRUE, runs separate analysis for each category
  # Otherwise runs single analysis
  #
  # Args:
  #   config_row: Single row from regression.configs.tb
  #   run_brant_test: Whether to attempt Brant test
  #   verbose: Print progress messages
  #
  # Returns:
  #   List with:
  #     - is_split: TRUE if multiple categories analyzed
  #     - categories: vector of category names (or NULL if not split)
  #     - results: list of results (one per category, or single result)

  # Check if split_by_category is requested
  should_split <- !is.null(config_row$split_by_category) &&
    !is.na(config_row$split_by_category) &&
    config_row$split_by_category == TRUE

  if (!should_split) {
    # Run single analysis
    single_result <- run_single_pom_analysis(
      config_row = config_row,
      run_brant_test = run_brant_test,
      verbose = verbose
    )

    return(list(
      is_split = FALSE,
      categories = NULL,
      results = list(single_result)
    ))
  }

  # SPLIT BY CATEGORY LOGIC
  if (verbose) {
    cat("\n################################################################################\n")
    cat("SPLIT BY CATEGORY ANALYSIS:", config_row$model_id, "\n")
    cat("################################################################################\n\n")
  }

  # Get the data source
  data_source_name <- config_row$data_source
  if (!exists(data_source_name)) {
    stop(paste("Data source", data_source_name, "not found"))
  }
  source_data <- get(data_source_name)

  # Check if data has category column
  if (!"category" %in% names(source_data)) {
    warning("split_by_category=TRUE but data does not have 'category' column. Running single analysis.")
    single_result <- run_single_pom_analysis(
      config_row = config_row,
      run_brant_test = run_brant_test,
      verbose = verbose
    )
    return(list(
      is_split = FALSE,
      categories = NULL,
      results = list(single_result)
    ))
  }

  # Get unique categories
  all_categories <- unique(source_data$category)
  all_categories <- sort(all_categories[!is.na(all_categories)])

  if (verbose) {
    cat("Found", length(all_categories), "categories to analyze:\n")
    for (cat_name in all_categories) {
      cat("  -", cat_name, "\n")
    }
    cat("\n")
  }

  # Run analysis for each category
  category_results <- list()

  for (i in seq_along(all_categories)) {
    cat_name <- all_categories[i]

    if (verbose) {
      cat("\n================================================================================\n")
      cat("CATEGORY", i, "of", length(all_categories), ":", cat_name, "\n")
      cat("================================================================================\n")
    }

    # Create modified config with filter for this category
    config_for_category <- config_row

    # Add or modify filter_conditions to include category filter
    category_filter <- paste0("category == '", cat_name, "'")

    if (!is.null(config_row$filter_conditions) &&
      !is.na(config_row$filter_conditions) &&
      is.character(config_row$filter_conditions) &&
      nchar(trimws(config_row$filter_conditions)) > 0) {
      # Combine existing filter with category filter
      config_for_category$filter_conditions <- paste0(
        "(",
        config_row$filter_conditions,
        ") & (",
        category_filter,
        ")"
      )
    } else {
      # Use only category filter
      config_for_category$filter_conditions <- category_filter
    }

    # Run analysis for this category
    cat_result <- tryCatch({
      run_single_pom_analysis(
        config_row = config_for_category,
        run_brant_test = run_brant_test,
        verbose = verbose
      )
    }, error = function(e) {
      if (verbose) {
        cat("ERROR for category", cat_name, ":", e$message, "\n")
      }
      return(list(
        status = "FAILED",
        error = e$message,
        model_info = list(
          model_id = config_row$model_id,
          category = cat_name
        )
      ))
    })

    # Store result with category name
    category_results[[cat_name]] <- cat_result
  }

  if (verbose) {
    cat("\n################################################################################\n")
    cat("SPLIT ANALYSIS COMPLETE\n")
    cat("################################################################################\n\n")
  }

  return(list(
    is_split = TRUE,
    categories = all_categories,
    results = category_results
  ))
}

cat("Orchestration functions loaded successfully.\n")
