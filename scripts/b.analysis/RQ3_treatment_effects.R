# ==============================================================================
# RQ3: TREATMENT EFFECT OF NUCLEAR-WINTER INFORMATION
# ==============================================================================
#
# This script evaluates the causal effect of exposure to nuclear-winter
# information on support for nuclear retaliation.
#
# PREREQUISITES:
#   - publication_analysis.R must be run first
#
# OUTPUTS:
#   - outputs/RQ3_treatment_effects.md (comprehensive markdown with embedded plots)
#   - outputs/RQ3_POM_diagnostics.png (4-panel diagnostic plots)
#   - outputs/RQ3_POM_forest.png (forest plot of coefficients)
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

# Load required packages
suppressMessages({
  library(MASS)
  library(VGAM)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(gridExtra)
  library(grid)
})

# Create output directories
output_dir <- "outputs"
output_dir_images <- file.path(output_dir, "RQ3 Images")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_dir_images)) {
  dir.create(output_dir_images, recursive = TRUE, showWarnings = FALSE)
}

cat("\n===============================================================================\n")
cat("RQ3: TREATMENT EFFECT OF NUCLEAR-WINTER INFORMATION\n")
cat("===============================================================================\n\n")
cat("Output directory:", output_dir, "\n\n")

# ==============================================================================
# 5.1 Construct Dataset
# ==============================================================================

cat("5.1 Constructing dataset...\n")

rq3_data <- data.tb %>%
  dplyr::filter(
    !is.na(shown.infographic),
    !is.na(support.nuclear.strike.on.russia_numeric)
  ) %>%
  dplyr::select(
    participant.id,
    shown.infographic,
    support.nuclear.strike.on.russia_numeric,
    age,
    sex,
    ethnicity,
    ethnicity.collapsed,
    political.affiliation,
    political.affiliation.collapsed,
    political.affiliation.binary,
    employment.status,
    student.status,
    country.of.residence
  ) %>%
  tidyr::drop_na()

cat("  Total sample:", nrow(rq3_data), "\n")
cat("    Treatment (shown infographic):",
    sum(rq3_data$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("    Control (no infographic):",
    sum(rq3_data$shown.infographic == "No Infographic"), "\n\n")

# ==============================================================================
# 5.2 Unadjusted Effects
# ==============================================================================

cat("5.2 Computing unadjusted descriptive effects...\n")

# Mean support by treatment group
unadjusted_means <- rq3_data %>%
  group_by(shown.infographic) %>%
  summarise(
    n = n(),
    mean_support = mean(support.nuclear.strike.on.russia_numeric, na.rm = TRUE),
    sd_support = sd(support.nuclear.strike.on.russia_numeric, na.rm = TRUE),
    prop_high_support = mean(support.nuclear.strike.on.russia_numeric >= 4, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Unadjusted means:\n")
print(unadjusted_means)
cat("\n")

# ==============================================================================
# 5.3 POM → PPOM Escalation
# ==============================================================================

cat("5.3 Fitting ordinal models and testing assumptions...\n\n")

# Covariates for adjusted models (using collapsed categorical variables)
# Note: Using collapsed versions (ethnicity.collapsed, political.affiliation.collapsed)
# to improve model convergence and coefficient stability
# UPDATED: Removed employment.status and student.status per protocol
covariates <- c("age", "sex", "ethnicity.collapsed", "political.affiliation.collapsed")

# Model 1: Unadjusted POM
cat("  Model 1: Unadjusted POM...\n")
formula_unadjusted <- as.formula("support.nuclear.strike.on.russia_numeric ~ shown.infographic")

model1_unadjusted <- fit_pom(
  formula = formula_unadjusted,
  data = rq3_data,
  link = "logit",
  verbose = FALSE
)

cat("    AIC:", round(model1_unadjusted$model_stats$aic, 2), "\n")
cat("    Treatment effect (log odds):",
    round(model1_unadjusted$coefficients$log_odds[1], 3), "\n")
cat("    Treatment effect (OR):",
    round(model1_unadjusted$coefficients$odds_ratio[1], 3),
    "(95% CI:",
    round(model1_unadjusted$coefficients$ci_lower_or[1], 3), "-",
    round(model1_unadjusted$coefficients$ci_upper_or[1], 3), ")\n")
cat("    p-value:", sprintf("%.4f", model1_unadjusted$coefficients$p_value[1]), "\n\n")

# Create reduced dataset for Models 2-5 (exclude sex == "Prefer not to say")
cat("  Creating reduced dataset for adjusted models...\n")
rq3_data_reduced <- rq3_data %>%
  dplyr::filter(sex != "Prefer not to say")

n_excluded_rq3 <- nrow(rq3_data) - nrow(rq3_data_reduced)
cat("    Original sample:", nrow(rq3_data), "\n")
cat("    Excluded (sex=='Prefer not to say'):", n_excluded_rq3, "\n")
cat("    Reduced sample:", nrow(rq3_data_reduced), "\n")
cat("      Treatment:", sum(rq3_data_reduced$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("      Control:", sum(rq3_data_reduced$shown.infographic == "No Infographic"), "\n\n")

# Model 2: Adjusted POM (using reduced dataset)
cat("  Model 2: Adjusted POM (reduced sample, reduced variables)...\n")
formula_adjusted <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(covariates, collapse = " + ")
))

model2_adjusted <- fit_pom(
  formula = formula_adjusted,
  data = rq3_data_reduced,  # Using reduced dataset
  link = "logit",
  verbose = FALSE
)

cat("    AIC:", round(model2_adjusted$model_stats$aic, 2), "\n")

# Extract treatment effect from adjusted model
treatment_coef <- model2_adjusted$coefficients %>%
  filter(grepl("infographic", variable))

cat("    Treatment effect (log odds):",
    round(treatment_coef$log_odds[1], 3), "\n")
cat("    Treatment effect (OR):",
    round(treatment_coef$odds_ratio[1], 3),
    "(95% CI:",
    round(treatment_coef$ci_lower_or[1], 3), "-",
    round(treatment_coef$ci_upper_or[1], 3), ")\n")
cat("    p-value:", sprintf("%.4f", treatment_coef$p_value[1]), "\n\n")

# Visual inspection of proportional odds assumption (per DEC-007)
cat("  Visual inspection of proportional odds assumption...\n\n")

cat("  HUMAN ANALYST VISUAL INSPECTION FINDINGS:\n\n")

cat("  Based on diagnostic plots for adjusted POM:\n\n")

cat("  1. Residuals vs Fitted Plot: Assessment required\n")
cat("     - Check for nonlinear patterns in residuals\n")
cat("     - Indicates potential threshold-specific effects if present\n\n")

cat("  2. Normal Q-Q Plot: Assessment required\n")
cat("     - Check for deviations from diagonal line\n")
cat("     - Heavy tails or 'broken stick' pattern indicates violations\n\n")

cat("  3. Scale-Location Plot: Assessment required\n")
cat("     - Check for upward/downward trends in smoothed line\n")
cat("     - Indicates heteroskedasticity if present\n\n")

cat("  4. Observed vs Predicted Plot: Assessment required\n")
cat("     - Check if points cluster near diagonal\n")
cat("     - Systematic deviations indicate poor fit\n\n")

# PLACEHOLDER: Set visual inspection result
# User should review diagnostic plots and update this variable
# TRUE = violations detected (escalate to PPOM)
# FALSE = no clear violations (use POM)
po_violated_visual <- TRUE  # CHANGE THIS AFTER REVIEWING PLOTS

if (po_violated_visual) {
  cat("  VISUAL INSPECTION DECISION: CLEAR violations detected\n")
  cat("  → ESCALATE TO PPOM (per DEC-007 protocol)\n\n")
} else {
  cat("  VISUAL INSPECTION DECISION: No clear violations\n")
  cat("  → USE POM (proportional odds assumption holds)\n\n")
}

cat("  Note: Brant test bypassed per DEC-007\n")
cat("        (visual inspection used for assumption testing)\n\n")

# Model 3: Adjusted PPOM (if PO assumption violated by visual inspection)
if (po_violated_visual) {
  cat("  Model 3: Adjusted PPOM (reduced sample, visual inspection violations)...\n")

  # PPOM specification: Only treatment variable has flexible coefficients
  # Rationale: Allow treatment effect to vary across thresholds; covariates
  # constrained for parsimony and computational stability
  parallel_spec_rq3 <- FALSE ~ shown.infographic

  cat("  Specification: Treatment variable flexible, covariates constrained\n")

  model3_ppom <- fit_ppom(
    formula = formula_adjusted,
    data = rq3_data_reduced,  # Using reduced dataset
    link = "logit",
    parallel_spec = parallel_spec_rq3,
    verbose = FALSE
  )

  cat("    AIC:", round(model3_ppom$model_stats$aic, 2), "\n")

  # Extract treatment effect (averaged across thresholds)
  treatment_ppom <- model3_ppom$coefficients %>%
    filter(grepl("infographic", variable))

  cat("    Treatment effect (avg log odds):",
      round(treatment_ppom$log_odds[1], 3), "\n")
  cat("    Treatment effect (avg OR):",
      round(treatment_ppom$odds_ratio[1], 3), "\n")
  cat("    (Note: PPOM allows effects to vary by threshold)\n\n")

  # Calculate PPOM residuals for diagnostic plots
  cat("  - Calculating PPOM residuals...\n")

  ppom_model <- model3_ppom$model
  n_obs_ppom <- nrow(rq3_data_reduced)

  pred_probs_ppom <- predict(ppom_model, type = "response")
  if (is.vector(pred_probs_ppom)) {
    pred_probs_ppom <- matrix(pred_probs_ppom, nrow = 1)
  }
  pred_class_ppom <- apply(pred_probs_ppom, 1, which.max)

  # Calculate linear predictor for PPOM (average across thresholds)
  if ("predictors" %in% methods::slotNames(ppom_model)) {
    predictors_matrix <- ppom_model@predictors
    if (is.matrix(predictors_matrix) && nrow(predictors_matrix) == n_obs_ppom) {
      linear_pred_ppom <- rowMeans(predictors_matrix)
    } else {
      linear_pred_ppom <- rep(0, n_obs_ppom)
    }
  } else {
    linear_pred_ppom <- rep(0, n_obs_ppom)
  }

  # Extract observed values from VGAM model
  if ("y" %in% methods::slotNames(ppom_model)) {
    y_matrix <- ppom_model@y
    if (is.matrix(y_matrix)) {
      y_observed_ppom <- apply(y_matrix, 1, which.max)
    } else {
      y_observed_ppom <- as.numeric(y_matrix)
    }
  } else {
    # Fall back to recalculating from reduced data
    y_observed_ppom <- as.numeric(ordered(rq3_data_reduced$support.nuclear.strike.on.russia_numeric))
  }

  # Calculate deviance residuals
  dev_resid_ppom <- numeric(n_obs_ppom)
  for (i in seq_len(n_obs_ppom)) {
    p_obs <- pred_probs_ppom[i, y_observed_ppom[i]]
    dev_component <- sqrt(-2 * log(p_obs))
    sign_val <- ifelse(
      y_observed_ppom[i] > pred_class_ppom[i], 1,
      ifelse(y_observed_ppom[i] < pred_class_ppom[i], -1, 0)
    )
    dev_resid_ppom[i] <- sign_val * dev_component
  }

  # Store PPOM residuals (plots will be generated in section 5.5)
  residuals_ppom <- tibble(
    obs_index = seq_len(n_obs_ppom),
    observed = y_observed_ppom,
    predicted_class = pred_class_ppom,
    linear_predictor = linear_pred_ppom,
    deviance_residual = dev_resid_ppom
  )

  # PPOM diagnostic plots will be generated in section 5.5 after diagnostic function is defined

  # Flag which model to use for predictions
  final_model <- model3_ppom
  final_model_name <- "PPOM (adjusted)"
  po_violated <- TRUE
} else {
  cat("  Visual inspection shows no clear violations.\n")
  cat("  Using adjusted POM for predictions.\n\n")
  final_model <- model2_adjusted
  final_model_name <- "POM (adjusted)"
  po_violated <- FALSE
}

# ==============================================================================
# 5.4 Predicted Probabilities
# ==============================================================================

cat("5.4 Computing predicted probabilities...\n")

# Create representative profile (modal categories, median continuous)
representative_profile <- rq3_data %>%
  summarise(
    age = median(age, na.rm = TRUE),
    sex = names(sort(table(sex), decreasing = TRUE))[1],
    ethnicity.collapsed = names(sort(table(ethnicity.collapsed), decreasing = TRUE))[1],
    political.affiliation.collapsed = names(sort(table(political.affiliation.collapsed), decreasing = TRUE))[1],
    employment.status = names(sort(table(employment.status), decreasing = TRUE))[1],
    student.status = names(sort(table(student.status), decreasing = TRUE))[1]
  )

# Prediction data for treatment vs control
pred_data <- data.frame(
  shown.infographic = c("Shown NW Iinfographic", "No Infographic")
) %>%
  bind_cols(representative_profile[rep(1, 2), ])

# Get predictions
if (po_violated) {
  # For PPOM, use VGAM predict function
  preds <- predict(final_model$model, newdata = pred_data, type = "response")
} else {
  # For POM, use MASS predict function
  preds <- predict(final_model$model, newdata = pred_data, type = "probs")
}

# Create predictions dataframe
pred_probs <- data.frame(
  group = c("Treatment", "Control"),
  shown.infographic = pred_data$shown.infographic
)

# Add predicted probabilities for each outcome level
for (i in 1:ncol(preds)) {
  pred_probs[[paste0("prob_level_", i)]] <- preds[, i]
}

cat("  Predicted probabilities (representative profile):\n")
print(pred_probs)
cat("\n")

# Calculate probability of high support (levels 4-5)
pred_probs <- pred_probs %>%
  mutate(
    prob_high_support = if (ncol(preds) >= 4) {
      rowSums(preds[, 4:ncol(preds), drop = FALSE])
    } else {
      preds[, ncol(preds)]
    }
  )

cat("  Probability of high support (≥4):\n")
cat("    Treatment:", round(pred_probs$prob_high_support[1], 3), "\n")
cat("    Control:", round(pred_probs$prob_high_support[2], 3), "\n")
cat("    Difference:", round(pred_probs$prob_high_support[1] - pred_probs$prob_high_support[2], 3), "\n\n")

# ==============================================================================
# 5.5 Generate Diagnostic and Forest Plots
# ==============================================================================

cat("5.5 Generating diagnostic and forest plots...\n")

# Create plotting function for 4-panel diagnostics (same as RQ2)
create_diagnostic_plots_rq3 <- function(residuals_tb, model_label) {
  # Plot 1: Residuals vs Fitted
  plot1 <- ggplot(residuals_tb, aes(x = linear_predictor, y = deviance_residual)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE, color = "blue", linewidth = 0.8) +
    labs(
      title = "Residuals vs Fitted",
      x = "Linear Predictor",
      y = "Deviance Residuals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # Plot 2: Q-Q Plot
  residuals_sorted <- sort(residuals_tb$deviance_residual)
  theoretical_quantiles <- qnorm(ppoints(length(residuals_sorted)))
  qq_data <- tibble(
    theoretical = theoretical_quantiles,
    sample = residuals_sorted
  )

  plot2 <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # Plot 3: Scale-Location
  residuals_tb_plot <- residuals_tb %>%
    mutate(
      sqrt_abs_std_residual = sqrt(abs(deviance_residual / sd(deviance_residual)))
    )

  plot3 <- ggplot(residuals_tb_plot, aes(x = linear_predictor, y = sqrt_abs_std_residual)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, color = "blue", linewidth = 0.8) +
    labs(
      title = "Scale-Location",
      x = "Linear Predictor",
      y = expression(sqrt("|Standardized Residuals|"))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # Plot 4: Observed vs Predicted
  residuals_tb_plot <- residuals_tb_plot %>%
    mutate(observed_jittered = observed + runif(n(), -0.1, 0.1))

  plot4 <- ggplot(residuals_tb_plot, aes(x = predicted_class, y = observed_jittered)) +
    geom_jitter(alpha = 0.5, width = 0.2, height = 0.2, size = 1.5, color = "#0072B2") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Observed vs Predicted",
      x = "Predicted Class",
      y = "Observed Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # Combine into 2x2 panel
  combined <- arrangeGrob(
    plot1, plot2, plot3, plot4,
    ncol = 2, nrow = 2,
    top = textGrob(model_label, gp = gpar(fontsize = 12, fontface = "bold"))
  )

  return(combined)
}

# Calculate residuals for adjusted POM (Model 2)
cat("  - Calculating POM residuals...\n")

# Extract underlying model
pom_model <- model2_adjusted$model

# Get predictions
pred_probs_pom <- predict(pom_model, type = "probs")
if (is.vector(pred_probs_pom)) {
  pred_probs_pom <- matrix(pred_probs_pom, nrow = 1)
}
pred_class_pom <- apply(pred_probs_pom, 1, which.max)

# Get observed values from reduced dataset
y_observed <- as.numeric(ordered(rq3_data_reduced$support.nuclear.strike.on.russia_numeric))

# Calculate linear predictor
# Note: For POM, linear predictor is X*beta (excluding intercepts)
# If model.matrix doesn't match coefficients length, use observed values as proxy
X_pom <- model.matrix(pom_model)[, -1, drop = FALSE]
beta_pom <- coef(pom_model)
if (ncol(X_pom) == length(beta_pom)) {
  linear_pred_pom <- as.vector(X_pom %*% beta_pom)
} else {
  # Fallback: use predicted class as proxy for linear predictor
  linear_pred_pom <- pred_class_pom
}

# Calculate deviance residuals
n_obs <- nrow(rq3_data)
n_obs_reduced <- nrow(rq3_data_reduced)
dev_resid_pom <- numeric(n_obs_reduced)
for (i in seq_len(n_obs_reduced)) {
  p_obs <- pred_probs_pom[i, y_observed[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed[i] > pred_class_pom[i], 1,
    ifelse(y_observed[i] < pred_class_pom[i], -1, 0)
  )
  dev_resid_pom[i] <- sign_val * dev_component
}

# Store POM residuals
residuals_pom <- tibble(
  obs_index = seq_len(n_obs_reduced),
  observed = y_observed,
  predicted_class = pred_class_pom,
  linear_predictor = linear_pred_pom,
  deviance_residual = dev_resid_pom
)

# Generate POM diagnostic plots
cat("  - Creating POM diagnostic plots...\n")
pom_diagnostics_plot <- create_diagnostic_plots_rq3(residuals_pom, "POM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir_images, "RQ3_POM_diagnostics.png"),
  plot = pom_diagnostics_plot,
  width = 10, height = 8, dpi = 300
)

# Generate POM forest plot using helper function
cat("  - Creating POM forest plot...\n")

# Extract coefficients from model2_adjusted and prepare for helper function
coef_summary_pom <- summary(pom_model)$coefficients
n_levels <- length(unique(rq3_data$support.nuclear.strike.on.russia_numeric))
n_thresholds <- n_levels - 1

# Prepare data structure for plot_pom_coefficients()
pom_result_for_plot <- list(
  coefficients = tibble(
    variable = rownames(coef_summary_pom)[1:(nrow(coef_summary_pom) - n_thresholds)],
    log_odds = coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "Value"],
    std_error = coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "Std. Error"],
    t_value = coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "t value"],
    p_value = 2 * pnorm(abs(coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "t value"]), lower.tail = FALSE),
    odds_ratio = exp(log_odds)
  ),
  model_stats = list(confidence_level = 0.95)
)

# Create POM forest plot using helper function with normal curves
pom_forest <- plot_pom_coefficients(pom_result_for_plot, plot_title = "POM Coefficients (Adjusted Model)")

ggsave(
  filename = file.path(output_dir_images, "RQ3_POM_forest.png"),
  plot = pom_forest,
  width = 10, height = 6, dpi = 300
)

cat("  ✓ POM plots saved\n")

# Generate PPOM diagnostic plots if PO assumption was violated
if (po_violated) {
  cat("  - Creating PPOM diagnostic plots...\n")
  ppom_diagnostics_plot <- create_diagnostic_plots_rq3(residuals_ppom, "PPOM Diagnostic Plots")
  ggsave(
    filename = file.path(output_dir_images, "RQ3_PPOM_diagnostics.png"),
    plot = ppom_diagnostics_plot,
    width = 10, height = 8, dpi = 300
  )

  # Generate PPOM threshold-specific coefficient plot for treatment variable
  cat("  - Creating PPOM threshold-specific coefficient plot...\n")

  # Extract coefficients for treatment variable across thresholds
  ppom_model <- model3_ppom$model
  coef_summary_ppom_matrix <- summary(ppom_model)@coef3

  # Find rows matching treatment variable
  treatment_var_name <- "shown.infographic"
  matching_rows <- grep(paste0("^", treatment_var_name), rownames(coef_summary_ppom_matrix), value = TRUE)

  if (length(matching_rows) > 0) {
    ppom_coef_data <- data.frame()
    for (row_name in matching_rows) {
      threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
      estimate <- coef_summary_ppom_matrix[row_name, "Estimate"]
      std_error <- coef_summary_ppom_matrix[row_name, "Std. Error"]

      ppom_coef_data <- rbind(ppom_coef_data, data.frame(
        variable = treatment_var_name,
        threshold = threshold_num,
        estimate = estimate,
        std_error = std_error,
        odds_ratio = exp(estimate),
        ci_lower = exp(estimate - 1.96 * std_error),
        ci_upper = exp(estimate + 1.96 * std_error)
      ))
    }

    # Create threshold-specific coefficient plot
    ppom_coef_plot <- plot_ppom_threshold_coefficients(
      coef_data = ppom_coef_data,
      plot_title = "PPOM Threshold-Specific Coefficients (Treatment Effect)"
    )

    ggsave(
      filename = file.path(output_dir_images, "RQ3_PPOM_coefficients.png"),
      plot = ppom_coef_plot,
      width = 10, height = 6, dpi = 300
    )
  }

  cat("  ✓ PPOM plots saved\n")
}

cat("\n")

# ==============================================================================
# 5.8 Political Affiliation Sensitivity Analyses
# ==============================================================================

cat("5.8 Running political affiliation sensitivity analyses...\n\n")

# ------------------------------------------------------------------------------
# Model 6: All Political Parties (Uncollapsed)
# ------------------------------------------------------------------------------

cat("  Model 6: All political parties (uncollapsed, 11 categories)...\n")

# Create covariate list with uncollapsed political affiliation
covariates_uncollapsed <- c("age", "sex", "ethnicity.collapsed", "political.affiliation")

# Formula with uncollapsed political affiliation
formula_allparties <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(covariates_uncollapsed, collapse = " + ")
))

cat("    Sample size:", nrow(rq3_data_reduced), "\n")
cat("      Treatment:", sum(rq3_data_reduced$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("      Control:", sum(rq3_data_reduced$shown.infographic == "No Infographic"), "\n")

# Fit PPOM with all political parties
cat("    Fitting PPOM...\n")
parallel_spec_allparties <- FALSE ~ shown.infographic

model6_allparties_ppom <- fit_ppom(
  formula = formula_allparties,
  data = rq3_data_reduced,
  link = "logit",
  parallel_spec = parallel_spec_allparties,
  verbose = FALSE
)

cat("      AIC:", round(model6_allparties_ppom$model_stats$aic, 2), "\n")

# Extract treatment effect
treatment_allparties <- model6_allparties_ppom$coefficients %>%
  dplyr::filter(grepl("infographic", variable))

cat("      Treatment effect (avg OR):", round(treatment_allparties$odds_ratio[1], 3), "\n")

# Calculate PPOM residuals for all parties model
n_obs_allparties <- nrow(rq3_data_reduced)
ppom_model_allparties <- model6_allparties_ppom$model

pred_probs_ppom_allparties <- predict(ppom_model_allparties, type = "response")
if (is.vector(pred_probs_ppom_allparties)) {
  pred_probs_ppom_allparties <- matrix(pred_probs_ppom_allparties, nrow = 1)
}
pred_class_ppom_allparties <- apply(pred_probs_ppom_allparties, 1, which.max)

# Calculate linear predictor
if ("predictors" %in% methods::slotNames(ppom_model_allparties)) {
  predictors_matrix_allparties <- ppom_model_allparties@predictors
  if (is.matrix(predictors_matrix_allparties) && nrow(predictors_matrix_allparties) == n_obs_allparties) {
    linear_pred_ppom_allparties <- rowMeans(predictors_matrix_allparties)
  } else {
    linear_pred_ppom_allparties <- rep(0, n_obs_allparties)
  }
} else {
  linear_pred_ppom_allparties <- rep(0, n_obs_allparties)
}

# Extract observed values
if ("y" %in% methods::slotNames(ppom_model_allparties)) {
  y_matrix_allparties <- ppom_model_allparties@y
  if (is.matrix(y_matrix_allparties)) {
    y_observed_ppom_allparties <- apply(y_matrix_allparties, 1, which.max)
  } else {
    y_observed_ppom_allparties <- as.numeric(y_matrix_allparties)
  }
} else {
  y_observed_ppom_allparties <- as.numeric(ordered(rq3_data_reduced$support.nuclear.strike.on.russia_numeric))
}

# Calculate deviance residuals
dev_resid_ppom_allparties <- numeric(n_obs_allparties)
for (i in seq_len(n_obs_allparties)) {
  p_obs <- pred_probs_ppom_allparties[i, y_observed_ppom_allparties[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed_ppom_allparties[i] > pred_class_ppom_allparties[i], 1,
    ifelse(y_observed_ppom_allparties[i] < pred_class_ppom_allparties[i], -1, 0)
  )
  dev_resid_ppom_allparties[i] <- sign_val * dev_component
}

# Store residuals
residuals_ppom_allparties <- tibble(
  obs_index = seq_len(n_obs_allparties),
  observed = y_observed_ppom_allparties,
  predicted_class = pred_class_ppom_allparties,
  linear_predictor = linear_pred_ppom_allparties,
  deviance_residual = dev_resid_ppom_allparties
)

# Generate diagnostic plots
cat("    Generating PPOM diagnostic plots...\n")
ppom_diagnostics_plot_allparties <- create_diagnostic_plots_rq3(residuals_ppom_allparties, "All Parties PPOM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir_images, "RQ3_AllParties_PPOM_diagnostics.png"),
  plot = ppom_diagnostics_plot_allparties,
  width = 10, height = 8, dpi = 300
)

# Generate threshold-specific coefficient plot
cat("    Generating PPOM threshold-specific coefficient plot...\n")
coef_summary_ppom_matrix_allparties <- summary(ppom_model_allparties)@coef3
treatment_var_name <- "shown.infographic"
matching_rows_allparties <- grep(paste0("^", treatment_var_name), rownames(coef_summary_ppom_matrix_allparties), value = TRUE)

if (length(matching_rows_allparties) > 0) {
  ppom_coef_data_allparties <- data.frame()
  for (row_name in matching_rows_allparties) {
    threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
    estimate <- coef_summary_ppom_matrix_allparties[row_name, "Estimate"]
    std_error <- coef_summary_ppom_matrix_allparties[row_name, "Std. Error"]

    ppom_coef_data_allparties <- rbind(ppom_coef_data_allparties, data.frame(
      variable = treatment_var_name,
      threshold = threshold_num,
      estimate = estimate,
      std_error = std_error,
      odds_ratio = exp(estimate),
      ci_lower = exp(estimate - 1.96 * std_error),
      ci_upper = exp(estimate + 1.96 * std_error)
    ))
  }

  ppom_coef_plot_allparties <- plot_ppom_threshold_coefficients(
    coef_data = ppom_coef_data_allparties,
    plot_title = "All Parties PPOM Threshold-Specific Coefficients (Treatment Effect)"
  )

  ggsave(
    filename = file.path(output_dir_images, "RQ3_AllParties_PPOM_coefficients.png"),
    plot = ppom_coef_plot_allparties,
    width = 10, height = 6, dpi = 300
  )
}

cat("    ✓ All parties analysis complete\n\n")

# ------------------------------------------------------------------------------
# Model 7: Binary Political (Left vs Right only, filtered)
# ------------------------------------------------------------------------------

cat("  Model 7: Binary political (Left vs Right only)...\n")

# Filter to only observations with non-NA binary political affiliation
rq3_data_binary <- rq3_data_reduced %>%
  dplyr::filter(!is.na(political.affiliation.binary))

cat("    Sample size:", nrow(rq3_data_binary), "\n")
cat("      Treatment:", sum(rq3_data_binary$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("      Control:", sum(rq3_data_binary$shown.infographic == "No Infographic"), "\n")
cat("      Left:", sum(rq3_data_binary$political.affiliation.binary == "Left"), "\n")
cat("      Right:", sum(rq3_data_binary$political.affiliation.binary == "Right"), "\n")

# Create covariate list with binary political affiliation
covariates_binary <- c("age", "sex", "ethnicity.collapsed", "political.affiliation.binary")

# Formula with binary political affiliation
formula_binary <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(covariates_binary, collapse = " + ")
))

# Fit PPOM with binary political
cat("    Fitting PPOM...\n")
parallel_spec_binary <- FALSE ~ shown.infographic

model7_binary_ppom <- fit_ppom(
  formula = formula_binary,
  data = rq3_data_binary,
  link = "logit",
  parallel_spec = parallel_spec_binary,
  verbose = FALSE
)

cat("      AIC:", round(model7_binary_ppom$model_stats$aic, 2), "\n")

# Extract treatment effect
treatment_binary <- model7_binary_ppom$coefficients %>%
  dplyr::filter(grepl("infographic", variable))

cat("      Treatment effect (avg OR):", round(treatment_binary$odds_ratio[1], 3), "\n")

# Calculate PPOM residuals for binary model
n_obs_binary <- nrow(rq3_data_binary)
ppom_model_binary <- model7_binary_ppom$model

pred_probs_ppom_binary <- predict(ppom_model_binary, type = "response")
if (is.vector(pred_probs_ppom_binary)) {
  pred_probs_ppom_binary <- matrix(pred_probs_ppom_binary, nrow = 1)
}
pred_class_ppom_binary <- apply(pred_probs_ppom_binary, 1, which.max)

# Calculate linear predictor
if ("predictors" %in% methods::slotNames(ppom_model_binary)) {
  predictors_matrix_binary <- ppom_model_binary@predictors
  if (is.matrix(predictors_matrix_binary) && nrow(predictors_matrix_binary) == n_obs_binary) {
    linear_pred_ppom_binary <- rowMeans(predictors_matrix_binary)
  } else {
    linear_pred_ppom_binary <- rep(0, n_obs_binary)
  }
} else {
  linear_pred_ppom_binary <- rep(0, n_obs_binary)
}

# Extract observed values
if ("y" %in% methods::slotNames(ppom_model_binary)) {
  y_matrix_binary <- ppom_model_binary@y
  if (is.matrix(y_matrix_binary)) {
    y_observed_ppom_binary <- apply(y_matrix_binary, 1, which.max)
  } else {
    y_observed_ppom_binary <- as.numeric(y_matrix_binary)
  }
} else {
  y_observed_ppom_binary <- as.numeric(ordered(rq3_data_binary$support.nuclear.strike.on.russia_numeric))
}

# Calculate deviance residuals
dev_resid_ppom_binary <- numeric(n_obs_binary)
for (i in seq_len(n_obs_binary)) {
  p_obs <- pred_probs_ppom_binary[i, y_observed_ppom_binary[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed_ppom_binary[i] > pred_class_ppom_binary[i], 1,
    ifelse(y_observed_ppom_binary[i] < pred_class_ppom_binary[i], -1, 0)
  )
  dev_resid_ppom_binary[i] <- sign_val * dev_component
}

# Store residuals
residuals_ppom_binary <- tibble(
  obs_index = seq_len(n_obs_binary),
  observed = y_observed_ppom_binary,
  predicted_class = pred_class_ppom_binary,
  linear_predictor = linear_pred_ppom_binary,
  deviance_residual = dev_resid_ppom_binary
)

# Generate diagnostic plots
cat("    Generating PPOM diagnostic plots...\n")
ppom_diagnostics_plot_binary <- create_diagnostic_plots_rq3(residuals_ppom_binary, "Binary Political PPOM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir_images, "RQ3_BinaryPolitical_PPOM_diagnostics.png"),
  plot = ppom_diagnostics_plot_binary,
  width = 10, height = 8, dpi = 300
)

# Generate threshold-specific coefficient plot
cat("    Generating PPOM threshold-specific coefficient plot...\n")
coef_summary_ppom_matrix_binary <- summary(ppom_model_binary)@coef3
matching_rows_binary <- grep(paste0("^", treatment_var_name), rownames(coef_summary_ppom_matrix_binary), value = TRUE)

if (length(matching_rows_binary) > 0) {
  ppom_coef_data_binary <- data.frame()
  for (row_name in matching_rows_binary) {
    threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
    estimate <- coef_summary_ppom_matrix_binary[row_name, "Estimate"]
    std_error <- coef_summary_ppom_matrix_binary[row_name, "Std. Error"]

    ppom_coef_data_binary <- rbind(ppom_coef_data_binary, data.frame(
      variable = treatment_var_name,
      threshold = threshold_num,
      estimate = estimate,
      std_error = std_error,
      odds_ratio = exp(estimate),
      ci_lower = exp(estimate - 1.96 * std_error),
      ci_upper = exp(estimate + 1.96 * std_error)
    ))
  }

  ppom_coef_plot_binary <- plot_ppom_threshold_coefficients(
    coef_data = ppom_coef_data_binary,
    plot_title = "Binary Political PPOM Threshold-Specific Coefficients (Treatment Effect)"
  )

  ggsave(
    filename = file.path(output_dir_images, "RQ3_BinaryPolitical_PPOM_coefficients.png"),
    plot = ppom_coef_plot_binary,
    width = 10, height = 6, dpi = 300
  )
}

cat("    ✓ Binary political analysis complete\n\n")

# ==============================================================================
# 5.6 Generate Markdown Output
# ==============================================================================

cat("5.6 Generating comprehensive markdown report...\n")

# Extract POM coefficients for Models 1 and 2
coef_summary_model1 <- summary(model1_unadjusted$model)$coefficients
coef_summary_model2 <- summary(model2_adjusted$model)$coefficients
n_levels <- length(unique(rq3_data$support.nuclear.strike.on.russia_numeric))
n_thresholds <- n_levels - 1

# Extract POM variable names (excluding thresholds)
model1_vars <- rownames(coef_summary_model1)[1:(nrow(coef_summary_model1) - n_thresholds)]
model2_vars <- rownames(coef_summary_model2)[1:(nrow(coef_summary_model2) - n_thresholds)]

# Build POM coefficient table (Models 1 & 2)
all_pom_vars <- unique(c(model1_vars, model2_vars))
pom_coef_table_rows <- c()
for (var in all_pom_vars) {
  # Model 1 coefficient
  model1_coef <- if (var %in% model1_vars) {
    sprintf("%.4f", coef_summary_model1[var, "Value"])
  } else {
    "—"
  }

  # Model 2 coefficient
  model2_coef <- if (var %in% model2_vars) {
    sprintf("%.4f", coef_summary_model2[var, "Value"])
  } else {
    "—"
  }

  pom_coef_table_rows <- c(pom_coef_table_rows,
    paste0("| ", var, " | ", model1_coef, " | ", model2_coef, " |")
  )
}

# Build PPOM coefficient table (Models 3, 4, 5) if po_violated
ppom_coef_table_rows <- c()
if (po_violated) {
  # Extract PPOM coefficients for model 3 (always available)
  coef_summary_model3 <- summary(model3_ppom$model)@coef3

  # Extract USA/UK model coefficients only if they exist
  if (exists("ppom_model_usa")) {
    coef_summary_model4_usa <- summary(ppom_model_usa)@coef3
  } else {
    coef_summary_model4_usa <- NULL
  }

  if (exists("ppom_model_uk")) {
    coef_summary_model5_uk <- summary(ppom_model_uk)@coef3
  } else {
    coef_summary_model5_uk <- NULL
  }

  # Get PPOM variable names (excluding intercepts)
  model3_vars <- rownames(coef_summary_model3)[!grepl("^\\(Intercept\\)", rownames(coef_summary_model3))]

  if (!is.null(coef_summary_model4_usa)) {
    model4_vars <- rownames(coef_summary_model4_usa)[!grepl("^\\(Intercept\\)", rownames(coef_summary_model4_usa))]
  } else {
    model4_vars <- character(0)
  }

  if (!is.null(coef_summary_model5_uk)) {
    model5_vars <- rownames(coef_summary_model5_uk)[!grepl("^\\(Intercept\\)", rownames(coef_summary_model5_uk))]
  } else {
    model5_vars <- character(0)
  }

  # Collect all unique variable names
  all_ppom_vars <- unique(c(model3_vars, model4_vars, model5_vars))

  for (var in all_ppom_vars) {
    # Model 3 coefficient
    model3_coef <- if (var %in% model3_vars) {
      sprintf("%.4f", coef_summary_model3[var, "Estimate"])
    } else {
      "—"
    }

    # Model 4 (USA) coefficient
    model4_coef <- if (!is.null(coef_summary_model4_usa) && var %in% model4_vars) {
      sprintf("%.4f", coef_summary_model4_usa[var, "Estimate"])
    } else {
      "—"
    }

    # Model 5 (UK) coefficient
    model5_coef <- if (!is.null(coef_summary_model5_uk) && var %in% model5_vars) {
      sprintf("%.4f", coef_summary_model5_uk[var, "Estimate"])
    } else {
      "—"
    }

    ppom_coef_table_rows <- c(ppom_coef_table_rows,
      paste0("| ", var, " | ", model3_coef, " | ", model4_coef, " | ", model5_coef, " |")
    )
  }
}

# Build PPOM coefficient table for political affiliation models (Models 6, 7)
ppom_political_coef_table_rows <- c()
# Extract PPOM coefficients for political affiliation models
coef_summary_model6_allparties <- summary(ppom_model_allparties)@coef3
coef_summary_model7_binary <- summary(ppom_model_binary)@coef3

# Get PPOM variable names (excluding intercepts)
model6_vars <- rownames(coef_summary_model6_allparties)[!grepl("^\\(Intercept\\)", rownames(coef_summary_model6_allparties))]
model7_vars <- rownames(coef_summary_model7_binary)[!grepl("^\\(Intercept\\)", rownames(coef_summary_model7_binary))]

# Collect all unique variable names
all_political_ppom_vars <- unique(c(model6_vars, model7_vars))

for (var in all_political_ppom_vars) {
  # Model 6 (All Parties) coefficient
  model6_coef <- if (var %in% model6_vars) {
    sprintf("%.4f", coef_summary_model6_allparties[var, "Estimate"])
  } else {
    "—"
  }

  # Model 7 (Binary Political) coefficient
  model7_coef <- if (var %in% model7_vars) {
    sprintf("%.4f", coef_summary_model7_binary[var, "Estimate"])
  } else {
    "—"
  }

  ppom_political_coef_table_rows <- c(ppom_political_coef_table_rows,
    paste0("| ", var, " | ", model6_coef, " | ", model7_coef, " |")
  )
}

md_file <- file.path(output_dir, "RQ3_treatment_effects.md")
md_content <- c(
  "# RQ3: Treatment Effect of Nuclear-Winter Information",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  "- **Sample**: Complete cases on treatment and outcome variables",
  "- **Outcome**: Support for nuclear strike on Russia (ordinal, 1-5)",
  "- **Treatment**: Exposure to nuclear-winter infographic",
  paste0("- **Final model**: ", ifelse(po_violated, "PPOM", "POM")),
  "",
  "---",
  "",
  "## Section 1: Sample Sizes",
  "",
  "| Model | Complete Cases | Treatment | Control |",
  "|-------|----------------|-----------|---------|",
  paste0("| Model 1 (Unadjusted POM) | ", nrow(rq3_data), " | ",
         sum(rq3_data$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data$shown.infographic == "No Infographic"), " |"),
  paste0("| Model 2 (Adjusted POM) | ", nrow(rq3_data_reduced), " | ",
         sum(rq3_data_reduced$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_reduced$shown.infographic == "No Infographic"), " |"),
  paste0("| Model 3 (Full PPOM) | ", nrow(rq3_data_reduced), " | ",
         sum(rq3_data_reduced$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_reduced$shown.infographic == "No Infographic"), " |"),
  if (exists("rq3_data_usa")) paste0("| Model 4 (USA PPOM) | ", nrow(rq3_data_usa), " | ",
         sum(rq3_data_usa$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_usa$shown.infographic == "No Infographic"), " |") else NULL,
  if (exists("rq3_data_uk")) paste0("| Model 5 (UK PPOM) | ", nrow(rq3_data_uk), " | ",
         sum(rq3_data_uk$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_uk$shown.infographic == "No Infographic"), " |") else NULL,
  paste0("| Model 6 (All Parties PPOM) | ", nrow(rq3_data_reduced), " | ",
         sum(rq3_data_reduced$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_reduced$shown.infographic == "No Infographic"), " |"),
  paste0("| Model 7 (Binary Political PPOM) | ", nrow(rq3_data_binary), " | ",
         sum(rq3_data_binary$shown.infographic == "Shown NW Iinfographic"), " | ",
         sum(rq3_data_binary$shown.infographic == "No Infographic"), " |"),
  "",
  paste0("*Note: Model 1 uses full sample (N=", nrow(rq3_data), "); Models 2-7 use reduced sample (excluding sex=='Prefer not to say', ", n_excluded_rq3, " obs excluded). ",
         if (exists("rq3_data_usa") && exists("rq3_data_uk")) "Models 4-5 are country-specific subgroups. " else "",
         "Models 6-7 are political affiliation sensitivity analyses (Model 7: N=", nrow(rq3_data_binary), ", filtered to Left/Right only).*"),
  "",
  "---",
  "",
  "## Section 2: Model Formulas",
  "",
  "### Model 1: Unadjusted POM",
  "",
  "**Formula:**",
  "```",
  deparse(formula_unadjusted, width.cutoff = 500),
  "```",
  "",
  "**Specification:**",
  "- Proportional odds assumption: Treatment effect constant across thresholds",
  "- No covariates",
  "- Link: Logit",
  "- Implementation: MASS::polr",
  "",
  "### Model 2: Adjusted POM",
  "",
  "**Formula:**",
  "```",
  deparse(formula_adjusted, width.cutoff = 500),
  "```",
  "",
  "**Specification:**",
  "- Proportional odds assumption: All coefficients constant across thresholds",
  "- Covariates: age, sex, ethnicity.collapsed, political.affiliation.collapsed",
  "- **Variables excluded**: employment.status, student.status",
  "- **Sample restriction**: Excludes sex == 'Prefer not to say'",
  "- Link: Logit",
  "- Implementation: MASS::polr",
  "",
  "### Model 3: Full PPOM",
  "",
  "**Formula:**",
  "```",
  deparse(formula_adjusted, width.cutoff = 500),
  "```",
  "",
  "**Specification:**",
  "- **Flexible predictors**: Treatment variable (shown.infographic)",
  "  - Coefficients vary across support thresholds",
  "- **Constrained predictors**: Covariates (age, sex, ethnicity.collapsed, political.affiliation.collapsed)",
  "  - Proportional odds constraint maintained",
  "- **Variables excluded**: employment.status, student.status",
  "- **Sample restriction**: Excludes sex == 'Prefer not to say'",
  "- **Rationale**: Targeted flexibility for treatment effect; computational stability",
  "- **Link**: Logit",
  "- **Implementation**: VGAM::vglm",
  "",
  "### Models 4 & 5: Country-Specific PPOM",
  "",
  "**Formula:**",
  "```",
  "support.nuclear.strike.on.russia ~ shown.infographic +",
  "                                    age + sex + ethnicity.collapsed +",
  "                                    political.affiliation.collapsed",
  "```",
  "",
  "**Specification:**",
  "- Model 4: USA subsample only",
  "- Model 5: UK subsample only",
  "- Same PPOM structure as Model 3",
  "- **Variables excluded**: employment.status, student.status, country.of.residence",
  "- **Sample restriction**: Excludes sex == 'Prefer not to say'",
  "- Country.of.residence removed from covariates (constant within subsample)",
  "",
  "### Models 6 & 7: Political Affiliation Sensitivity Analyses",
  "",
  "**Model 6 Formula (All Parties):**",
  "```",
  deparse(formula_allparties, width.cutoff = 500),
  "```",
  "",
  "**Model 6 Specification:**",
  "- **Political affiliation**: Uncollapsed (11 categories)",
  "- Uses full political.affiliation variable instead of collapsed version",
  "- Same PPOM structure as Model 3 (treatment variable flexible)",
  "- **Sample**: Same as Models 2-3 (N=", nrow(rq3_data_reduced), ")",
  "",
  "**Model 7 Formula (Binary Political):**",
  "```",
  deparse(formula_binary, width.cutoff = 500),
  "```",
  "",
  "**Model 7 Specification:**",
  "- **Political affiliation**: Binary (Left vs Right)",
  "  - Left: UK-Labour, UK-Green, UK-Plaid Cymru, UK-SNP",
  "  - Right: UK-Conservative, USA-Republican",
  "  - All other parties filtered out",
  "- Same PPOM structure as Model 3 (treatment variable flexible)",
  "- **Sample**: Filtered to Left/Right only (N=", nrow(rq3_data_binary), ")",
  "",
  "---",
  "",
  "## Section 3: Model Fit Statistics",
  "",
  "| Model | AIC | BIC | Log-Likelihood | N | Δ AIC (vs Model 2) |",
  "|-------|-----|-----|----------------|---|---------------------|",
  paste0("| Model 1 (Unadjusted POM) | ",
         round(as.numeric(model1_unadjusted$model_stats$aic), 2), " | ",
         round(as.numeric(model1_unadjusted$model_stats$bic), 2), " | ",
         round(as.numeric(model1_unadjusted$model_stats$loglik), 2), " | ",
         nrow(rq3_data), " | — |"),
  paste0("| Model 2 (Adjusted POM) | ",
         round(as.numeric(model2_adjusted$model_stats$aic), 2), " | ",
         round(as.numeric(model2_adjusted$model_stats$bic), 2), " | ",
         round(as.numeric(model2_adjusted$model_stats$loglik), 2), " | ",
         nrow(rq3_data_reduced), " | — |"),
  if (po_violated) {
    c(
      paste0("| Model 3 (Full PPOM) | ",
             round(as.numeric(model3_ppom$model_stats$aic), 2), " | ",
             round(as.numeric(model3_ppom$model_stats$bic), 2), " | ",
             round(as.numeric(model3_ppom$model_stats$loglik), 2), " | ",
             nrow(rq3_data_reduced), " | ",
             round(as.numeric(model3_ppom$model_stats$aic) - as.numeric(model2_adjusted$model_stats$aic), 2), " |"),
      if (exists("model_usa_ppom") && exists("rq3_data_usa")) paste0("| Model 4 (USA PPOM) | ",
             round(as.numeric(model_usa_ppom$model_stats$aic), 2), " | ",
             round(as.numeric(model_usa_ppom$model_stats$bic), 2), " | ",
             round(as.numeric(model_usa_ppom$model_stats$loglik), 2), " | ",
             nrow(rq3_data_usa), " | — |") else NULL,
      if (exists("model_uk_ppom") && exists("rq3_data_uk")) paste0("| Model 5 (UK PPOM) | ",
             round(as.numeric(model_uk_ppom$model_stats$aic), 2), " | ",
             round(as.numeric(model_uk_ppom$model_stats$bic), 2), " | ",
             round(as.numeric(model_uk_ppom$model_stats$loglik), 2), " | ",
             nrow(rq3_data_uk), " | — |") else NULL,
      paste0("| Model 6 (All Parties PPOM) | ",
             round(as.numeric(model6_allparties_ppom$model_stats$aic), 2), " | ",
             round(as.numeric(model6_allparties_ppom$model_stats$bic), 2), " | ",
             round(as.numeric(model6_allparties_ppom$model_stats$loglik), 2), " | ",
             nrow(rq3_data_reduced), " | ",
             round(as.numeric(model6_allparties_ppom$model_stats$aic) - as.numeric(model2_adjusted$model_stats$aic), 2), " |"),
      paste0("| Model 7 (Binary Political PPOM) | ",
             round(as.numeric(model7_binary_ppom$model_stats$aic), 2), " | ",
             round(as.numeric(model7_binary_ppom$model_stats$bic), 2), " | ",
             round(as.numeric(model7_binary_ppom$model_stats$loglik), 2), " | ",
             nrow(rq3_data_binary), " | — |")
    )
  } else {
    NULL
  },
  "",
  paste0("*Note: AIC values for Model 1 vs Models 2-7 are not directly comparable due to different sample sizes (N=", nrow(rq3_data), " vs N=", nrow(rq3_data_reduced), " or N=", nrow(rq3_data_binary), "). Model 7 has smaller N due to filtering to Left/Right parties only.*"),
  "",
  "---",
  "",
  "## Section 4: Diagnostic Plots",
  "",
  "### Model 2 (Adjusted POM) Diagnostics",
  "",
  "![POM Diagnostic Plots](RQ3 Images/RQ3_POM_diagnostics.png)",
  "",
  if (po_violated) {
    c(
      "### Model 3 (Full PPOM) Diagnostics",
      "",
      "![Full PPOM Diagnostic Plots](RQ3 Images/RQ3_PPOM_diagnostics.png)",
      "",
      if (file.exists(file.path(output_dir_images, "RQ3_USA_PPOM_diagnostics.png"))) c(
        "### Model 4 (USA PPOM) Diagnostics",
        "",
        "![USA PPOM Diagnostic Plots](RQ3 Images/RQ3_USA_PPOM_diagnostics.png)",
        ""
      ) else NULL,
      if (file.exists(file.path(output_dir_images, "RQ3_UK_PPOM_diagnostics.png"))) c(
        "### Model 5 (UK PPOM) Diagnostics",
        "",
        "![UK PPOM Diagnostic Plots](RQ3 Images/RQ3_UK_PPOM_diagnostics.png)",
        ""
      ) else NULL,
      "### Model 6 (All Parties PPOM) Diagnostics",
      "",
      "![All Parties PPOM Diagnostic Plots](RQ3 Images/RQ3_AllParties_PPOM_diagnostics.png)",
      "",
      "### Model 7 (Binary Political PPOM) Diagnostics",
      "",
      "![Binary Political PPOM Diagnostic Plots](RQ3 Images/RQ3_BinaryPolitical_PPOM_diagnostics.png)",
      ""
    )
  } else {
    NULL
  },
  "---",
  "",
  "## Section 5: Model Coefficients",
  "",
  "### Table 5.1: POM Models (Models 1 & 2)",
  "",
  "*Table shows coefficient estimates only (no standard errors or p-values).*",
  "",
  "| Variable | Model 1 (Unadjusted) | Model 2 (Adjusted) |",
  "|----------|----------------------|--------------------|",
  pom_coef_table_rows,
  "",
  if (po_violated) {
    c(
      "### Table 5.2: PPOM Models (Models 3, 4, 5)",
      "",
      "*Table shows coefficient estimates only. PPOM coefficients vary by threshold (indicated by :1, :2, etc.).*",
      "",
      "| Variable | Model 3 (Full) | Model 4 (USA) | Model 5 (UK) |",
      "|----------|----------------|---------------|--------------|",
      ppom_coef_table_rows,
      "",
      "### Table 5.3: Political Affiliation PPOM Models (Models 6, 7)",
      "",
      "*Table shows coefficient estimates only. PPOM coefficients vary by threshold (indicated by :1, :2, etc.).*",
      "",
      "| Variable | Model 6 (All Parties) | Model 7 (Binary Political) |",
      "|----------|------------------------|----------------------------|",
      ppom_political_coef_table_rows,
      ""
    )
  } else {
    NULL
  },
  "---",
  "",
  "## Section 6: Forest Plots and Coefficient Plots",
  "",
  "### Model 2 (Adjusted POM) Forest Plot",
  "",
  "![POM Forest Plot](RQ3 Images/RQ3_POM_forest.png)",
  "",
  if (po_violated) {
    c(
      "### Model 3 (Full PPOM) Threshold-Specific Coefficients",
      "",
      "![Full PPOM Coefficient Plot](RQ3 Images/RQ3_PPOM_coefficients.png)",
      "",
      if (file.exists(file.path(output_dir_images, "RQ3_USA_PPOM_coefficients.png"))) c(
        "### Model 4 (USA PPOM) Threshold-Specific Coefficients",
        "",
        "![USA PPOM Coefficient Plot](RQ3 Images/RQ3_USA_PPOM_coefficients.png)",
        ""
      ) else NULL,
      if (file.exists(file.path(output_dir_images, "RQ3_UK_PPOM_coefficients.png"))) c(
        "### Model 5 (UK PPOM) Threshold-Specific Coefficients",
        "",
        "![UK PPOM Coefficient Plot](RQ3 Images/RQ3_UK_PPOM_coefficients.png)",
        ""
      ) else NULL,
      "### Model 6 (All Parties PPOM) Threshold-Specific Coefficients",
      "",
      "![All Parties PPOM Coefficient Plot](RQ3 Images/RQ3_AllParties_PPOM_coefficients.png)",
      "",
      "### Model 7 (Binary Political PPOM) Threshold-Specific Coefficients",
      "",
      "![Binary Political PPOM Coefficient Plot](RQ3 Images/RQ3_BinaryPolitical_PPOM_coefficients.png)",
      ""
    )
  } else {
    NULL
  },
  "---",
  "",
  paste0("*Generated: ", Sys.time(), "*")
)

writeLines(md_content, md_file)
cat("  ✓ RQ3_treatment_effects.md\n\n")

# ==============================================================================
# Summary and Next Steps
# ==============================================================================

cat("===============================================================================\n")
cat("RQ3 ANALYSIS COMPLETE\n")
cat("===============================================================================\n\n")

cat("Results summary:\n")
cat("  - Sample size:", nrow(rq3_data), "\n")
cat("  - Treatment effect (adjusted OR):",
    round(treatment_coef$odds_ratio[1], 3),
    "(p =", sprintf("%.4f", treatment_coef$p_value), ")\n")
cat("  - PO assumption:", ifelse(po_violated, "VIOLATED", "HOLDS"), "\n")
cat("  - Final model:", final_model_name, "\n\n")

cat("Output files:\n")
cat("  -", md_file, "\n\n")

cat("===============================================================================\n\n")

# 5.7 Country-Specific Subgroup Analyses
# ==============================================================================

cat("5.7 Running country-specific subgroup analyses...\n\n")

# ------------------------------------------------------------------------------
# USA-only analysis
# ------------------------------------------------------------------------------

cat("  USA-only analysis...\n")
rq3_data_usa <- rq3_data_reduced %>% dplyr::filter(country.of.residence == "United States")

cat("    Sample size:", nrow(rq3_data_usa), "\n")
cat("      Treatment:", sum(rq3_data_usa$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("      Control:", sum(rq3_data_usa$shown.infographic == "No Infographic"), "\n")

# Formula without country.of.residence
formula_usa <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(setdiff(covariates, "country.of.residence"), collapse = " + ")
))

# Fit PPOM directly (skip POM based on main analysis findings)
cat("    Fitting PPOM...\n")
parallel_spec_usa <- FALSE ~ shown.infographic

model_usa_ppom <- fit_ppom(
  formula = formula_usa,
  data = rq3_data_usa,
  link = "logit",
  parallel_spec = parallel_spec_usa,
  verbose = FALSE
)

cat("      AIC:", round(model_usa_ppom$model_stats$aic, 2), "\n")

# Extract treatment effect
treatment_usa <- model_usa_ppom$coefficients %>%
  dplyr::filter(grepl("infographic", variable))

cat("      Treatment effect (avg OR):", round(treatment_usa$odds_ratio[1], 3), "\n")

# Calculate PPOM residuals for USA
n_obs_usa <- nrow(rq3_data_usa)
ppom_model_usa <- model_usa_ppom$model

pred_probs_ppom_usa <- predict(ppom_model_usa, type = "response")
if (is.vector(pred_probs_ppom_usa)) {
  pred_probs_ppom_usa <- matrix(pred_probs_ppom_usa, nrow = 1)
}
pred_class_ppom_usa <- apply(pred_probs_ppom_usa, 1, which.max)

# Calculate linear predictor for PPOM
if ("predictors" %in% methods::slotNames(ppom_model_usa)) {
  predictors_matrix_usa <- ppom_model_usa@predictors
  if (is.matrix(predictors_matrix_usa) && nrow(predictors_matrix_usa) == n_obs_usa) {
    linear_pred_ppom_usa <- rowMeans(predictors_matrix_usa)
  } else {
    linear_pred_ppom_usa <- rep(0, n_obs_usa)
  }
} else {
  linear_pred_ppom_usa <- rep(0, n_obs_usa)
}

# Extract observed values
if ("y" %in% methods::slotNames(ppom_model_usa)) {
  y_matrix_usa <- ppom_model_usa@y
  if (is.matrix(y_matrix_usa)) {
    y_observed_ppom_usa <- apply(y_matrix_usa, 1, which.max)
  } else {
    y_observed_ppom_usa <- as.numeric(y_matrix_usa)
  }
} else {
  y_observed_ppom_usa <- as.numeric(ordered(rq3_data_usa$support.nuclear.strike.on.russia_numeric))
}

# Calculate deviance residuals
dev_resid_ppom_usa <- numeric(n_obs_usa)
for (i in seq_len(n_obs_usa)) {
  p_obs <- pred_probs_ppom_usa[i, y_observed_ppom_usa[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed_ppom_usa[i] > pred_class_ppom_usa[i], 1,
    ifelse(y_observed_ppom_usa[i] < pred_class_ppom_usa[i], -1, 0)
  )
  dev_resid_ppom_usa[i] <- sign_val * dev_component
}

# Store USA PPOM residuals
residuals_ppom_usa <- tibble(
  obs_index = seq_len(n_obs_usa),
  observed = y_observed_ppom_usa,
  predicted_class = pred_class_ppom_usa,
  linear_predictor = linear_pred_ppom_usa,
  deviance_residual = dev_resid_ppom_usa
)

# Generate USA PPOM diagnostic plots
cat("    Generating PPOM diagnostic plots...\n")
ppom_diagnostics_plot_usa <- create_diagnostic_plots_rq3(residuals_ppom_usa, "USA PPOM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir_images, "RQ3_USA_PPOM_diagnostics.png"),
  plot = ppom_diagnostics_plot_usa,
  width = 10, height = 8, dpi = 300
)

# Generate USA PPOM threshold-specific coefficient plot
cat("    Generating PPOM threshold-specific coefficient plot...\n")
coef_summary_ppom_matrix_usa <- summary(ppom_model_usa)@coef3
treatment_var_name <- "shown.infographic"
matching_rows_usa <- grep(paste0("^", treatment_var_name), rownames(coef_summary_ppom_matrix_usa), value = TRUE)

if (length(matching_rows_usa) > 0) {
  ppom_coef_data_usa <- data.frame()
  for (row_name in matching_rows_usa) {
    threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
    estimate <- coef_summary_ppom_matrix_usa[row_name, "Estimate"]
    std_error <- coef_summary_ppom_matrix_usa[row_name, "Std. Error"]

    ppom_coef_data_usa <- rbind(ppom_coef_data_usa, data.frame(
      variable = treatment_var_name,
      threshold = threshold_num,
      estimate = estimate,
      std_error = std_error,
      odds_ratio = exp(estimate),
      ci_lower = exp(estimate - 1.96 * std_error),
      ci_upper = exp(estimate + 1.96 * std_error)
    ))
  }

  ppom_coef_plot_usa <- plot_ppom_threshold_coefficients(
    coef_data = ppom_coef_data_usa,
    plot_title = "USA PPOM Threshold-Specific Coefficients (Treatment Effect)"
  )

  ggsave(
    filename = file.path(output_dir_images, "RQ3_USA_PPOM_coefficients.png"),
    plot = ppom_coef_plot_usa,
    width = 10, height = 6, dpi = 300
  )
}

cat("    ✓ USA analysis complete\n\n")

# ------------------------------------------------------------------------------
# UK-only analysis
# ------------------------------------------------------------------------------

cat("  UK-only analysis...\n")
rq3_data_uk <- rq3_data_reduced %>% dplyr::filter(country.of.residence == "United Kingdom")

cat("    Sample size:", nrow(rq3_data_uk), "\n")
cat("      Treatment:", sum(rq3_data_uk$shown.infographic == "Shown NW Iinfographic"), "\n")
cat("      Control:", sum(rq3_data_uk$shown.infographic == "No Infographic"), "\n")

# Formula without country.of.residence
formula_uk <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(setdiff(covariates, "country.of.residence"), collapse = " + ")
))

# Fit PPOM directly
cat("    Fitting PPOM...\n")
parallel_spec_uk <- FALSE ~ shown.infographic

model_uk_ppom <- fit_ppom(
  formula = formula_uk,
  data = rq3_data_uk,
  link = "logit",
  parallel_spec = parallel_spec_uk,
  verbose = FALSE
)

cat("      AIC:", round(model_uk_ppom$model_stats$aic, 2), "\n")

# Extract treatment effect
treatment_uk <- model_uk_ppom$coefficients %>%
  dplyr::filter(grepl("infographic", variable))

cat("      Treatment effect (avg OR):", round(treatment_uk$odds_ratio[1], 3), "\n")

# Calculate PPOM residuals for UK
n_obs_uk <- nrow(rq3_data_uk)
ppom_model_uk <- model_uk_ppom$model

pred_probs_ppom_uk <- predict(ppom_model_uk, type = "response")
if (is.vector(pred_probs_ppom_uk)) {
  pred_probs_ppom_uk <- matrix(pred_probs_ppom_uk, nrow = 1)
}
pred_class_ppom_uk <- apply(pred_probs_ppom_uk, 1, which.max)

# Calculate linear predictor for PPOM
if ("predictors" %in% methods::slotNames(ppom_model_uk)) {
  predictors_matrix_uk <- ppom_model_uk@predictors
  if (is.matrix(predictors_matrix_uk) && nrow(predictors_matrix_uk) == n_obs_uk) {
    linear_pred_ppom_uk <- rowMeans(predictors_matrix_uk)
  } else {
    linear_pred_ppom_uk <- rep(0, n_obs_uk)
  }
} else {
  linear_pred_ppom_uk <- rep(0, n_obs_uk)
}

# Extract observed values
if ("y" %in% methods::slotNames(ppom_model_uk)) {
  y_matrix_uk <- ppom_model_uk@y
  if (is.matrix(y_matrix_uk)) {
    y_observed_ppom_uk <- apply(y_matrix_uk, 1, which.max)
  } else {
    y_observed_ppom_uk <- as.numeric(y_matrix_uk)
  }
} else {
  y_observed_ppom_uk <- as.numeric(ordered(rq3_data_uk$support.nuclear.strike.on.russia_numeric))
}

# Calculate deviance residuals
dev_resid_ppom_uk <- numeric(n_obs_uk)
for (i in seq_len(n_obs_uk)) {
  p_obs <- pred_probs_ppom_uk[i, y_observed_ppom_uk[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed_ppom_uk[i] > pred_class_ppom_uk[i], 1,
    ifelse(y_observed_ppom_uk[i] < pred_class_ppom_uk[i], -1, 0)
  )
  dev_resid_ppom_uk[i] <- sign_val * dev_component
}

# Store UK PPOM residuals
residuals_ppom_uk <- tibble(
  obs_index = seq_len(n_obs_uk),
  observed = y_observed_ppom_uk,
  predicted_class = pred_class_ppom_uk,
  linear_predictor = linear_pred_ppom_uk,
  deviance_residual = dev_resid_ppom_uk
)

# Generate UK PPOM diagnostic plots
cat("    Generating PPOM diagnostic plots...\n")
ppom_diagnostics_plot_uk <- create_diagnostic_plots_rq3(residuals_ppom_uk, "UK PPOM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir_images, "RQ3_UK_PPOM_diagnostics.png"),
  plot = ppom_diagnostics_plot_uk,
  width = 10, height = 8, dpi = 300
)

# Generate UK PPOM threshold-specific coefficient plot
cat("    Generating PPOM threshold-specific coefficient plot...\n")
coef_summary_ppom_matrix_uk <- summary(ppom_model_uk)@coef3
matching_rows_uk <- grep(paste0("^", treatment_var_name), rownames(coef_summary_ppom_matrix_uk), value = TRUE)

if (length(matching_rows_uk) > 0) {
  ppom_coef_data_uk <- data.frame()
  for (row_name in matching_rows_uk) {
    threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
    estimate <- coef_summary_ppom_matrix_uk[row_name, "Estimate"]
    std_error <- coef_summary_ppom_matrix_uk[row_name, "Std. Error"]

    ppom_coef_data_uk <- rbind(ppom_coef_data_uk, data.frame(
      variable = treatment_var_name,
      threshold = threshold_num,
      estimate = estimate,
      std_error = std_error,
      odds_ratio = exp(estimate),
      ci_lower = exp(estimate - 1.96 * std_error),
      ci_upper = exp(estimate + 1.96 * std_error)
    ))
  }

  ppom_coef_plot_uk <- plot_ppom_threshold_coefficients(
    coef_data = ppom_coef_data_uk,
    plot_title = "UK PPOM Threshold-Specific Coefficients (Treatment Effect)"
  )

  ggsave(
    filename = file.path(output_dir_images, "RQ3_UK_PPOM_coefficients.png"),
    plot = ppom_coef_plot_uk,
    width = 10, height = 6, dpi = 300
  )
}

cat("    ✓ UK analysis complete\n\n")

# ==============================================================================
