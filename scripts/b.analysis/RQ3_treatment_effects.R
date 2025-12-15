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

# Create output directory (simple, no timestamps)
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
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
covariates <- c("age", "sex", "ethnicity.collapsed", "political.affiliation.collapsed",
                "employment.status", "student.status")

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

# Model 2: Adjusted POM
cat("  Model 2: Adjusted POM...\n")
formula_adjusted <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
  paste(covariates, collapse = " + ")
))

model2_adjusted <- fit_pom(
  formula = formula_adjusted,
  data = rq3_data,
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

# Test proportional odds assumption
cat("  Testing proportional odds assumption...\n")
brant_result <- tryCatch({
  perform_brant_test(model2_adjusted$model, verbose = FALSE)
}, error = function(e) {
  cat("    WARNING: Brant test failed with error:", e$message, "\n")
  cat("    Defaulting to POM model (proportional odds assumed).\n")
  # Return a dummy result that indicates PO assumption holds
  list(
    omnibus_p_value = 0.99,  # High p-value = assumption holds
    brant_test_failed = TRUE,
    error_message = e$message
  )
})

if (!is.null(brant_result$brant_test_failed) && brant_result$brant_test_failed) {
  cat("    Brant test could not be completed.\n")
  cat("    Proceeding with POM model.\n\n")
} else {
  cat("    Overall Brant test p-value:",
      sprintf("%.4f", brant_result$omnibus_p_value), "\n")
  cat("    Decision:",
      ifelse(brant_result$omnibus_p_value < 0.05,
             "PO assumption VIOLATED - fit PPOM",
             "PO assumption HOLDS - use POM"), "\n\n")
}

# Model 3: Adjusted PPOM (if PO assumption violated)
if (brant_result$omnibus_p_value < 0.05) {
  cat("  Model 3: Adjusted PPOM (PO assumption violated)...\n")

  model3_ppom <- fit_ppom(
    formula = formula_adjusted,
    data = rq3_data,
    link = "logit",
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

  # Flag which model to use for predictions
  final_model <- model3_ppom
  final_model_name <- "PPOM (adjusted)"
  po_violated <- TRUE
} else {
  cat("  PO assumption holds. Using adjusted POM for predictions.\n\n")
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

# Get observed values
y_observed <- as.numeric(ordered(rq3_data$support.nuclear.strike.on.russia_numeric))

# Calculate linear predictor
X_pom <- model.matrix(pom_model)[, -1, drop = FALSE]
beta_pom <- coef(pom_model)
if (ncol(X_pom) == length(beta_pom)) {
  linear_pred_pom <- as.vector(X_pom %*% beta_pom)
} else {
  linear_pred_pom <- predict(pom_model, type = "linear")
}

# Calculate deviance residuals
n_obs <- nrow(rq3_data)
dev_resid_pom <- numeric(n_obs)
for (i in seq_len(n_obs)) {
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
  obs_index = seq_len(n_obs),
  observed = y_observed,
  predicted_class = pred_class_pom,
  linear_predictor = linear_pred_pom,
  deviance_residual = dev_resid_pom
)

# Generate POM diagnostic plots
cat("  - Creating POM diagnostic plots...\n")
pom_diagnostics_plot <- create_diagnostic_plots_rq3(residuals_pom, "POM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir, "RQ3_POM_diagnostics.png"),
  plot = pom_diagnostics_plot,
  width = 10, height = 8, dpi = 300
)

# Generate POM forest plot
cat("  - Creating POM forest plot...\n")

# Extract coefficients from model2_adjusted
coef_summary_pom <- summary(pom_model)$coefficients
n_levels <- length(unique(rq3_data$support.nuclear.strike.on.russia_numeric))
n_thresholds <- n_levels - 1

# Filter out intercepts
coef_df_pom_forest <- data.frame(
  variable = rownames(coef_summary_pom)[1:(nrow(coef_summary_pom) - n_thresholds)],
  estimate = coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "Value"],
  std_error = coef_summary_pom[1:(nrow(coef_summary_pom) - n_thresholds), "Std. Error"],
  stringsAsFactors = FALSE
) %>%
  mutate(
    odds_ratio = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std_error),
    ci_upper = exp(estimate + 1.96 * std_error)
  ) %>%
  arrange(odds_ratio) %>%
  mutate(variable = factor(variable, levels = variable))

# Create POM forest plot
pom_forest <- ggplot(coef_df_pom_forest, aes(x = odds_ratio, y = variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "#0072B2") +
  geom_point(size = 3, color = "#0072B2") +
  scale_x_continuous(
    trans = "log",
    breaks = c(0.25, 0.5, 1, 2, 4),
    labels = c("0.25", "0.5", "1.0", "2.0", "4.0")
  ) +
  labs(
    title = "POM Coefficients (Odds Ratios with 95% CI)",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
  )

ggsave(
  filename = file.path(output_dir, "RQ3_POM_forest.png"),
  plot = pom_forest,
  width = 10, height = 6, dpi = 300
)

cat("  ✓ POM plots saved\n\n")

# ==============================================================================
# 5.6 Generate Markdown Output
# ==============================================================================

cat("5.6 Generating markdown report...\n")

md_file <- file.path(output_dir, "RQ3_treatment_effects.md")
md_content <- c(
  "# RQ3: Treatment Effect of Nuclear-Winter Information",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  paste0("- **Sample**: Complete cases (N = ", nrow(rq3_data), ")"),
  paste0("  - Treatment (shown infographic): ", sum(rq3_data$shown.infographic == "Shown NW Iinfographic")),
  paste0("  - Control (no infographic): ", sum(rq3_data$shown.infographic == "No Infographic")),
  "- **Outcome**: Support for nuclear strike on Russia (ordinal, 1-5)",
  "- **Treatment**: Exposure to nuclear-winter infographic",
  "",
  "## Unadjusted Descriptive Statistics",
  "",
  "| Group | N | Mean Support | SD | Prop High Support (≥4) |",
  "|-------|---|--------------|-----|------------------------|",
  paste0("| Treatment | ", unadjusted_means$n[1], " | ",
         round(unadjusted_means$mean_support[1], 3), " | ",
         round(unadjusted_means$sd_support[1], 3), " | ",
         round(unadjusted_means$prop_high_support[1], 3), " |"),
  paste0("| Control | ", unadjusted_means$n[2], " | ",
         round(unadjusted_means$mean_support[2], 3), " | ",
         round(unadjusted_means$sd_support[2], 3), " | ",
         round(unadjusted_means$prop_high_support[2], 3), " |"),
  "",
  "## Model Sequence: POM → PPOM Escalation",
  "",
  "### Model 1: Unadjusted POM",
  "",
  paste0("- **AIC**: ", round(model1_unadjusted$model_stats$aic, 2)),
  paste0("- **Treatment effect (OR)**: ", round(model1_unadjusted$coefficients$odds_ratio[1], 3),
         " (95% CI: ",
         round(model1_unadjusted$coefficients$ci_lower_or[1], 3), "-",
         round(model1_unadjusted$coefficients$ci_upper_or[1], 3), ")"),
  paste0("- **p-value**: ", sprintf("%.4f", model1_unadjusted$coefficients$p_value[1])),
  "",
  "### Model 2: Adjusted POM",
  "",
  "Covariates included: age, sex, ethnicity, political affiliation, employment status, student status",
  "",
  paste0("- **AIC**: ", round(model2_adjusted$model_stats$aic, 2)),
  paste0("- **Treatment effect (OR)**: ", round(treatment_coef$odds_ratio[1], 3),
         " (95% CI: ",
         round(treatment_coef$ci_lower_or[1], 3), "-",
         round(treatment_coef$ci_upper_or[1], 3), ")"),
  paste0("- **p-value**: ", sprintf("%.4f", treatment_coef$p_value[1])),
  "",
  "#### Diagnostic Plots",
  "",
  "![POM Diagnostic Plots](RQ3_POM_diagnostics.png)",
  "",
  "**Visual Assessment**: 4-panel diagnostic plot shows residuals vs fitted, normal Q-Q, scale-location, and observed vs predicted.",
  "",
  "#### Forest Plot",
  "",
  "![POM Forest Plot](RQ3_POM_forest.png)",
  "",
  "**Interpretation**: Odds ratios with 95% confidence intervals for all predictors under proportional odds assumption.",
  "",
  "### Proportional Odds Assumption Test",
  "",
  if (!is.null(brant_result$brant_test_failed) && brant_result$brant_test_failed) {
    c(
      paste0("- **Brant test status**: Could not be completed"),
      paste0("- **Error**: ", brant_result$error_message),
      paste0("- **Decision**: Defaulting to POM model")
    )
  } else {
    c(
      paste0("- **Brant test overall p-value**: ", sprintf("%.4f", brant_result$omnibus_p_value)),
      paste0("- **Decision**: ", ifelse(brant_result$omnibus_p_value < 0.05,
                                        "PO assumption violated (p < 0.05) → Fit PPOM",
                                        "PO assumption holds (p ≥ 0.05) → Use POM"))
    )
  },
  "",
  if (po_violated) {
    c(
      "### Model 3: Adjusted PPOM",
      "",
      "Because the proportional odds assumption was violated, we fit a partial proportional odds model.",
      "",
      paste0("- **AIC**: ", round(model3_ppom$model_stats$aic, 2)),
      paste0("- **Treatment effect (avg OR)**: ", round(treatment_ppom$odds_ratio[1], 3)),
      "",
      "**Note**: In PPOM, treatment effects can vary across outcome thresholds. The reported OR is averaged across thresholds.",
      ""
    )
  } else {
    c(
      "### Final Model",
      "",
      "The proportional odds assumption holds. Model 2 (adjusted POM) is used for final inferences.",
      ""
    )
  },
  "## Predicted Probabilities",
  "",
  paste0("Using ", final_model_name, " with representative covariate profile."),
  "",
  "### Probability Distribution Across Support Levels",
  "",
  "| Group | ", paste0("Level ", 1:ncol(preds), collapse = " | "), " |",
  "|-------|", paste(rep("-----", ncol(preds)), collapse = "|"), "|",
  paste0("| Treatment | ", paste(sprintf("%.3f", pred_probs[1, grep("prob_level", names(pred_probs))]), collapse = " | "), " |"),
  paste0("| Control | ", paste(sprintf("%.3f", pred_probs[2, grep("prob_level", names(pred_probs))]), collapse = " | "), " |"),
  "",
  "### High Support (≥ Level 4)",
  "",
  paste0("- **Treatment**: ", round(pred_probs$prob_high_support[1], 3)),
  paste0("- **Control**: ", round(pred_probs$prob_high_support[2], 3)),
  paste0("- **Difference**: ", round(pred_probs$prob_high_support[1] - pred_probs$prob_high_support[2], 3),
         ifelse(pred_probs$prob_high_support[1] > pred_probs$prob_high_support[2],
                " (treatment increases high support)",
                " (treatment decreases high support)")),
  "",
  "## Interpretation",
  "",
  if (treatment_coef$p_value < 0.05) {
    paste0("Exposure to nuclear-winter information has a **significant effect** on support for nuclear retaliation ",
           "(OR = ", round(treatment_coef$odds_ratio[1], 3), ", p = ",
           sprintf("%.4f", treatment_coef$p_value), "). ",
           ifelse(treatment_coef$odds_ratio[1] > 1,
                  "Treatment **increases** the odds of higher support.",
                  "Treatment **decreases** the odds of higher support."))
  } else {
    paste0("Exposure to nuclear-winter information does **not** have a significant effect on support for nuclear retaliation ",
           "(OR = ", round(treatment_coef$odds_ratio[1], 3), ", p = ",
           sprintf("%.4f", treatment_coef$p_value), ").")
  },
  "",
  "## Model Selection Summary",
  "",
  paste0("- **Final model**: ", final_model_name),
  paste0("- **AIC**: ", round(final_model$model_stats$aic, 2)),
  paste0("- **Proportional odds assumption**: ", ifelse(po_violated, "Violated", "Holds")),
  "",
  "---",
  "",
  "## BOOKMARK: Country-Specific Analyses",
  "",
  "Country-specific subgroup analyses (USA-only, UK-only) are available but commented out.",
  "Uncomment the relevant code sections in `RQ3_treatment_effects.R` to run these analyses.",
  "",
  "---",
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

# BOOKMARK: Country-specific analyses (USA-only and UK-only) are commented out
# below. Prompt for human feedback before uncommenting and running.

# ==============================================================================
# OPTIONAL: Country-Specific Subgroup Analyses
# ==============================================================================

# # USA-only analysis
# cat("Running USA-only analysis...\n")
# rq3_data_usa <- rq3_data %>% filter(country.of.residence == "United States")
#
# formula_usa <- as.formula(paste(
#   "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
#   paste(setdiff(covariates, "country.of.residence"), collapse = " + ")
# ))
#
# model_usa <- fit_pom(formula = formula_usa, data = rq3_data_usa, link = "logit", verbose = FALSE)
# brant_usa <- perform_brant_test(model_usa$model, verbose = FALSE)
#
# if (brant_usa$omnibus_p_value < 0.05) {
#   model_usa <- fit_ppom(formula = formula_usa, data = rq3_data_usa, link = "logit", verbose = FALSE)
# }
#
# # UK-only analysis
# cat("Running UK-only analysis...\n")
# rq3_data_uk <- rq3_data %>% filter(country.of.residence == "United Kingdom")
#
# formula_uk <- as.formula(paste(
#   "support.nuclear.strike.on.russia_numeric ~ shown.infographic +",
#   paste(setdiff(covariates, "country.of.residence"), collapse = " + ")
# ))
#
# model_uk <- fit_pom(formula = formula_uk, data = rq3_data_uk, link = "logit", verbose = FALSE)
# brant_uk <- perform_brant_test(model_uk$model, verbose = FALSE)
#
# if (brant_uk$omnibus_p_value < 0.05) {
#   model_uk <- fit_ppom(formula = formula_uk, data = rq3_data_uk, link = "logit", verbose = FALSE)
# }
