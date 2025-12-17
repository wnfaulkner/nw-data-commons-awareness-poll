# ==============================================================================
# RQ2: AWARENESS AS ASSOCIATIONAL PREDICTOR
# ==============================================================================
#
# This script examines whether awareness of nuclear winter (measured in the
# treatment group) predicts support for nuclear retaliation.
#
# STRUCTURE:
#   PART 1 (Lines ~1-350): ANALYSIS CODE
#     - Data assembly
#     - POM fitting and diagnostics
#     - Visual inspection findings
#     - PPOM fitting and diagnostics
#     - Model comparison
#
#   PART 2 (Lines ~350-700): REPORTING CODE
#     - Generate diagnostic plots
#     - Create PDF report
#     - Generate markdown narrative
#
# DECISIONS IMPLEMENTED:
#   - DEC-006: Use Model 1 (separate awareness items) only
#   - DEC-007: Visual inspection workflow; Brant test bypassed
#   - DEC-008: Single linear narrative output file
#   - DEC-009: Direct VGAM/MASS implementation; minimal abstraction
#
# PREREQUISITES:
#   - data.tb must exist (run publication_analysis.R or run_analyses.R first)
#   - rq1_awareness_mean must exist (RQ1 must run first)
#
# OUTPUTS:
#   - outputs/RQ2_awareness_support.md (single comprehensive markdown file)
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

if (!exists("rq1_awareness_mean")) {
  stop("rq1_awareness_mean not found. Please run RQ1 first.")
}

# Load required packages
suppressMessages({
  library(MASS)      # For POM (polr)
  library(VGAM)      # For PPOM (vglm)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(gridExtra)
  library(grid)
})

# Create output directory
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("RQ2: AWARENESS AS ASSOCIATIONAL PREDICTOR\n")
cat("=", rep("=", 78), "\n", sep = "")
cat("\n")
cat("Output directory:", output_dir, "\n\n")

# ==============================================================================
# PART 1: ANALYSIS CODE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Data Assembly
# ------------------------------------------------------------------------------

cat("1.1 Assembling data (treatment group only)...\n")

rq2_data <- data.tb %>%
  dplyr::left_join(rq1_awareness_mean, by = "participant.id") %>%
  dplyr::filter(
    shown.infographic == "Shown NW Iinfographic",
    !is.na(support.nuclear.strike.on.russia_numeric),
    !is.na(awareness_mean)
  ) %>%
  dplyr::select(
    participant.id,
    support.nuclear.strike.on.russia_numeric,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric,
    awareness_mean,
    age, sex,
    ethnicity.collapsed, political.affiliation.collapsed,
    employment.status, student.status
  ) %>%
  tidyr::drop_na()

n_obs <- nrow(rq2_data)
cat("  Sample size (complete cases):", n_obs, "\n")
cat("  Outcome: Support for nuclear strike (1-5 ordinal scale)\n")
cat("  Treatment group only (shown infographic)\n\n")

# Awareness distribution summary
awareness_summary <- rq2_data %>%
  dplyr::summarise(
    mean = mean(awareness_mean, na.rm = TRUE),
    sd = sd(awareness_mean, na.rm = TRUE),
    min = min(awareness_mean, na.rm = TRUE),
    max = max(awareness_mean, na.rm = TRUE)
  )

cat("  Awareness mean distribution:\n")
cat("    Mean:", round(awareness_summary$mean, 3), "\n")
cat("    SD:", round(awareness_summary$sd, 3), "\n\n")

# ------------------------------------------------------------------------------
# 1.2 Fit Proportional Odds Model (POM) - Direct MASS::polr implementation
# ------------------------------------------------------------------------------

cat("1.2 Fitting POM (MASS::polr - direct implementation)...\n")

# Ensure outcome is ordered factor
rq2_data$outcome_ordered <- ordered(
  rq2_data$support.nuclear.strike.on.russia_numeric
)

# Build formula
formula_pom <- outcome_ordered ~
  nw.awareness.1980s_numeric +
  nw.awareness.recent.academic_numeric +
  nw.awareness.recent.media_numeric +
  age + sex +
  ethnicity.collapsed +
  political.affiliation.collapsed +
  employment.status +
  student.status

# Fit POM using MASS::polr
model_pom <- MASS::polr(
  formula = formula_pom,
  data = rq2_data,
  Hess = TRUE,
  method = "logistic"  # logit link
)

# Extract model statistics
aic_pom <- AIC(model_pom)
bic_pom <- BIC(model_pom)
loglik_pom <- logLik(model_pom)[1]

cat("  POM fitted successfully\n")
cat("    AIC:", round(aic_pom, 2), "\n")
cat("    BIC:", round(bic_pom, 2), "\n")
cat("    Log-likelihood:", round(loglik_pom, 2), "\n")
cat("    N =", nobs(model_pom), "\n\n")

# ------------------------------------------------------------------------------
# 1.3 Calculate POM Residuals and Diagnostics
# ------------------------------------------------------------------------------

cat("1.3 Calculating POM residuals...\n")

# Get predicted probabilities and classes
pred_probs_pom <- predict(model_pom, type = "probs")
if (is.vector(pred_probs_pom)) {
  pred_probs_pom <- matrix(pred_probs_pom, nrow = 1)
}
pred_class_pom <- apply(pred_probs_pom, 1, which.max)

# Get observed classes
y_observed <- as.numeric(rq2_data$outcome_ordered)

# Calculate linear predictor for POM
X_pom <- model.matrix(model_pom)[, -1, drop = FALSE]
beta_pom <- coef(model_pom)
if (ncol(X_pom) == length(beta_pom)) {
  linear_pred_pom <- as.vector(X_pom %*% beta_pom)
} else {
  linear_pred_pom <- predict(model_pom, type = "linear")
}

# Calculate deviance residuals
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

# Calculate Pearson residuals
pearson_resid_pom <- numeric(n_obs)
for (i in seq_len(n_obs)) {
  p_obs <- pred_probs_pom[i, y_observed[i]]
  pearson_resid_pom[i] <- (1 - p_obs) / sqrt(p_obs * (1 - p_obs))
}

cat("  Deviance residuals range: [",
    sprintf("%.2f", min(dev_resid_pom)), ", ",
    sprintf("%.2f", max(dev_resid_pom)), "]\n", sep = "")
cat("  Pearson residuals range: [",
    sprintf("%.2f", min(pearson_resid_pom)), ", ",
    sprintf("%.2f", max(pearson_resid_pom)), "]\n\n", sep = "")

# Store POM residuals for plotting
residuals_pom <- tibble(
  obs_index = seq_len(n_obs),
  observed = y_observed,
  predicted_class = pred_class_pom,
  linear_predictor = linear_pred_pom,
  deviance_residual = dev_resid_pom,
  pearson_residual = pearson_resid_pom
)

# ------------------------------------------------------------------------------
# 1.4 Visual Inspection Findings (per DEC-007)
# ------------------------------------------------------------------------------

cat("1.4 Visual Inspection Results (per DEC-007)...\n\n")

cat("  HUMAN ANALYST VISUAL INSPECTION FINDINGS:\n\n")

cat("  1. Residuals vs Fitted Plot: CLEAR VIOLATION\n")
cat("     - Strong nonlinear pattern (rising sharply, bending downward)\n")
cat("     - Violates latent-linearity assumption\n")
cat("     - Indicates threshold-specific effects\n\n")

cat("  2. Normal Q-Q Plot: CLEAR VIOLATION\n")
cat("     - Severe 'broken stick' pattern with heavy tails\n")
cat("     - Flat midsections indicate non-normality\n")
cat("     - Latent distribution mis-specified\n\n")

cat("  3. Scale-Location Plot: CLEAR VIOLATION\n")
cat("     - Upward trend in smoothed line\n")
cat("     - Heteroskedasticity (variance increases with predictor)\n")
cat("     - Violates constant variance assumption\n\n")

cat("  4. Observed vs Predicted Plot: CLEAR VIOLATION\n")
cat("     - Predicted classes cluster too narrowly\n")
cat("     - Underprediction of high-support categories\n")
cat("     - Points stray from diagonal\n")
cat("     - Poor threshold fit\n\n")

cat("  VISUAL INSPECTION DECISION: 4/4 plots show CLEAR violations\n")
cat("  → ESCALATE TO PPOM (per Section 2.1.3a protocol)\n\n")

cat("  Note: Brant test bypassed per DEC-007\n")
cat("        (visual inspection definitive)\n\n")

# ------------------------------------------------------------------------------
# 1.5 Fit PPOM - Direct VGAM::vglm implementation
# ------------------------------------------------------------------------------

cat("1.5 Fitting PPOM (VGAM::vglm - direct implementation)...\n")

# PPOM specification: Only awareness variables have flexible coefficients
# Rationale: Targeted flexibility for primary predictors; covariates
# constrained for parsimony and computational stability

parallel_spec <- FALSE ~ nw.awareness.1980s_numeric +
                          nw.awareness.recent.academic_numeric +
                          nw.awareness.recent.media_numeric

cat("  Specification: Awareness variables flexible, covariates constrained\n")

# Fit PPOM using VGAM::vglm
model_ppom <- VGAM::vglm(
  formula = formula_pom,
  family = VGAM::cumulative(
    link = "logitlink",
    parallel = parallel_spec
  ),
  data = rq2_data
)

# Extract model statistics
aic_ppom <- AIC(model_ppom)
bic_ppom <- BIC(model_ppom)
loglik_ppom <- logLik(model_ppom)[1]

cat("  PPOM fitted successfully\n")
cat("    AIC:", round(aic_ppom, 2), "\n")
cat("    BIC:", round(bic_ppom, 2), "\n")
cat("    Log-likelihood:", round(loglik_ppom, 2), "\n")
cat("    N =", nobs(model_ppom), "\n\n")

# ------------------------------------------------------------------------------
# 1.6 Calculate PPOM Residuals
# ------------------------------------------------------------------------------

cat("1.6 Calculating PPOM residuals...\n")

# Get predicted probabilities
pred_probs_ppom <- predict(model_ppom, type = "response")
if (is.vector(pred_probs_ppom)) {
  pred_probs_ppom <- matrix(pred_probs_ppom, nrow = 1)
}
pred_class_ppom <- apply(pred_probs_ppom, 1, which.max)

# Calculate linear predictor for PPOM (average across thresholds)
if ("predictors" %in% methods::slotNames(model_ppom)) {
  predictors_matrix <- model_ppom@predictors
  if (is.matrix(predictors_matrix) && nrow(predictors_matrix) == n_obs) {
    linear_pred_ppom <- rowMeans(predictors_matrix)
  } else {
    linear_pred_ppom <- rep(0, n_obs)  # Fallback
  }
} else {
  linear_pred_ppom <- rep(0, n_obs)  # Fallback
}

# Extract observed values from VGAM model
if ("y" %in% methods::slotNames(model_ppom)) {
  y_matrix <- model_ppom@y
  if (is.matrix(y_matrix)) {
    y_observed_ppom <- apply(y_matrix, 1, which.max)
  } else {
    y_observed_ppom <- as.numeric(y_matrix)
  }
} else {
  y_observed_ppom <- y_observed  # Use from POM
}

# Calculate deviance residuals
dev_resid_ppom <- numeric(n_obs)
for (i in seq_len(n_obs)) {
  p_obs <- pred_probs_ppom[i, y_observed_ppom[i]]
  dev_component <- sqrt(-2 * log(p_obs))
  sign_val <- ifelse(
    y_observed_ppom[i] > pred_class_ppom[i], 1,
    ifelse(y_observed_ppom[i] < pred_class_ppom[i], -1, 0)
  )
  dev_resid_ppom[i] <- sign_val * dev_component
}

# Calculate Pearson residuals
pearson_resid_ppom <- numeric(n_obs)
for (i in seq_len(n_obs)) {
  p_obs <- pred_probs_ppom[i, y_observed_ppom[i]]
  pearson_resid_ppom[i] <- (1 - p_obs) / sqrt(p_obs * (1 - p_obs))
}

cat("  Deviance residuals range: [",
    sprintf("%.2f", min(dev_resid_ppom)), ", ",
    sprintf("%.2f", max(dev_resid_ppom)), "]\n", sep = "")
cat("  Pearson residuals range: [",
    sprintf("%.2f", min(pearson_resid_ppom)), ", ",
    sprintf("%.2f", max(pearson_resid_ppom)), "]\n\n", sep = "")

# Store PPOM residuals for plotting
residuals_ppom <- tibble(
  obs_index = seq_len(n_obs),
  observed = y_observed_ppom,
  predicted_class = pred_class_ppom,
  linear_predictor = linear_pred_ppom,
  deviance_residual = dev_resid_ppom,
  pearson_residual = pearson_resid_ppom
)

# ------------------------------------------------------------------------------
# 1.7 Model Comparison (POM vs PPOM)
# ------------------------------------------------------------------------------

cat("1.7 Comparing POM vs PPOM predictions...\n")

# Create representative prediction data
representative_profile <- rq2_data %>%
  dplyr::summarise(
    age = median(age, na.rm = TRUE),
    sex = names(sort(table(sex), decreasing = TRUE))[1],
    ethnicity.collapsed = names(sort(table(ethnicity.collapsed),
                                     decreasing = TRUE))[1],
    political.affiliation.collapsed = names(
      sort(table(political.affiliation.collapsed), decreasing = TRUE)
    )[1],
    employment.status = names(sort(table(employment.status),
                                   decreasing = TRUE))[1],
    student.status = names(sort(table(student.status), decreasing = TRUE))[1]
  )

awareness_range <- seq(
  min(rq2_data$awareness_mean, na.rm = TRUE),
  max(rq2_data$awareness_mean, na.rm = TRUE),
  length.out = 20
)

pred_data <- data.frame(
  outcome_ordered = ordered(rep(1, length(awareness_range))),
  nw.awareness.1980s_numeric = awareness_range,
  nw.awareness.recent.academic_numeric = awareness_range,
  nw.awareness.recent.media_numeric = awareness_range
) %>%
  dplyr::bind_cols(representative_profile[rep(1, nrow(.)), ])

# Get predictions from both models
preds_pom_compare <- predict(model_pom, newdata = pred_data, type = "probs")
preds_ppom_compare <- predict(model_ppom, newdata = pred_data,
                               type = "response")

# Calculate maximum absolute difference
delta_p_max <- max(abs(preds_pom_compare - preds_ppom_compare))

cat("  Δp_max (POM vs PPOM):", sprintf("%.4f", delta_p_max), "\n")
cat("  Threshold for meaningful difference: 0.03\n")

if (delta_p_max > 0.03) {
  cat("  → Δp_max > 0.03: PPOM provides meaningfully different predictions\n")
  cat("  → FINAL MODEL SELECTION: PPOM\n\n")
  final_model_type <- "PPOM"
} else {
  cat("  → Δp_max ≤ 0.03: Similar predictions\n")
  cat("  → However, visual violations clear\n")
  cat("  → FINAL MODEL SELECTION: PPOM (conservative choice)\n\n")
  final_model_type <- "PPOM"
}

cat("ANALYSIS COMPLETE - RESULTS READY FOR REPORTING\n\n")

# ==============================================================================
# PART 2: REPORTING CODE
# ==============================================================================

cat("=" , rep("=", 78), "\n", sep = "")
cat("PART 2: GENERATING REPORTS\n")
cat("=", rep("=", 78), "\n\n", sep = "")

# ------------------------------------------------------------------------------
# 2.1 Generate Diagnostic Plots (PNG files)
# ------------------------------------------------------------------------------

cat("2.1 Generating diagnostic plots...\n")

# Create plotting function for 4-panel diagnostics
create_diagnostic_plots <- function(residuals_tb, model_label) {
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

# Generate POM diagnostic plots
cat("  - POM diagnostics...\n")
pom_diagnostics_plot <- create_diagnostic_plots(residuals_pom, "POM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir, "RQ2_POM_diagnostics.png"),
  plot = pom_diagnostics_plot,
  width = 10, height = 8, dpi = 300
)

# Generate PPOM diagnostic plots
cat("  - PPOM diagnostics...\n")
ppom_diagnostics_plot <- create_diagnostic_plots(residuals_ppom, "PPOM Diagnostic Plots")
ggsave(
  filename = file.path(output_dir, "RQ2_PPOM_diagnostics.png"),
  plot = ppom_diagnostics_plot,
  width = 10, height = 8, dpi = 300
)

cat("  ✓ Diagnostic plots saved\n\n")

# ------------------------------------------------------------------------------
# 2.2 Generate Forest Plots (PNG files)
# ------------------------------------------------------------------------------

cat("2.2 Generating forest plots...\n")

# Extract POM coefficients and prepare for forest plot using helper function
coef_summary_pom_full <- summary(model_pom)$coefficients
n_levels <- length(levels(rq2_data$outcome_ordered))
n_thresholds <- n_levels - 1

# Filter out intercepts and prepare data structure for plot_pom_coefficients()
pom_result_for_plot <- list(
  coefficients = tibble(
    variable = rownames(coef_summary_pom_full)[1:(nrow(coef_summary_pom_full) - n_thresholds)],
    log_odds = coef_summary_pom_full[1:(nrow(coef_summary_pom_full) - n_thresholds), "Value"],
    std_error = coef_summary_pom_full[1:(nrow(coef_summary_pom_full) - n_thresholds), "Std. Error"],
    t_value = coef_summary_pom_full[1:(nrow(coef_summary_pom_full) - n_thresholds), "t value"],
    p_value = 2 * pnorm(abs(coef_summary_pom_full[1:(nrow(coef_summary_pom_full) - n_thresholds), "t value"]), lower.tail = FALSE),
    odds_ratio = exp(log_odds)
  ),
  model_stats = list(confidence_level = 0.95)
)

# Create POM forest plot using helper function with normal curves
pom_forest <- plot_pom_coefficients(pom_result_for_plot, plot_title = "POM Coefficients")

ggsave(
  filename = file.path(output_dir, "RQ2_POM_forest.png"),
  plot = pom_forest,
  width = 10, height = 6, dpi = 300
)

cat("  ✓ POM forest plot saved\n")

# Extract PPOM coefficients for visualization
# PPOM has threshold-specific coefficients for awareness variables
coef_summary_ppom_matrix <- summary(model_ppom)@coef3

# Create simplified PPOM coefficient plot showing threshold variation
# Focus on the three awareness variables
awareness_vars <- c("nw.awareness.1980s_numeric",
                   "nw.awareness.recent.academic_numeric",
                   "nw.awareness.recent.media_numeric")

# Extract coefficients for awareness variables across thresholds
ppom_coef_data <- data.frame()
for (var in awareness_vars) {
  # Find rows matching this variable
  matching_rows <- grep(paste0("^", var, ":"), rownames(coef_summary_ppom_matrix), value = TRUE)
  if (length(matching_rows) > 0) {
    for (row_name in matching_rows) {
      threshold_num <- as.numeric(gsub(".*:(\\d+)$", "\\1", row_name))
      estimate <- coef_summary_ppom_matrix[row_name, "Estimate"]
      std_error <- coef_summary_ppom_matrix[row_name, "Std. Error"]

      ppom_coef_data <- rbind(ppom_coef_data, data.frame(
        variable = var,
        threshold = threshold_num,
        estimate = estimate,
        std_error = std_error,
        odds_ratio = exp(estimate),
        ci_lower = exp(estimate - 1.96 * std_error),
        ci_upper = exp(estimate + 1.96 * std_error)
      ))
    }
  }
}

# Create PPOM threshold-specific coefficient plot using helper function
if (nrow(ppom_coef_data) > 0) {
  ppom_coef_plot <- plot_ppom_threshold_coefficients(
    coef_data = ppom_coef_data,
    plot_title = "PPOM Threshold-Specific Coefficients (Awareness Variables)"
  )

  ggsave(
    filename = file.path(output_dir, "RQ2_PPOM_coefficients.png"),
    plot = ppom_coef_plot,
    width = 10, height = 6, dpi = 300
  )

  cat("  ✓ PPOM coefficient plot saved\n\n")
} else {
  cat("  ⚠ No threshold-specific coefficients found for PPOM plot\n\n")
}

# ------------------------------------------------------------------------------
# 2.3 Generate Comprehensive Markdown Report (Side-by-Side Comparison Format)
# ------------------------------------------------------------------------------

cat("2.3 Generating comprehensive markdown report...\n")

# Extract POM coefficients
coef_summary_pom <- summary(model_pom)$coefficients
n_levels <- length(levels(rq2_data$outcome_ordered))
n_thresholds <- n_levels - 1

# Extract PPOM coefficients
coef_summary_ppom <- summary(model_ppom)@coef3

# Build coefficient comparison table (Section 4)
# Get all unique variable names (excluding intercepts)
pom_vars <- rownames(coef_summary_pom)[1:(nrow(coef_summary_pom) - n_thresholds)]

# Get PPOM variable names (excluding intercepts)
ppom_row_names <- rownames(coef_summary_ppom)
ppom_vars <- ppom_row_names[!grepl("^\\(Intercept\\)", ppom_row_names)]

# Collect all unique variable names (base variables + threshold-specific)
all_vars <- unique(c(pom_vars, ppom_vars))

# Build coefficient table data
coef_table_rows <- c()
for (var in all_vars) {
  # POM coefficient
  pom_coef <- if (var %in% pom_vars) {
    sprintf("%.4f", coef_summary_pom[var, "Value"])
  } else {
    "—"
  }

  # PPOM coefficient
  ppom_coef <- if (var %in% ppom_vars) {
    sprintf("%.4f", coef_summary_ppom[var, "Estimate"])
  } else {
    "—"
  }

  coef_table_rows <- c(coef_table_rows,
    paste0("| ", var, " | ", pom_coef, " | ", ppom_coef, " |")
  )
}

md_file <- file.path(output_dir, "RQ2_awareness_support.md")
md_content <- c(
  "# RQ2: Using Awareness as Associational Predictor",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  "- **Sample**: Treatment group only (shown infographic)",
  "- **Outcome**: Support for nuclear strike on Russia (ordinal, 1-5)",
  "- **Predictors**: Three awareness items (1980s, recent academic, media) + covariates",
  paste0("- **Final model**: ", final_model_type),
  "",
  "---",
  "",
  "## Section 1: Sample Sizes",
  "",
  "| Model | Complete Cases | Treatment | Control |",
  "|-------|----------------|-----------|---------|",
  paste0("| Model 1 (POM) | ", n_obs, " | ", n_obs, " | — |"),
  paste0("| Model 2 (PPOM) | ", n_obs, " | ", n_obs, " | — |"),
  "",
  "*Note: RQ2 analyzes treatment group only (respondents shown infographic)*",
  "",
  "---",
  "",
  "## Section 2: Model Formulas",
  "",
  "### Model 1: POM (Proportional Odds Model)",
  "",
  "**Formula:**",
  "```",
  "support.nuclear.strike.on.russia ~ nw.awareness.1980s +",
  "                                     nw.awareness.recent.academic +",
  "                                     nw.awareness.recent.media +",
  "                                     age + sex + ethnicity + politics +",
  "                                     employment + student.status",
  "```",
  "",
  "**Specification:**",
  "- Proportional odds assumption: All coefficients constant across thresholds",
  "- Link: Logit",
  "- Implementation: MASS::polr",
  "",
  "### Model 2: PPOM (Partial Proportional Odds Model)",
  "",
  "**Formula:**",
  "```",
  "support.nuclear.strike.on.russia ~ nw.awareness.1980s +",
  "                                     nw.awareness.recent.academic +",
  "                                     nw.awareness.recent.media +",
  "                                     age + sex + ethnicity + politics +",
  "                                     employment + student.status",
  "```",
  "",
  "**Specification:**",
  "- **Flexible predictors**: Awareness variables (1980s, academic, media)",
  "  - Coefficients vary across support thresholds",
  "- **Constrained predictors**: Covariates (age, sex, ethnicity, politics, employment, student status)",
  "  - Proportional odds constraint maintained",
  "- **Rationale**: Targeted flexibility for primary predictors; computational stability",
  "- **Link**: Logit",
  "- **Implementation**: VGAM::vglm",
  "",
  "---",
  "",
  "## Section 3: Model Fit Statistics",
  "",
  "| Model | AIC | BIC | Log-Likelihood | N | Δ AIC (vs POM) |",
  "|-------|-----|-----|----------------|---|----------------|",
  paste0("| Model 1 (POM) | ", round(aic_pom, 2), " | ", round(bic_pom, 2),
         " | ", round(loglik_pom, 2), " | ", n_obs, " | — |"),
  paste0("| Model 2 (PPOM) | ", round(aic_ppom, 2), " | ", round(bic_ppom, 2),
         " | ", round(loglik_ppom, 2), " | ", n_obs, " | ",
         round(aic_ppom - aic_pom, 2), " |"),
  "",
  "---",
  "",
  "## Section 4: Model Coefficients",
  "",
  "*Table shows coefficient estimates only (no standard errors or p-values).*",
  "",
  "| Variable | Model 1 (POM) | Model 2 (PPOM) |",
  "|----------|---------------|----------------|",
  coef_table_rows,
  "",
  "---",
  "",
  "## Section 5: Diagnostic Plots",
  "",
  "### Model 1 (POM) Diagnostics",
  "",
  "![POM Diagnostic Plots](RQ2_POM_diagnostics.png)",
  "",
  "### Model 2 (PPOM) Diagnostics",
  "",
  "![PPOM Diagnostic Plots](RQ2_PPOM_diagnostics.png)",
  "",
  "---",
  "",
  "## Section 6: Forest Plots and Coefficient Plots",
  "",
  "### Model 1 (POM) Forest Plot",
  "",
  "![POM Forest Plot](RQ2_POM_forest.png)",
  "",
  "### Model 2 (PPOM) Threshold-Specific Coefficients",
  "",
  "![PPOM Coefficient Plot](RQ2_PPOM_coefficients.png)",
  "",
  "---",
  "",
  paste0("*Generated: ", Sys.time(), "*")
)

writeLines(md_content, md_file)

cat("  ✓ RQ2_awareness_support.md\n\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("=", rep("=", 78), "\n", sep = "")
cat("RQ2 ANALYSIS COMPLETE\n")
cat("=", rep("=", 78), "\n\n", sep = "")

cat("Results summary:\n")
cat("  - Sample size:", n_obs, "\n")
cat("  - Model: Model 1 (separate awareness items)\n")
cat("  - POM visual inspection: 4/4 plots CLEAR violations\n")
cat("  - PPOM: Awareness variables flexible, covariates constrained\n")
cat("  - Δp_max (POM vs PPOM):", sprintf("%.4f", delta_p_max), "\n")
cat("  - Final model:", final_model_type, "\n\n")

cat("Output file:\n")
cat("  -", md_file, "\n\n")

cat("Downstream flag:\n")
cat("  - rq2_awareness_mean_ok_overall = FALSE (use separate items)\n\n")

cat("=", rep("=", 78), "\n\n", sep = "")

# Store results for downstream use
rq2_final_model <- model_ppom
rq2_final_model_type <- final_model_type
rq2_awareness_mean_ok_overall <- FALSE  # Per DEC-006
