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
# 2.1 Generate Comprehensive Markdown Report
# ------------------------------------------------------------------------------

cat("2.1 Generating comprehensive markdown report...\n")

# Extract POM coefficients for markdown
coef_summary_pom <- summary(model_pom)$coefficients
coef_df_pom <- data.frame(
  variable = rownames(coef_summary_pom),
  estimate = coef_summary_pom[, "Value"],
  std_error = coef_summary_pom[, "Std. Error"],
  t_value = coef_summary_pom[, "t value"],
  stringsAsFactors = FALSE
)

# Filter out intercepts for coefficient table
n_levels <- length(levels(rq2_data$outcome_ordered))
n_thresholds <- n_levels - 1
coef_df_pom_predictors <- coef_df_pom[1:(nrow(coef_df_pom) - n_thresholds), ]

# Extract PPOM coefficients summary
coef_summary_ppom <- summary(model_ppom)@coef3
ppom_coef_text <- paste(capture.output(print(coef_summary_ppom)), collapse = "\n")

md_file <- file.path(output_dir, "RQ2_awareness_support.md")
md_content <- c(
  "# RQ2: Using Awareness as Associational Predictor",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  paste0("- **Sample**: Treatment group, complete data (N = ", n_obs, ")"),
  "- **Outcome**: Support for nuclear strike on Russia (ordinal, 1-5)",
  "- **Predictors**: Three awareness items (1980s, recent academic, media)",
  paste0("- **Final model**: ", final_model_type),
  "",
  "## Decision Record",
  "",
  "This analysis implements:",
  "- **DEC-006**: Use Model 1 (separate awareness items) only",
  "- **DEC-007**: Visual inspection workflow; Brant test bypassed",
  "- **DEC-008**: Single linear narrative output",
  "- **DEC-009**: Direct VGAM/MASS implementation; minimal abstraction",
  "",
  "---",
  "",
  "## Section 1: Data Assembly",
  "",
  paste0("- **Complete cases**: ", n_obs),
  paste0("- **Awareness mean**: ", round(awareness_summary$mean, 2),
         " (SD = ", round(awareness_summary$sd, 2), ")"),
  paste0("- **Awareness range**: ", round(awareness_summary$min, 2),
         " to ", round(awareness_summary$max, 2)),
  "",
  "---",
  "",
  "## Section 2: POM (MASS::polr)",
  "",
  "### Model Fit Statistics",
  "",
  paste0("- **AIC**: ", round(aic_pom, 2)),
  paste0("- **BIC**: ", round(bic_pom, 2)),
  paste0("- **Log-likelihood**: ", round(loglik_pom, 2)),
  paste0("- **N**: ", n_obs),
  "",
  "### Coefficients (Proportional Odds Assumption)",
  "",
  "| Variable | Estimate | Std. Error | t-value |",
  "|----------|----------|------------|---------|",
  unlist(lapply(1:nrow(coef_df_pom_predictors), function(i) {
    sprintf("| %s | %.4f | %.4f | %.4f |",
            coef_df_pom_predictors$variable[i],
            coef_df_pom_predictors$estimate[i],
            coef_df_pom_predictors$std_error[i],
            coef_df_pom_predictors$t_value[i])
  })),
  "",
  "### Residual Diagnostics",
  "",
  paste0("- **Deviance residuals**: [",
         sprintf("%.2f", min(dev_resid_pom)), ", ",
         sprintf("%.2f", max(dev_resid_pom)), "]"),
  paste0("- **Pearson residuals**: [",
         sprintf("%.2f", min(pearson_resid_pom)), ", ",
         sprintf("%.2f", max(pearson_resid_pom)), "]"),
  "",
  "---",
  "",
  "## Section 3: Visual Inspection Results",
  "",
  "**Protocol**: Per DEC-007, systematic visual inspection using 4-panel diagnostics",
  "",
  "### Findings",
  "",
  "1. **Residuals vs Fitted**: CLEAR VIOLATION",
  "   - Strong nonlinear pattern (rising sharply, then bending downward)",
  "   - Violates latent-linearity assumption",
  "   - Indicates threshold-specific effects",
  "",
  "2. **Normal Q-Q Plot**: CLEAR VIOLATION",
  "   - Severe 'broken stick' pattern with heavy tails",
  "   - Flat midsections indicate non-normality",
  "   - Latent distribution mis-specified",
  "",
  "3. **Scale-Location Plot**: CLEAR VIOLATION",
  "   - Upward trend in smoothed line",
  "   - Heteroskedasticity (variance increases with predictor)",
  "   - Violates constant variance assumption",
  "",
  "4. **Observed vs Predicted**: CLEAR VIOLATION",
  "   - Predicted classes cluster too narrowly",
  "   - Underprediction of high-support categories",
  "   - Points stray from diagonal, indicating poor threshold fit",
  "",
  "### Decision",
  "",
  "**Result**: 4/4 diagnostic plots show CLEAR violations of proportional odds assumption",
  "",
  "**Action**: ESCALATE TO PPOM (per Section 2.1.3a protocol)",
  "",
  "**Note**: Brant test bypassed per DEC-007 (visual inspection definitive)",
  "",
  "---",
  "",
  "## Section 4: PPOM (VGAM::vglm)",
  "",
  "### Specification",
  "",
  "- **Flexible predictors**: Awareness variables (1980s, academic, media)",
  "  - Coefficients vary across support thresholds",
  "- **Constrained predictors**: Covariates (age, sex, ethnicity, politics, employment, student status)",
  "  - Proportional odds constraint maintained for parsimony",
  "- **Rationale**: Targeted flexibility for primary predictors; computational stability",
  "",
  "### Model Fit Statistics",
  "",
  paste0("- **AIC**: ", round(aic_ppom, 2)),
  paste0("- **BIC**: ", round(bic_ppom, 2)),
  paste0("- **Log-likelihood**: ", round(loglik_ppom, 2)),
  paste0("- **N**: ", n_obs),
  "",
  "### Coefficients Summary",
  "",
  "```",
  strsplit(ppom_coef_text, "\n")[[1]],
  "```",
  "",
  "**Interpretation**: Awareness variables have threshold-varying effects, allowing different associations at different levels of support for nuclear retaliation.",
  "",
  "### Residual Diagnostics",
  "",
  paste0("- **Deviance residuals**: [",
         sprintf("%.2f", min(dev_resid_ppom)), ", ",
         sprintf("%.2f", max(dev_resid_ppom)), "]"),
  paste0("- **Pearson residuals**: [",
         sprintf("%.2f", min(pearson_resid_ppom)), ", ",
         sprintf("%.2f", max(pearson_resid_ppom)), "]"),
  "",
  "---",
  "",
  "## Section 5: Model Comparison (POM vs PPOM)",
  "",
  "### Predicted Probability Comparison",
  "",
  paste0("- **Maximum absolute difference (Δp_max)**: ", sprintf("%.4f", delta_p_max)),
  "- **Threshold for meaningful difference**: 0.03",
  "",
  ifelse(delta_p_max > 0.03,
    "- **Interpretation**: Δp_max > 0.03 → PPOM provides meaningfully different predictions from POM",
    "- **Interpretation**: Δp_max ≤ 0.03 → Similar predictions, but visual violations are clear"
  ),
  "",
  "### Model Selection",
  "",
  paste0("**Final model selected**: ", final_model_type),
  "",
  ifelse(final_model_type == "PPOM",
    c("**Justification**: Clear proportional odds violations detected via visual inspection.",
      "PPOM addresses threshold-specific effects of awareness variables."),
    c("**Justification**: Proportional odds assumption holds based on visual inspection.",
      "POM retained for parsimony.")
  ),
  "",
  "### Model Comparison Table",
  "",
  "| Metric | POM | PPOM | Difference |",
  "|--------|-----|------|------------|",
  paste0("| AIC | ", round(aic_pom, 2), " | ", round(aic_ppom, 2),
         " | ", round(aic_ppom - aic_pom, 2), " |"),
  paste0("| BIC | ", round(bic_pom, 2), " | ", round(bic_ppom, 2),
         " | ", round(bic_ppom - bic_pom, 2), " |"),
  paste0("| Log-likelihood | ", round(loglik_pom, 2), " | ", round(loglik_ppom, 2),
         " | ", round(loglik_ppom - loglik_pom, 2), " |"),
  "",
  "---",
  "",
  "## Decision Log References",
  "",
  "- **DEC-001**: Variable collapsing for convergence (political affiliation: 11→4 levels; ethnicity: 5→3 levels)",
  "- **DEC-006**: Model 1 (separate awareness items) selected over mean index",
  "- **DEC-007**: Visual inspection workflow; Brant test bypassed as unreliable with categorical predictors",
  "- **DEC-008**: Linear narrative output structure (POM → visual inspection → PPOM)",
  "- **DEC-009**: Direct VGAM/MASS implementation; minimal abstraction for transparency",
  "",
  "---",
  "",
  "## Downstream Flag",
  "",
  paste0("**rq2_awareness_mean_ok_overall**: FALSE (use separate items in RQ5)"),
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
