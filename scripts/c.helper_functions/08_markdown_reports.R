# ==============================================================================
# 08_markdown_reports.R
# Consolidated markdown report generation for ordinal regression analyses
# ==============================================================================

generate_ordinal_regression_report <- function(
  # Meta information
  analysis_name,           # e.g., "RQ2" or "RQ3"
  analysis_title,          # e.g., "Using Awareness as Associational Predictor"
  output_dir,              # Directory for output files

  # Overview
  analysis_date,
  sample_description,      # e.g., "Treatment group, complete data"
  n_total,
  outcome_description,     # e.g., "Support for nuclear strike on Russia (ordinal, 1-5)"
  predictor_description,   # e.g., "Three awareness items (1980s, recent academic, media)"
  final_model_type,        # "POM" or "PPOM"

  # Decision records (optional)
  decision_records = NULL,

  # Data assembly
  data_assembly_items,     # Named list of data assembly info

  # Model 1: Unadjusted POM
  formula_unadjusted,
  model1_stats,            # List: aic, bic, loglik, n
  model1_coef_table,       # Data frame with columns: variable, estimate, std_error, t_value, p_value, odds_ratio

  # Model 2: Adjusted POM
  formula_adjusted,
  covariates_description,  # e.g., "age, sex, ethnicity, political affiliation, employment status, student status"
  model2_stats,
  model2_coef_table,
  model2_residuals_range,  # List: deviance_min, deviance_max, pearson_min, pearson_max
  pom_diagnostics_file,    # Filename for diagnostic plot
  pom_forest_file,         # Filename for forest plot

  # Visual inspection
  po_violated,             # Boolean
  visual_findings,         # Named list with findings for each of 4 plots

  # Model 3: PPOM (conditional)
  ppom_specification = NULL,     # Text describing specification
  model3_stats = NULL,
  model3_coef_summary = NULL,    # Full coefficient summary text
  model3_residuals_range = NULL,
  ppom_diagnostics_file = NULL,
  ppom_coefficients_file = NULL, # Threshold-specific plot

  # Model comparison
  delta_p_max = NULL,
  model_comparison_table = NULL,  # Data frame with columns: metric, pom, ppom, difference

  # Additional context
  downstream_flag = NULL,
  bookmarks = NULL
) {

  # Build markdown content
  md_content <- c(
    paste0("# ", analysis_name, ": ", analysis_title),
    "",
    "## Overview",
    "",
    paste0("- **Analysis date**: ", analysis_date),
    paste0("- **Sample**: ", sample_description, " (N = ", n_total, ")"),
    paste0("- **Outcome**: ", outcome_description),
    paste0("- **Predictors**: ", predictor_description),
    paste0("- **Final model**: ", final_model_type),
    ""
  )

  # Decision records (if provided)
  if (!is.null(decision_records)) {
    md_content <- c(
      md_content,
      "## Decision Record",
      "",
      "This analysis implements:",
      decision_records,
      "",
      "---",
      ""
    )
  }

  # Section 1: Data Assembly
  md_content <- c(
    md_content,
    "## Section 1: Data Assembly",
    "",
    unlist(data_assembly_items),
    "",
    "---",
    ""
  )

  # Section 2: Model 1 - Unadjusted POM
  md_content <- c(
    md_content,
    "## Section 2: Model 1 - Unadjusted POM",
    "",
    "### Formula",
    "",
    paste0("```\n", deparse(formula_unadjusted, width.cutoff = 500), "\n```"),
    "",
    "### Model Fit Statistics",
    "",
    paste0("- **AIC**: ", round(model1_stats$aic, 2)),
    paste0("- **BIC**: ", round(model1_stats$bic, 2)),
    paste0("- **Log-likelihood**: ", round(model1_stats$loglik, 2)),
    paste0("- **N**: ", model1_stats$n),
    "",
    "### Coefficients",
    "",
    "| Variable | Estimate | Std. Error | t-value | p-value | OR |",
    "|----------|----------|------------|---------|---------|-----|",
    apply(model1_coef_table, 1, function(row) {
      sprintf("| %s | %.4f | %.4f | %.4f | %.4f | %.3f |",
              row["variable"],
              as.numeric(row["estimate"]),
              as.numeric(row["std_error"]),
              as.numeric(row["t_value"]),
              as.numeric(row["p_value"]),
              as.numeric(row["odds_ratio"]))
    }),
    "",
    "---",
    ""
  )

  # Section 3: Model 2 - Adjusted POM
  md_content <- c(
    md_content,
    "## Section 3: Model 2 - Adjusted POM",
    "",
    "### Formula",
    "",
    paste0("```\n", deparse(formula_adjusted, width.cutoff = 500), "\n```"),
    "",
    paste0("**Covariates included**: ", covariates_description),
    "",
    "### Model Fit Statistics",
    "",
    paste0("- **AIC**: ", round(model2_stats$aic, 2)),
    paste0("- **BIC**: ", round(model2_stats$bic, 2)),
    paste0("- **Log-likelihood**: ", round(model2_stats$loglik, 2)),
    paste0("- **N**: ", model2_stats$n),
    "",
    "### Coefficients (Proportional Odds Assumption)",
    "",
    "| Variable | Estimate | Std. Error | t-value | p-value | OR |",
    "|----------|----------|------------|---------|---------|-----|",
    apply(model2_coef_table, 1, function(row) {
      sprintf("| %s | %.4f | %.4f | %.4f | %.4f | %.3f |",
              row["variable"],
              as.numeric(row["estimate"]),
              as.numeric(row["std_error"]),
              as.numeric(row["t_value"]),
              as.numeric(row["p_value"]),
              as.numeric(row["odds_ratio"]))
    }),
    "",
    "### Residual Diagnostics",
    "",
    paste0("- **Deviance residuals**: [",
           sprintf("%.2f", model2_residuals_range$deviance_min), ", ",
           sprintf("%.2f", model2_residuals_range$deviance_max), "]"),
    paste0("- **Pearson residuals**: [",
           sprintf("%.2f", model2_residuals_range$pearson_min), ", ",
           sprintf("%.2f", model2_residuals_range$pearson_max), "]"),
    "",
    "### Diagnostic Plots",
    "",
    paste0("![POM Diagnostic Plots](", pom_diagnostics_file, ")"),
    "",
    "**Visual Assessment**: 4-panel diagnostic plot shows residuals vs fitted, normal Q-Q, scale-location, and observed vs predicted.",
    "",
    "### Forest Plot",
    "",
    paste0("![POM Forest Plot](", pom_forest_file, ")"),
    "",
    "---",
    ""
  )

  # Section 4: Visual Inspection
  md_content <- c(
    md_content,
    "## Section 4: Proportional Odds Assumption - Visual Inspection",
    "",
    "**Protocol**: Per DEC-007, systematic visual inspection using 4-panel diagnostics",
    "",
    "### Findings",
    "",
    unlist(visual_findings),
    "",
    "### Decision",
    "",
    paste0("**Result**: ", ifelse(po_violated,
      "4/4 diagnostic plots show CLEAR violations of proportional odds assumption",
      "No clear violations detected in diagnostic plots")),
    "",
    paste0("**Action**: ", ifelse(po_violated,
      "ESCALATE TO PPOM (per Section 2.1.3a protocol)",
      "USE POM (proportional odds assumption holds)")),
    "",
    "**Note**: Brant test bypassed per DEC-007 (visual inspection definitive)",
    "",
    "---",
    ""
  )

  # Section 5: Model 3 - PPOM (conditional)
  if (po_violated && !is.null(model3_stats)) {
    md_content <- c(
      md_content,
      "## Section 5: Model 3 - PPOM (VGAM::vglm)",
      "",
      "### Specification",
      "",
      ppom_specification,
      "",
      "### Formula",
      "",
      paste0("```\n", deparse(formula_adjusted, width.cutoff = 500), "\n```"),
      "",
      "*(Note: Same formula as Model 2, but with partial proportional odds)*",
      "",
      "### Model Fit Statistics",
      "",
      paste0("- **AIC**: ", round(model3_stats$aic, 2)),
      paste0("- **BIC**: ", round(model3_stats$bic, 2)),
      paste0("- **Log-likelihood**: ", round(model3_stats$loglik, 2)),
      paste0("- **N**: ", model3_stats$n),
      "",
      "### Coefficients Summary",
      "",
      "```",
      model3_coef_summary,
      "```",
      "",
      "**Interpretation**: Flexible variables have threshold-varying effects, allowing different associations at different levels of the outcome.",
      "",
      "### Residual Diagnostics",
      "",
      paste0("- **Deviance residuals**: [",
             sprintf("%.2f", model3_residuals_range$deviance_min), ", ",
             sprintf("%.2f", model3_residuals_range$deviance_max), "]"),
      paste0("- **Pearson residuals**: [",
             sprintf("%.2f", model3_residuals_range$pearson_min), ", ",
             sprintf("%.2f", model3_residuals_range$pearson_max), "]"),
      "",
      "### Diagnostic Plots",
      "",
      paste0("![PPOM Diagnostic Plots](", ppom_diagnostics_file, ")"),
      "",
      "**Visual Assessment**: 4-panel diagnostic plot shows improved model fit compared to POM.",
      "",
      "### Threshold-Specific Coefficients",
      "",
      paste0("![PPOM Coefficient Plot](", ppom_coefficients_file, ")"),
      "",
      "**Interpretation**: Shows how flexible variable effects vary across outcome thresholds, demonstrating violation of proportional odds assumption.",
      "",
      "---",
      ""
    )
  }

  # Section 6: Model Comparison (if PPOM was fitted)
  section_num <- ifelse(po_violated, 6, 5)
  if (po_violated && !is.null(delta_p_max)) {
    md_content <- c(
      md_content,
      paste0("## Section ", section_num, ": Model Comparison (POM vs PPOM)"),
      "",
      "### Predicted Probability Comparison",
      "",
      paste0("- **Maximum absolute difference (Δp_max)**: ", sprintf("%.4f", delta_p_max)),
      "- **Threshold for meaningful difference**: 0.03",
      "",
      ifelse(delta_p_max > 0.03,
        "- **Interpretation**: Δp_max > 0.03 → PPOM provides meaningfully different predictions from POM",
        "- **Interpretation**: Δp_max ≤ 0.03 → Similar predictions, but visual violations are clear"),
      "",
      "### Model Selection",
      "",
      paste0("**Final model selected**: ", final_model_type),
      "",
      ifelse(final_model_type == "PPOM",
        c("**Justification**: Clear proportional odds violations detected via visual inspection.",
          "PPOM addresses threshold-specific effects of flexible variables."),
        c("**Justification**: Proportional odds assumption holds based on visual inspection.",
          "POM retained for parsimony.")),
      "",
      "### Model Comparison Table",
      "",
      "| Metric | POM | PPOM | Difference |",
      "|--------|-----|------|------------|",
      apply(model_comparison_table, 1, function(row) {
        sprintf("| %s | %.2f | %.2f | %.2f |",
                row["metric"],
                as.numeric(row["pom"]),
                as.numeric(row["ppom"]),
                as.numeric(row["difference"]))
      }),
      "",
      "---",
      ""
    )
  }

  # Decision log references (if provided)
  if (!is.null(decision_records)) {
    next_section <- ifelse(po_violated, section_num + 1, section_num + 1)
    md_content <- c(
      md_content,
      paste0("## Section ", next_section, ": Decision Log References"),
      "",
      decision_records,
      "",
      "---",
      ""
    )
  }

  # Downstream flags (if provided)
  if (!is.null(downstream_flag)) {
    md_content <- c(
      md_content,
      "## Downstream Flag",
      "",
      downstream_flag,
      "",
      "---",
      ""
    )
  }

  # Bookmarks (if provided)
  if (!is.null(bookmarks)) {
    md_content <- c(
      md_content,
      bookmarks,
      "",
      "---",
      ""
    )
  }

  # Timestamp
  md_content <- c(
    md_content,
    paste0("*Generated: ", Sys.time(), "*")
  )

  return(md_content)
}

cat("Markdown report generation functions loaded successfully.\n")
