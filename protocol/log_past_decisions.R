# Log Past Decisions
#
# This script logs the three major methodological decisions made in Session 5
# to establish the decision history baseline.

source("protocol/log_decision.R")

# Decision 1: Collapsed categorical variables
cat("Logging Decision 1: Collapsed categorical variables...\n")
dec1 <- log_decision(
  stage = "Data Processing - Variable Collapsing",
  trigger = "High-cardinality categorical variables (political.affiliation=11 levels, ethnicity=5 levels) caused convergence issues and unstable coefficient estimates in ordinal regressions",
  evidence = list(
    political_affiliation_original_levels = 11,
    ethnicity_original_levels = 5,
    convergence_failures = "Multiple models failed to converge",
    coefficient_instability = "Wide confidence intervals, unstable estimates"
  ),
  options = c(
    "Option A: Remove categorical variables from models",
    "Option B: Collapse high-cardinality variables into theoretically justified groups",
    "Option C: Use penalized regression methods (e.g., ridge, lasso)",
    "Option D: Convert to binary indicators (reference vs all others)"
  ),
  decision = "Option B: Collapse high-cardinality variables into theoretically justified groups",
  rationale = "Preserves important demographic covariates while addressing convergence. Groupings based on political alignment theory (left/right/unaffiliated) and sample size. Option A loses important information. Option C adds complexity without addressing root cause. Option D oversimplifies politically meaningful distinctions.",
  protocol_sections = c(
    "Section 2.2 - Covariates",
    "Section 0.3 - Key covariates for adjusted models"
  ),
  implementation = list(
    function_created = "create_collapsed_categories() in R/02_data_processing.R:135-187",
    political_affiliation_mapping = "11 levels → 4 levels (Left-leaning, Right-leaning, Unaffiliated/Uncertain, Other)",
    ethnicity_mapping = "5 levels → 3 levels (White, Black, Asian & Other)",
    scripts_modified = c(
      "R/02_data_processing.R",
      "publication_analysis.R",
      "analysis/RQ2_awareness_support.R",
      "analysis/RQ3_treatment_effects.R"
    ),
    testing_status = "Full pipeline tested successfully (2025-12-09)"
  ),
  change_type = "minor"
)

# Decision 2: Visual inspection approach for proportional odds assumption
cat("\nLogging Decision 2: Visual inspection approach for PO testing...\n")
dec2 <- log_decision(
  stage = "Methodology - Proportional Odds Assumption Testing",
  trigger = "Automated proportional odds tests (Brant test, ordinal::nominal_test) unreliable with complex categorical models, returning NA/p=1.0 even after variable collapsing",
  evidence = list(
    nominal_test_result = "p=1.0 for all predictors (indicates test failure)",
    brant_test_result = "Also unreliable with high-dimensional categorical models",
    root_cause = "Tests attempt to fit very flexible models with separate coefficients per threshold, causing rank-deficiency",
    collapsed_variables_insufficient = "Variable collapsing reduced degrees of freedom but did not fix test failures"
  ),
  options = c(
    "Option A: Continue using automated tests and accept p=1.0 as 'assumption holds'",
    "Option B: Use visual inspection of diagnostic plots as primary method",
    "Option C: Test PO assumption only for continuous predictors",
    "Option D: Default to PPOM for all models without testing"
  ),
  decision = "Option B: Use visual inspection of diagnostic plots as primary method",
  rationale = "Visual inspection is standard practice in ordinal regression literature and more trustworthy than automated tests for complex models. Automated tests are fundamentally unreliable with categorical predictors. Option A risks false negatives. Option C loses information about categorical predictors. Option D is overly conservative and computationally expensive.",
  protocol_sections = c(
    "Section 2.1.3 - Proportional odds assumption testing",
    "Section 2.1 - Ordinal modeling workflow"
  ),
  implementation = list(
    visual_criteria_documented = c(
      "Residuals vs fitted: Look for non-parallel patterns across thresholds",
      "Q-Q plot: Look for threshold-specific deviations",
      "Scale-location: Look for heteroscedasticity varying by threshold",
      "Observed vs predicted: Look for systematic miscalibration"
    ),
    workflow_change = "POM → Diagnostics → VISUAL INSPECTION BOOKMARK → (if violated) PPOM → Diagnostics",
    protocol_documentation = "Section 2.1.3 limitation documented (analysis_protocol_v3.md:234-262)",
    conservative_default = "When test fails, default to POM (more restrictive model)"
  ),
  change_type = "minor"
)

# Decision 3: RQ2 model escalation to PPOM
cat("\nLogging Decision 3: RQ2 PPOM model escalation...\n")
dec3 <- log_decision(
  stage = "RQ2 - Awareness as Predictor",
  trigger = "Visual inspection of POM diagnostic plots revealed proportional odds assumption violations for RQ2 Model 1 (separate awareness items)",
  evidence = list(
    diagnostic_observations = c(
      "Non-parallel residual patterns across support outcome thresholds",
      "Systematic deviations in Q-Q plot at different thresholds",
      "Heteroscedasticity patterns varying by threshold in scale-location plot"
    ),
    visual_inspection_decision = "PO assumption violated",
    model_specification = "support ~ awareness_1980s + awareness_recent_academic + awareness_recent_media + covariates"
  ),
  options = c(
    "Option A: Retain POM despite violation (assume mild violation acceptable)",
    "Option B: Use PPOM with all predictors flexible (parallel=FALSE for all)",
    "Option C: Use PPOM with only violating predictors flexible (partial flexibility)"
  ),
  decision = "Option B: Use PPOM with all predictors flexible (parallel=FALSE for all)",
  rationale = "Visual inspection shows violation affects multiple predictors, not just one. Conservative approach: allow all coefficients to vary across thresholds. Simplifies interpretation compared to partial flexibility. Exploratory analysis phase justifies flexibility over parsimony.",
  protocol_sections = c(
    "Section 2.1.4 - Model escalation: Partial Proportional Odds Model",
    "Section 5 - RQ2: Using awareness as associational predictor"
  ),
  implementation = list(
    implementation_status = "PLANNED - to be implemented (paused 2025-12-09)",
    model_specification = "Model 1 PPOM with parallel=FALSE for all predictors",
    expected_output = "6-page PDF: POM diagnostics (pages 1-3), PPOM diagnostics (pages 4-6)",
    narrative_structure = "Linear: POM → Visual inspection → PPOM (not comparative reporting)",
    script_location = "analysis/RQ2_awareness_support.R"
  ),
  change_type = "minor"
)

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("PAST DECISIONS LOGGED SUCCESSFULLY\n")
cat(rep("=", 80), "\n", sep = "")
cat("Decision IDs:", dec1, ",", dec2, ",", dec3, "\n")
cat("Protocol version updated to reflect historical changes\n")
cat("Review protocol/decision_log.json for full details\n")
cat(rep("=", 80), "\n\n", sep = "")
