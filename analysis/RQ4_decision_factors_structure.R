# ==============================================================================
# RQ4: DECISION FACTORS – STRUCTURE VIA EFA
# ==============================================================================
#
# This script explores the latent structure of decision factors that
# respondents consider when evaluating nuclear retaliation scenarios.
#
# PREREQUISITES:
#   - publication_analysis.R must be run first
#
# OUTPUTS (in timestamped output folder):
#   - RQ4_decision_factors_loadings_1factor.csv
#   - RQ4_decision_factors_loadings_2factor.csv
#   - RQ4_decision_factors_loadings_3factor.csv
#   - RQ4_decision_factors_structure.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

# Use output directory from previous RQs
rq4_dir <- file.path(dirname(rq1_dir), "RQ4_decision_factors_structure")
dir.create(rq4_dir, recursive = TRUE, showWarnings = FALSE)

cat("RQ4 output directory:", rq4_dir, "\n")

# ==============================================================================
# 6.1 Items Construction
# ==============================================================================



# ==============================================================================
# 6.2 Descriptive Profiles
# ==============================================================================



# ==============================================================================
# 6.3 Polychoric EFA
# ==============================================================================



# ==============================================================================
# 6.4 BOOKMARK FOR HUMAN INTERPRETATION
# ==============================================================================
#
# Do NOT automatically generate factor-based indices.
# Review whether 2-factor "deterrence vs risk-avoidance" structure is defensible.
# Do not create indices until instructed.
#
# ==============================================================================

# ==============================================================================
# 6.5 Generate Outputs
# ==============================================================================



cat("\nRQ4 analysis complete. Results saved to:", rq4_dir, "\n")
cat("\n*** BOOKMARK: Human interpretation required before creating factor indices ***\n")
