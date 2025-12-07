# ==============================================================================
# RQ2: USING AWARENESS AS ASSOCIATIONAL PREDICTOR
# ==============================================================================
#
# This script examines whether awareness of nuclear winter (measured in the
# treatment group) predicts support for nuclear retaliation.
#
# PREREQUISITES:
#   - publication_analysis.R must be run first
#   - RQ1_awareness_structure.R must be run first to create awareness_mean
#
# OUTPUTS (in timestamped output folder):
#   - RQ2_awareness_mean_model_comparison.csv
#   - RQ2_awareness_mean_effects.csv
#   - RQ2_awareness_support.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

if (!exists("rq1_awareness_mean")) {
  stop("rq1_awareness_mean not found. Please run RQ1_awareness_structure.R first.")
}

# Use output directory from RQ1
rq2_dir <- file.path(dirname(rq1_dir), "RQ2_awareness_support")
dir.create(rq2_dir, recursive = TRUE, showWarnings = FALSE)

cat("RQ2 output directory:", rq2_dir, "\n")

# ==============================================================================
# 4.1 Data Assembly (Treatment Group Only)
# ==============================================================================



# ==============================================================================
# 4.2 Check A – Distribution & Monotonicity
# ==============================================================================



# ==============================================================================
# 4.3 Check B – Item-wise vs Mean Model Comparison
# ==============================================================================



# ==============================================================================
# 4.4 Final Decision Flag
# ==============================================================================



# ==============================================================================
# 4.5 Generate Outputs
# ==============================================================================



cat("\nRQ2 analysis complete. Results saved to:", rq2_dir, "\n")
