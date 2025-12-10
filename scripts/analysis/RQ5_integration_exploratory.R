# ==============================================================================
# RQ5: EXPLORATORY INTEGRATION (AWARENESS × DECISION FACTORS × SUPPORT)
# ==============================================================================
#
# This script integrates findings from RQ2 and RQ4 to explore how awareness
# and decision factors jointly predict support for nuclear retaliation.
#
# PREREQUISITES:
#   - publication_analysis.R must be run first
#   - RQ2 must be complete (to determine if awareness_mean is valid)
#   - RQ4 must have human interpretation of factor structure
#
# OUTPUTS:
#   - outputs/RQ5_integration.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

if (!exists("rq2_awareness_mean_ok_overall")) {
  stop("RQ2 decision flag not found. Please run RQ2_awareness_support.R first.")
}

# Create output directory (simple, no timestamps)
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("RQ5 output directory:", output_dir, "\n")

# ==============================================================================
# 7.1 Conditional Logic
# ==============================================================================

cat("\nRQ2 awareness_mean flag:", rq2_awareness_mean_ok_overall, "\n")
cat("(Proceed only after RQ4 human interpretation of factor structure)\n\n")

# ==============================================================================
# 7.2 Example Exploratory Models
# ==============================================================================



# ==============================================================================
# 7.3 Generate Outputs
# ==============================================================================



cat("\nRQ5 analysis complete. Results saved to:", output_dir, "\n")
