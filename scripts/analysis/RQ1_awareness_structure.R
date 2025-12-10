# ==============================================================================
# RQ1: STRUCTURE OF NUCLEAR-WINTER AWARENESS
# ==============================================================================
#
# This script analyzes the internal structure of the three nuclear-winter
# awareness items to assess whether they form a coherent construct.
#
# PREREQUISITES:
#   - publication_analysis.R must be run first to create data.tb
#
# OUTPUTS:
#   - outputs/RQ1_awareness_structure.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

# Load required packages
library(psych)

# Create output directory (simple, no timestamps)
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("\n===============================================================================\n")
cat("RQ1: STRUCTURE OF NUCLEAR-WINTER AWARENESS\n")
cat("===============================================================================\n\n")
cat("Output directory:", output_dir, "\n\n")

# ==============================================================================
# 3.1 Input Construction
# ==============================================================================

cat("3.1 Constructing input data...\n")

# Filter for treatment group only (those who saw awareness questions)
# Use numeric versions of awareness variables
rq1_awareness_items <- data.tb %>%
  filter(shown.infographic == "Shown NW Iinfographic") %>%
  select(
    participant.id,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric
  ) %>%
  # Remove rows with all missing awareness data
  filter(!is.na(nw.awareness.1980s_numeric) |
         !is.na(nw.awareness.recent.academic_numeric) |
         !is.na(nw.awareness.recent.media_numeric))

cat("  Sample size (treatment group with awareness data):", nrow(rq1_awareness_items), "\n")
cat("  Variables: 1980s awareness, recent academic awareness, recent media awareness\n\n")

# ==============================================================================
# 3.2 Diagnostics
# ==============================================================================

cat("3.2 Computing diagnostics...\n")

# Prepare matrix without participant.id for analysis
awareness_matrix <- rq1_awareness_items %>%
  select(-participant.id) %>%
  as.matrix()

# Cronbach's alpha
cat("  Computing Cronbach's alpha...\n")
rq1_alpha <- psych::alpha(awareness_matrix, check.keys = TRUE)

cat("    Raw alpha:", round(rq1_alpha$total$raw_alpha, 3), "\n")
cat("    Standardized alpha:", round(rq1_alpha$total$std.alpha, 3), "\n")

# Pearson correlation matrix
cat("  Computing Pearson correlations...\n")
rq1_cor_pearson <- cor(awareness_matrix, use = "pairwise.complete.obs", method = "pearson")
colnames(rq1_cor_pearson) <- c("1980s", "Recent Academic", "Recent Media")
rownames(rq1_cor_pearson) <- c("1980s", "Recent Academic", "Recent Media")

# Polychoric correlation matrix
cat("  Computing polychoric correlations...\n")
rq1_polychoric <- psych::polychoric(awareness_matrix)
rq1_cor_polychoric <- rq1_polychoric$rho
colnames(rq1_cor_polychoric) <- c("1980s", "Recent Academic", "Recent Media")
rownames(rq1_cor_polychoric) <- c("1980s", "Recent Academic", "Recent Media")

cat("  Diagnostics complete.\n\n")

# ==============================================================================
# 3.3 Factor Analysis
# ==============================================================================

cat("3.3 Conducting factor analysis...\n")

# 1-factor solution using polychoric correlations
rq1_fa <- psych::fa(
  rq1_cor_polychoric,
  nfactors = 1,
  fm = "minres",
  rotate = "none"
)

cat("  Factor analysis complete.\n")
cat("    Variance explained:", round(rq1_fa$Vaccounted["Proportion Var", "MR1"] * 100, 1), "%\n")
cat("    Loadings:\n")
cat("      1980s awareness:", round(rq1_fa$loadings[1, 1], 3), "\n")
cat("      Recent academic:", round(rq1_fa$loadings[2, 1], 3), "\n")
cat("      Recent media:", round(rq1_fa$loadings[3, 1], 3), "\n\n")

# Extract loadings and communalities
rq1_fa_loadings <- data.frame(
  item = c("nw.awareness.1980s", "nw.awareness.recent.academic", "nw.awareness.recent.media"),
  loading = as.vector(rq1_fa$loadings[, 1]),
  communality = rq1_fa$communality,
  uniqueness = rq1_fa$uniquenesses
)

# ==============================================================================
# 3.4 Awareness Mean Index
# ==============================================================================

cat("3.4 Creating awareness mean index...\n")

# Create mean index (raw scale 1-4)
rq1_awareness_mean <- rq1_awareness_items %>%
  mutate(
    awareness_mean = rowMeans(
      select(., nw.awareness.1980s_numeric,
             nw.awareness.recent.academic_numeric,
             nw.awareness.recent.media_numeric),
      na.rm = TRUE
    )
  ) %>%
  select(participant.id, awareness_mean)

# Create standardized version
rq1_awareness_mean <- rq1_awareness_mean %>%
  mutate(awareness_mean_z = as.vector(scale(awareness_mean)))

cat("  Awareness mean index created.\n")
cat("    Mean (raw scale):", round(mean(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3), "\n")
cat("    SD (raw scale):", round(sd(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3), "\n")
cat("    Range:", round(min(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3),
    "to", round(max(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3), "\n\n")

# ==============================================================================
# 3.5 Generate Outputs
# ==============================================================================

cat("3.5 Generating markdown report...\n")

# Generate Markdown summary
md_file <- file.path(output_dir, "RQ1_awareness_structure.md")
md_content <- c(
  "# RQ1: Structure of Nuclear-Winter Awareness",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  paste0("- **Sample**: Treatment group respondents (N = ", nrow(rq1_awareness_items), ")"),
  "- **Variables**: Three awareness items (1980s, recent academic, recent media)",
  "",
  "## Internal Consistency",
  "",
  "### Cronbach's Alpha",
  "",
  paste0("- **Raw alpha**: ", round(rq1_alpha$total$raw_alpha, 3)),
  paste0("- **Standardized alpha**: ", round(rq1_alpha$total$std.alpha, 3)),
  "",
  ifelse(rq1_alpha$total$raw_alpha >= 0.7,
         "✓ **Interpretation**: Good internal consistency (α ≥ 0.70)",
         "⚠ **Interpretation**: Questionable internal consistency (α < 0.70)"),
  "",
  "## Correlation Matrices",
  "",
  "### Pearson Correlations",
  "",
  "```",
  capture.output(print(round(rq1_cor_pearson, 3))),
  "```",
  "",
  "### Polychoric Correlations",
  "",
  "```",
  capture.output(print(round(rq1_cor_polychoric, 3))),
  "```",
  "",
  "## Factor Analysis",
  "",
  "### One-Factor Solution (Polychoric Correlations)",
  "",
  paste0("- **Variance explained**: ", round(rq1_fa$Vaccounted["Proportion Var", "MR1"] * 100, 1), "%"),
  paste0("- **Method**: Minimum residual (minres)"),
  "",
  "### Factor Loadings",
  "",
  "| Item | Loading | Communality | Uniqueness |",
  "|------|---------|-------------|------------|",
  paste0("| 1980s awareness | ", round(rq1_fa_loadings$loading[1], 3),
         " | ", round(rq1_fa_loadings$communality[1], 3),
         " | ", round(rq1_fa_loadings$uniqueness[1], 3), " |"),
  paste0("| Recent academic | ", round(rq1_fa_loadings$loading[2], 3),
         " | ", round(rq1_fa_loadings$communality[2], 3),
         " | ", round(rq1_fa_loadings$uniqueness[2], 3), " |"),
  paste0("| Recent media | ", round(rq1_fa_loadings$loading[3], 3),
         " | ", round(rq1_fa_loadings$communality[3], 3),
         " | ", round(rq1_fa_loadings$uniqueness[3], 3), " |"),
  "",
  "## Awareness Mean Index",
  "",
  "Created as simple mean of the three items (raw scale 1–4).",
  "",
  "### Descriptive Statistics",
  "",
  paste0("- **Mean**: ", round(mean(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3)),
  paste0("- **SD**: ", round(sd(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3)),
  paste0("- **Range**: ", round(min(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3),
         " to ", round(max(rq1_awareness_mean$awareness_mean, na.rm = TRUE), 3)),
  paste0("- **N**: ", sum(!is.na(rq1_awareness_mean$awareness_mean))),
  "",
  "A standardized version (z-score) is also available as `awareness_mean_z`.",
  "",
  "## Conclusions",
  "",
  ifelse(rq1_alpha$total$raw_alpha >= 0.7 & rq1_fa$Vaccounted["Proportion Var", "MR1"] >= 0.5,
         paste0("The three awareness items demonstrate good internal consistency (α = ",
                round(rq1_alpha$total$raw_alpha, 3),
                ") and load strongly on a single factor (",
                round(rq1_fa$Vaccounted["Proportion Var", "MR1"] * 100, 1),
                "% variance explained). The awareness mean index is appropriate for use in subsequent analyses."),
         paste0("The three awareness items show α = ",
                round(rq1_alpha$total$raw_alpha, 3),
                " and explain ",
                round(rq1_fa$Vaccounted["Proportion Var", "MR1"] * 100, 1),
                "% of variance in a single-factor solution. Review individual item correlations before proceeding.")),
  "",
  "---",
  paste0("*Generated: ", Sys.time(), "*")
)

writeLines(md_content, md_file)
cat("  ✓ RQ1_awareness_structure.md\n")
cat("  (Note: CSV outputs disabled - data retained in R environment)\n\n")

# ==============================================================================
# Summary and Next Steps
# ==============================================================================

cat("===============================================================================\n")
cat("RQ1 ANALYSIS COMPLETE\n")
cat("===============================================================================\n\n")

cat("Results summary:\n")
cat("  - Internal consistency (α):", round(rq1_alpha$total$raw_alpha, 3), "\n")
cat("  - Variance explained (1-factor):", round(rq1_fa$Vaccounted["Proportion Var", "MR1"] * 100, 1), "%\n")
cat("  - Awareness mean index: created for", sum(!is.na(rq1_awareness_mean$awareness_mean)), "respondents\n\n")

cat("Output file:", md_file, "\n\n")

cat("Next step: Review RQ1_awareness_structure.md, then proceed to RQ2.\n")
cat("===============================================================================\n\n")

# Make awareness index available in global environment for RQ2
cat("Note: rq1_awareness_mean is now available in the global environment for RQ2.\n\n")
