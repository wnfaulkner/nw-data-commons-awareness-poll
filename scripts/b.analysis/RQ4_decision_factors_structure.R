# ==============================================================================
# RQ4: DECISION FACTORS STRUCTURE VIA EFA
# ==============================================================================
#
# This script examines the factor structure of the five decision-factor items
# using exploratory factor analysis (EFA).
#
# PREREQUISITES:
#   - publication_analysis.R must be run first
#
# OUTPUTS:
#   - outputs/RQ4_decision_factors_structure.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

# Load psych package for factor analysis
if (!requireNamespace("psych", quietly = TRUE)) {
  stop("Package 'psych' is required. Please install it with: install.packages('psych')")
}
library(psych)

# Create output directory (simple, no timestamps)
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("\n===============================================================================\n")
cat("RQ4: DECISION FACTORS STRUCTURE VIA EFA\n")
cat("===============================================================================\n\n")
cat("Output directory:", output_dir, "\n\n")

# ==============================================================================
# 6.1 Items Construction
# ==============================================================================

cat("6.1 Constructing decision factor items...\n")

rq4_decision_items <- data.tb %>%
  dplyr::select(
    participant.id,
    decision.reduce.russian.retaliation_numeric,
    decision.punish.russia.signal.aggressors_numeric,
    decision.limit.diplomatic.support.for.russia_numeric,
    decision.avoid.killing.civilians.global.famine_numeric,
    decision.avoid.escalation_numeric
  ) %>%
  dplyr::filter(if_any(starts_with("decision."), ~ !is.na(.)))

cat("  Sample size (with at least one decision factor response):", nrow(rq4_decision_items), "\n\n")

# Count complete cases (all 5 items)
complete_cases <- rq4_decision_items %>%
  dplyr::filter(complete.cases(.))

cat("  Complete cases (all 5 items):", nrow(complete_cases), "\n")

# Create matrix for analysis (excluding ID column)
decision_matrix <- as.matrix(complete_cases %>% dplyr::select(-participant.id))

# Item names for cleaner output
item_short_names <- c(
  "Reduce Russian retaliation",
  "Punish Russia / signal aggressors",
  "Limit diplomatic support for Russia",
  "Avoid killing civilians / famine",
  "Avoid escalation"
)

cat("\n")

# ==============================================================================
# 6.2 Descriptive Profiles
# ==============================================================================

cat("6.2 Computing descriptive profiles...\n")

# Join with support variable
decision_with_support <- complete_cases %>%
  left_join(
    data.tb %>% dplyr::select(participant.id, support.nuclear.strike.on.russia_numeric),
    by = "participant.id"
  )

# Calculate means by support level
decision_by_support <- decision_with_support %>%
  dplyr::filter(!is.na(support.nuclear.strike.on.russia_numeric)) %>%
  dplyr::group_by(support.nuclear.strike.on.russia_numeric) %>%
  dplyr::summarise(
    n = n(),
    reduce_retaliation = mean(decision.reduce.russian.retaliation_numeric, na.rm = TRUE),
    punish_signal = mean(decision.punish.russia.signal.aggressors_numeric, na.rm = TRUE),
    limit_support = mean(decision.limit.diplomatic.support.for.russia_numeric, na.rm = TRUE),
    avoid_famine = mean(decision.avoid.killing.civilians.global.famine_numeric, na.rm = TRUE),
    avoid_escalation = mean(decision.avoid.escalation_numeric, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Decision factor means by support level:\n")
print(decision_by_support)
cat("\n")

# Calculate overall descriptives
decision_descriptives <- decision_with_support %>%
  dplyr::select(starts_with("decision.")) %>%
  dplyr::summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))

# ==============================================================================
# 6.3 Polychoric EFA
# ==============================================================================

cat("6.3 Conducting polychoric EFA...\n\n")

# Compute polychoric correlation matrix
cat("  Computing polychoric correlations...\n")
decision_poly <- polychoric(decision_matrix)

# Extract correlation matrix
decision_poly_cor <- decision_poly$rho
rownames(decision_poly_cor) <- item_short_names
colnames(decision_poly_cor) <- item_short_names

cat("  Polychoric correlation matrix computed.\n\n")

# 1-factor solution
cat("  Fitting 1-factor model...\n")
rq4_fa1 <- fa(
  decision_poly_cor,
  nfactors = 1,
  fm = "minres",
  rotate = "none"
)

cat("    Variance explained:", round(rq4_fa1$Vaccounted[2,1] * 100, 1), "%\n")

# 2-factor solution
cat("  Fitting 2-factor model...\n")
rq4_fa2 <- fa(
  decision_poly_cor,
  nfactors = 2,
  fm = "minres",
  rotate = "varimax"
)

cat("    Cumulative variance explained:", round(sum(rq4_fa2$Vaccounted[2, 1:2]) * 100, 1), "%\n")

# 3-factor solution
cat("  Fitting 3-factor model...\n")
rq4_fa3 <- fa(
  decision_poly_cor,
  nfactors = 3,
  fm = "minres",
  rotate = "varimax"
)

cat("    Cumulative variance explained:", round(sum(rq4_fa3$Vaccounted[2, 1:3]) * 100, 1), "%\n\n")

# ==============================================================================
# 6.4 Extract Loadings
# ==============================================================================

cat("6.4 Extracting factor loadings...\n")

# 1-factor loadings
loadings_1f <- as.data.frame(unclass(rq4_fa1$loadings))
loadings_1f$item <- item_short_names
loadings_1f$communality <- rq4_fa1$communality
loadings_1f$uniqueness <- rq4_fa1$uniquenesses

# 2-factor loadings
loadings_2f <- as.data.frame(unclass(rq4_fa2$loadings))
loadings_2f$item <- item_short_names
loadings_2f$communality <- rq4_fa2$communality
loadings_2f$uniqueness <- rq4_fa2$uniquenesses

# 3-factor loadings
loadings_3f <- as.data.frame(unclass(rq4_fa3$loadings))
loadings_3f$item <- item_short_names
loadings_3f$communality <- rq4_fa3$communality
loadings_3f$uniqueness <- rq4_fa3$uniquenesses

cat("  Factor loadings extracted for all models.\n\n")

# ==============================================================================
# 6.5 Generate Markdown Output
# ==============================================================================

cat("6.5 Generating markdown report...\n")

md_file <- file.path(output_dir, "RQ4_decision_factors_structure.md")
md_content <- c(
  "# RQ4: Decision Factors Structure via EFA",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  paste0("- **Sample**: Complete cases on all 5 decision factors (N = ", nrow(complete_cases), ")"),
  "- **Variables**: Five decision-factor items (1-5 scale)",
  "- **Method**: Polychoric correlation + exploratory factor analysis",
  "",
  "## Decision Factor Items",
  "",
  "1. Reduce Russian retaliation (`decision.reduce.russian.retaliation`)",
  "2. Punish Russia / signal to other aggressors (`decision.punish.russia.signal.aggressors`)",
  "3. Limit diplomatic support for Russia (`decision.limit.diplomatic.support.for.russia`)",
  "4. Avoid killing civilians / global famine (`decision.avoid.killing.civilians.global.famine`)",
  "5. Avoid escalation (`decision.avoid.escalation`)",
  "",
  "## Descriptive Profile",
  "",
  "### Decision Factor Means by Support for Nuclear Retaliation",
  "",
  "| Support Level | N | Reduce Retaliation | Punish/Signal | Limit Support | Avoid Famine | Avoid Escalation |",
  "|---------------|---|--------------------|---------------|---------------|--------------|------------------|",
  apply(decision_by_support, 1, function(row) {
    paste0("| ", row[1], " | ", row[2], " | ",
           sprintf("%.2f", as.numeric(row[3])), " | ",
           sprintf("%.2f", as.numeric(row[4])), " | ",
           sprintf("%.2f", as.numeric(row[5])), " | ",
           sprintf("%.2f", as.numeric(row[6])), " | ",
           sprintf("%.2f", as.numeric(row[7])), " |")
  }),
  "",
  "## Polychoric Correlation Matrix",
  "",
  "```",
  paste(capture.output(print(round(decision_poly_cor, 3))), collapse = "\n"),
  "```",
  "",
  "## Factor Analysis Results",
  "",
  "### 1-Factor Solution",
  "",
  paste0("- **Variance explained**: ", round(rq4_fa1$Vaccounted[2,1] * 100, 1), "%"),
  paste0("- **Method**: Minimum residual (minres), no rotation"),
  "",
  "**Factor Loadings:**",
  "",
  "| Item | Loading | Communality | Uniqueness |",
  "|------|---------|-------------|------------|",
  apply(loadings_1f, 1, function(row) {
    paste0("| ", row["item"], " | ",
           sprintf("%.3f", as.numeric(row[1])), " | ",
           sprintf("%.3f", as.numeric(row["communality"])), " | ",
           sprintf("%.3f", as.numeric(row["uniqueness"])), " |")
  }),
  "",
  "### 2-Factor Solution",
  "",
  paste0("- **Cumulative variance explained**: ", round(sum(rq4_fa2$Vaccounted[2, 1:2]) * 100, 1), "%"),
  paste0("- **Method**: Minimum residual (minres), varimax rotation"),
  "",
  "**Factor Loadings:**",
  "",
  "| Item | Factor 1 | Factor 2 | Communality | Uniqueness |",
  "|------|----------|----------|-------------|------------|",
  apply(loadings_2f, 1, function(row) {
    paste0("| ", row["item"], " | ",
           sprintf("%.3f", as.numeric(row[1])), " | ",
           sprintf("%.3f", as.numeric(row[2])), " | ",
           sprintf("%.3f", as.numeric(row["communality"])), " | ",
           sprintf("%.3f", as.numeric(row["uniqueness"])), " |")
  }),
  "",
  "### 3-Factor Solution",
  "",
  paste0("- **Cumulative variance explained**: ", round(sum(rq4_fa3$Vaccounted[2, 1:3]) * 100, 1), "%"),
  paste0("- **Method**: Minimum residual (minres), varimax rotation"),
  "",
  "**Factor Loadings:**",
  "",
  "| Item | Factor 1 | Factor 2 | Factor 3 | Communality | Uniqueness |",
  "|------|----------|----------|----------|-------------|------------|",
  apply(loadings_3f, 1, function(row) {
    paste0("| ", row["item"], " | ",
           sprintf("%.3f", as.numeric(row[1])), " | ",
           sprintf("%.3f", as.numeric(row[2])), " | ",
           sprintf("%.3f", as.numeric(row[3])), " | ",
           sprintf("%.3f", as.numeric(row["communality"])), " | ",
           sprintf("%.3f", as.numeric(row["uniqueness"])), " |")
  }),
  "",
  "---",
  "",
  "## BOOKMARK FOR HUMAN INTERPRETATION",
  "",
  "**Do not proceed to create factor-based indices without explicit instruction.**",
  "",
  "Review the factor loadings above to determine:",
  "",
  "1. Is a **1-factor** solution sufficient (general \"hawkishness\" vs \"dovishness\")?",
  "2. Does the **2-factor** solution suggest a meaningful \"deterrence vs risk-avoidance\" structure?",
  "3. Is the **3-factor** solution over-fitted or interpretable?",
  "",
  "**Next steps** (after human review):",
  "",
  "- If 1-factor: Create a simple mean index",
  "- If 2-factor: Decide which items load on \"deterrence\" vs \"risk-avoidance\" and create two indices",
  "- If item-wise: Use individual items in RQ5 models",
  "",
  "---",
  paste0("*Generated: ", Sys.time(), "*")
)

writeLines(md_content, md_file)
cat("  ✓ RQ4_decision_factors_structure.md\n")
cat("  (Note: CSV outputs disabled - data retained in R environment)\n\n")

# ==============================================================================
# Summary and Next Steps
# ==============================================================================

cat("===============================================================================\n")
cat("RQ4 ANALYSIS COMPLETE\n")
cat("===============================================================================\n\n")

cat("Results summary:\n")
cat("  - Sample size:", nrow(complete_cases), "\n")
cat("  - 1-factor variance explained:", round(rq4_fa1$Vaccounted[2,1] * 100, 1), "%\n")
cat("  - 2-factor variance explained:", round(sum(rq4_fa2$Vaccounted[2, 1:2]) * 100, 1), "%\n")
cat("  - 3-factor variance explained:", round(sum(rq4_fa3$Vaccounted[2, 1:3]) * 100, 1), "%\n\n")

cat("Output files:\n")
cat("  -", md_file, "\n\n")

cat("IMPORTANT: Review RQ4_decision_factors_structure.md for factor interpretation.\n")
cat("Do not create factor indices until human review is complete.\n")
cat("===============================================================================\n\n")
