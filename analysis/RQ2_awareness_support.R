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
#   - RQ2_awareness_support.md
# ==============================================================================

# Check prerequisites
if (!exists("data.tb")) {
  stop("data.tb not found. Please run publication_analysis.R first.")
}

if (!exists("rq1_awareness_mean")) {
  stop("rq1_awareness_mean not found. Please run RQ1_awareness_structure.R first.")
}

if (!exists("rq1_alpha")) {
  stop("rq1_alpha not found. Please run RQ1_awareness_structure.R first.")
}

# Use output directory from RQ1
rq2_dir <- file.path(dirname(rq1_dir), "RQ2_awareness_support")
dir.create(rq2_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n===============================================================================\n")
cat("RQ2: USING AWARENESS AS ASSOCIATIONAL PREDICTOR\n")
cat("===============================================================================\n\n")
cat("Output directory:", rq2_dir, "\n\n")

# ==============================================================================
# 4.1 Data Assembly (Treatment Group Only)
# ==============================================================================

cat("4.1 Assembling data (treatment group only)...\n")

# Join awareness mean with main data, filter for treatment group
rq2_data <- data.tb %>%
  left_join(rq1_awareness_mean, by = "participant.id") %>%
  filter(
    shown.infographic == "Shown NW Iinfographic",
    !is.na(support.nuclear.strike.on.russia_numeric),
    !is.na(awareness_mean)
  ) %>%
  select(
    participant.id,
    support.nuclear.strike.on.russia_numeric,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric,
    awareness_mean,
    awareness_mean_z,
    age,
    sex,
    ethnicity,
    political.affiliation,
    employment.status,
    student.status
  ) %>%
  drop_na()

cat("  Sample size (complete cases):", nrow(rq2_data), "\n")
cat("  Outcome: Support for nuclear strike on Russia (1-5 scale)\n")
cat("  Treatment group only (shown infographic)\n\n")

# ==============================================================================
# 4.2 Check A – Distribution & Monotonicity
# ==============================================================================

cat("4.2 Checking distribution and monotonicity...\n")

# Distribution of awareness_mean
awareness_summary <- rq2_data %>%
  summarise(
    mean = mean(awareness_mean, na.rm = TRUE),
    sd = sd(awareness_mean, na.rm = TRUE),
    min = min(awareness_mean, na.rm = TRUE),
    max = max(awareness_mean, na.rm = TRUE),
    median = median(awareness_mean, na.rm = TRUE)
  )

cat("  Awareness mean distribution:\n")
cat("    Mean:", round(awareness_summary$mean, 3), "\n")
cat("    SD:", round(awareness_summary$sd, 3), "\n")
cat("    Range:", round(awareness_summary$min, 3), "to", round(awareness_summary$max, 3), "\n\n")

# Create quartiles and examine support means
rq2_data <- rq2_data %>%
  mutate(
    awareness_quartile = cut(
      awareness_mean,
      breaks = quantile(awareness_mean, probs = c(0, 0.25, 0.5, 0.75, 1)),
      include.lowest = TRUE,
      labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")
    )
  )

support_by_quartile <- rq2_data %>%
  group_by(awareness_quartile) %>%
  summarise(
    n = n(),
    mean_support = mean(support.nuclear.strike.on.russia_numeric, na.rm = TRUE),
    sd_support = sd(support.nuclear.strike.on.russia_numeric, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Support by awareness quartile:\n")
print(support_by_quartile)
cat("\n")

# ==============================================================================
# 4.3 Check B – Item-wise vs Mean Model Comparison
# ==============================================================================

cat("4.3 Comparing item-wise vs mean index models...\n")

# Covariates for adjusted models (using available variables)
covariates <- c("age", "sex", "ethnicity", "political.affiliation", "employment.status", "student.status")

# Create prediction data (needed for both initial comparison and PPOM comparison)
representative_profile <- rq2_data %>%
  summarise(
    age = median(age, na.rm = TRUE),
    sex = names(sort(table(sex), decreasing = TRUE))[1],
    ethnicity = names(sort(table(ethnicity), decreasing = TRUE))[1],
    political.affiliation = names(sort(table(political.affiliation), decreasing = TRUE))[1],
    employment.status = names(sort(table(employment.status), decreasing = TRUE))[1],
    student.status = names(sort(table(student.status), decreasing = TRUE))[1]
  )

awareness_range <- seq(
  min(rq2_data$awareness_mean, na.rm = TRUE),
  max(rq2_data$awareness_mean, na.rm = TRUE),
  length.out = 20
)

pred_data_items <- data.frame(
  nw.awareness.1980s_numeric = awareness_range,
  nw.awareness.recent.academic_numeric = awareness_range,
  nw.awareness.recent.media_numeric = awareness_range
) %>%
  bind_cols(representative_profile[rep(1, nrow(.)), ])

pred_data_mean <- data.frame(
  awareness_mean = awareness_range
) %>%
  bind_cols(representative_profile[rep(1, nrow(.)), ])

# Model 1: Separate awareness items
cat("  Fitting Model 1 (separate items)...\n")
formula_items <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~",
  "nw.awareness.1980s_numeric +",
  "nw.awareness.recent.academic_numeric +",
  "nw.awareness.recent.media_numeric +",
  paste(covariates, collapse = " + ")
))

model1_items <- fit_pom(
  formula = formula_items,
  data = rq2_data,
  link = "logit",
  verbose = FALSE
)

cat("    Model 1 AIC:", round(model1_items$model_stats$aic, 2), "\n")

# Model 2: Mean index
cat("  Fitting Model 2 (mean index)...\n")
formula_mean <- as.formula(paste(
  "support.nuclear.strike.on.russia_numeric ~",
  "awareness_mean +",
  paste(covariates, collapse = " + ")
))

model2_mean <- fit_pom(
  formula = formula_mean,
  data = rq2_data,
  link = "logit",
  verbose = FALSE
)

cat("    Model 2 AIC:", round(model2_mean$model_stats$aic, 2), "\n\n")

# ==============================================================================
# 4.3a Brant Test for Proportional Odds Assumption
# ==============================================================================

cat("4.3a Testing proportional odds assumption...\n")

# Test Model 1
cat("  Testing Model 1 (separate items)...\n")
brant1 <- tryCatch({
  perform_brant_test(model1_items$model, verbose = FALSE)
}, error = function(e) {
  cat("    WARNING: Brant test failed:", e$message, "\n")
  list(omnibus_p_value = 0.99, brant_test_failed = TRUE, error_message = e$message)
})

cat("    Omnibus p-value:", sprintf("%.4f", brant1$omnibus_p_value), "\n")
model1_violates_po <- !is.null(brant1$brant_test_failed) || brant1$omnibus_p_value <= 0.05

# Test Model 2
cat("  Testing Model 2 (mean index)...\n")
brant2 <- tryCatch({
  perform_brant_test(model2_mean$model, verbose = FALSE)
}, error = function(e) {
  cat("    WARNING: Brant test failed:", e$message, "\n")
  list(omnibus_p_value = 0.99, brant_test_failed = TRUE, error_message = e$message)
})

cat("    Omnibus p-value:", sprintf("%.4f", brant2$omnibus_p_value), "\n")
model2_violates_po <- !is.null(brant2$brant_test_failed) || brant2$omnibus_p_value <= 0.05

# ==============================================================================
# 4.3b PPOM Escalation (if needed)
# ==============================================================================

# Initialize PPOM model placeholders
model1_ppom <- NULL
model2_ppom <- NULL
delta_p_max_model1 <- NA
delta_p_max_model2 <- NA
model1_final <- model1_items
model2_final <- model2_mean
model1_final_type <- "POM"
model2_final_type <- "POM"

# Model 1 PPOM
if (model1_violates_po) {
  cat("\n4.3b Fitting PPOM for Model 1 (PO assumption violated)...\n")

  model1_ppom <- tryCatch({
    fit_ppom(
      formula = formula_items,
      data = rq2_data,
      link = "logit",
      verbose = TRUE
    )
  }, error = function(e) {
    cat("    ERROR: PPOM fitting failed:", e$message, "\n")
    cat("    Defaulting to POM model.\n")
    NULL
  })

  if (!is.null(model1_ppom)) {
    # Compare predictions
    cat("  Comparing Model 1 POM vs PPOM predictions...\n")
    preds_m1_pom <- predict(model1_items$model, newdata = pred_data_items, type = "probs")
    preds_m1_ppom <- predict(model1_ppom$model, newdata = pred_data_items, type = "response")
    delta_p_max_model1 <- max(abs(preds_m1_pom - preds_m1_ppom))

    cat("    Δp_max (POM vs PPOM):", sprintf("%.4f", delta_p_max_model1), "\n")

    # Select final model
    if (delta_p_max_model1 > 0.03) {
      model1_final <- model1_ppom
      model1_final_type <- "PPOM"
      cat("    Final selection: PPOM (Δp_max > 0.03)\n")
    } else {
      cat("    Final selection: POM (Δp_max ≤ 0.03, parsimony preferred)\n")
    }
  }
} else {
  cat("\n4.3b Model 1: PO assumption holds (Brant p > 0.05)\n")
  cat("  Retaining POM model.\n")
}

# Model 2 PPOM
if (model2_violates_po) {
  cat("\n4.3b Fitting PPOM for Model 2 (PO assumption violated)...\n")

  model2_ppom <- tryCatch({
    fit_ppom(
      formula = formula_mean,
      data = rq2_data,
      link = "logit",
      verbose = TRUE
    )
  }, error = function(e) {
    cat("    ERROR: PPOM fitting failed:", e$message, "\n")
    cat("    Defaulting to POM model.\n")
    NULL
  })

  if (!is.null(model2_ppom)) {
    # Compare predictions
    cat("  Comparing Model 2 POM vs PPOM predictions...\n")
    preds_m2_pom <- predict(model2_mean$model, newdata = pred_data_mean, type = "probs")
    preds_m2_ppom <- predict(model2_ppom$model, newdata = pred_data_mean, type = "response")
    delta_p_max_model2 <- max(abs(preds_m2_pom - preds_m2_ppom))

    cat("    Δp_max (POM vs PPOM):", sprintf("%.4f", delta_p_max_model2), "\n")

    # Select final model
    if (delta_p_max_model2 > 0.03) {
      model2_final <- model2_ppom
      model2_final_type <- "PPOM"
      cat("    Final selection: PPOM (Δp_max > 0.03)\n")
    } else {
      cat("    Final selection: POM (Δp_max ≤ 0.03, parsimony preferred)\n")
    }
  }
} else {
  cat("\n4.3b Model 2: PO assumption holds (Brant p > 0.05)\n")
  cat("  Retaining POM model.\n")
}

cat("\n")

# ==============================================================================
# 4.3c Cross-Model Comparison (Items vs Mean)
# ==============================================================================

cat("4.3c Comparing final Model 1 vs Model 2...\n")

# Use final selected models for comparison
if (model1_final_type == "PPOM") {
  preds_m1_final <- predict(model1_final$model, newdata = pred_data_items, type = "response")
} else {
  preds_m1_final <- predict(model1_final$model, newdata = pred_data_items, type = "probs")
}

if (model2_final_type == "PPOM") {
  preds_m2_final <- predict(model2_final$model, newdata = pred_data_mean, type = "response")
} else {
  preds_m2_final <- predict(model2_final$model, newdata = pred_data_mean, type = "probs")
}

delta_p_max_cross <- max(abs(preds_m1_final - preds_m2_final))

cat("  Δp_max (Model 1 final vs Model 2 final):", sprintf("%.4f", delta_p_max_cross), "\n")
cat("  Threshold for using mean index: 0.03\n")
cat("  Decision:", ifelse(delta_p_max_cross <= 0.03, "USE mean index", "Use separate items"), "\n\n")

# ==============================================================================
# 4.3d Model Diagnostics for All Models
# ==============================================================================

cat("4.3d Computing model diagnostics...\n")

# Calculate residuals and diagnostics for POMs
cat("  POM diagnostics for Model 1...\n")
residuals_items_pom <- calculate_pom_residuals(model1_items, verbose = FALSE)
diag_plot_items_pom <- plot_pom_diagnostics(
  pom_result = model1_items,
  residuals_tb = residuals_items_pom,
  plot_title = "Model 1 POM: Separate Awareness Items"
)
forest_plot_items_pom <- plot_pom_coefficients(
  pom_result = model1_items,
  plot_title = "Model 1 POM: Separate Awareness Items"
)

cat("  POM diagnostics for Model 2...\n")
residuals_mean_pom <- calculate_pom_residuals(model2_mean, verbose = FALSE)
diag_plot_mean_pom <- plot_pom_diagnostics(
  pom_result = model2_mean,
  residuals_tb = residuals_mean_pom,
  plot_title = "Model 2 POM: Awareness Mean Index"
)
forest_plot_mean_pom <- plot_pom_coefficients(
  pom_result = model2_mean,
  plot_title = "Model 2 POM: Awareness Mean Index"
)

# PPOM diagnostics if fitted
residuals_items_ppom <- NULL
diag_plot_items_ppom <- NULL
forest_plot_items_ppom <- NULL

residuals_mean_ppom <- NULL
diag_plot_mean_ppom <- NULL
forest_plot_mean_ppom <- NULL

if (!is.null(model1_ppom)) {
  cat("  PPOM diagnostics for Model 1...\n")
  residuals_items_ppom <- calculate_pom_residuals(model1_ppom, verbose = FALSE)
  diag_plot_items_ppom <- plot_pom_diagnostics(
    pom_result = model1_ppom,
    residuals_tb = residuals_items_ppom,
    plot_title = "Model 1 PPOM: Separate Awareness Items"
  )
  forest_plot_items_ppom <- plot_pom_coefficients(
    pom_result = model1_ppom,
    plot_title = "Model 1 PPOM: Separate Awareness Items"
  )
}

if (!is.null(model2_ppom)) {
  cat("  PPOM diagnostics for Model 2...\n")
  residuals_mean_ppom <- calculate_pom_residuals(model2_ppom, verbose = FALSE)
  diag_plot_mean_ppom <- plot_pom_diagnostics(
    pom_result = model2_ppom,
    residuals_tb = residuals_mean_ppom,
    plot_title = "Model 2 PPOM: Awareness Mean Index"
  )
  forest_plot_mean_ppom <- plot_pom_coefficients(
    pom_result = model2_ppom,
    plot_title = "Model 2 PPOM: Awareness Mean Index"
  )
}

cat("  Diagnostics completed.\n\n")

# ==============================================================================
# 4.3e Generate PDF Report with Diagnostics
# ==============================================================================

cat("4.3e Generating comprehensive PDF report...\n")

# NOTE: Current PDF only shows POM models
# TODO: Expand to include PPOM models if fitted (pages 7-14)

# Create PDF file
pdf_file <- file.path(rq2_dir, "RQ2_POM_diagnostics.pdf")
pdf(pdf_file, width = 11, height = 8.5, onefile = TRUE)

# ==============================================================================
# MODEL 1: SEPARATE AWARENESS ITEMS (Pages 1-3)
# ==============================================================================

# PAGE 1: MODEL 1 - COEFFICIENTS TABLE
grid.newpage()
grid.text("Model 1: Separate Awareness Items",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Regression Coefficients Table",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

# Model fit statistics
y_pos <- 0.88
grid.text("Model Fit Statistics:", x = 0.1, y = y_pos, just = "left",
  gp = gpar(fontsize = 11, fontface = "bold"))
y_pos <- y_pos - 0.03
grid.text(paste0("AIC: ", round(model1_items$model_stats$aic, 2),
                 "  |  BIC: ", round(model1_items$model_stats$bic, 2),
                 "  |  Log-Likelihood: ", round(model1_items$model_stats$log_likelihood, 2),
                 "  |  N = ", model1_items$model_stats$n_obs),
  x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

# Full coefficients table
y_pos <- y_pos - 0.04
coef_display_1 <- model1_items$coefficients %>%
  mutate(
    log_odds = sprintf("%.3f", log_odds),
    std_error = sprintf("%.3f", std_error),
    p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.4f", p_value)),
    odds_ratio = sprintf("%.3f", odds_ratio),
    ci_95 = paste0("[", sprintf("%.3f", ci_lower_or), ", ", sprintf("%.3f", ci_upper_or), "]")
  ) %>%
  select(variable, log_odds, std_error, odds_ratio, ci_95, p_value)

coef_table_1 <- tableGrob(coef_display_1, rows = NULL,
  theme = ttheme_minimal(base_size = 9))
vp1 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.75)
pushViewport(vp1)
grid.draw(coef_table_1)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# PAGE 2: MODEL 1 - FOREST PLOT
grid.newpage()
grid.text("Model 1: Separate Awareness Items",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Coefficient Forest Plot (Odds Ratios)",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

vp2 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.85)
pushViewport(vp2)
print(forest_plot_items_pom, newpage = FALSE)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# PAGE 3: MODEL 1 - DIAGNOSTIC PLOTS
grid.newpage()
grid.text("Model 1: Separate Awareness Items",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Diagnostic Plots (4-Panel)",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

vp3 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.85)
pushViewport(vp3)
grid.draw(diag_plot_items_pom)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# ==============================================================================
# MODEL 2: AWARENESS MEAN INDEX (Pages 4-6)
# ==============================================================================

# PAGE 4: MODEL 2 - COEFFICIENTS TABLE
grid.newpage()
grid.text("Model 2: Awareness Mean Index",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Regression Coefficients Table",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

# Model fit statistics
y_pos <- 0.88
grid.text("Model Fit Statistics:", x = 0.1, y = y_pos, just = "left",
  gp = gpar(fontsize = 11, fontface = "bold"))
y_pos <- y_pos - 0.03
grid.text(paste0("AIC: ", round(model2_mean$model_stats$aic, 2),
                 "  |  BIC: ", round(model2_mean$model_stats$bic, 2),
                 "  |  Log-Likelihood: ", round(model2_mean$model_stats$log_likelihood, 2),
                 "  |  N = ", model2_mean$model_stats$n_obs),
  x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

# Full coefficients table
y_pos <- y_pos - 0.04
coef_display_2 <- model2_mean$coefficients %>%
  mutate(
    log_odds = sprintf("%.3f", log_odds),
    std_error = sprintf("%.3f", std_error),
    p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.4f", p_value)),
    odds_ratio = sprintf("%.3f", odds_ratio),
    ci_95 = paste0("[", sprintf("%.3f", ci_lower_or), ", ", sprintf("%.3f", ci_upper_or), "]")
  ) %>%
  select(variable, log_odds, std_error, odds_ratio, ci_95, p_value)

coef_table_2 <- tableGrob(coef_display_2, rows = NULL,
  theme = ttheme_minimal(base_size = 9))
vp4 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.75)
pushViewport(vp4)
grid.draw(coef_table_2)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# PAGE 5: MODEL 2 - FOREST PLOT
grid.newpage()
grid.text("Model 2: Awareness Mean Index",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Coefficient Forest Plot (Odds Ratios)",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

vp5 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.85)
pushViewport(vp5)
print(forest_plot_mean_pom, newpage = FALSE)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# PAGE 6: MODEL 2 - DIAGNOSTIC PLOTS
grid.newpage()
grid.text("Model 2: Awareness Mean Index",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Diagnostic Plots (4-Panel)",
  x = 0.5, y = 0.94, gp = gpar(fontsize = 11, col = "gray30"))
grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92), gp = gpar(lwd = 2))

vp6 <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.85)
pushViewport(vp6)
grid.draw(diag_plot_mean_pom)
popViewport()

grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  x = 0.5, y = 0.02, gp = gpar(fontsize = 8, col = "gray50"))

# Close PDF
dev.off()

cat("  ✓ RQ2_POM_diagnostics.pdf (6 pages: 3 per model)\n\n")

# ==============================================================================
# 4.4 Final Decision Flag
# ==============================================================================

cat("4.4 Creating final decision flag...\n")

rq2_awareness_mean_ok_overall <- (rq1_alpha$total$raw_alpha >= 0.7) & (delta_p_max_cross <= 0.03)

cat("  Cronbach's alpha:", round(rq1_alpha$total$raw_alpha, 3), "(threshold: 0.70)\n")
cat("  Delta_p_max (cross-model):", round(delta_p_max_cross, 4), "(threshold: 0.03)\n")
cat("  --> Final decision: awareness_mean is",
    ifelse(rq2_awareness_mean_ok_overall, "APPROVED", "NOT APPROVED"),
    "for downstream use\n\n")

# Store coefficients for reporting (use final models)
awareness_items_coefs <- model1_final$coefficients %>%
  filter(grepl("awareness", variable))

awareness_mean_coef <- model2_final$coefficients %>%
  filter(variable == "awareness_mean")

# ==============================================================================
# 4.5 Generate Markdown Output
# ==============================================================================

cat("4.5 Generating markdown report...\n")

md_file <- file.path(rq2_dir, "RQ2_awareness_support.md")
md_content <- c(
  "# RQ2: Using Awareness as Associational Predictor",
  "",
  "## Overview",
  "",
  paste0("- **Analysis date**: ", Sys.Date()),
  paste0("- **Sample**: Treatment group with complete data (N = ", nrow(rq2_data), ")"),
  "- **Outcome**: Support for nuclear strike on Russia (ordinal, 1-5)",
  "- **Predictor of interest**: Nuclear winter awareness (from RQ1)",
  "",
  "## Data Assembly",
  "",
  "Analysis restricted to treatment group respondents (shown infographic) with complete awareness and outcome data.",
  "",
  paste0("- **Complete cases**: ", nrow(rq2_data)),
  paste0("- **Awareness mean**: ", round(awareness_summary$mean, 2), " (SD = ", round(awareness_summary$sd, 2), ")"),
  "",
  "## Distribution & Monotonicity Check",
  "",
  "### Support by Awareness Quartile",
  "",
  "| Quartile | N | Mean Support | SD |",
  "|----------|---|--------------|-----|",
  paste0("| ", support_by_quartile$awareness_quartile, " | ",
         support_by_quartile$n, " | ",
         round(support_by_quartile$mean_support, 3), " | ",
         round(support_by_quartile$sd_support, 3), " |"),
  "",
  "## Model Comparison: Items vs Mean Index",
  "",
  "### Model 1: Separate Awareness Items",
  "",
  "**Formula**: support ~ 1980s + recent_academic + recent_media + age + sex + ethnicity + political_affiliation + employment_status + student_status",
  "",
  paste0("- **AIC**: ", round(model1_items$model_stats$aic, 2)),
  paste0("- **BIC**: ", round(model1_items$model_stats$bic, 2)),
  paste0("- **Log-Likelihood**: ", round(model1_items$model_stats$log_likelihood, 2)),
  paste0("- **N predictors**: ", model1_items$model_stats$n_predictors),
  "",
  "#### Full Coefficients Table (Model 1)",
  "",
  "| Variable | Log Odds | SE | OR | 95% CI | p-value |",
  "|----------|----------|-----|-----|--------|---------|",
  apply(model1_items$coefficients, 1, function(row) {
    paste0("| ", row["variable"], " | ",
           sprintf("%.3f", as.numeric(row["log_odds"])), " | ",
           sprintf("%.3f", as.numeric(row["std_error"])), " | ",
           sprintf("%.3f", as.numeric(row["odds_ratio"])), " | ",
           sprintf("%.3f-%.3f", as.numeric(row["ci_lower_or"]), as.numeric(row["ci_upper_or"])), " | ",
           sprintf("%.4f", as.numeric(row["p_value"])), " |")
  }),
  "",
  "### Model 2: Awareness Mean Index",
  "",
  "**Formula**: support ~ awareness_mean + age + sex + ethnicity + political_affiliation + employment_status + student_status",
  "",
  paste0("- **AIC**: ", round(model2_mean$model_stats$aic, 2)),
  paste0("- **BIC**: ", round(model2_mean$model_stats$bic, 2)),
  paste0("- **Log-Likelihood**: ", round(model2_mean$model_stats$log_likelihood, 2)),
  paste0("- **N predictors**: ", model2_mean$model_stats$n_predictors),
  "",
  "#### Full Coefficients Table (Model 2)",
  "",
  "| Variable | Log Odds | SE | OR | 95% CI | p-value |",
  "|----------|----------|-----|-----|--------|---------|",
  apply(model2_mean$coefficients, 1, function(row) {
    paste0("| ", row["variable"], " | ",
           sprintf("%.3f", as.numeric(row["log_odds"])), " | ",
           sprintf("%.3f", as.numeric(row["std_error"])), " | ",
           sprintf("%.3f", as.numeric(row["odds_ratio"])), " | ",
           sprintf("%.3f-%.3f", as.numeric(row["ci_lower_or"]), as.numeric(row["ci_upper_or"])), " | ",
           sprintf("%.4f", as.numeric(row["p_value"])), " |")
  }),
  "",
  "### Model Diagnostics Summary",
  "",
  "#### Residual Statistics",
  "",
  "**Model 1 (Separate Items):**",
  "",
  paste0("- Deviance residuals range: [",
         sprintf("%.2f", min(residuals_items_pom$deviance_residual)), ", ",
         sprintf("%.2f", max(residuals_items_pom$deviance_residual)), "]"),
  paste0("- Pearson residuals range: [",
         sprintf("%.2f", min(residuals_items_pom$pearson_residual)), ", ",
         sprintf("%.2f", max(residuals_items_pom$pearson_residual)), "]"),
  "",
  "**Model 2 (Awareness Mean):**",
  "",
  paste0("- Deviance residuals range: [",
         sprintf("%.2f", min(residuals_mean_pom$deviance_residual)), ", ",
         sprintf("%.2f", max(residuals_mean_pom$deviance_residual)), "]"),
  paste0("- Pearson residuals range: [",
         sprintf("%.2f", min(residuals_mean_pom$pearson_residual)), ", ",
         sprintf("%.2f", max(residuals_mean_pom$pearson_residual)), "]"),
  "",
  "**Note**: Full diagnostic plots (residuals vs fitted, Q-Q plots, scale-location, observed vs predicted) and coefficient forest plots are available in `RQ2_POM_diagnostics.pdf`.",
  "",
  "### Predicted Probability Comparison",
  "",
  paste0("- **Maximum probability difference (Δp_max)**: ", round(delta_p_max_cross, 4)),
  paste0("- **Threshold for parsimony**: 0.03"),
  paste0("- **Decision**: ", ifelse(delta_p_max_cross <= 0.03,
                                    "Mean index is acceptable (Δp ≤ 0.03)",
                                    "Use separate items (Δp > 0.03)")),
  "",
  "## Final Decision Flag",
  "",
  "### Criteria for Using Awareness Mean Index Downstream",
  "",
  "1. **Internal consistency**: Cronbach's α ≥ 0.70",
  paste0("   - RQ1 result: α = ", round(rq1_alpha$total$raw_alpha, 3),
         ifelse(rq1_alpha$total$raw_alpha >= 0.7, " ✓", " ✗")),
  "",
  "2. **Model parsimony**: Δp_max ≤ 0.03",
  paste0("   - RQ2 result: Δp = ", round(delta_p_max_cross, 4),
         ifelse(delta_p_max_cross <= 0.03, " ✓", " ✗")),
  "",
  paste0("### **Overall Decision: ", toupper(ifelse(rq2_awareness_mean_ok_overall,
                                                     "Approved",
                                                     "Not Approved")), "**"),
  "",
  ifelse(rq2_awareness_mean_ok_overall,
         "The awareness mean index meets both criteria and is approved for use in RQ5 exploratory analyses.",
         "The awareness mean index does not meet all criteria. Consider using separate awareness items in downstream analyses."),
  "",
  "## Interpretation",
  "",
  ifelse(awareness_mean_coef$p_value < 0.05,
         paste0("Awareness of nuclear winter is significantly associated with support for nuclear retaliation ",
                "(OR = ", round(awareness_mean_coef$odds_ratio, 3), ", p = ",
                sprintf("%.4f", awareness_mean_coef$p_value), "). ",
                ifelse(awareness_mean_coef$odds_ratio > 1,
                       "Higher awareness is associated with greater support.",
                       "Higher awareness is associated with lower support.")),
         paste0("Awareness of nuclear winter is not significantly associated with support for nuclear retaliation ",
                "(OR = ", round(awareness_mean_coef$odds_ratio, 3), ", p = ",
                sprintf("%.4f", awareness_mean_coef$p_value), ").")),
  "",
  "**Note**: This is an associational analysis within the treatment group only. Causal claims require comparison with the control group (see RQ3).",
  "",
  "## Covariates Included",
  "",
  paste0("- ", paste(covariates, collapse = ", ")),
  "",
  "---",
  paste0("*Generated: ", Sys.time(), "*")
)

writeLines(md_content, md_file)
cat("  ✓ RQ2_awareness_support.md\n\n")

# ==============================================================================
# Summary and Next Steps
# ==============================================================================

cat("===============================================================================\n")
cat("RQ2 ANALYSIS COMPLETE\n")
cat("===============================================================================\n\n")

cat("Results summary:\n")
cat("  - Sample size:", nrow(rq2_data), "\n")
cat("  - Model 1 final type:", model1_final_type, "\n")
cat("  - Model 2 final type:", model2_final_type, "\n")
cat("  - Awareness mean → support OR:", round(awareness_mean_coef$odds_ratio, 3),
    "(p =", sprintf("%.4f", awareness_mean_coef$p_value), ")\n")
cat("  - Delta_p_max (cross-model):", round(delta_p_max_cross, 4), "\n")
cat("  - Awareness mean approved for downstream use:",
    ifelse(rq2_awareness_mean_ok_overall, "YES", "NO"), "\n\n")

cat("Output files:\n")
cat("  - RQ2_awareness_support.md (comprehensive markdown summary)\n")
cat("  - RQ2_POM_diagnostics.pdf (diagnostic report)\n")
cat("    NOTE: PDF currently shows POM models only. PPOM pages not yet implemented.\n\n")

cat("Output location:", rq2_dir, "\n\n")

cat("Note: rq2_awareness_mean_ok_overall flag is now available for RQ5.\n")
cat("===============================================================================\n\n")
