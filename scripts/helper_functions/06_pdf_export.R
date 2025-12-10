# ==============================================================================
# 06_pdf_export.R
# PDF report generation and export functions
# ==============================================================================

create_timestamped_output_dir <- function(base_dir = "outputs") {
  # Create timestamped subdirectory for regression results
  # Matches pattern: YYYY-MM-DD HH.MM.SS.microseconds

  timestamp <- format(Sys.time(), "%Y-%m-%d %H.%M.%OS6")
  output_dir <- file.path(base_dir, timestamp)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(output_dir)
}

# PDF GENERATION HELPER FUNCTIONS
# These functions reduce repetition in PDF report generation

pdf_new_page_with_title <- function(title, subtitle = NULL, add_line = TRUE) {
  # Start a new PDF page with title and optional subtitle
  grid.newpage()

  grid.text(title, x = 0.5, y = 0.95,
            gp = gpar(fontsize = 14, fontface = "bold"))

  if (!is.null(subtitle)) {
    grid.text(subtitle, x = 0.5, y = 0.91,
              gp = gpar(fontsize = 12, col = "gray30"))
  }

  if (add_line) {
    line_y <- if (!is.null(subtitle)) 0.88 else 0.91
    grid.lines(x = c(0.1, 0.9), y = c(line_y, line_y),
               gp = gpar(lwd = 2))
  }

  # Return starting y position for content
  return(if (!is.null(subtitle)) 0.83 else 0.86)
}

pdf_add_section_header <- function(text, y_pos, x = 0.1) {
  # Add a section header with underline
  grid.text(text, x = x, y = y_pos,
            just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos),
             gp = gpar(lwd = 1, col = "gray60"))

  # Return next content position
  return(y_pos - 0.04)
}

pdf_add_key_value <- function(key, value, y_pos, x = 0.12,
                              bold_key = FALSE, fontsize = 10) {
  # Add a key-value pair
  text <- paste0(key, ": ", value)
  font_face <- if (bold_key) "bold" else "plain"

  grid.text(text, x = x, y = y_pos, just = "left",
            gp = gpar(fontsize = fontsize, fontface = font_face))

  # Return next position
  return(y_pos - 0.03)
}

pdf_add_table <- function(data, y_pos, theme = ttheme_minimal(base_size = 9),
                         height = 0.15, width = 0.8) {
  # Add a table to the current page
  table_grob <- tableGrob(data, rows = NULL, theme = theme)

  vp <- viewport(x = 0.5, y = y_pos - height / 2,
                width = width, height = height)
  pushViewport(vp)
  grid.draw(table_grob)
  popViewport()

  # Return next position
  return(y_pos - height - 0.04)
}

pdf_add_plot <- function(plot_obj, y_pos, height = 0.4, width = 0.8) {
  # Add a plot to the current page
  vp <- viewport(x = 0.5, y = y_pos - height / 2,
                width = width, height = height)
  pushViewport(vp)
  print(plot_obj)
  popViewport()

  # Return next position
  return(y_pos - height - 0.04)
}

pdf_add_footer <- function(y_pos = 0.05) {
  # Add standard footer with timestamp
  grid.text(paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            x = 0.5, y = y_pos,
            gp = gpar(fontsize = 8, col = "gray50"))
}

# EXPORT PDF REPORT
export_pdf_report <- function(results, output_dir = NULL,
                              filename = NULL, verbose = TRUE) {
  # Export comprehensive PDF report of POM analysis results
  #
  # Args:
  #   results: Results object from run_single_pom_analysis()
  #   output_dir: Directory for PDF output (if NULL, creates timestamped dir)
  #   filename: Optional custom filename (default: {model_id}_report.pdf)
  #   verbose: Print progress messages
  #
  # Returns:
  #   File path of exported PDF

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for PDF reports. Install with: install.packages('gridExtra')")
  }
  library(gridExtra)
  library(grid)

  # Create timestamped output directory if not specified
  if (is.null(output_dir)) {
    output_dir <- create_timestamped_output_dir("outputs")
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Set filename
  model_id <- results$model_info$model_id
  if (is.null(filename)) {
    filename <- paste0(model_id, "_report.pdf")
  }
  file_path <- file.path(output_dir, filename)

  if (verbose) {
    cat("\n========================================\n")
    cat("GENERATING PDF REPORT:", model_id, "\n")
    cat("========================================\n\n")
  }

  # Open PDF device (suppress display)
  pdf(file_path, width = 8.5, height = 11, onefile = TRUE)

  # Build full model specification
  formula_str <- deparse(results$formula)
  if (length(formula_str) > 1) {
    formula_str <- paste(formula_str, collapse = " ")
  }

  # PAGE 1: TITLE AND MODEL INFORMATION ----
  grid.newpage()

  # Build title from outcome and predictors
  # Use outcome_label (human-readable) if available, otherwise use outcome_var
  outcome_name <- if (!is.null(results$data_prep$outcome_label)) {
    results$data_prep$outcome_label
  } else {
    results$model_info$outcome_var
  }
  predictor_names <- paste(results$data_prep$predictors, collapse = " + ")
  title_text <- paste(outcome_name, "~", predictor_names)

  # Title: Model specification formula
  grid.text(title_text,
    x = 0.5, y = 0.95,
    gp = gpar(fontsize = 14, fontface = "bold"))

  # Model ID subtitle
  grid.text(paste("Model ID:", model_id),
    x = 0.5, y = 0.91,
    gp = gpar(fontsize = 12, col = "gray30"))

  # Horizontal line
  grid.lines(x = c(0.1, 0.9), y = c(0.88, 0.88),
    gp = gpar(lwd = 2))

  # Model Specification section
  y_pos <- 0.83
  grid.text("Model Specification", x = 0.1, y = y_pos,
    just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

  y_pos <- y_pos - 0.04
  grid.text(paste("Data Source:", results$model_info$data_source),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Outcome Variable:", results$model_info$outcome_var),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  # List all predictors explicitly (no summarizing)
  predictor_list <- paste(results$data_prep$predictors, collapse = ", ")
  grid.text("Predictors:", x = 0.12, y = y_pos, just = "left",
    gp = gpar(fontsize = 10, fontface = "bold"))
  y_pos <- y_pos - 0.025
  # Wrap long predictor list if needed
  max_width <- 70
  if (nchar(predictor_list) > max_width) {
    words <- strsplit(predictor_list, ", ")[[1]]
    lines <- character()
    current_line <- ""
    for (word in words) {
      test_line <- if (current_line == "") word else paste(current_line, word, sep = ", ")
      if (nchar(test_line) > max_width && current_line != "") {
        lines <- c(lines, current_line)
        current_line <- word
      } else {
        current_line <- test_line
      }
    }
    if (current_line != "") lines <- c(lines, current_line)

    for (line in lines) {
      grid.text(line, x = 0.15, y = y_pos, just = "left", gp = gpar(fontsize = 9))
      y_pos <- y_pos - 0.025
    }
  } else {
    grid.text(predictor_list, x = 0.15, y = y_pos, just = "left", gp = gpar(fontsize = 9))
    y_pos <- y_pos - 0.03
  }

  y_pos <- y_pos - 0.01

  # Interaction terms (if specified)
  if (!is.null(results$model_info$interaction_terms) &&
    !is.na(results$model_info$interaction_terms) &&
    nchar(trimws(results$model_info$interaction_terms)) > 0) {
    y_pos <- y_pos - 0.03
    grid.text("Interaction Terms:", x = 0.12, y = y_pos, just = "left",
      gp = gpar(fontsize = 10, fontface = "bold"))
    y_pos <- y_pos - 0.025
    grid.text(results$model_info$interaction_terms, x = 0.15, y = y_pos, just = "left",
      gp = gpar(fontsize = 9, col = "darkblue"))
    y_pos <- y_pos - 0.01
  }

  # Filter condition (if applied)
  if (!is.null(results$filter_info) && results$filter_info$filter_applied) {
    y_pos <- y_pos - 0.03
    grid.text("Filter Condition:", x = 0.12, y = y_pos, just = "left",
      gp = gpar(fontsize = 10, fontface = "bold"))
    y_pos <- y_pos - 0.025
    grid.text(results$filter_info$filter_condition, x = 0.15, y = y_pos, just = "left",
      gp = gpar(fontsize = 9, col = "darkblue"))
    y_pos <- y_pos - 0.025
    grid.text(paste("Excluded:", results$filter_info$n_excluded, "observations"),
      x = 0.15, y = y_pos, just = "left", gp = gpar(fontsize = 9, col = "gray40"))
    y_pos <- y_pos - 0.01
  }

  y_pos <- y_pos - 0.03
  status_color <- ifelse(results$status == "SUCCESS", "darkgreen", "darkred")
  grid.text(paste("Status:", results$status),
    x = 0.12, y = y_pos, just = "left",
    gp = gpar(fontsize = 10, col = status_color, fontface = "bold"))

  # Sample Size section
  y_pos <- y_pos - 0.06
  grid.text("Sample Size", x = 0.1, y = y_pos,
    just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

  y_pos <- y_pos - 0.04
  grid.text(paste("Original N:", format(results$data_prep$n_original, big.mark = ",")),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Final N:", format(results$data_prep$n_final, big.mark = ",")),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  pct_dropped <- round(100 * results$data_prep$n_dropped / results$data_prep$n_original, 1)
  grid.text(paste("Dropped:", format(results$data_prep$n_dropped, big.mark = ","),
    paste0("(", pct_dropped, "%)")),
  x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  # Model Fit Statistics section
  y_pos <- y_pos - 0.06
  grid.text("Model Fit Statistics", x = 0.1, y = y_pos,
    just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

  y_pos <- y_pos - 0.04
  grid.text(paste("Link Function:", results$pom_result$model_stats$link_function),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("AIC:", round(results$pom_result$model_stats$aic, 2)),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("BIC:", round(results$pom_result$model_stats$bic, 2)),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Log-Likelihood:", round(results$pom_result$model_stats$log_likelihood, 2)),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Number of Predictors:", results$pom_result$model_stats$n_predictors),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Confidence Level:", results$pom_result$model_stats$confidence_level),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  # Link comparison if multiple links tested
  if (nrow(results$link_comparison) > 1) {
    y_pos <- y_pos - 0.06
    grid.text("Link Function Comparison", x = 0.1, y = y_pos,
      just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
    y_pos <- y_pos - 0.03
    grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

    y_pos <- y_pos - 0.04
    link_table <- tableGrob(results$link_comparison,
      rows = NULL,
      theme = ttheme_minimal(base_size = 9))
    grid.draw(link_table)
    vp <- viewport(x = 0.5, y = y_pos - 0.08, width = 0.8, height = 0.15)
    pushViewport(vp)
    grid.draw(link_table)
    popViewport()
  }

  # Footer
  grid.text(paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    x = 0.5, y = 0.05,
    gp = gpar(fontsize = 8, col = "gray50"))

  # PAGE 2: VIF DIAGNOSTICS (if available) ----
  if (!is.null(results$vif_results)) {
    grid.newpage()

    # Title
    grid.text("Multicollinearity Diagnostics", x = 0.5, y = 0.95,
      gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text("Variance Inflation Factors (VIF)", x = 0.5, y = 0.92,
      gp = gpar(fontsize = 11, col = "gray30"))
    grid.lines(x = c(0.05, 0.95), y = c(0.90, 0.90), gp = gpar(lwd = 2))

    # Format VIF for display
    vif_display <- results$vif_results %>%
      mutate(
        vif = sprintf("%.2f", vif),
        tolerance = sprintf("%.3f", tolerance),
        interpretation = case_when(
          as.numeric(vif) < 5 ~ "Low",
          as.numeric(vif) < 10 ~ "Moderate",
          TRUE ~ "High"
        )
      )

    # Create table
    vif_table <- tableGrob(vif_display,
      rows = NULL,
      theme = ttheme_minimal(
        base_size = 10,
        core = list(fg_params = list(hjust = 0, x = 0.05)),
        colhead = list(fg_params = list(fontface = "bold"))
    ))

    # Draw table
    vp <- viewport(x = 0.5, y = 0.65, width = 0.80, height = 0.50)
    pushViewport(vp)
    grid.draw(vif_table)
    popViewport()

    # Interpretation guide
    y_pos <- 0.30
    grid.text("VIF Interpretation Guide:", x = 0.15, y = y_pos,
      just = "left", gp = gpar(fontsize = 11, fontface = "bold"))

    y_pos <- y_pos - 0.04
    grid.text("VIF < 5: Low multicollinearity (acceptable)",
      x = 0.17, y = y_pos, just = "left", gp = gpar(fontsize = 10))

    y_pos <- y_pos - 0.03
    grid.text("VIF 5-10: Moderate multicollinearity (monitor closely)",
      x = 0.17, y = y_pos, just = "left", gp = gpar(fontsize = 10))

    y_pos <- y_pos - 0.03
    grid.text("VIF > 10: High multicollinearity (problematic)",
      x = 0.17, y = y_pos, just = "left", gp = gpar(fontsize = 10))

    # Footer
    grid.text(paste("Model:", model_id),
      x = 0.5, y = 0.05,
      gp = gpar(fontsize = 8, col = "gray50"))
  }

  # PAGE 3: DIAGNOSTIC PLOTS ----
  if (!is.null(results$diagnostic_plot)) {
    grid.newpage()

    # Title
    grid.text("Model Diagnostic Plots", x = 0.5, y = 0.97,
      gp = gpar(fontsize = 16, fontface = "bold"))
    grid.lines(x = c(0.05, 0.95), y = c(0.95, 0.95), gp = gpar(lwd = 2))

    # Draw the 2x2 diagnostic plot using grid.draw
    vp <- viewport(x = 0.5, y = 0.48, width = 0.90, height = 0.85)
    pushViewport(vp)
    grid.draw(results$diagnostic_plot)
    popViewport()

    # Footer
    grid.text(paste("Model:", model_id),
      x = 0.5, y = 0.02,
      gp = gpar(fontsize = 8, col = "gray50"))
  }

  # PAGE 4: COEFFICIENTS TABLE ----
  grid.newpage()

  # Title
  grid.text("Regression Coefficients", x = 0.5, y = 0.95,
    gp = gpar(fontsize = 16, fontface = "bold"))
  grid.text("Log-Odds, Odds Ratios, and Confidence Intervals",
    x = 0.5, y = 0.92,
    gp = gpar(fontsize = 11, col = "gray30"))
  grid.lines(x = c(0.05, 0.95), y = c(0.90, 0.90), gp = gpar(lwd = 2))

  # Format coefficients for display
  # Create dynamic CI column name based on actual confidence level
  conf_level <- results$pom_result$model_stats$confidence_level
  ci_col_name <- paste0("ci_", round(conf_level * 100))

  coef_display <- results$pom_result$coefficients %>%
    mutate(
      log_odds = sprintf("%.3f", log_odds),
      std_error = sprintf("%.3f", std_error),
      p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value)),
      odds_ratio = sprintf("%.3f", odds_ratio),
      !!ci_col_name := paste0("[", sprintf("%.3f", ci_lower_or), ", ",
        sprintf("%.3f", ci_upper_or), "]")
    ) %>%
    select(variable, log_odds, std_error, p_value, odds_ratio, !!ci_col_name)

  # Create table
  coef_table <- tableGrob(coef_display,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 9,
      core = list(fg_params = list(hjust = 0, x = 0.05)),
      colhead = list(fg_params = list(fontface = "bold"))
  ))

  # Draw table
  vp <- viewport(x = 0.5, y = 0.50, width = 0.90, height = 0.75)
  pushViewport(vp)
  grid.draw(coef_table)
  popViewport()

  # Base categories note (if categorical variables present)
  base_categories <- get_base_categories(results$pom_result)
  base_note <- format_base_categories_note(base_categories)
  if (!is.null(base_note)) {
    grid.text(base_note,
      x = 0.5, y = 0.12,
      gp = gpar(fontsize = 9, col = "gray40", fontface = "italic"))
  }

  # Footer
  grid.text(paste("Model:", model_id),
    x = 0.5, y = 0.05,
    gp = gpar(fontsize = 8, col = "gray50"))

  # PAGE 5: COEFFICIENT FOREST PLOT ----
  if (!is.null(results$pom_result)) {
    grid.newpage()

    # Title
    grid.text("Coefficient Forest Plot", x = 0.5, y = 0.97,
      gp = gpar(fontsize = 16, fontface = "bold"))
    grid.lines(x = c(0.05, 0.95), y = c(0.95, 0.95), gp = gpar(lwd = 2))

    # Create forest plot
    forest_plot <- plot_pom_coefficients(
      pom_result = results$pom_result,
      plot_title = NULL  # Will use default title from function
    )

    # Draw forest plot
    vp <- viewport(x = 0.5, y = 0.48, width = 0.85, height = 0.75)
    pushViewport(vp)
    print(forest_plot, newpage = FALSE)
    popViewport()

    # Footer
    grid.text(paste("Model:", model_id),
      x = 0.5, y = 0.02,
      gp = gpar(fontsize = 8, col = "gray50"))
  }

  # Close PDF device
  dev.off()

  if (verbose) {
    cat("PDF report saved to:", file_path, "\n")
    cat("========================================\n\n")
  }

  # Open PDF automatically
  if (Sys.info()["sysname"] == "Linux") {
    system(paste("xdg-open", shQuote(file_path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(file_path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Windows") {
    shell.exec(file_path)
  }

  return(file_path)
}

# EXPORT MULTI-CATEGORY PDF REPORT
export_multi_category_pdf_report <- function(split_results, config_row,
                                             output_dir = NULL,
                                             filename = NULL, verbose = TRUE) {
  # Export PDF report with cover page + results for each category
  #
  # Args:
  #   split_results: Results object from run_pom_with_splits()
  #   config_row: Original config row
  #   output_dir: Directory for PDF output
  #   filename: Optional custom filename
  #   verbose: Print progress messages
  #
  # Returns:
  #   File path of exported PDF

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for PDF reports")
  }
  library(gridExtra)
  library(grid)

  # Create output directory
  if (is.null(output_dir)) {
    output_dir <- create_timestamped_output_dir("outputs")
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Set filename
  model_id <- config_row$model_id
  if (is.null(filename)) {
    filename <- paste0(model_id, "_split_report.pdf")
  }
  file_path <- file.path(output_dir, filename)

  if (verbose) {
    cat("\n========================================\n")
    cat("GENERATING MULTI-CATEGORY PDF REPORT:", model_id, "\n")
    cat("========================================\n\n")
  }

  # Open PDF device
  pdf(file_path, width = 8.5, height = 11, onefile = TRUE)

  # COVER PAGE
  grid.newpage()

  # Title
  grid.text(paste("Split Category Analysis:", config_row$model_label),
    x = 0.5, y = 0.95,
    gp = gpar(fontsize = 14, fontface = "bold"))

  # Model ID
  grid.text(paste("Model ID:", model_id),
    x = 0.5, y = 0.91,
    gp = gpar(fontsize = 12, col = "gray30"))

  grid.lines(x = c(0.1, 0.9), y = c(0.88, 0.88), gp = gpar(lwd = 2))

  # Model specification
  y_pos <- 0.83
  grid.text("Model Specification", x = 0.1, y = y_pos,
    just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

  y_pos <- y_pos - 0.04
  grid.text(paste("Data Source:", config_row$data_source),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text(paste("Outcome Variable:", config_row$outcome_var),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  y_pos <- y_pos - 0.03
  grid.text("Predictors:",
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10, fontface = "bold"))
  y_pos <- y_pos - 0.025
  predictor_list <- config_row$predictors
  grid.text(predictor_list,
    x = 0.15, y = y_pos, just = "left", gp = gpar(fontsize = 9))

  # Interaction terms (if specified)
  if (!is.null(config_row$interaction_terms) &&
    !is.na(config_row$interaction_terms) &&
    nchar(trimws(config_row$interaction_terms)) > 0) {
    y_pos <- y_pos - 0.03
    grid.text("Interaction Terms:", x = 0.12, y = y_pos, just = "left",
      gp = gpar(fontsize = 10, fontface = "bold"))
    y_pos <- y_pos - 0.025
    grid.text(config_row$interaction_terms, x = 0.15, y = y_pos, just = "left",
      gp = gpar(fontsize = 9, col = "darkblue"))
  }

  # Filter condition (check first result for filter info)
  first_result <- split_results$results[[split_results$categories[1]]]
  if (!is.null(first_result$filter_info) && first_result$filter_info$filter_applied) {
    y_pos <- y_pos - 0.03
    grid.text("Filter Condition:", x = 0.12, y = y_pos, just = "left",
      gp = gpar(fontsize = 10, fontface = "bold"))
    y_pos <- y_pos - 0.025
    grid.text(first_result$filter_info$filter_condition, x = 0.15, y = y_pos, just = "left",
      gp = gpar(fontsize = 9, col = "darkblue"))
    y_pos <- y_pos - 0.01
  }

  y_pos <- y_pos - 0.04
  grid.text(paste("Categories Analyzed:", length(split_results$categories)),
    x = 0.12, y = y_pos, just = "left", gp = gpar(fontsize = 10))

  # Model fit statistics table
  y_pos <- y_pos - 0.05
  grid.text("Model Fit Statistics by Category",
    x = 0.1, y = y_pos, just = "left",
    gp = gpar(fontsize = 12, fontface = "bold"))
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.1, 0.9), y = c(y_pos, y_pos), gp = gpar(lwd = 1, col = "gray60"))

  # Build fit statistics table
  fit_stats_list <- list()
  for (cat_name in split_results$categories) {
    result <- split_results$results[[cat_name]]
    if (result$status == "SUCCESS" && !is.null(result$pom_result)) {
      fit_stats_list[[cat_name]] <- data.frame(
        Category = cat_name,
        N = result$data_prep$n_final,
        Link = result$pom_result$model_stats$link_function,
        AIC = round(result$pom_result$model_stats$aic, 2),
        BIC = round(result$pom_result$model_stats$bic, 2),
        LogLik = round(result$pom_result$model_stats$log_likelihood, 2),
        stringsAsFactors = FALSE
      )
    } else {
      fit_stats_list[[cat_name]] <- data.frame(
        Category = cat_name,
        N = NA,
        Link = "FAILED",
        AIC = NA,
        BIC = NA,
        LogLik = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  fit_stats_df <- do.call(rbind, fit_stats_list)

  # Draw table
  y_pos <- y_pos - 0.04
  vp_table <- viewport(x = 0.5, y = y_pos - 0.15, width = 0.80, height = 0.30)
  pushViewport(vp_table)
  grid.table(fit_stats_df, rows = NULL,
    theme = ttheme_default(base_size = 9,
      core = list(fg_params = list(hjust = 0, x = 0.1))))
  popViewport()

  # Footer
  grid.text(paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    x = 0.5, y = 0.05,
    gp = gpar(fontsize = 8, col = "gray50"))

  # NOW GENERATE PAGES FOR EACH CATEGORY
  for (cat_name in split_results$categories) {
    result <- split_results$results[[cat_name]]

    if (result$status != "SUCCESS") {
      # Error page for this category
      grid.newpage()
      grid.text(paste("Category:", cat_name),
        x = 0.5, y = 0.95,
        gp = gpar(fontsize = 14, fontface = "bold"))
      grid.text(paste("Status:", result$status),
        x = 0.5, y = 0.5,
        gp = gpar(fontsize = 12, col = "red"))
      next
    }

    # PAGE 1: VIF DIAGNOSTICS
    if (!is.null(result$vif_results)) {
      grid.newpage()

      grid.text(cat_name,
        x = 0.5, y = 0.97,
        gp = gpar(fontsize = 14, fontface = "bold"))
      grid.text("Multicollinearity Diagnostics",
        x = 0.5, y = 0.93,
        gp = gpar(fontsize = 12))
      grid.lines(x = c(0.05, 0.95), y = c(0.91, 0.91), gp = gpar(lwd = 2))

      # Format VIF table
      vif_display <- result$vif_results %>%
        mutate(
          vif = round(vif, 2),
          tolerance = round(tolerance, 3),
          interpretation = case_when(
            vif > 10 ~ "High",
            vif > 5 ~ "Moderate",
            TRUE ~ "Low"
          )
        )

      # Draw VIF table
      vp_vif <- viewport(x = 0.5, y = 0.50, width = 0.80, height = 0.70)
      pushViewport(vp_vif)
      grid.table(vif_display, rows = NULL,
        theme = ttheme_default(base_size = 9,
          core = list(fg_params = list(hjust = 0, x = 0.05))))
      popViewport()

      # VIF guide
      grid.text("VIF Interpretation Guide:",
        x = 0.1, y = 0.15, just = "left",
        gp = gpar(fontsize = 10, fontface = "bold"))
      grid.text("VIF < 5: Low multicollinearity (acceptable)",
        x = 0.12, y = 0.12, just = "left",
        gp = gpar(fontsize = 9))
      grid.text("VIF 5-10: Moderate multicollinearity (monitor closely)",
        x = 0.12, y = 0.09, just = "left",
        gp = gpar(fontsize = 9))
      grid.text("VIF > 10: High multicollinearity (problematic)",
        x = 0.12, y = 0.06, just = "left",
        gp = gpar(fontsize = 9))

      grid.text(paste("Model:", model_id, "|", cat_name),
        x = 0.5, y = 0.02,
        gp = gpar(fontsize = 8, col = "gray50"))
    }

    # PAGE 2: DIAGNOSTIC PLOTS
    if (!is.null(result$diagnostic_plot)) {
      grid.newpage()

      grid.text(cat_name,
        x = 0.5, y = 0.97,
        gp = gpar(fontsize = 14, fontface = "bold"))
      grid.text("Model Diagnostic Plots",
        x = 0.5, y = 0.93,
        gp = gpar(fontsize = 12))
      grid.lines(x = c(0.05, 0.95), y = c(0.91, 0.91), gp = gpar(lwd = 2))

      # Draw diagnostic plots
      vp_diag <- viewport(x = 0.5, y = 0.45, width = 0.90, height = 0.80)
      pushViewport(vp_diag)
      grid.draw(result$diagnostic_plot)
      popViewport()

      grid.text(paste("Model:", model_id, "|", cat_name),
        x = 0.5, y = 0.02,
        gp = gpar(fontsize = 8, col = "gray50"))
    }

    # PAGE 3: SAMPLE SIZES & REGRESSION COEFFICIENTS
    grid.newpage()

    grid.text(cat_name,
      x = 0.5, y = 0.97,
      gp = gpar(fontsize = 14, fontface = "bold"))
    grid.text("Sample Sizes & Regression Coefficients",
      x = 0.5, y = 0.93,
      gp = gpar(fontsize = 12))
    grid.lines(x = c(0.05, 0.95), y = c(0.91, 0.91), gp = gpar(lwd = 2))

    # SECTION 1: CATEGORY SAMPLE SIZES
    y_pos <- 0.87
    grid.text("Category Sample Sizes",
      x = 0.1, y = y_pos, just = "left",
      gp = gpar(fontsize = 11, fontface = "bold"))

    # Build category sizes table
    category_sizes_list <- list()

    # Get the actual data used in the regression
    regression_data <- result$data_prep$data
    predictor_vars <- result$data_prep$predictors

    for (var_name in predictor_vars) {
      if (var_name %in% names(regression_data)) {
        var_data <- regression_data[[var_name]]

        # Check if variable is categorical (character or factor)
        if (is.character(var_data) || is.factor(var_data)) {
          # Get counts for each level
          level_counts <- table(var_data, useNA = "no")

          # Add each level to the table
          for (level_name in names(level_counts)) {
            category_sizes_list[[length(category_sizes_list) + 1]] <- data.frame(
              Variable = var_name,
              Category = level_name,
              N = as.integer(level_counts[level_name]),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    # Draw category sizes table if we have categorical variables
    if (length(category_sizes_list) > 0) {
      category_sizes_df <- do.call(rbind, category_sizes_list)

      # Calculate table height based on number of rows
      num_cat_rows <- nrow(category_sizes_df)
      cat_table_height <- min(0.18, 0.03 + num_cat_rows * 0.015)

      vp_cat_sizes <- viewport(x = 0.5, y = y_pos - 0.02 - cat_table_height / 2,
        width = 0.60, height = cat_table_height)
      pushViewport(vp_cat_sizes)
      grid.table(category_sizes_df, rows = NULL,
        theme = ttheme_default(base_size = 8,
          core = list(fg_params = list(hjust = 0, x = 0.05))))
      popViewport()

      y_pos <- y_pos - 0.04 - cat_table_height
    } else {
      grid.text("No categorical variables",
        x = 0.5, y = y_pos - 0.05,
        gp = gpar(fontsize = 9, col = "gray50", fontface = "italic"))
      y_pos <- y_pos - 0.10
    }

    # SECTION 2: REGRESSION COEFFICIENTS
    y_pos <- y_pos - 0.03
    grid.text("Regression Coefficients",
      x = 0.1, y = y_pos, just = "left",
      gp = gpar(fontsize = 11, fontface = "bold"))

    # Format coefficients table
    # Create dynamic CI column name based on actual confidence level
    conf_level <- result$pom_result$model_stats$confidence_level
    ci_col_name <- paste0("ci_", round(conf_level * 100))

    coef_display <- result$pom_result$coefficients %>%
      mutate(
        log_odds = round(log_odds, 3),
        std_error = round(std_error, 3),
        p_value = ifelse(p_value < 0.001, "< 0.001",
          ifelse(p_value < 0.01, format(round(p_value, 3), nsmall = 3),
            format(round(p_value, 2), nsmall = 2))),
        odds_ratio = round(odds_ratio, 3),
        !!ci_col_name := paste0("[", round(ci_lower_or, 3), ", ", round(ci_upper_or, 3), "]")
      ) %>%
      select(variable, log_odds, std_error, p_value, odds_ratio, !!ci_col_name)

    # Calculate remaining space for coefficients table
    num_coef_rows <- nrow(coef_display)
    coef_table_height <- min(y_pos - 0.15, 0.05 + num_coef_rows * 0.02)

    # Draw coefficients table
    vp_coef <- viewport(x = 0.5, y = y_pos - 0.02 - coef_table_height / 2,
      width = 0.85, height = coef_table_height)
    pushViewport(vp_coef)
    grid.table(coef_display, rows = NULL,
      theme = ttheme_default(base_size = 8,
        core = list(fg_params = list(hjust = 0, x = 0.05))))
    popViewport()

    # Base categories note (if categorical variables present)
    base_categories <- get_base_categories(result$pom_result)
    base_note <- format_base_categories_note(base_categories)
    if (!is.null(base_note)) {
      grid.text(base_note,
        x = 0.5, y = 0.08,
        gp = gpar(fontsize = 8, col = "gray40", fontface = "italic"))
    }

    grid.text(paste("Model:", model_id, "|", cat_name),
      x = 0.5, y = 0.02,
      gp = gpar(fontsize = 8, col = "gray50"))

    # PAGE 4: COEFFICIENT FOREST PLOT
    if (!is.null(result$pom_result)) {
      grid.newpage()

      grid.text(cat_name,
        x = 0.5, y = 0.97,
        gp = gpar(fontsize = 14, fontface = "bold"))
      grid.text("Coefficient Forest Plot",
        x = 0.5, y = 0.93,
        gp = gpar(fontsize = 12))
      grid.lines(x = c(0.05, 0.95), y = c(0.91, 0.91), gp = gpar(lwd = 2))

      # Create forest plot
      forest_plot <- plot_pom_coefficients(
        pom_result = result$pom_result,
        plot_title = NULL
      )

      # Draw forest plot
      vp_forest <- viewport(x = 0.5, y = 0.45, width = 0.85, height = 0.75)
      pushViewport(vp_forest)
      print(forest_plot, newpage = FALSE)
      popViewport()

      grid.text(paste("Model:", model_id, "|", cat_name),
        x = 0.5, y = 0.02,
        gp = gpar(fontsize = 8, col = "gray50"))

      # PAGE 5: THRESHOLD-SPECIFIC COEFFICIENTS (PPOM only)
      # Check if this is a PPOM model
      if (!is.null(result$pom_result$model_type) &&
        result$pom_result$model_type == "PPOM" &&
        !is.null(result$pom_result$threshold_specific_coefs) &&
        nrow(result$pom_result$threshold_specific_coefs) > 0) {

        grid.newpage()

        grid.text(cat_name,
          x = 0.5, y = 0.97,
          gp = gpar(fontsize = 14, fontface = "bold"))
        grid.text("Threshold-Specific Coefficients (PPOM)",
          x = 0.5, y = 0.93,
          gp = gpar(fontsize = 12))
        grid.lines(x = c(0.05, 0.95), y = c(0.91, 0.91), gp = gpar(lwd = 2))

        # Add explanatory text
        grid.text("In PPOM, coefficients can vary across thresholds, relaxing the proportional odds assumption.",
          x = 0.5, y = 0.87,
          gp = gpar(fontsize = 9, col = "gray30", fontface = "italic"))

        # Create threshold-specific coefficient plot
        threshold_plot <- plot_ppom_threshold_coefficients(
          ppom_result = result$pom_result,
          plot_title = NULL
        )

        # Draw threshold plot
        vp_threshold <- viewport(x = 0.5, y = 0.42, width = 0.85, height = 0.70)
        pushViewport(vp_threshold)
        print(threshold_plot, newpage = FALSE)
        popViewport()

        grid.text(paste("Model:", model_id, "|", cat_name),
          x = 0.5, y = 0.02,
          gp = gpar(fontsize = 8, col = "gray50"))
      }
    }
  }

  # Close PDF
  dev.off()

  if (verbose) {
    cat("PDF report saved to:", file_path, "\n")
    cat("========================================\n\n")
  }

  # Open PDF automatically
  if (Sys.info()["sysname"] == "Linux") {
    system(paste("xdg-open", shQuote(file_path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(file_path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Windows") {
    shell.exec(file_path)
  }

  return(file_path)
}


cat("PDF export functions loaded successfully.\n")
