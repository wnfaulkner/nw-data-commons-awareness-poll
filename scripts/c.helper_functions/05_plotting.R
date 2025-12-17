# ==============================================================================
# 05_plotting.R
# Plotting and visualization functions
# ==============================================================================

# Function to blend two hex colors (average RGB values)
blend_colors <- function(col1, col2) {
  if (col1 == col2) return(col1)
  rgb1 <- col2rgb(col1)
  rgb2 <- col2rgb(col2)
  rgb_avg <- (rgb1 + rgb2) / 2
  return(rgb(rgb_avg[1], rgb_avg[2], rgb_avg[3], maxColorValue = 255))
}

# Function to get flow color based on connected awareness levels
get_flow_color <- function(level1, level2, level3) {
  col1 <- awareness_colors[as.character(level1)]
  col2 <- awareness_colors[as.character(level2)]
  col3 <- awareness_colors[as.character(level3)]

  # If all three are the same, use that color
  if (level1 == level2 && level2 == level3) {
    return(col1)
  }

  # Otherwise, blend colors progressively (average axis1->axis2, then average with axis3)
  blend_12 <- blend_colors(col1, col2)
  blend_all <- blend_colors(blend_12, col3)
  return(blend_all)
}

# PLOT POM DIAGNOSTICS (2x2 panel)
plot_pom_diagnostics <- function(pom_result, residuals_tb, plot_title = NULL) {
  # Create a 2x2 panel of diagnostic plots for proportional odds models
  #
  # Args:
  #   pom_result: Output from fit_pom() containing model object and stats
  #   residuals_tb: Output from calculate_pom_residuals() with residuals and predictions
  #   plot_title: Optional main title for the plot panel
  #
  # Returns:
  #   A ggplot object (combined 2x2 panel)

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for plot panels. Install with: install.packages('gridExtra')")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required for plot annotations.")
  }

  library(ggplot2)
  library(gridExtra)
  library(grid)

  # Extract model information for plot titles
  model_label <- if (!is.null(plot_title)) {
    plot_title
  } else {
    paste("POM Diagnostics:", pom_result$outcome_var)
  }

  n_obs <- nrow(residuals_tb)

  # PLOT 1: Residuals vs Fitted (Linear Predictor)
  plot1 <- ggplot(residuals_tb, aes(x = linear_predictor, y = deviance_residual)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE, color = "blue", linewidth = 0.8) +
    labs(
      title = "Residuals vs Fitted",
      x = "Linear Predictor",
      y = "Deviance Residuals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # PLOT 2: Q-Q Plot of Deviance Residuals
  # Calculate theoretical quantiles
  residuals_sorted <- sort(residuals_tb$deviance_residual)
  theoretical_quantiles <- qnorm(ppoints(length(residuals_sorted)))

  qq_data <- tibble(
    theoretical = theoretical_quantiles,
    sample = residuals_sorted
  )

  plot2 <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles (Deviance Residuals)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # PLOT 3: Scale-Location Plot (sqrt of absolute standardized residuals)
  residuals_tb <- residuals_tb %>%
    mutate(
      sqrt_abs_std_residual = sqrt(abs(deviance_residual / sd(deviance_residual)))
    )

  plot3 <- ggplot(residuals_tb, aes(x = linear_predictor, y = sqrt_abs_std_residual)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, color = "blue", linewidth = 0.8) +
    labs(
      title = "Scale-Location",
      x = "Linear Predictor",
      y = expression(sqrt("|Standardized Residuals|"))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9)
    )

  # PLOT 4: Observed vs Predicted Probabilities
  # Calculate average predicted probability for observed category
  obs_pred_prob <- numeric(n_obs)
  for (i in seq_len(n_obs)) {
    obs_category <- residuals_tb$observed[i]
    prob_col_name <- paste0("prob_class_", obs_category)
    obs_pred_prob[i] <- residuals_tb[[prob_col_name]][i]
  }

  residuals_tb <- residuals_tb %>%
    mutate(prob_observed_category = obs_pred_prob)

  # Create jittered observed values for better visualization
  residuals_tb <- residuals_tb %>%
    mutate(observed_jittered = observed + runif(n(), -0.1, 0.1))

  plot4 <- ggplot(residuals_tb, aes(x = predicted_class, y = observed_jittered)) +
    geom_jitter(aes(color = prob_observed_category), alpha = 0.5, width = 0.2, height = 0.2, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    scale_color_gradient(low = "red", high = "blue", name = "P(Observed)") +
    labs(
      title = "Observed vs Predicted Class",
      x = "Predicted Class",
      y = "Observed Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7)
    )

  # Combine all plots into 2x2 panel
  # Use arrangeGrob to create without displaying
  combined_plot <- arrangeGrob(
    plot1, plot2, plot3, plot4,
    ncol = 2, nrow = 2,
    top = textGrob(
      model_label,
      gp = gpar(fontsize = 12, fontface = "bold")
    )
  )

  return(combined_plot)
}

# PLOT POM COEFFICIENTS (forest plot)
plot_pom_coefficients <- function(pom_result, plot_title = NULL) {
  # Create forest plot showing odds ratios as normal curve distributions
  #
  # Args:
  #   pom_result: Output from fit_pom() containing coefficients with ORs and CIs
  #   plot_title: Optional title for the plot
  #
  # Returns:
  #   A ggplot object (forest plot with normal curves)

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for plotting.")
  }
  library(ggplot2)
  library(dplyr)

  # Extract coefficients with odds ratios
  coef_data <- pom_result$coefficients

  # Check if we have the required columns
  required_cols <- c("variable", "odds_ratio", "std_error", "p_value")
  if (!all(required_cols %in% names(coef_data))) {
    stop("pom_result$coefficients must contain: variable, odds_ratio, std_error, p_value")
  }

  # Order variables by odds ratio for better visualization
  # IMPORTANT: Must actually sort rows, not just reorder factor levels
  coef_data <- coef_data %>%
    arrange(odds_ratio) %>%
    mutate(variable = factor(variable, levels = variable))

  # Determine plot limits (symmetric on log scale around 1)
  # Use confidence level from model to calculate z-critical value
  conf_level <- pom_result$model_stats$confidence_level
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha / 2)

  log_or <- log(coef_data$odds_ratio)
  log_se <- coef_data$std_error
  log_lower <- log_or - z_crit * log_se
  log_upper <- log_or + z_crit * log_se
  log_range <- max(abs(c(log_lower, log_upper)))

  # Set x-axis limits (symmetric on log scale)
  x_limits <- exp(c(-log_range * 1.2, log_range * 1.2))

  # Generate normal curve data for each coefficient
  n_points <- 30  # Resolution per curve
  curve_data <- lapply(1:nrow(coef_data), function(i) {
    row <- coef_data[i, ]

    # Generate x values (on log scale for odds ratios)
    log_mean <- log(row$odds_ratio)
    log_se <- row$std_error

    # Create sequence of x values around the mean (±4 SD to capture full curve)
    x_log_seq <- seq(log_mean - 4 * log_se, log_mean + 4 * log_se, length.out = n_points)
    x_or_seq <- exp(x_log_seq)

    # Calculate normal density
    density_vals <- dnorm(x_log_seq, mean = log_mean, sd = log_se)

    # Normalize density to a reasonable height for plotting
    # Scale so the peak has height proportional to the y-position
    max_density <- max(density_vals)
    height_scale <- 0.35  # controls how tall the curves are
    density_scaled <- (density_vals / max_density) * height_scale

    # Apply p-value based opacity
    # p<0.01 = 100% opaque, p<0.05 = 80% opaque, p<0.1 = 60% opaque, p>=0.1 = 0% opaque
    opacity <- if (row$p_value < 0.01) {
      1.0
    } else if (row$p_value < 0.05) {
      0.8
    } else if (row$p_value < 0.1) {
      0.6
    } else {
      0.0
    }

    data.frame(
      variable = row$variable,
      x = x_or_seq,
      y_base = i,
      y_curve = i + density_scaled,
      odds_ratio = row$odds_ratio,
      p_value = row$p_value,
      opacity = opacity
    )
  }) %>% bind_rows()

  # Create title
  main_title <- if (!is.null(plot_title)) {
    plot_title
  } else {
    "Coefficient Forest Plot"
  }

  # Create the forest plot with normal curves
  forest_plot <- ggplot() +
    # Light grey vertical lines at x-axis breaks
    geom_vline(xintercept = c(0.25, 0.5, 2, 4), linetype = "solid", color = "gray85", linewidth = 0.3) +

    # Reference line at OR = 1
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +

    # Horizontal lines at each variable position (the grey lines curves will overlap)
    geom_hline(yintercept = 1:nrow(coef_data), color = "gray70", linewidth = 0.5) +

    # Normal curves (using geom_ribbon to fill under curve)
    geom_ribbon(data = curve_data,
      aes(x = x, ymin = y_base, ymax = y_curve,
        group = variable, alpha = opacity),
      fill = "#0072B2", color = "#0072B2", linewidth = 0.5) +

    # Styling
    scale_x_continuous(
      trans = "log",
      breaks = c(0.25, 0.5, 1, 2, 4),
      labels = c("0.25", "0.5", "1.0", "2.0", "4.0"),
      limits = x_limits
    ) +
    scale_y_continuous(
      breaks = 1:nrow(coef_data),
      labels = levels(coef_data$variable),
      limits = c(0.5, nrow(coef_data) + 0.5)
    ) +
    scale_alpha_identity() +  # Use opacity values directly
    labs(
      title = main_title,
      x = "Odds Ratio (log scale)",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  return(forest_plot)
}

# PLOT PPOM THRESHOLD-SPECIFIC COEFFICIENTS
plot_ppom_threshold_coefficients <- function(coef_data, plot_title = NULL) {
  # Create plot showing how coefficients vary across thresholds in PPOM
  # Uses normal curves to represent uncertainty, similar to forest plots
  #
  # Args:
  #   coef_data: Data frame with columns: variable, threshold, estimate, std_error, odds_ratio
  #   plot_title: Optional title for the plot
  #
  # Returns:
  #   A ggplot object showing threshold-specific coefficients with normal curves

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for plotting.")
  }
  library(ggplot2)
  library(dplyr)

  if (nrow(coef_data) == 0) {
    stop("No threshold-specific coefficients found in coef_data")
  }

  # Calculate p-values if not present (using z-test for logistic regression)
  if (!"p_value" %in% names(coef_data)) {
    coef_data <- coef_data %>%
      mutate(
        z_value = estimate / std_error,
        p_value = 2 * pnorm(abs(z_value), lower.tail = FALSE)
      )
  }

  # Define color mapping for variables
  variable_colors <- c(
    "nw.awareness.1980s_numeric" = "#D55E00",
    "nw.awareness.recent.academic_numeric" = "#E69F00",
    "nw.awareness.recent.media_numeric" = "#0072B2"
  )

  # All curves should be centered at threshold level (no vertical offset)
  coef_data <- coef_data %>%
    arrange(threshold) %>%
    mutate(
      threshold_num = as.numeric(threshold),
      y_pos = threshold_num  # All curves centered on threshold line
    )

  # Calculate plot limits for x-axis (odds ratio, log scale)
  log_or_all <- log(coef_data$odds_ratio)
  log_se_all <- coef_data$std_error
  log_lower_all <- log_or_all - 1.96 * log_se_all
  log_upper_all <- log_or_all + 1.96 * log_se_all
  log_range <- max(abs(c(log_lower_all, log_upper_all)))
  x_limits <- exp(c(-log_range * 1.2, log_range * 1.2))

  # Generate normal curve data for each coefficient
  n_points <- 30
  curve_data <- lapply(1:nrow(coef_data), function(i) {
    row <- coef_data[i, ]

    # Generate x values on log scale
    log_mean <- log(row$odds_ratio)
    log_se <- row$std_error

    x_log_seq <- seq(log_mean - 4 * log_se, log_mean + 4 * log_se, length.out = n_points)
    x_or_seq <- exp(x_log_seq)

    # Calculate normal density
    density_vals <- dnorm(x_log_seq, mean = log_mean, sd = log_se)

    # Normalize and scale density
    max_density <- max(density_vals)
    height_scale <- 0.25  # Slightly smaller than forest plot since we have 3 variables per threshold
    density_scaled <- (density_vals / max_density) * height_scale

    # Apply p-value based opacity
    # p<0.01 = 80% opaque, p<0.05 = 50% opaque, p<0.1 = 25% opaque, p>=0.1 = 0% opaque
    opacity <- if (row$p_value < 0.01) {
      0.8
    } else if (row$p_value < 0.05) {
      0.5
    } else if (row$p_value < 0.1) {
      0.25
    } else {
      0.0
    }

    # Get color for this variable
    var_color <- variable_colors[as.character(row$variable)]
    if (is.na(var_color)) var_color <- "#999999"  # Fallback color

    data.frame(
      variable = row$variable,
      threshold = row$threshold,
      x = x_or_seq,
      y_base = row$y_pos,
      y_curve = row$y_pos + density_scaled,
      odds_ratio = row$odds_ratio,
      p_value = row$p_value,
      opacity = opacity,
      color = var_color,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  # Create threshold labels
  threshold_levels <- sort(unique(coef_data$threshold_num))
  threshold_labels <- as.character(threshold_levels)

  # Create title
  main_title <- if (!is.null(plot_title)) {
    plot_title
  } else {
    "PPOM Threshold-Specific Coefficients"
  }

  # Create the plot with swapped axes
  threshold_plot <- ggplot() +
    # Light grey vertical lines at x-axis breaks
    geom_vline(xintercept = c(0.25, 0.5, 2, 4), linetype = "solid", color = "gray85", linewidth = 0.3) +

    # Reference line at OR = 1
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +

    # Horizontal lines at each threshold position
    geom_hline(yintercept = threshold_levels, color = "gray70", linewidth = 0.5) +

    # Normal curves (one per coefficient, colored by variable)
    # Map variable to fill and color, then use manual scales
    geom_ribbon(data = curve_data,
      aes(x = x, ymin = y_base, ymax = y_curve,
          group = interaction(variable, threshold),
          fill = variable, color = variable, alpha = opacity),
      linewidth = 0.5) +

    # Styling
    scale_x_continuous(
      trans = "log",
      breaks = c(0.25, 0.5, 1, 2, 4),
      labels = c("0.25", "0.5", "1.0", "2.0", "4.0"),
      limits = x_limits
    ) +
    scale_y_continuous(
      breaks = threshold_levels,
      labels = threshold_labels,
      limits = c(min(threshold_levels) - 0.5, max(threshold_levels) + 0.5)
    ) +
    scale_fill_manual(
      values = variable_colors,
      labels = c("1980s awareness", "Recent academic", "Recent media"),
      name = "Awareness Variable"
    ) +
    scale_color_manual(
      values = variable_colors,
      labels = c("1980s awareness", "Recent academic", "Recent media"),
      name = "Awareness Variable"
    ) +
    scale_alpha_identity() +  # Use actual alpha values

    labs(
      title = main_title,
      x = "Odds Ratio (log scale)",
      y = "Threshold"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  return(threshold_plot)
}

cat("Plotting functions loaded successfully.\n")
