# ==============================================================================
# 01_validation.R
# Data validation and model configuration functions
# ==============================================================================

# DATA VALIDATION FUNCTIONS
# These functions catch configuration errors early with clear messages

validate_google_sheets_data <- function(data.tb, questions.tb,
                                       response.options.tb = NULL,
                                       regression.configs.tb = NULL,
                                       verbose = TRUE) {
  # Validate imported Google Sheets data for required structure
  #
  # Args:
  #   data.tb: Main data tibble
  #   questions.tb: Questions metadata tibble
  #   response.options.tb: Response options lookup (optional)
  #   regression.configs.tb: Regression configurations (optional)
  #   verbose: Print validation progress
  #
  # Returns:
  #   TRUE if validation passes (otherwise stops with error)

  if (verbose) cat("\nValidating Google Sheets data...\n")

  # Validate data.tb
  if (nrow(data.tb) == 0) {
    stop("ERROR: data.tb is empty. Check Google Sheets 'data' tab.")
  }
  if (verbose) cat("  ✓ data.tb: ", nrow(data.tb), " rows\n", sep = "")

  # Validate questions.tb
  required_question_cols <- c("q.id", "var.name", "var.category")
  missing_cols <- setdiff(required_question_cols, names(questions.tb))
  if (length(missing_cols) > 0) {
    stop("ERROR: questions.tb missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (any(is.na(questions.tb$var.name))) {
    stop("ERROR: questions.tb has NA values in var.name column")
  }
  if (verbose) {
    cat("  ✓ questions.tb: ", nrow(questions.tb),
        " rows, all required columns present\n", sep = "")
  }

  # Validate response.options.tb if provided
  if (!is.null(response.options.tb)) {
    required_response_cols <- c("q.theme", "response.option")
    missing_cols <- setdiff(required_response_cols, names(response.options.tb))
    if (length(missing_cols) > 0) {
      stop("ERROR: response.options.tb missing required columns: ",
           paste(missing_cols, collapse = ", "))
    }
    if (verbose) {
      cat("  ✓ response.options.tb: ", nrow(response.options.tb),
          " rows\n", sep = "")
    }
  }

  # Validate regression.configs.tb if provided
  if (!is.null(regression.configs.tb)) {
    required_config_cols <- c("model_id", "model_label", "outcome_var",
                             "predictors", "regression_method")
    missing_cols <- setdiff(required_config_cols, names(regression.configs.tb))
    if (length(missing_cols) > 0) {
      stop("ERROR: regression.configs.tb missing required columns: ",
           paste(missing_cols, collapse = ", "))
    }

    # Check for duplicate model_ids
    if (any(duplicated(regression.configs.tb$model_id))) {
      dup_ids <- regression.configs.tb$model_id[duplicated(
        regression.configs.tb$model_id
      )]
      stop("ERROR: Duplicate model_id values in regression.configs.tb: ",
           paste(unique(dup_ids), collapse = ", "))
    }

    if (verbose) {
      cat("  ✓ regression.configs.tb: ", nrow(regression.configs.tb),
          " models defined\n", sep = "")
    }
  }

  if (verbose) cat("Data validation complete!\n\n")
  return(TRUE)
}

# Helper function: Map generic column names to table-specific names
map_column_names <- function(column_names, data_source_name) {
  # Mapping for reshaped tables with renamed value columns
  column_mapping <- list(
    "awareness.tb" = list(
      "value.num" = "awareness.num",
      "value.text" = "awareness.text"
    ),
    "casualty.causes.tb" = list(
      "value.num" = "casualty.num",
      "value.text" = "casualty.text"
    ),
    "support.reaction.tb" = list(
      "value.num" = "support.num",
      "value.text" = "support.text"
    ),
    "decision.factors.tb" = list(
      "value.num" = "decision.num",
      "value.text" = "decision.text"
    )
  )

  # Apply mapping if data source has mappings defined
  if (data_source_name %in% names(column_mapping)) {
    mappings <- column_mapping[[data_source_name]]
    column_names <- sapply(column_names, function(col) {
      if (col %in% names(mappings)) mappings[[col]] else col
    }, USE.NAMES = FALSE)
  }

  return(column_names)
}

validate_model_config <- function(config_row, verbose = TRUE) {
  # Validate a single model configuration before fitting
  #
  # Args:
  #   config_row: Single row from regression.configs.tb
  #   verbose: Print validation messages
  #
  # Returns:
  #   TRUE if valid (otherwise stops with error)

  model_id <- config_row$model_id

  if (verbose) cat("  Validating config for", model_id, "...\n")

  # Get the data source specified in config
  data_source_name <- config_row$data_source
  if (is.na(data_source_name) || data_source_name == "") {
    stop("ERROR [", model_id, "]: data_source field is empty")
  }

  # Check if data source exists
  if (!exists(data_source_name, envir = .GlobalEnv)) {
    stop("ERROR [", model_id, "]: data_source '", data_source_name,
         "' not found in environment")
  }

  # Get the actual data
  source_data <- get(data_source_name, envir = .GlobalEnv)

  if (verbose) {
    cat("    Using data source:", data_source_name,
        "(", nrow(source_data), "rows )\n")
  }

  # Check outcome variable exists
  outcome_var <- config_row$outcome_var
  outcome_var <- map_column_names(outcome_var, data_source_name)
  if (!outcome_var %in% names(source_data)) {
    stop("ERROR [", model_id, "]: outcome_var '", outcome_var,
         "' not found in ", data_source_name, ". Available columns: ",
         paste(head(names(source_data), 10), collapse = ", "), "...")
  }

  # Check outcome is not all NA
  if (all(is.na(source_data[[outcome_var]]))) {
    stop("ERROR [", model_id, "]: outcome_var '", outcome_var,
         "' contains only NA values")
  }

  # Parse and validate predictor variables
  predictors <- config_row$predictors
  if (is.na(predictors) || predictors == "") {
    stop("ERROR [", model_id, "]: predictors field is empty")
  }

  # Split predictors (assuming comma or + separated)
  pred_vars <- unlist(strsplit(predictors, "[,+]"))
  pred_vars <- trimws(pred_vars)
  pred_vars <- map_column_names(pred_vars, data_source_name)

  # Check each predictor exists
  missing_preds <- setdiff(pred_vars, names(source_data))
  if (length(missing_preds) > 0) {
    stop("ERROR [", model_id, "]: predictor variables not found in ",
         data_source_name, ": ",
         paste(missing_preds, collapse = ", "),
         "\nAvailable columns: ",
         paste(head(names(source_data), 10), collapse = ", "), "...")
  }

  # Check regression_method is not empty
  if (is.na(config_row$regression_method) ||
      config_row$regression_method == "") {
    stop("ERROR [", model_id, "]: regression_method field is empty. ",
         "Specify regression method (e.g., 'pom_logit', 'ppom_logit', ",
         "'multinomial')")
  }

  # Validate filter_conditions syntax if present
  if (!is.null(config_row$filter_conditions) &&
      !is.na(config_row$filter_conditions) &&
      config_row$filter_conditions != "") {

    filter_cond <- config_row$filter_conditions

    # Basic syntax check - look for dangerous patterns
    if (grepl("(rm\\(|unlink\\(|system\\(|file\\.remove)", filter_cond)) {
      stop("ERROR [", model_id, "]: filter_conditions contains potentially ",
           "dangerous code: ", filter_cond)
    }

    # Try to parse the filter condition
    tryCatch({
      # Don't actually evaluate, just parse to check syntax
      parse(text = paste("data.tb %>% filter(", filter_cond, ")"))
    }, error = function(e) {
      stop("ERROR [", model_id, "]: filter_conditions has invalid syntax: ",
           filter_cond, "\nParse error: ", e$message)
    })
  }

  if (verbose) cat("    ✓ Config valid\n")
  return(TRUE)
}

# UNIFIED MODEL FITTING FRAMEWORK
# This framework provides a consistent interface for all regression model types

get_model_type <- function(model_spec) {
  # Parse model specification to determine model type
  #
  # Args:
  #   model_spec: String like "logit", "ppom_logit", "ols", "multinomial", etc.
  #
  # Returns:
  #   List with model_type and link_function

  model_spec <- tolower(trimws(model_spec))

  # Model type patterns (order matters - check specific patterns first)
  if (grepl("^ppom[_-]?", model_spec)) {
    link <- sub("^ppom[_-]?", "", model_spec)
    if (nchar(link) == 0) link <- "logit"
    return(list(
      model_type = "ppom",
      link_function = link,
      family = "ordinal"
    ))
  } else if (grepl("^pom[_-]?", model_spec)) {
    link <- sub("^pom[_-]?", "", model_spec)
    if (nchar(link) == 0) link <- "logit"
    return(list(
      model_type = "pom",
      link_function = link,
      family = "ordinal"
    ))
  } else if (grepl("^gam[_-]?ordinal", model_spec)) {
    return(list(
      model_type = "gam_ordinal",
      link_function = "identity",
      family = "ordinal"
    ))
  } else if (grepl("^multinomial", model_spec)) {
    link <- sub("^multinomial[_-]?", "", model_spec)
    if (nchar(link) == 0) link <- "logit"
    return(list(
      model_type = "multinomial",
      link_function = link,
      family = "multinomial"
    ))
  } else if (grepl("^binary[_-]", model_spec)) {
    link <- sub("^binary[_-]?", "", model_spec)
    if (nchar(link) == 0) link <- "logit"
    return(list(
      model_type = "binary",
      link_function = link,
      family = "binomial"
    ))
  } else if (grepl("^(ols|linear)", model_spec)) {
    return(list(
      model_type = "ols",
      link_function = "identity",
      family = "gaussian"
    ))
  } else if (model_spec %in% c("logit", "probit", "cloglog",
                               "loglog", "cauchit")) {
    # Standard ordinal regression (POM)
    return(list(
      model_type = "pom",
      link_function = model_spec,
      family = "ordinal"
    ))
  } else {
    # Default: assume POM with specified link
    return(list(
      model_type = "pom",
      link_function = model_spec,
      family = "ordinal"
    ))
  }
}

check_cell_counts <- function(data, outcome_var, min_count = 10) {
  cell_counts <- data %>%
    count(.data[[outcome_var]]) %>%
    arrange(n)

  issues <- cell_counts %>% filter(n < min_count)

  result <- list(
    all_counts = cell_counts,
    min_count = min(cell_counts$n),
    issues = issues,
    has_issues = nrow(issues) > 0
  )

  return(result)
}

cat("Validation functions loaded successfully.\n")
