# 00000000000000000000000000000000000000000000000000000000000#
# 0000       2024-11 NW Awareness Poll Data              0000#
# 00000000000000000000000000000000000000000000000000000000000#

# 0-SETUP -------------------------------------------------------------------------------

# INITIAL SETUP
rm(list = ls()) # Remove lists
gc()
options(java.parameters = "- Xmx8g") # helps r not to fail when importing large xlsx files with xlsx package

# SECTION & CODE CLOCKING
sections.all.starttime <- Sys.time()
section0.starttime <- sections.all.starttime

# SET WORKING DIRECTORY
wd <- "/home/wnf/code/nw-data-commons-awareness-poll"

# LOAD LIBRARIES/PACKAGES
library(wnf.utils)
LoadCommonPackages()
library(googledrive)
drive_auth(cache = "~/.R/gargle_cache", email = "william@fluxrme.com")
library(purrr)
# library(ncdf4)
library(janitor)
library(lubridate)
library(openxlsx2)
# library(MASS)         # For polr (proportional odds logistic regression)
library(ggplot2)
library(ggalluvial)   # For Sankey/alluvial diagrams
# library(ggpubr)       # For statistical plots
# library(psych)        # For descriptive stats
# library(Hmisc)        # For rcorr if needed
# library(broom)

# SECTION CLOCKING
section0.duration <- Sys.time() - section0.starttime
section0.duration

# 0.5-HELPER FUNCTIONS -------------------------------------------------------------------------------

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
  if (verbose) cat("  ✓ questions.tb: ", nrow(questions.tb), " rows, all required columns present\n", sep = "")

  # Validate response.options.tb if provided
  if (!is.null(response.options.tb)) {
    required_response_cols <- c("q.theme", "response.option")
    missing_cols <- setdiff(required_response_cols, names(response.options.tb))
    if (length(missing_cols) > 0) {
      stop("ERROR: response.options.tb missing required columns: ",
           paste(missing_cols, collapse = ", "))
    }
    if (verbose) cat("  ✓ response.options.tb: ", nrow(response.options.tb), " rows\n", sep = "")
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
      dup_ids <- regression.configs.tb$model_id[duplicated(regression.configs.tb$model_id)]
      stop("ERROR: Duplicate model_id values in regression.configs.tb: ",
           paste(unique(dup_ids), collapse = ", "))
    }

    if (verbose) cat("  ✓ regression.configs.tb: ", nrow(regression.configs.tb),
                    " models defined\n", sep = "")
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

  if (verbose) cat("    Using data source:", data_source_name,
                   "(", nrow(source_data), "rows )\n")

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
  if (is.na(config_row$regression_method) || config_row$regression_method == "") {
    stop("ERROR [", model_id, "]: regression_method field is empty. ",
         "Specify regression method (e.g., 'pom_logit', 'ppom_logit', 'multinomial')")
  }

  # Validate filter_conditions syntax if present
  if (!is.null(config_row$filter_conditions) &&
      !is.na(config_row$filter_conditions) &&
      config_row$filter_conditions != "") {

    filter_cond <- config_row$filter_conditions

    # Basic syntax check - look for dangerous patterns
    if (grepl("(rm\\(|unlink\\(|system\\(|file\\.remove)", filter_cond)) {
      stop("ERROR [", model_id, "]: filter_conditions contains potentially dangerous code: ",
           filter_cond)
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
# and makes it easy to add new model types (OLS, binary logit, multinomial, GAM)

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
  } else if (model_spec %in% c("logit", "probit", "cloglog", "loglog", "cauchit")) {
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

fit_model <- function(model_spec, formula, data, confidence_level = 0.95,
                     verbose = TRUE) {
  # Unified model fitting function that routes to appropriate model type
  #
  # Args:
  #   model_spec: String specifying model type (e.g., "logit", "ppom_logit", "ols")
  #   formula: Model formula
  #   data: Data frame
  #   confidence_level: Confidence level for CIs
  #   verbose: Print progress messages
  #
  # Returns:
  #   Model results with standard structure

  # Parse model specification
  model_info <- get_model_type(model_spec)

  if (verbose) {
    cat("  Model type:", toupper(model_info$model_type), "\n")
    if (model_info$link_function != "identity") {
      cat("  Link function:", model_info$link_function, "\n")
    }
  }

  # Route to appropriate fitting function
  result <- switch(model_info$model_type,
    "pom" = fit_pom(
      formula = formula,
      data = data,
      link = model_info$link_function,
      confidence_level = confidence_level,
      verbose = verbose
    ),
    "ppom" = fit_ppom(
      formula = formula,
      data = data,
      link = model_info$link_function,
      confidence_level = confidence_level,
      verbose = verbose
    ),
    "ols" = not_implemented_model("ols"),
    "binary" = not_implemented_model("binary"),
    "multinomial" = not_implemented_model("multinomial"),
    "gam_ordinal" = not_implemented_model("gam_ordinal"),
    stop("Unknown model type: ", model_info$model_type)
  )

  # Add model type info to result
  if (!is.null(result)) {
    result$model_info <- model_info
  }

  return(result)
}

# Stub function for model types not yet implemented
not_implemented_model <- function(model_type) {
  messages <- list(
    "ols" = "OLS regression not yet implemented. Use 'pom_logit' for ordinal outcomes.",
    "binary" = "Binary logistic/probit not yet implemented. Use 'pom_logit' for ordinal outcomes.",
    "multinomial" = "Multinomial logistic regression not yet implemented. Coming soon!",
    "gam_ordinal" = "GAM-based ordinal regression not yet implemented. Coming soon!"
  )
  stop(messages[[model_type]])
}

# 1-IMPORT DATA & CONFIGS -------------------------------------------------------------------------------

gs4_auth(email = "william@fluxrme.com")

sheet.id = "https://docs.google.com/spreadsheets/d/1dcMT2lv9TBz_MPeq6jEMGD7H2Qn3FX-fLMEjc9d5_9Y"

data_and_configs.ss <-
  as_sheets_id(sheet.id)
sheet.names.v <- sheet_names(data_and_configs.ss)

data.and.configs.ls <-
  lapply(
    sheet.names.v,
    function(x) {
      read_sheet(data_and_configs.ss, sheet = x)
    }
  )

names(data.and.configs.ls) <- sheet.names.v

# Assign each table to its own tibble object
ListToTibbleObjects(data.and.configs.ls) # Converts list elements to separate tibble objects names with their respective sheet names with ".tb" appended

# Validate imported data structure
validate_google_sheets_data(
  data.tb = data.tb,
  questions.tb = questions.tb,
  response.options.tb = response.options.tb,
  regression.configs.tb = regression.configs.tb,
  verbose = TRUE
)

# 2-CLEANING & RESHAPING -------------------------------------------------------------------------------

# CLEAN UP NAMES ACCORDING TO CONFIGS TABLE
names(data.tb) <- IndexMatchToVectorFromTibble(
  names(data.tb),
  questions.tb,
  match.varname = "q.id",
  replacement.vals.varname = "var.name",
  mult.replacements.per.cell = FALSE,
  print.matches = TRUE
)

id.varnames <- questions.tb %>%
  dplyr::filter(var.category == "id.var") %>%
  dplyr::pull(var.name)

# REMOVE EXTRA COLUMNS NOT GOING TO USE
data.tb %<>%
  dplyr::select(
    -started.at, -reviewed.at, -archived.at, -completion.code, -total.approvals, -status, -submission.id,
    -nationality
  )

# RECODE INFOGRAPHIC
data.tb %<>%
  mutate(shown.infographic = dplyr::case_when(
    shown.infographic == "A" ~ "shown infographic",
    shown.infographic == "B" ~ "no infographic"
  ))

# RECODE TEXT VALUES FROM RESPONSE OPTIONS

# Step 1: Create a lookup table that connects var.name (from data.tb) to q.theme (in response.options.tb)
recode.mapping.tb <- questions.tb %>%
  dplyr::filter(q.theme %in% unique(response.options.tb$q.theme)) %>%
  dplyr::select(var.name, q.theme)

# Step 2: Recode each variable based on the response.options.tb
recode_from_theme <- function(varname, theme) {
  if (!varname %in% names(data.tb)) return(NULL)

  # Get response options for this theme
  options.tb <- response.options.tb %>%
    dplyr::filter(q.theme == theme) %>%
    dplyr::select(response.option, response.text)

  if (nrow(options.tb) == 0) return(NULL)

  # Ensure column is character for safe indexing
  data.tb[[varname]] <- as.character(data.tb[[varname]])

  # Build named vector for recoding
  recode.vector <- setNames(options.tb$response.text, as.character(options.tb$response.option))

  # Perform recoding using named vector
  data.tb[[varname]] <<- recode.vector[data.tb[[varname]]]
}

# Step 3: Apply to each variable that needs recoding
walk2(recode.mapping.tb$var.name, recode.mapping.tb$q.theme, recode_from_theme)

# RECODE POLITICAL AFFILIATION & ADD USEFUL VARIABLES
data.tb %<>%
  mutate(
    political.affiliation = dplyr::case_when(
      political.affiliation %in% c("Don't know", "Dont know") ~ "Don't Know",
      political.affiliation %in% c("Other") ~ "Other",
      political.affiliation %in% c("Would not vote") ~ "Would not vote",
      political.affiliation %in% c("Democrat") ~ "USA-Democrat",
      political.affiliation %in% c("Republican") ~ "USA-Republican",
      political.affiliation %in% c("Independent", "independent") & country.of.residence == "United States" ~ "USA-Independent",
      political.affiliation %in% c("Independent", "independent") & country.of.residence == "United Kingdom" ~ "UK-Independent",
      political.affiliation %in% c("Labour") ~ "UK-Labour",
      political.affiliation %in% c("Conservative") ~ "UK-Conservative",
      political.affiliation %in% c("Green") ~ "UK-Green",
      political.affiliation %in% c("SNP") ~ "UK-SNP",
      political.affiliation %in% c("Plaid Cymru") ~ "UK-Plaid Cymru",
      TRUE ~ political.affiliation
    )
  ) %>%
  mutate(
    age.group = dplyr::case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 & age <= 49 ~ "40-49",
      age >= 50 & age <= 64 ~ "50-64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_   # Handles missing or out-of-range ages
    ),
    nationality.native = dplyr::case_when(
      country.of.birth == country.of.residence ~ "native",
      !is.na(country.of.birth) & !is.na(country.of.residence) & country.of.birth != country.of.residence ~ "non.native",
      TRUE ~ NA_character_
    ),
    language.native = dplyr::case_when(
      language == "English" ~ "english",
      !is.na(language) & language != "English" ~ "other",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(age.group, .after = age) %>%
  relocate(nationality.native, .after = country.of.residence) %>%
  relocate(language.native, .after = language) %>%
  relocate(political.affiliation, .after = sex)

# ADD USEFUL CALCULATED VARIABLES

# Helper function to extract first numeric character from ordinal responses
extract_numeric_from_ordinal <- function(x) {
  if (is.na(x)) return(NA_real_)
  first_char <- substr(as.character(x), 1, 1)
  if (grepl("^[0-9]$", first_char)) {
    return(as.numeric(first_char))
  } else {
    return(NA_real_)
  }
}

data.tb <- data.tb %>%
  mutate(
    # AWARENESS VARIABLES - Numeric conversions
    nw.awareness.1980s_numeric = sapply(nw.awareness.1980s, extract_numeric_from_ordinal),
    nw.awareness.recent.media_numeric = sapply(nw.awareness.recent.media, extract_numeric_from_ordinal),
    nw.awareness.recent.academic_numeric = sapply(nw.awareness.recent.academic, extract_numeric_from_ordinal),

    # AWARENESS VARIABLES - Mean (calculated from available values)
    nw.awareness_mean = rowMeans(
      cbind(nw.awareness.1980s_numeric, nw.awareness.recent.media_numeric, nw.awareness.recent.academic_numeric),
      na.rm = TRUE
    ),

    # AWARENESS VARIABLES - Any awareness binary
    nw.awareness_any = case_when(
      is.na(nw.awareness.1980s) & is.na(nw.awareness.recent.media) & is.na(nw.awareness.recent.academic) ~ NA_real_,
      nw.awareness.1980s_numeric == 1 & nw.awareness.recent.media_numeric == 1 & nw.awareness.recent.academic_numeric == 1 ~ 0,
      TRUE ~ 1
    ),

    # CASUALTY CAUSES - Log-midpoint numeric conversions (geometric mean of range)
    casualties.blast_numeric_log = case_when(
      casualties.blast == "(a) < 1M" ~ log10(5e5),
      casualties.blast == "(b) 1-10M" ~ log10(sqrt(1e6 * 1e7)),
      casualties.blast == "(c) 10-100M" ~ log10(sqrt(1e7 * 1e8)),
      casualties.blast == "(d) 100M-1B" ~ log10(sqrt(1e8 * 1e9)),
      casualties.blast == "(e) 1-8B" ~ log10(sqrt(1e9 * 8e9)),
      TRUE ~ NA_real_
    ),
    casualties.radiation_numeric_log = case_when(
      casualties.radiation == "(a) < 1M" ~ log10(5e5),
      casualties.radiation == "(b) 1-10M" ~ log10(sqrt(1e6 * 1e7)),
      casualties.radiation == "(c) 10-100M" ~ log10(sqrt(1e7 * 1e8)),
      casualties.radiation == "(d) 100M-1B" ~ log10(sqrt(1e8 * 1e9)),
      casualties.radiation == "(e) 1-8B" ~ log10(sqrt(1e9 * 8e9)),
      TRUE ~ NA_real_
    ),
    casualties.starvation_numeric_log = case_when(
      casualties.starvation == "(a) < 1M" ~ log10(5e5),
      casualties.starvation == "(b) 1-10M" ~ log10(sqrt(1e6 * 1e7)),
      casualties.starvation == "(c) 10-100M" ~ log10(sqrt(1e7 * 1e8)),
      casualties.starvation == "(d) 100M-1B" ~ log10(sqrt(1e8 * 1e9)),
      casualties.starvation == "(e) 1-8B" ~ log10(sqrt(1e9 * 8e9)),
      TRUE ~ NA_real_
    ),

    # SUPPORT VARIABLES - Numeric conversions
    support.nuclear.strike.on.russia_numeric = sapply(support.nuclear.strike.on.russia, extract_numeric_from_ordinal),
    support.conventional.strike.on.russia_numeric = sapply(support.conventional.strike.on.russia, extract_numeric_from_ordinal),
    support.increase.ukraine.aid_numeric = sapply(support.increase.ukraine.aid, extract_numeric_from_ordinal),
    support.crippling.sanctions.on.russia_numeric = sapply(support.crippling.sanctions.on.russia, extract_numeric_from_ordinal),
    support.ceasefire.ultimatum_numeric = sapply(support.ceasefire.ultimatum, extract_numeric_from_ordinal),

    # SUPPORT VARIABLES - Binary conversions (1-2=0, 4-5=1, 3=NA)
    support.nuclear.strike.on.russia_binary = case_when(
      support.nuclear.strike.on.russia_numeric %in% c(1, 2) ~ 0,
      support.nuclear.strike.on.russia_numeric %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),
    support.conventional.strike.on.russia_binary = case_when(
      support.conventional.strike.on.russia_numeric %in% c(1, 2) ~ 0,
      support.conventional.strike.on.russia_numeric %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),
    support.increase.ukraine.aid_binary = case_when(
      support.increase.ukraine.aid_numeric %in% c(1, 2) ~ 0,
      support.increase.ukraine.aid_numeric %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),
    support.crippling.sanctions.on.russia_binary = case_when(
      support.crippling.sanctions.on.russia_numeric %in% c(1, 2) ~ 0,
      support.crippling.sanctions.on.russia_numeric %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),
    support.ceasefire.ultimatum_binary = case_when(
      support.ceasefire.ultimatum_numeric %in% c(1, 2) ~ 0,
      support.ceasefire.ultimatum_numeric %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),

    # DECISION FACTORS - Numeric conversions
    decision.reduce.russian.retaliation_numeric = sapply(decision.reduce.russian.retaliation, extract_numeric_from_ordinal),
    decision.punish.russia.signal.aggressors_numeric = sapply(decision.punish.russia.signal.aggressors, extract_numeric_from_ordinal),
    decision.limit.diplomatic.support.for.russia_numeric = sapply(decision.limit.diplomatic.support.for.russia, extract_numeric_from_ordinal),
    decision.avoid.killing.civilians.global.famine_numeric = sapply(decision.avoid.killing.civilians.global.famine, extract_numeric_from_ordinal),
    decision.avoid.escalation_numeric = sapply(decision.avoid.escalation, extract_numeric_from_ordinal)
  )

# RESHAPE - DEFINE ABSTRACTED FUNCTION
ReshapeThemeTable <- function(theme, data_table, questions_table, response_options_table, drop_not_participating = FALSE, extra_id_vars = NULL) {
  # 1. Identify ID vars
  id.vars <- questions_table %>%
    dplyr::filter(var.category == "id.var") %>%
    dplyr::pull(var.name)

  # 1b. Add extra derived ID variables if provided
  if (!is.null(extra_id_vars)) {
    id.vars <- c(id.vars, extra_id_vars)
  }

  # 2. Identify columns for the given theme
  theme.vars <- questions_table %>%
    dplyr::filter(q.theme == theme) %>%
    dplyr::select(q.theme, var.name, q.num)  # keep both var name and q.num for later join

  # 3. Reshape data from wide to long
  long.tb <- data_table %>%
    dplyr::select(all_of(id.vars), all_of(theme.vars$var.name)) %>%
    tidyr::pivot_longer(
      cols = -all_of(id.vars),
      names_to = "category",
      values_to = "value"
    )

  # 4. Attach q.theme and q.num to each category using questions.tb
  long.tb <- long.tb %>%
    left_join(theme.vars, by = c("category" = "var.name"))

  # 5. Rename value to value.text
  long.tb <- long.tb %>%
    rename(value.text = value)

  # 6. Join response.option (numerical value) from response_options_table
  response.lookup <- response_options_table %>%
    dplyr::filter(q.theme == theme) %>%
    dplyr::select(response.text, response.option)

  long.tb <- long.tb %>%
    left_join(response.lookup, by = c("value.text" = "response.text")) %>%
    rename(value.num = response.option)

  # 7. Convert "0. not participating" → NA for both text and numeric values
  long.tb <- long.tb %>%
    mutate(
      value.text = ifelse(value.text == "0. not participating", NA, value.text),
      value.num = ifelse(value.num == 0, NA_real_, as.numeric(value.num))
    )

  # 8. Optionally drop rows that were non-participants (after conversion to NA)
  if (drop_not_participating) {
    long.tb <- long.tb %>%
      dplyr::filter(!is.na(value.num))
  }

  return(long.tb)
}

# DEFINE EXTRA ID VARIABLES (derived variables to include in reshaped tables)
# Include calculated variables based on single columns (not multi-column aggregates like means)
extra.id.vars <- c(
  "age.group", "nationality.native", "language.native",
  # Awareness numeric conversions
  "nw.awareness.1980s_numeric", "nw.awareness.recent.media_numeric", "nw.awareness.recent.academic_numeric",
  # Casualty log-midpoint conversions
  "casualties.blast_numeric_log", "casualties.radiation_numeric_log", "casualties.starvation_numeric_log",
  # Support numeric conversions
  "support.nuclear.strike.on.russia_numeric", "support.conventional.strike.on.russia_numeric",
  "support.increase.ukraine.aid_numeric", "support.crippling.sanctions.on.russia_numeric",
  "support.ceasefire.ultimatum_numeric",
  # Support binary conversions
  "support.nuclear.strike.on.russia_binary", "support.conventional.strike.on.russia_binary",
  "support.increase.ukraine.aid_binary", "support.crippling.sanctions.on.russia_binary",
  "support.ceasefire.ultimatum_binary",
  # Decision factors numeric conversions
  "decision.reduce.russian.retaliation_numeric", "decision.punish.russia.signal.aggressors_numeric",
  "decision.limit.diplomatic.support.for.russia_numeric", "decision.avoid.killing.civilians.global.famine_numeric",
  "decision.avoid.escalation_numeric"
)

# RESHAPE - AWARENESS
awareness.tb <- ReshapeThemeTable(
  theme = "awareness",
  data_table = data.tb,
  questions_table = questions.tb,
  response_options_table = response.options.tb,
  drop_not_participating = TRUE,
  extra_id_vars = extra.id.vars
)  %>%
  relocate(age.group, .after = age) %>%
  relocate(nationality.native, .after = country.of.residence) %>%
  relocate(language.native, .after = language) %>%
  relocate(political.affiliation, .after = sex) %>%
  rename(awareness.text = value.text, awareness.num = value.num)

# RESHAPE - CASUALTY CAUSES
casualty.causes.tb <- ReshapeThemeTable(
  theme = "casualty.causes",
  data_table = data.tb,
  questions_table = questions.tb,
  response_options_table = response.options.tb,
  drop_not_participating = TRUE,
  extra_id_vars = extra.id.vars
) %>%
  relocate(age.group, .after = age) %>%
  relocate(nationality.native, .after = country.of.residence) %>%
  relocate(language.native, .after = language) %>%
  relocate(political.affiliation, .after = sex)

# ADD INTERVAL BOUNDS FOR CASUALTY VARIABLES (for interval regression)
casualty.causes.tb <- casualty.causes.tb %>%
  mutate(
    # Parse casualty intervals and create lower/upper bounds
    interval_lower = case_when(
      value.text == "(a) < 1M" ~ 0,
      value.text == "(b) 1-10M" ~ 1000000,
      value.text == "(c) 10-100M" ~ 10000000,
      value.text == "(d) 100M-1B" ~ 100000000,
      value.text == "(e) 1-8B" ~ 1000000000,
      TRUE ~ NA_real_
    ),
    interval_upper = case_when(
      value.text == "(a) < 1M" ~ 1000000,
      value.text == "(b) 1-10M" ~ 10000000,
      value.text == "(c) 10-100M" ~ 100000000,
      value.text == "(d) 100M-1B" ~ 1000000000,
      value.text == "(e) 1-8B" ~ 8000000000,
      TRUE ~ NA_real_
    ),
    # Calculate midpoint for descriptive purposes
    interval_midpoint = (interval_lower + interval_upper) / 2
  ) %>%
  rename(casualty.text = value.text, casualty.num = value.num)

# RESHAPE - SUPPORT REACTION
support.reaction.tb <- ReshapeThemeTable(
  theme = "support.reaction",
  data_table = data.tb,
  questions_table = questions.tb,
  response_options_table = response.options.tb,
  extra_id_vars = extra.id.vars
) %>%
  relocate(age.group, .after = age) %>%
  relocate(nationality.native, .after = country.of.residence) %>%
  relocate(language.native, .after = language) %>%
  relocate(political.affiliation, .after = sex) %>%
  rename(support.text = value.text, support.num = value.num)

# RESHAPE - DECISION FACTORS
decision.factors.tb <- ReshapeThemeTable(
  theme = "decision.factors",
  data_table = data.tb,
  questions_table = questions.tb,
  response_options_table = response.options.tb,
  extra_id_vars = extra.id.vars
) %>%
  relocate(age.group, .after = age) %>%
  relocate(nationality.native, .after = country.of.residence) %>%
  relocate(language.native, .after = language) %>%
  relocate(political.affiliation, .after = sex) %>%
  rename(decision.text = value.text, decision.num = value.num)

# CREATE FINAL LIST OF CLEANED & REFORMATTED TABLES FOR EXPORT

clean_object_names <-
  c(
    "data.tb",
    "awareness.tb",
    "casualty.causes.tb",
    "support.reaction.tb",
    "decision.factors.tb",
    "questions.tb"
  )

clean_table_names <-
  c(
    "1.wide.data",
    "2.awareness",
    "3.casualty.causes",
    "4.support.reaction",
    "5.decision.factors",
    "questions"
  )

export.ls <-
  lapply(
    clean_object_names,
    function(x) {
      if (exists(x)) get(x) else NULL
    }
  ) %>%
  purrr::compact() %>% # Remove NULL entries for non-existent tibbles
  lapply(., function(x) {x %>% select(-any_of(c("q.num", "q.id", "q.theme")))}) # Remove q.num and q.id if they exist in any table

names(export.ls) <- clean_table_names[clean_object_names %in% ls()]
# 3-EXPORT CLEANED DATA -------------------------------------------------------------------------------
export.logical <- FALSE

if (export.logical) {

  # DEFINE & CREATE OUTPUT DIRECTORY

  output.base.name <-
    Sys.time() %>%
    gsub(":", ".", .)

  output.dir <-
    paste(
      wd,
      "/outputs/",
      output.base.name,
      "_data/",
      sep = ""
    )

  if (output.dir %>% dir.exists %>% not) {
    dir.create(output.dir, recursive = TRUE)
  }

  # WRITE TABLES INTO SINGLE EXCEL FILE IN OUTPUT DIRECTORY

  output.file.path <-
    paste(output.dir, "Reformatted Data_Analysis_", Sys.Date(), ".xlsx", sep = "") %>%
    file.path(.)

  wb <- wb_workbook()

  for (i in seq_along(export.ls)) {
    sheet_name <- names(export.ls)[i]  # Get the name of the list element (if named)
    if (is.null(sheet_name) || sheet_name == "") {
      sheet_name <- paste0("Sheet", i)  # Assign default sheet names if missing
    }

    # Clean data: Replace NA, NaN, Inf with empty strings to avoid Excel errors
    data_cleaned <- export.ls[[i]] %>%
      mutate(across(everything(), ~{
        if (is.numeric(.)) {
          # For numeric columns: replace NA, NaN, Inf with NA (will become empty)
          ifelse(is.na(.) | is.nan(.) | is.infinite(.), NA_real_, .)
        } else {
          # For character columns: replace NA with empty string
          ifelse(is.na(.), "", as.character(.))
        }
      }))

    wb$add_worksheet(sheet_name)  # Create a new sheet
    wb$add_data(sheet = sheet_name, x = data_cleaned, na.strings = "")  # Write data with NAs as empty strings
  }

  wb$save(output.file.path, overwrite = TRUE)

  cat("Excel file successfully saved at:", output.file.path, "\n") # Print confirmation

  # CODE CLOCKING
  code.duration <- Sys.time() - sections.all.starttime
  code.duration

}

# 4-ANALYSIS, STATISTICAL TESTS -------------------------------------------------------------------------------
# 4A SPECIAL VISUALIZATIONS

# SANKEY DIAGRAM - NUCLEAR WINTER AWARENESS FLOWS
# This diagram visualizes how awareness levels flow across three sources:
# 1980s awareness, recent academic awareness, and recent media awareness

# Prepare data for Sankey diagram
sankey_data <- data.tb %>%
  select(
    nw.awareness.1980s,
    nw.awareness.recent.academic,
    nw.awareness.recent.media
  ) %>%
  # Remove any rows with missing data
  filter(
    !is.na(nw.awareness.1980s),
    !is.na(nw.awareness.recent.academic),
    !is.na(nw.awareness.recent.media)
  ) %>%
  # Count frequency of each unique pattern
  group_by(
    nw.awareness.1980s,
    nw.awareness.recent.academic,
    nw.awareness.recent.media
  ) %>%
  summarise(freq = n(), .groups = 'drop') %>%
  # Ensure awareness levels are ordered factors (reversed so "4. know a lot" appears on top)
  mutate(
    nw.awareness.1980s = factor(
      nw.awareness.1980s,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    ),
    nw.awareness.recent.academic = factor(
      nw.awareness.recent.academic,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    ),
    nw.awareness.recent.media = factor(
      nw.awareness.recent.media,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    )
  )

# Calculate total number of respondents
total_n <- sum(sankey_data$freq)

# Define color palette for awareness levels
awareness_colors <- c(
  "4. know a lot" = "#313695",        # Darkest blue
  "3. know something" = "#4575b4",   # Medium blue
  "2. heard a little" = "#91bfdb",   # Light blue
  "1. never heard" = "#e0f3f8"       # Lightest blue
)

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

# Add flow color column to sankey_data
sankey_data <- sankey_data %>%
  rowwise() %>%
  mutate(
    flow_color = get_flow_color(nw.awareness.1980s, nw.awareness.recent.academic, nw.awareness.recent.media)
  ) %>%
  ungroup()

# Create Sankey/alluvial diagram
awareness_sankey <- ggplot(
  sankey_data,
  aes(
    y = freq,
    axis1 = nw.awareness.1980s,
    axis2 = nw.awareness.recent.academic,
    axis3 = nw.awareness.recent.media
  )
) +
  # Add flows (alluvium) - colored based on connected awareness levels
  geom_alluvium(
    aes(fill = flow_color),
    width = 1/6,
    alpha = 0.7,
    curve_type = "cubic"
  ) +
  # Add stacked bars (strata) - colored by awareness level
  geom_stratum(
    aes(fill = c("4. know a lot" = "#313695", "3. know something" = "#4575b4",
                 "2. heard a little" = "#91bfdb", "1. never heard" = "#e0f3f8")[after_stat(stratum)]),
    width = 1/6,
    color = "grey30",
    size = 0.3
  ) +
  # Add labels to strata with percentages
  geom_text(
    stat = "stratum",
    aes(label = paste0(after_stat(stratum), "\n(",
                       round(after_stat(count) / total_n * 100, 1), "%)")),
    size = 4,
    fontface = "bold"
  ) +
  # Use identity scale to apply exact colors
  scale_fill_identity() +
  # Axis labels - reduced expand to bring labels closer
  scale_x_discrete(
    limits = c("1980s", "Recent\nAcademic", "Recent\nMedia"),
    expand = c(0.05, 0.05)
  ) +
  # Labels and theme
  labs(
    title = "Nuclear Winter Awareness Flows Across Information Sources",
    subtitle = "Each flow represents respondents with the same awareness pattern",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  )

# Display the plot
print(awareness_sankey)

# Save the plot as PNG
ggsave(
  filename = "outputs/awareness_sankey_diagram.png",
  plot = awareness_sankey,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nSankey diagram saved to: outputs/awareness_sankey_diagram.png\n")


# 4B-BIVARIATE TESTS 
run.bivariate.tests <- FALSE

if (run.bivariate.tests) {
  # DEFINE FUNCTION FOR CONVERTING CHARACTER VARIABLES TO FACTORS
  convert_character_var_to_factor <- function(x) {
    factor(
      na_if(x, "0. not participating"),
      levels = c(
        "1. never heard",
        "2. heard a little",
        "3. know something",
        "4. know a lot"
      ),
      ordered = TRUE
    )
  }

  # GENERALIZED BIVARIATE TESTING FUNCTION
  BivariateTesting <- function(
                               data_long,                        # Long-format data (e.g., awareness.tb, support.reaction.tb)
                               rhs_var = "value.num",            # RHS ordinal variable (numeric response values)
                               lhs_var,                          # LHS demographic variable name
                               questions_config = questions.tb,  # Configuration table with stat.test.remove.vals
                               rhs_category_var = "category",    # Column containing subcategories
                               use_subcategories = TRUE,         # Whether to analyze by subcategory
                               verbose = TRUE                    # Print detailed output
  ) {

    # HELPER FUNCTION: Parse and filter values based on config
    filter_values <- function(data, var_name, config_table) {
      # Get counts before filtering (excluding NA)
      counts_before <- data %>%
        filter(!is.na(.data[[var_name]])) %>%
        group_by(.data[[var_name]]) %>%
        summarize(n = n(), .groups = "drop")

      # Always remove NA
      data_filtered <- data %>% filter(!is.na(.data[[var_name]]))

      # Get removal values from config
      remove_vals_str <- config_table %>%
        filter(var.name == var_name) %>%
        pull(stat.test.remove.vals)

      if (length(remove_vals_str) > 0 && !is.na(remove_vals_str[1])) {
        # Parse comma-separated values
        remove_vals <- strsplit(remove_vals_str[1], ",\\s*|,")[[1]]
        remove_vals <- trimws(remove_vals)

        # Get counts of values being removed
        removed_counts <- counts_before %>%
          filter(.data[[var_name]] %in% remove_vals)

        # Filter out specified values
        data_filtered <- data_filtered %>%
          filter(!(.data[[var_name]] %in% remove_vals))

        if (verbose && nrow(removed_counts) > 0) {
          cat("  Config-based filtering for", var_name, ":\n")
          for (i in seq_len(nrow(removed_counts))) {
            cat("    Removed:", removed_counts[[var_name]][i], "(n =", removed_counts$n[i], ")\n")
          }
        }
      }

      return(data_filtered)
    }

    # HELPER FUNCTION: Detect variable type based on R class
    detect_var_type <- function(var_data) {
      var_class <- class(var_data)[1]
      n_unique <- var_data %>% na.omit() %>% unique() %>% length()

      # Numeric detection: numeric/integer class AND >10 unique values
      if (var_class %in% c("numeric", "integer") && n_unique > 10) {
        return(list(
          type = "numeric",
          test_method = "spearman",
          n_levels = n_unique,
          min_group_size = 30  # For correlation, need reasonable sample size
        ))
      }

      # Categorical/Binary/Ordinal: character/factor OR ≤10 unique values
      if (var_class %in% c("character", "factor") || n_unique <= 10) {
        if (n_unique == 2) {
          var_type <- "binary"
          min_size <- 10  # Binary comparisons can work with smaller samples
        } else if (n_unique >= 3) {
          var_type <- "categorical"
          min_size <- 5  # Multiple groups need smaller minimum per group
        } else {
          var_type <- "single_value"
          min_size <- NA
        }

        return(list(
          type = var_type,
          test_method = "kruskal_wallis",
          n_levels = n_unique,
          min_group_size = min_size
        ))
      }

      # Fallback
      return(list(
        type = "unknown",
        test_method = NA,
        n_levels = n_unique,
        min_group_size = NA
      ))
    }

    # HELPER FUNCTION: Filter small groups and report
    filter_small_groups <- function(data, var_name, min_size, var_info) {
      # Get group sizes before filtering
      group_sizes <- data %>%
        group_by(.data[[var_name]]) %>%
        summarize(n = n(), .groups = "drop")

      # Identify small groups
      small_groups <- group_sizes %>%
        filter(n < min_size)

      # Filter out small groups
      if (nrow(small_groups) > 0) {
        data_filtered <- data %>%
          group_by(.data[[var_name]]) %>%
          filter(n() >= min_size) %>%
          ungroup()

        if (verbose) {
          cat("  Small group filtering rule: min", min_size, "observations per", var_info$type, "group\n")
          cat("  Removed", nrow(small_groups), "group(s) with insufficient data:\n")
          for (i in seq_len(nrow(small_groups))) {
            cat("    -", small_groups[[var_name]][i], ": n =", small_groups$n[i], "\n")
          }
          cat("  Remaining groups:\n")
          remaining_sizes <- data_filtered %>%
            group_by(.data[[var_name]]) %>%
            summarize(n = n(), .groups = "drop")
          for (i in seq_len(nrow(remaining_sizes))) {
            cat("    -", remaining_sizes[[var_name]][i], ": n =", remaining_sizes$n[i], "\n")
          }
        }

        return(data_filtered)
      } else {
        if (verbose) {
          cat("  Small group filtering rule: min", min_size, "observations per", var_info$type, "group\n")
          cat("  Removed groups: none\n")
          cat("  All groups:\n")
          for (i in seq_len(nrow(group_sizes))) {
            cat("    -", group_sizes[[var_name]][i], ": n =", group_sizes$n[i], "\n")
          }
        }
        return(data)
      }
    }

    # ========== STEP 1: VALIDATE & FILTER DATA ==========
    if (verbose) {
      cat("\n==================================================\n")
      cat("BIVARIATE TESTING: ", lhs_var, " vs ", rhs_var, "\n")
      cat("==================================================\n\n")
    }

    # Check if variables exist
    if (!rhs_var %in% names(data_long)) {
      stop(paste("RHS variable", rhs_var, "not found in data"))
    }
    if (!lhs_var %in% names(data_long)) {
      stop(paste("LHS variable", lhs_var, "not found in data"))
    }

    # Filter data: remove NAs and configured exclusion values
    if (verbose) cat("Step 1: Filtering data\n")

    data_filtered <- data_long %>%
      filter(!is.na(.data[[rhs_var]])) %>%
      filter(!is.na(.data[[lhs_var]]))

    # Apply LHS filtering based on config
    data_filtered <- filter_values(data_filtered, lhs_var, questions_config)

    n_after_config_filter <- nrow(data_filtered)
    if (verbose) cat("  Observations after config-based filtering:", n_after_config_filter, "\n\n")

    # ========== STEP 2: DETECT VARIABLE TYPES ==========
    if (verbose) cat("Step 2: Detecting variable types\n")

    lhs_info <- detect_var_type(data_filtered[[lhs_var]])

    if (verbose) {
      cat("  LHS Variable:", lhs_var, "\n")
      cat("    - Class:", class(data_filtered[[lhs_var]])[1], "\n")
      cat("    - Detected type:", lhs_info$type, "\n")
      cat("    - Test method:", lhs_info$test_method, "\n")
      cat("    - Number of unique values:", lhs_info$n_levels, "\n")
      cat("    - Minimum sample size:", lhs_info$min_group_size, "\n\n")
    }

    # ========== STEP 3: APPLY MINIMUM SIZE FILTERING ==========
    if (verbose) cat("Step 3: Applying minimum size filtering\n")

    # For numeric variables, check total sample size
    if (lhs_info$test_method == "spearman") {
      if (n_after_config_filter < lhs_info$min_group_size) {
        if (verbose) {
          cat("  ERROR: Insufficient total observations (n =", n_after_config_filter,
            ") for correlation analysis\n")
          cat("  Minimum required:", lhs_info$min_group_size, "\n\n")
        }
        return(list(
          lhs_var = lhs_var,
          rhs_var = rhs_var,
          lhs_type = lhs_info$type,
          error = paste("Insufficient data: n =", n_after_config_filter,
            "< minimum", lhs_info$min_group_size)
        ))
      } else {
        if (verbose) {
          cat("  Sample size rule: min", lhs_info$min_group_size, "observations for correlation\n")
          cat("  Total observations:", n_after_config_filter, "✓\n\n")
        }
        data_filtered_final <- data_filtered
        n_total <- n_after_config_filter
      }
    } else if (lhs_info$test_method == "kruskal_wallis") {
      # For categorical variables, filter small groups
      data_filtered_final <- filter_small_groups(
        data_filtered,
        lhs_var,
        lhs_info$min_group_size,
        lhs_info
      )
      n_total <- nrow(data_filtered_final)

      # Check if at least 2 groups remain
      n_groups_remaining <- data_filtered_final %>%
        pull(.data[[lhs_var]]) %>%
        unique() %>%
        length()

      if (n_groups_remaining < 2) {
        if (verbose) {
          cat("  ERROR: Fewer than 2 groups remaining after filtering\n\n")
        }
        return(list(
          lhs_var = lhs_var,
          rhs_var = rhs_var,
          lhs_type = lhs_info$type,
          error = "Insufficient groups after filtering"
        ))
      }

      if (verbose) {
        cat("  Groups remaining:", n_groups_remaining, "\n")
        cat("  Total observations:", n_total, "\n\n")
      }
    }

    # Initialize results list
    results <- list(
      lhs_var = lhs_var,
      rhs_var = rhs_var,
      lhs_class = class(data_filtered_final[[lhs_var]])[1],
      lhs_type = lhs_info$type,
      test_method = lhs_info$test_method,
      min_group_size = lhs_info$min_group_size,
      n_total = n_total,
      combined_results = list(),
      subcategory_results = list()
    )

    # ========== STEP 4: COMBINED ANALYSIS (All Subcategories) ==========
    if (verbose) cat("Step 4: Combined analysis (all subcategories together)\n\n")

    if (lhs_info$test_method == "spearman") {
      # NUMERIC LHS: Spearman correlation

      cor_value <- cor(
        as.numeric(data_filtered_final[[rhs_var]]),
        as.numeric(data_filtered_final[[lhs_var]]),
        method = "spearman",
        use = "complete.obs"
      )

      cor_test <- cor.test(
        as.numeric(data_filtered_final[[rhs_var]]),
        as.numeric(data_filtered_final[[lhs_var]]),
        method = "spearman",
        exact = FALSE
      )

      results$combined_results <- list(
        n = n_total,
        correlation = cor_value,
        rho = cor_test$estimate,
        p_value = cor_test$p.value,
        statistic = cor_test$statistic
      )

      if (verbose) {
        cat("  Test: Spearman's Rank Correlation\n")
        cat("  N:", n_total, "\n")
        cat("  Correlation (rho):", round(cor_value, 4), "\n")
        cat("  P-value:", format.pval(cor_test$p.value, digits = 3), "\n\n")
      }

    } else if (lhs_info$test_method == "kruskal_wallis") {
      # CATEGORICAL/BINARY/ORDINAL LHS: Kruskal-Wallis + descriptive stats

      # Descriptive statistics by group
      descriptive_stats <- data_filtered_final %>%
        group_by(.data[[lhs_var]]) %>%
        summarize(
          n = n(),
          mean = mean(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
          median = median(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
          sd = sd(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(median))

      kw_test <- kruskal.test(
        as.numeric(data_filtered_final[[rhs_var]]) ~ data_filtered_final[[lhs_var]]
      )

      results$combined_results <- list(
        n = n_total,
        n_groups = nrow(descriptive_stats),
        descriptive_stats = descriptive_stats,
        kw_statistic = kw_test$statistic,
        kw_df = kw_test$parameter,
        p_value = kw_test$p.value,
        significance = ifelse(kw_test$p.value < 0.05, "Significant", "Not significant")
      )

      if (verbose) {
        cat("  Test: Kruskal-Wallis\n")
        cat("  N:", n_total, "\n")
        cat("  Groups:", nrow(descriptive_stats), "\n\n")
        cat("  Descriptive Statistics by Group:\n")
        print(descriptive_stats, n = Inf)
        cat("\n  Kruskal-Wallis chi-squared:", round(kw_test$statistic, 4), "\n")
        cat("  df:", kw_test$parameter, "\n")
        cat("  P-value:", format.pval(kw_test$p.value, digits = 3), "\n")
        cat("  Significance:", results$combined_results$significance, "\n\n")
      }
    }

    # ========== STEP 5: SUBCATEGORY ANALYSIS ==========
    if (use_subcategories && rhs_category_var %in% names(data_long)) {
      if (verbose) cat("Step 5: Subcategory analysis (broken out by each subcategory)\n\n")

      # Get unique subcategories
      subcategories <- data_filtered_final %>%
        filter(!is.na(.data[[rhs_category_var]])) %>%
        pull(.data[[rhs_category_var]]) %>%
        unique() %>%
        sort()

      for (subcat in subcategories) {
        if (verbose) cat("  Subcategory:", subcat, "\n")

        # Filter for this subcategory
        data_subcat <- data_filtered_final %>%
          filter(.data[[rhs_category_var]] == subcat)

        n_subcat_before <- nrow(data_subcat)

        # Apply same filtering logic for subcategory
        if (lhs_info$test_method == "spearman") {
          # Check total sample size
          if (n_subcat_before < lhs_info$min_group_size) {
            if (verbose) {
              cat("    WARNING: Insufficient data (n =", n_subcat_before,
                "< min", lhs_info$min_group_size, ")\n\n")
            }
            results$subcategory_results[[subcat]] <- list(
              n = n_subcat_before,
              error = "Insufficient data"
            )
            next
          }
          data_subcat_final <- data_subcat
          n_subcat <- n_subcat_before

        } else if (lhs_info$test_method == "kruskal_wallis") {
          # Filter small groups within subcategory
          data_subcat_final <- filter_small_groups(
            data_subcat,
            lhs_var,
            lhs_info$min_group_size,
            lhs_info
          )
          n_subcat <- nrow(data_subcat_final)

          # Check if at least 2 groups remain
          n_groups_subcat <- data_subcat_final %>%
            pull(.data[[lhs_var]]) %>%
            unique() %>%
            length()

          if (n_groups_subcat < 2) {
            if (verbose) cat("    WARNING: Fewer than 2 groups remaining\n\n")
            results$subcategory_results[[subcat]] <- list(
              n = n_subcat,
              error = "Insufficient groups"
            )
            next
          }
        }

        # Run appropriate test
        if (lhs_info$test_method == "spearman") {
          # Spearman correlation
          cor_test_sub <- cor.test(
            as.numeric(data_subcat_final[[rhs_var]]),
            as.numeric(data_subcat_final[[lhs_var]]),
            method = "spearman",
            exact = FALSE
          )

          results$subcategory_results[[subcat]] <- list(
            n = n_subcat,
            rho = cor_test_sub$estimate,
            p_value = cor_test_sub$p.value
          )

          if (verbose) {
            cat("    N:", n_subcat, "\n")
            cat("    Correlation (rho):", round(cor_test_sub$estimate, 4), "\n")
            cat("    P-value:", format.pval(cor_test_sub$p.value, digits = 3), "\n\n")
          }

        } else if (lhs_info$test_method == "kruskal_wallis") {
          # Kruskal-Wallis + descriptive stats
          descriptive_stats_sub <- data_subcat_final %>%
            group_by(.data[[lhs_var]]) %>%
            summarize(
              n = n(),
              mean = mean(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
              median = median(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
              sd = sd(as.numeric(.data[[rhs_var]]), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            arrange(desc(median))

          kw_test_sub <- kruskal.test(
            as.numeric(data_subcat_final[[rhs_var]]) ~ data_subcat_final[[lhs_var]]
          )

          results$subcategory_results[[subcat]] <- list(
            n = n_subcat,
            n_groups = nrow(descriptive_stats_sub),
            descriptive_stats = descriptive_stats_sub,
            kw_statistic = kw_test_sub$statistic,
            p_value = kw_test_sub$p.value,
            significance = ifelse(kw_test_sub$p.value < 0.05, "Significant", "Not significant")
          )

          if (verbose) {
            cat("    N:", n_subcat, "\n")
            cat("    Groups:", nrow(descriptive_stats_sub), "\n")
            print(descriptive_stats_sub, n = Inf)
            cat("    Kruskal-Wallis chi-squared:", round(kw_test_sub$statistic, 4), "\n")
            cat("    P-value:", format.pval(kw_test_sub$p.value, digits = 3), "\n")
            cat("    Significance:", results$subcategory_results[[subcat]]$significance, "\n\n")
          }
        }
      }
    }

    if (verbose) cat("==================================================\n\n")

    # Return results invisibly (can be captured in variable)
    invisible(results)
  }

  # RUN BIVARIATE TESTS FROM CONFIG TABLE ---------------------------------

  # Initialize empty list to collect all result rows
  bivariate_results_list <- list()

  # Loop through each test configuration
  for (i in seq_len(nrow(bivariate.tests.tb))) {

    # Check if this test should be implemented
    implement <- bivariate.tests.tb$implement[i]
    if (is.na(implement) || !implement) {
      next  # Skip this test
    }

    # Extract parameters for this test
    test_name <- bivariate.tests.tb$test_name[i]
    source_table_name <- bivariate.tests.tb$source_data_table[i]
    rhs_var <- bivariate.tests.tb$rhs_var[i]
    lhs_var <- bivariate.tests.tb$lhs_var[i]
    data_filter <- bivariate.tests.tb$data_filter[i]
    questions_config_name <- bivariate.tests.tb$questions_config[i]
    rhs_category_var <- bivariate.tests.tb$rhs_category_var[i]
    use_subcategories <- bivariate.tests.tb$use_subcategories[i]
    verbose <- bivariate.tests.tb$verbose[i]

    # Get the actual data object from the table name
    source_data <- get(source_table_name)

    # Apply filter if specified (not NA)
    if (!is.na(data_filter) && data_filter != "NA") {
      source_data <- source_data %>% filter(eval(parse(text = data_filter)))
    }

    # Get the questions config object
    questions_config <- get(questions_config_name)

    # Print test header
    cat("\n\n### Test:", test_name, "###\n")

    # Run the bivariate test
    test_result <- BivariateTesting(
      data_long = source_data,
      rhs_var = rhs_var,
      lhs_var = lhs_var,
      questions_config = questions_config,
      rhs_category_var = rhs_category_var,
      use_subcategories = use_subcategories,
      verbose = verbose
    )

    # Store result object (can be accessed later if needed)
    assign(paste0(test_name, "_results"), test_result)

    # Convert test_result to flat table format
    # Handle errors
    if (!is.null(test_result$error)) {
      # Create single row for error case
      error_row <- tibble(
        test_name = test_name,
        source_data_table = source_table_name,
        data_filter = ifelse(is.na(data_filter), NA_character_, data_filter),
        lhs_var = lhs_var,
        rhs_var = rhs_var,
        rhs_category = NA_character_,
        analysis_level = "overall",
        lhs_type = test_result$lhs_type,
        lhs_class = NA_character_,
        test_method = NA_character_,
        n_total = NA_integer_,
        statistic_name = NA_character_,
        statistic_value = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        lhs_group = NA_character_,
        n_groups = NA_integer_,
        group_n = NA_integer_,
        group_mean = NA_real_,
        group_median = NA_real_,
        group_sd = NA_real_,
        min_group_size = NA_real_,
        error_message = test_result$error
      )
      bivariate_results_list[[length(bivariate_results_list) + 1]] <- error_row
      next
    }

    # Process combined (overall) results
    if (test_result$test_method == "spearman") {
      # Spearman correlation - single row
      overall_row <- tibble(
        test_name = test_name,
        source_data_table = source_table_name,
        data_filter = ifelse(is.na(data_filter), NA_character_, data_filter),
        lhs_var = lhs_var,
        rhs_var = rhs_var,
        rhs_category = NA_character_,
        analysis_level = "overall",
        lhs_type = test_result$lhs_type,
        lhs_class = test_result$lhs_class,
        test_method = test_result$test_method,
        n_total = test_result$n_total,
        statistic_name = "rho",
        statistic_value = test_result$combined_results$rho,
        df = NA_real_,
        p_value = test_result$combined_results$p_value,
        lhs_group = NA_character_,
        n_groups = NA_integer_,
        group_n = NA_integer_,
        group_mean = NA_real_,
        group_median = NA_real_,
        group_sd = NA_real_,
        min_group_size = test_result$min_group_size,
        error_message = NA_character_
      )
      bivariate_results_list[[length(bivariate_results_list) + 1]] <- overall_row

    } else if (test_result$test_method == "kruskal_wallis") {
      # Kruskal-Wallis - one row per group
      desc_stats <- test_result$combined_results$descriptive_stats

      for (j in seq_len(nrow(desc_stats))) {
        group_row <- tibble(
          test_name = test_name,
          source_data_table = source_table_name,
          data_filter = ifelse(is.na(data_filter), NA_character_,
            data_filter),
          lhs_var = lhs_var,
          rhs_var = rhs_var,
          rhs_category = NA_character_,
          analysis_level = "overall",
          lhs_type = test_result$lhs_type,
          lhs_class = test_result$lhs_class,
          test_method = test_result$test_method,
          n_total = test_result$n_total,
          statistic_name = "chi-squared",
          statistic_value = test_result$combined_results$kw_statistic,
          df = test_result$combined_results$kw_df,
          p_value = test_result$combined_results$p_value,
          lhs_group = as.character(desc_stats[[lhs_var]][j]),
          n_groups = test_result$combined_results$n_groups,
          group_n = desc_stats$n[j],
          group_mean = desc_stats$mean[j],
          group_median = desc_stats$median[j],
          group_sd = desc_stats$sd[j],
          min_group_size = test_result$min_group_size,
          error_message = NA_character_
        )
        bivariate_results_list[[length(bivariate_results_list) + 1]] <-
          group_row
      }
    }

    # Process subcategory results
    if (length(test_result$subcategory_results) > 0) {
      for (subcat_name in names(test_result$subcategory_results)) {
        subcat_result <- test_result$subcategory_results[[subcat_name]]

        # Handle subcategory errors
        if (!is.null(subcat_result$error)) {
          subcat_error_row <- tibble(
            test_name = test_name,
            source_data_table = source_table_name,
            data_filter = ifelse(is.na(data_filter), NA_character_,
              data_filter),
            lhs_var = lhs_var,
            rhs_var = rhs_var,
            rhs_category = subcat_name,
            analysis_level = "subcategory",
            lhs_type = test_result$lhs_type,
            lhs_class = test_result$lhs_class,
            test_method = test_result$test_method,
            n_total = NA_integer_,
            statistic_name = NA_character_,
            statistic_value = NA_real_,
            df = NA_real_,
            p_value = NA_real_,
            lhs_group = NA_character_,
            n_groups = NA_integer_,
            group_n = NA_integer_,
            group_mean = NA_real_,
            group_median = NA_real_,
            group_sd = NA_real_,
            min_group_size = test_result$min_group_size,
            error_message = subcat_result$error
          )
          bivariate_results_list[[length(bivariate_results_list) + 1]] <-
            subcat_error_row
          next
        }

        # Spearman subcategory
        if (test_result$test_method == "spearman") {
          subcat_row <- tibble(
            test_name = test_name,
            source_data_table = source_table_name,
            data_filter = ifelse(is.na(data_filter), NA_character_,
              data_filter),
            lhs_var = lhs_var,
            rhs_var = rhs_var,
            rhs_category = subcat_name,
            analysis_level = "subcategory",
            lhs_type = test_result$lhs_type,
            lhs_class = test_result$lhs_class,
            test_method = test_result$test_method,
            n_total = subcat_result$n,
            statistic_name = "rho",
            statistic_value = subcat_result$rho,
            df = NA_real_,
            p_value = subcat_result$p_value,
            lhs_group = NA_character_,
            n_groups = NA_integer_,
            group_n = NA_integer_,
            group_mean = NA_real_,
            group_median = NA_real_,
            group_sd = NA_real_,
            min_group_size = test_result$min_group_size,
            error_message = NA_character_
          )
          bivariate_results_list[[length(bivariate_results_list) + 1]] <-
            subcat_row

        } else if (test_result$test_method == "kruskal_wallis") {
          # Kruskal-Wallis subcategory - one row per group
          desc_stats_sub <- subcat_result$descriptive_stats

          for (k in seq_len(nrow(desc_stats_sub))) {
            subcat_group_row <- tibble(
              test_name = test_name,
              source_data_table = source_table_name,
              data_filter = ifelse(is.na(data_filter), NA_character_,
                data_filter),
              lhs_var = lhs_var,
              rhs_var = rhs_var,
              rhs_category = subcat_name,
              analysis_level = "subcategory",
              lhs_type = test_result$lhs_type,
              lhs_class = test_result$lhs_class,
              test_method = test_result$test_method,
              n_total = subcat_result$n,
              statistic_name = "chi-squared",
              statistic_value = subcat_result$kw_statistic,
              df = NA_real_,
              p_value = subcat_result$p_value,
              lhs_group = as.character(desc_stats_sub[[lhs_var]][k]),
              n_groups = subcat_result$n_groups,
              group_n = desc_stats_sub$n[k],
              group_mean = desc_stats_sub$mean[k],
              group_median = desc_stats_sub$median[k],
              group_sd = desc_stats_sub$sd[k],
              min_group_size = test_result$min_group_size,
              error_message = NA_character_
            )
            bivariate_results_list[[length(bivariate_results_list) + 1]] <-
              subcat_group_row
          }
        }
      }
    }
  }

  # Combine all rows into single tibble
  bivariate.results.tb <- bind_rows(bivariate_results_list)

  # EXPORT BIVARIATE RESULTS -------------------------------------------
  export.bivariate.results <- TRUE

  if (export.bivariate.results) {
    # DEFINE & CREATE OUTPUT DIRECTORY FOR BIVARIATE TESTS
    bv.output.base.name <-
      Sys.time() %>%
      gsub(":", ".", .)

    bv.output.dir <-
      paste(
        wd,
        "/outputs/",
        bv.output.base.name,
        "_bv.tests/",
        sep = ""
      )

    if (bv.output.dir %>% dir.exists %>% not) {
      dir.create(bv.output.dir, recursive = TRUE)
    }

    # WRITE BIVARIATE RESULTS TO CSV
    bv.output.file.path <-
      paste(
        bv.output.dir,
        "bivariate_test_results_",
        Sys.Date(),
        ".csv",
        sep = ""
      )

    write.csv(
      bivariate.results.tb,
      file = bv.output.file.path,
      row.names = FALSE,
      na = ""
    )

    cat("Bivariate test results saved at:", bv.output.file.path, "\n")
  }
}


# 4C-REGRESSIONS ----

# REGRESSION HELPER FUNCTIONS

# CALCULATE VIF (Variance Inflation Factor) FOR MULTICOLLINEARITY DETECTION
calculate_vif <- function(model) {
  # Extract model matrix (without intercept)
  X <- model.matrix(model)[, -1, drop = FALSE]

  # Handle case where there's only one predictor
  if (ncol(X) == 1) {
    vif_results <- data.frame(
      variable = colnames(X),
      vif = 1,
      tolerance = 1
    )
    return(vif_results)
  }

  # Calculate VIF for each predictor
  vif_values <- numeric(ncol(X))
  names(vif_values) <- colnames(X)

  for (i in seq_len(ncol(X))) {
    # Regress each predictor on all other predictors
    # Wrap variable names in backticks to handle spaces and special characters
    lhs <- paste0("`", colnames(X)[i], "`")
    rhs <- paste(paste0("`", colnames(X)[-i], "`"), collapse = " + ")
    formula_str <- paste(lhs, "~", rhs)
    r_squared <- summary(lm(as.formula(formula_str), data = as.data.frame(X)))$r.squared

    # VIF = 1 / (1 - R²)
    vif_values[i] <- 1 / (1 - r_squared)
  }

  # Create results dataframe
  vif_results <- data.frame(
    variable = names(vif_values),
    vif = vif_values,
    tolerance = 1 / vif_values,
    row.names = NULL
  )

  return(vif_results)
}

# PREPARE REGRESSION DATA FROM CONFIG ROW
prepare_regression_data <- function(config_row, data_tb, id.varnames) {
  # Get data source
  data_source_name <- config_row$data_source

  # Check if data source exists
  if (!exists(data_source_name)) {
    stop(paste("Data source", data_source_name, "not found"))
  }

  source_data <- get(data_source_name)

  # Apply filter conditions if specified
  if (!is.null(config_row$filter_conditions) &&
    !is.na(config_row$filter_conditions) &&
    is.character(config_row$filter_conditions) &&
    nchar(trimws(config_row$filter_conditions)) > 0) {
    source_data <- source_data %>%
      filter(eval(parse(text = config_row$filter_conditions)))
  }

  # Get outcome and predictor variables
  outcome_var <- config_row$outcome_var
  outcome_var <- map_column_names(outcome_var, data_source_name)

  # Parse predictors with type checking
  if (!is.null(config_row$predictors) &&
    !is.na(config_row$predictors) &&
    is.character(config_row$predictors) &&
    nchar(trimws(config_row$predictors)) > 0) {
    predictors <- trimws(strsplit(config_row$predictors, ",")[[1]])
    predictors <- map_column_names(predictors, data_source_name)
  } else {
    stop("predictors must be specified in config")
  }

  # Add control variables if specified
  if (!is.null(config_row$control_vars) &&
    !is.na(config_row$control_vars) &&
    is.character(config_row$control_vars) &&
    nchar(trimws(config_row$control_vars)) > 0) {
    control_vars <- trimws(strsplit(config_row$control_vars, ",")[[1]])
    predictors <- c(predictors, control_vars)
  }

  # Select relevant columns
  all_vars <- unique(c(outcome_var, predictors))

  # For wide data, ensure all variables exist
  missing_vars <- setdiff(all_vars, names(source_data))
  if (length(missing_vars) > 0) {
    error_msg <- paste0(
      "\n\n",
      "================================================================================\n",
      "CONFIGURATION ERROR: Missing Variables\n",
      "================================================================================\n\n",
      "The following variable(s) specified in your regression config do not exist\n",
      "in the data source '", data_source_name, "':\n\n",
      "  Missing: ", paste(missing_vars, collapse = ", "), "\n\n",
      "ACTION REQUIRED:\n",
      "  1. Open the Google Sheets regression config tab:\n",
      "     https://docs.google.com/spreadsheets/d/1dcMT2lv9TBz_MPeq6jEMGD7H2Qn3FX-fLMEjc9d5_9Y\n",
      "  2. Find model_id: ", config_row$model_id, "\n",
      "  3. Check the 'predictors' and 'control_vars' columns\n",
      "  4. Fix or remove: ", paste(missing_vars, collapse = ", "), "\n\n",
      "Available variables in '", data_source_name, "':\n",
      "  ", paste(head(sort(names(source_data)), 20), collapse = ", "),
      ifelse(ncol(source_data) > 20, paste0("\n  ... and ", ncol(source_data) - 20, " more"), ""),
      "\n\n",
      "TIP: Use RStudio's View() function to inspect the full data:\n",
      "  View(", data_source_name, ")\n\n",
      "================================================================================\n"
    )
    stop(error_msg)
  }

  # Create regression dataset
  regression_data <- source_data %>%
    select(all_of(all_vars)) %>%
    drop_na()  # Remove rows with any NA in the model variables

  # Extract outcome label for better reporting
  # If outcome is generic "value.num" and data has "category" column, use category name
  outcome_label <- outcome_var
  if (outcome_var == "value.num" && "category" %in% names(source_data)) {
    unique_categories <- unique(source_data$category)
    if (length(unique_categories) == 1) {
      outcome_label <- as.character(unique_categories[1])
    } else if (length(unique_categories) > 1) {
      # Multiple categories - use data source name as label
      outcome_label <- gsub("\\.tb$", "", data_source_name)
    }
  }

  return(list(
    data = regression_data,
    outcome_var = outcome_var,
    outcome_label = outcome_label,  # Human-readable label
    predictors = predictors,
    n_original = nrow(source_data),
    n_final = nrow(regression_data),
    n_dropped = nrow(source_data) - nrow(regression_data)
  ))
}

# BUILD FORMULA FROM PREDICTORS AND INTERACTIONS
build_formula <- function(outcome_var, predictors, interaction_terms = NA) {
  # Start with main effects
  formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))

  # Add interactions if specified
  if (!is.null(interaction_terms) &&
    !is.na(interaction_terms) &&
    is.character(interaction_terms) &&
    nchar(trimws(interaction_terms)) > 0) {
    interactions <- trimws(strsplit(interaction_terms, ",")[[1]])
    # Replace * with : for interaction notation in formula
    interactions <- gsub("\\*", ":", interactions)
    formula_str <- paste(formula_str, "+", paste(interactions, collapse = " + "))
  }

  return(as.formula(formula_str))
}

# CHECK SUFFICIENT CELL COUNTS FOR ORDINAL REGRESSION
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

# FIT PROPORTIONAL ODDS MODEL WITH SPECIFIED LINK FUNCTION
fit_pom <- function(
                    formula,
                    data,
                    link = "logit",
                    confidence_level = 0.95,
                    verbose = TRUE
) {
  # Load MASS for polr
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for ordinal regression. Please install it.")
  }

  # Validate and convert link function names to MASS::polr method names
  # User-friendly name -> polr method name
  link_map <- c(
    "logit" = "logistic",
    "probit" = "probit",
    "cloglog" = "cloglog",
    "loglog" = "loglog",
    "cauchit" = "cauchit"
  )

  if (!(link %in% names(link_map))) {
    stop(paste("Invalid link function. Must be one of:",
      paste(names(link_map), collapse = ", ")))
  }

  # Get the polr method name
  polr_method <- link_map[link]

  # Extract outcome variable name from formula
  outcome_var <- as.character(formula)[2]

  # Ensure outcome is an ordered factor
  if (!is.ordered(data[[outcome_var]])) {
    if (verbose) {
      cat("  Converting outcome variable to ordered factor...\n")
    }
    # If numeric, assume natural ordering
    if (is.numeric(data[[outcome_var]])) {
      data[[outcome_var]] <- factor(data[[outcome_var]], ordered = TRUE)
    } else {
      # If character/factor, preserve existing levels or sort
      existing_levels <- if (is.factor(data[[outcome_var]])) {
        levels(data[[outcome_var]])
      } else {
        sort(unique(data[[outcome_var]]))
      }
      data[[outcome_var]] <- factor(data[[outcome_var]],
        levels = existing_levels,
        ordered = TRUE)
    }
  }

  # Check that all predictor variables have at least 2 levels
  predictor_vars <- all.vars(formula[[3]])  # Get variable names from RHS
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      n_levels <- length(unique(data[[pred_var]]))
      if (n_levels < 2) {
        stop(paste("Predictor variable", pred_var, "has only", n_levels,
          "level(s). All predictors must have at least 2 levels."))
      }
    }
  }

  # Check cell counts
  cell_check <- check_cell_counts(data, outcome_var, min_count = 10)
  if (cell_check$has_issues && verbose) {
    cat("  WARNING: Some outcome categories have fewer than 10 observations:\n")
    print(cell_check$issues)
    cat("\n")
  }

  # Set base category to largest group for all categorical predictors
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      # Check if variable is categorical (character or factor)
      if (is.character(data[[pred_var]]) || is.factor(data[[pred_var]])) {
        # Count observations per level
        level_counts <- table(data[[pred_var]], useNA = "no")

        # Get levels ordered by count (descending)
        levels_by_count <- names(sort(level_counts, decreasing = TRUE))

        # Convert to factor with most common level first
        data[[pred_var]] <- factor(data[[pred_var]], levels = levels_by_count)

        if (verbose) {
          cat("  Set base category for", pred_var, "to:", levels_by_count[1],
            "(n =", level_counts[levels_by_count[1]], ")\n")
        }
      }
    }
  }

  if (verbose) {
    cat("\n")
  }

  # Fit the model
  if (verbose) {
    cat("  Fitting proportional odds model with", link, "link...\n")
  }

  model <- tryCatch({
    MASS::polr(formula, data = data, Hess = TRUE, method = polr_method)
  }, error = function(e) {
    stop(paste("Model fitting failed:", e$message))
  })

  # Extract coefficients and standard errors
  coef_table <- coef(summary(model))

  # Calculate z-critical value for confidence intervals
  alpha <- 1 - confidence_level
  z_crit <- qnorm(1 - alpha / 2)

  # Process coefficients (predictors only, not thresholds)
  n_predictors <- nrow(coef_table) - (nlevels(data[[outcome_var]]) - 1)

  if (n_predictors > 0) {
    predictor_coefs <- coef_table[1:n_predictors, , drop = FALSE]

    # Create results dataframe
    results <- tibble(
      variable = rownames(predictor_coefs),
      log_odds = predictor_coefs[, "Value"],
      std_error = predictor_coefs[, "Std. Error"],
      t_value = predictor_coefs[, "t value"],
      p_value = 2 * pnorm(abs(predictor_coefs[, "t value"]), lower.tail = FALSE),
      # Calculate confidence intervals
      ci_lower_log_odds = log_odds - z_crit * std_error,
      ci_upper_log_odds = log_odds + z_crit * std_error,
      # Convert to odds ratios
      odds_ratio = exp(log_odds),
      ci_lower_or = exp(ci_lower_log_odds),
      ci_upper_or = exp(ci_upper_log_odds)
    )
  } else {
    results <- tibble(
      variable = character(),
      log_odds = numeric(),
      std_error = numeric(),
      t_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
  }

  # Extract thresholds (intercepts)
  threshold_indices <- (n_predictors + 1):nrow(coef_table)
  thresholds <- tibble(
    threshold = rownames(coef_table)[threshold_indices],
    value = coef_table[threshold_indices, "Value"],
    std_error = coef_table[threshold_indices, "Std. Error"]
  )

  # Calculate model fit statistics
  model_stats <- list(
    aic = AIC(model),
    bic = BIC(model),
    log_likelihood = logLik(model)[1],
    df = attr(logLik(model), "df"),
    n_obs = nobs(model),
    n_predictors = n_predictors,
    link_function = link,
    confidence_level = confidence_level
  )

  if (verbose) {
    cat("  Model fitted successfully\n")
    cat("  AIC:", round(model_stats$aic, 2), "\n")
    cat("  BIC:", round(model_stats$bic, 2), "\n")
    cat("  Log-likelihood:", round(model_stats$log_likelihood, 2), "\n\n")
  }

  return(list(
    model = model,
    coefficients = results,
    thresholds = thresholds,
    model_stats = model_stats,
    cell_counts = cell_check$all_counts,
    formula = formula,
    outcome_var = outcome_var
  ))
}

# FIT PARTIAL PROPORTIONAL ODDS MODEL (PPOM) WITH SPECIFIED LINK FUNCTION
fit_ppom <- function(
                     formula,
                     data,
                     link = "logit",
                     confidence_level = 0.95,
                     verbose = TRUE
) {
  # Load VGAM for vglm (partial proportional odds model)
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' is required for partial proportional odds model. Please install it with: install.packages('VGAM')")
  }

  # Must explicitly load VGAM to ensure summary.vglm method is available
  library(VGAM)

  # Validate link function for PPOM
  # VGAM::cumulative supports: logitlink, probitlink, clogloglink, cauchitlink
  link_map <- c(
    "logit" = "logitlink",
    "probit" = "probitlink",
    "cloglog" = "clogloglink",
    "cauchit" = "cauchitlink"
  )

  if (!(link %in% names(link_map))) {
    stop(paste("Invalid link function for PPOM. Must be one of:",
      paste(names(link_map), collapse = ", ")))
  }

  vgam_link <- link_map[link]

  # Extract outcome variable name from formula
  outcome_var <- as.character(formula)[2]

  # Ensure outcome is an ordered factor
  if (!is.ordered(data[[outcome_var]])) {
    if (verbose) {
      cat("  Converting outcome variable to ordered factor...\n")
    }
    if (is.numeric(data[[outcome_var]])) {
      data[[outcome_var]] <- factor(data[[outcome_var]], ordered = TRUE)
    } else {
      existing_levels <- if (is.factor(data[[outcome_var]])) {
        levels(data[[outcome_var]])
      } else {
        sort(unique(data[[outcome_var]]))
      }
      data[[outcome_var]] <- factor(data[[outcome_var]],
        levels = existing_levels,
        ordered = TRUE)
    }
  }

  # Check that all predictor variables have at least 2 levels
  predictor_vars <- all.vars(formula[[3]])
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      n_levels <- length(unique(data[[pred_var]]))
      if (n_levels < 2) {
        stop(paste("Predictor variable", pred_var, "has only", n_levels,
          "level(s). All predictors must have at least 2 levels."))
      }
    }
  }

  # Check cell counts
  cell_check <- check_cell_counts(data, outcome_var, min_count = 10)
  if (cell_check$has_issues && verbose) {
    cat("  WARNING: Some outcome categories have fewer than 10 observations:\n")
    print(cell_check$issues)
    cat("\n")
  }

  # Set base category to largest group for all categorical predictors
  for (pred_var in predictor_vars) {
    if (pred_var %in% names(data)) {
      if (is.character(data[[pred_var]]) || is.factor(data[[pred_var]])) {
        level_counts <- table(data[[pred_var]], useNA = "no")
        levels_by_count <- names(sort(level_counts, decreasing = TRUE))
        data[[pred_var]] <- factor(data[[pred_var]], levels = levels_by_count)

        if (verbose) {
          cat("  Set base category for", pred_var, "to:", levels_by_count[1],
            "(n =", level_counts[levels_by_count[1]], ")\n")
        }
      }
    }
  }

  if (verbose) {
    cat("\n")
  }

  # Fit the PPOM model
  if (verbose) {
    cat("  Fitting partial proportional odds model (PPOM) with", link, "link...\n")
  }

  model <- tryCatch({
    VGAM::vglm(formula,
      family = VGAM::cumulative(link = vgam_link, parallel = FALSE),
      data = data)
  }, error = function(e) {
    stop(paste("PPOM model fitting failed:", e$message))
  })

  # Extract coefficients - VGAM returns threshold-specific coefficients
  # summary(model) returns an S4 object with coefficient table in @coef3 slot
  coef_summary <- summary(model)@coef3

  # Get number of outcome levels and thresholds
  n_levels <- nlevels(data[[outcome_var]])
  n_thresholds <- n_levels - 1

  # Calculate z-critical value for confidence intervals
  alpha <- 1 - confidence_level
  z_crit <- qnorm(1 - alpha / 2)

  # Extract threshold names (intercepts)
  threshold_names <- paste0(levels(data[[outcome_var]])[1:(n_levels - 1)], "|",
    levels(data[[outcome_var]])[2:n_levels])

  # Parse coefficient names to separate predictors and thresholds
  # VGAM naming: "(Intercept):1", "(Intercept):2", "predictor:1", "predictor:2", etc.
  coef_names <- rownames(coef_summary)

  # Separate intercepts and predictor coefficients
  intercept_rows <- grep("^\\(Intercept\\):", coef_names)
  predictor_rows <- setdiff(1:nrow(coef_summary), intercept_rows)

  # Extract thresholds
  thresholds <- tibble(
    threshold = threshold_names,
    value = coef_summary[intercept_rows, "Estimate"],
    std_error = coef_summary[intercept_rows, "Std. Error"]
  )

  # Process predictor coefficients - they vary by threshold in PPOM
  if (length(predictor_rows) > 0) {
    # Get unique predictor names (without threshold suffix)
    predictor_coef_names <- coef_names[predictor_rows]
    unique_predictors <- unique(gsub(":[0-9]+$", "", predictor_coef_names))

    # Build threshold-specific coefficient dataframe
    threshold_coefs_list <- list()

    for (pred in unique_predictors) {
      pred_rows <- grep(paste0("^", pred, ":"), coef_names)

      for (i in seq_along(pred_rows)) {
        row_idx <- pred_rows[i]
        threshold_coefs_list[[length(threshold_coefs_list) + 1]] <- tibble(
          variable = pred,
          threshold = threshold_names[i],
          threshold_num = i,
          log_odds = coef_summary[row_idx, "Estimate"],
          std_error = coef_summary[row_idx, "Std. Error"],
          z_value = coef_summary[row_idx, "z value"],
          p_value = coef_summary[row_idx, "Pr(>|z|)"],
          ci_lower_log_odds = log_odds - z_crit * std_error,
          ci_upper_log_odds = log_odds + z_crit * std_error,
          odds_ratio = exp(log_odds),
          ci_lower_or = exp(ci_lower_log_odds),
          ci_upper_or = exp(ci_upper_log_odds)
        )
      }
    }

    threshold_specific_coefs <- bind_rows(threshold_coefs_list)

    # Also create average coefficients across thresholds for summary table
    avg_coefs <- threshold_specific_coefs %>%
      group_by(variable) %>%
      summarize(
        log_odds = mean(log_odds),
        std_error = mean(std_error),
        z_value = mean(z_value),
        p_value = mean(p_value),
        ci_lower_log_odds = mean(ci_lower_log_odds),
        ci_upper_log_odds = mean(ci_upper_log_odds),
        odds_ratio = exp(log_odds),
        ci_lower_or = exp(ci_lower_log_odds),
        ci_upper_or = exp(ci_upper_log_odds),
        .groups = "drop"
      )
  } else {
    threshold_specific_coefs <- tibble(
      variable = character(),
      threshold = character(),
      threshold_num = integer(),
      log_odds = numeric(),
      std_error = numeric(),
      z_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
    avg_coefs <- tibble(
      variable = character(),
      log_odds = numeric(),
      std_error = numeric(),
      z_value = numeric(),
      p_value = numeric(),
      ci_lower_log_odds = numeric(),
      ci_upper_log_odds = numeric(),
      odds_ratio = numeric(),
      ci_lower_or = numeric(),
      ci_upper_or = numeric()
    )
  }

  # Calculate model fit statistics
  model_stats <- list(
    aic = AIC(model),
    bic = BIC(model),
    log_likelihood = logLik(model)[1],
    df = attr(logLik(model), "df"),
    n_obs = nobs(model),
    n_predictors = length(unique_predictors),
    n_thresholds = n_thresholds,
    link_function = link,
    confidence_level = confidence_level,
    model_type = "PPOM"
  )

  if (verbose) {
    cat("  PPOM model fitted successfully\n")
    cat("  AIC:", round(model_stats$aic, 2), "\n")
    cat("  BIC:", round(model_stats$bic, 2), "\n")
    cat("  Log-likelihood:", round(model_stats$log_likelihood, 2), "\n")
    cat("  Number of thresholds:", n_thresholds, "\n\n")
  }

  return(list(
    model = model,
    coefficients = avg_coefs,  # Average coefficients for summary table
    threshold_specific_coefs = threshold_specific_coefs,  # Full threshold-specific coefficients
    thresholds = thresholds,
    model_stats = model_stats,
    cell_counts = cell_check$all_counts,
    formula = formula,
    outcome_var = outcome_var,
    model_type = "PPOM"
  ))
}

# GET BASE CATEGORIES FOR FACTOR VARIABLES
get_base_categories <- function(pom_result) {
  # Extract base categories for all factor variables in a POM or PPOM model
  #
  # Args:
  #   pom_result: Output from fit_pom() or fit_ppom() containing model object
  #
  # Returns:
  #   Named list where names are variable names and values are base categories

  model <- pom_result$model

  # Get xlevels from the model (contains factor levels)
  # Handle both S3 (polr) and S4 (vglm) model objects
  xlevels <- if (isS4(model)) {
    # VGAM::vglm models are S4 objects
    if ("xlevels" %in% methods::slotNames(model)) {
      slot(model, "xlevels")
    } else {
      NULL
    }
  } else {
    # MASS::polr models are S3 objects
    model$xlevels
  }

  if (is.null(xlevels) || length(xlevels) == 0) {
    return(list())
  }

  # For each factor variable, the base category is the first level
  # (R uses treatment contrasts by default, where first level is reference)
  base_cats <- lapply(xlevels, function(levels) levels[1])

  return(base_cats)
}

# FORMAT BASE CATEGORIES FOR DISPLAY
format_base_categories_note <- function(base_categories) {
  # Create a formatted text note showing base categories
  #
  # Args:
  #   base_categories: Named list from get_base_categories()
  #
  # Returns:
  #   Character string with formatted note, or NULL if no categorical variables

  if (length(base_categories) == 0) {
    return(NULL)
  }

  # Format each variable's base category
  formatted_items <- sapply(names(base_categories), function(var_name) {
    paste0(var_name, " (base: ", base_categories[[var_name]], ")")
  })

  # Combine into a single note
  note <- paste("Base categories:", paste(formatted_items, collapse = ", "))

  return(note)
}

# BRANT TEST FOR PROPORTIONAL ODDS ASSUMPTION
perform_brant_test <- function(pom_model, verbose = TRUE) {
  # Load brant package
  if (!requireNamespace("brant", quietly = TRUE)) {
    stop("Package 'brant' is required for Brant test. Please install it with: install.packages('brant')")
  }

  # Load the package into the namespace
  library(brant)

  if (verbose) {
    cat("  Running Brant test for proportional odds assumption...\n")
  }

  # Run Brant test
  brant_result <- tryCatch({
    brant(pom_model)
  }, error = function(e) {
    cat("\nDetailed error information:\n")
    cat("Error message:", e$message, "\n")
    cat("Model class:", class(pom_model), "\n")
    cat("Model method:", pom_model$method, "\n")
    stop(paste("Brant test failed:", e$message))
  })

  if (verbose) {
    cat("  Brant test completed. Processing results...\n")
  }

  # Extract results
  # The brant test returns a matrix with columns: X2, df, probability
  # Rows include: Omnibus test + individual predictor tests
  test_stats <- tryCatch({
    as.data.frame(brant_result, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat("Error details:\n")
    cat("  brant_result class:", class(brant_result), "\n")
    cat("  brant_result type:", typeof(brant_result), "\n")
    stop(paste("Failed to convert brant result to data frame:", e$message))
  })

  # Rename columns for clarity (avoid name conflict with df() function)
  colnames(test_stats) <- c("chi_squared", "degrees_of_freedom", "p_value")

  # Add interpretation column
  test_stats$interpretation <- ifelse(
    test_stats$p_value < 0.05,
    "VIOLATION: Proportional odds assumption violated",
    "OK: Proportional odds assumption holds"
  )

  # Create summary
  # Note: rownames should have "Omnibus" (capital O) based on brant output
  omnibus_row <- which(rownames(test_stats) == "Omnibus")
  if (length(omnibus_row) == 0) {
    # Try lowercase if uppercase not found
    omnibus_row <- which(tolower(rownames(test_stats)) == "omnibus")
  }

  summary_result <- list(
    test_statistics = test_stats,
    omnibus_statistic = test_stats[omnibus_row, "chi_squared"],
    omnibus_df = test_stats[omnibus_row, "degrees_of_freedom"],
    omnibus_p_value = test_stats[omnibus_row, "p_value"],
    omnibus_interpretation = ifelse(
      test_stats[omnibus_row, "p_value"] < 0.05,
      "VIOLATION: Overall proportional odds assumption violated",
      "OK: Overall proportional odds assumption holds"
    ),
    violations = test_stats[test_stats$p_value < 0.05 & seq_len(nrow(test_stats)) != omnibus_row, , drop = FALSE],
    has_violations = any(test_stats$p_value[seq_len(nrow(test_stats)) != omnibus_row] < 0.05)
  )

  if (verbose) {
    cat("\n  Brant Test Results:\n")
    cat("  ==================\n\n")
    cat("  Omnibus Test:\n")
    cat("    Chi-squared =", round(summary_result$omnibus_statistic, 4), "\n")
    cat("    df =", summary_result$omnibus_df, "\n")
    cat("    p-value =", format.pval(summary_result$omnibus_p_value, digits = 4), "\n")
    cat("    Result:", summary_result$omnibus_interpretation, "\n\n")

    cat("  Individual Variable Tests:\n")
    individual_tests <- test_stats[seq_len(nrow(test_stats)) != omnibus_row, , drop = FALSE]
    for (i in seq_len(nrow(individual_tests))) {
      var_name <- rownames(individual_tests)[i]
      cat("    ", var_name, ":\n", sep = "")
      cat("      Chi-squared =", round(individual_tests[i, "chi_squared"], 4), "\n")
      cat("      df =", individual_tests[i, "degrees_of_freedom"], "\n")
      cat("      p-value =", format.pval(individual_tests[i, "p_value"], digits = 4), "\n")
      cat("      Result:", individual_tests[i, "interpretation"], "\n")
    }
    cat("\n")

    if (summary_result$has_violations) {
      cat("  WARNING: Proportional odds assumption violated for one or more predictors.\n")
      cat("  Consider:\n")
      cat("    1. Partial Proportional Odds Model (PPOM)\n")
      cat("    2. Multinomial Logistic Regression\n")
      cat("    3. Examining/transforming problematic predictors\n\n")
    } else {
      cat("  GOOD NEWS: Proportional odds assumption holds for all predictors.\n")
      cat("  The POM model is appropriate for this data.\n\n")
    }
  }

  return(summary_result)
}

# CALCULATE RESIDUALS FOR PROPORTIONAL ODDS MODEL
calculate_pom_residuals <- function(pom_result, verbose = TRUE) {
  # Extract model object
  model <- pom_result$model

  if (verbose) {
    cat("  Calculating residuals for POM...\n")
  }

  # Get model data
  model_data <- model$model
  n_obs <- nrow(model_data)

  # Extract outcome variable
  outcome_var <- pom_result$outcome_var
  y_observed <- as.numeric(model_data[[outcome_var]])

  # Calculate linear predictor (X * beta)
  # For polr, the linear predictor is: X %*% beta (without intercepts/thresholds)
  X <- model.matrix(model)[, -1, drop = FALSE]  # Remove intercept column if present
  beta <- coef(model)

  # Handle case where model.matrix includes factor levels
  # polr coefficients don't include intercept, so align them
  if (ncol(X) == length(beta)) {
    linear_predictor <- as.vector(X %*% beta)
  } else {
    # If dimensions don't match, reconstruct more carefully
    linear_predictor <- predict(model, type = "linear")
  }

  # Get predicted probabilities for each category
  pred_probs <- predict(model, type = "probs")

  # If pred_probs is a vector (single observation), convert to matrix
  if (is.vector(pred_probs)) {
    pred_probs <- matrix(pred_probs, nrow = 1)
  }

  # Get predicted class (mode of predicted probabilities)
  pred_class_num <- apply(pred_probs, 1, which.max)

  # Calculate Pearson residuals
  # For ordinal regression: (observed indicator - predicted prob) / sqrt(predicted prob * (1 - predicted prob))
  # We'll calculate this for the observed category
  pearson_residuals <- numeric(n_obs)

  for (i in seq_len(n_obs)) {
    obs_category <- y_observed[i]
    p_obs <- pred_probs[i, obs_category]

    # Pearson residual for observed category
    pearson_residuals[i] <- (1 - p_obs) / sqrt(p_obs * (1 - p_obs))
  }

  # Calculate deviance residuals
  # Deviance residual = sqrt(-2 * log(predicted probability of observed category))
  # Sign matches whether observed is above or below predicted mode
  deviance_residuals <- numeric(n_obs)

  for (i in seq_len(n_obs)) {
    obs_category <- y_observed[i]
    p_obs <- pred_probs[i, obs_category]

    # Deviance component
    dev_component <- sqrt(-2 * log(p_obs))

    # Add sign based on observed vs predicted
    sign_val <- ifelse(obs_category > pred_class_num[i], 1,
      ifelse(obs_category < pred_class_num[i], -1, 0))

    deviance_residuals[i] <- sign_val * dev_component
  }

  # Create result tibble with observation-level data
  residuals_tb <- tibble(
    obs_index = seq_len(n_obs),
    observed = y_observed,
    predicted_class = pred_class_num,
    linear_predictor = linear_predictor,
    pearson_residual = pearson_residuals,
    deviance_residual = deviance_residuals
  )

  # Add predicted probabilities as separate columns
  prob_cols <- as_tibble(pred_probs)
  colnames(prob_cols) <- paste0("prob_class_", seq_len(ncol(pred_probs)))

  residuals_tb <- bind_cols(residuals_tb, prob_cols)

  if (verbose) {
    cat("  Residuals calculated for", n_obs, "observations\n")
    cat("  Pearson residuals range: [",
      round(min(pearson_residuals), 3), ",",
      round(max(pearson_residuals), 3), "]\n", sep = "")
    cat("  Deviance residuals range: [",
      round(min(deviance_residuals), 3), ",",
      round(max(deviance_residuals), 3), "]\n\n", sep = "")
  }

  return(residuals_tb)
}

# BRANT TEST FUNCTION
test_brant <- FALSE

# NOTE: The brant package has compatibility issues in some environments.
# If you encounter "object of type 'closure' is not subsettable" error,
# consider alternative approaches:
# 1. Test proportional odds using likelihood ratio tests comparing POM to PPOM
# 2. Visual inspection of parallel lines assumption using effect plots
# 3. Use the ordinal package's clm() function which has built-in tests
#
# The perform_brant_test function is implemented and ready to use when
# the brant package works correctly in your environment.

if (test_brant) {
  cat("\n=== TESTING BRANT TEST FUNCTION ===\n\n")

  # Create test dataset
  first_category <- unique(awareness.tb$category)[1]
  test_data <- awareness.tb %>%
    filter(category == first_category) %>%
    select(value.num, age, sex) %>%
    drop_na() %>%
    mutate(sex = factor(sex))

  cat("Test data prepared: n =", nrow(test_data), "\n\n")

  if (nrow(test_data) > 0) {
    # Fit POM model
    cat("Fitting POM model...\n")
    test_formula <- value.num ~ age + sex
    pom_result <- fit_pom(test_formula, test_data, link = "logit",
      confidence_level = 0.95, verbose = FALSE)

    cat("Model fitted. Now running Brant test...\n")

    # Run Brant test
    brant_result <- perform_brant_test(pom_result$model, verbose = TRUE)

    cat("Brant test complete.\n")
    cat("Has violations:", brant_result$has_violations, "\n")

    if (brant_result$has_violations) {
      cat("Variables with violations:\n")
      print(rownames(brant_result$violations))
    }

    cat("\n=== TEST COMPLETE ===\n\n")
  } else {
    cat("ERROR: No data available for testing.\n\n")
  }
}

# TEST FIT_POM FUNCTION
test_fit_pom <- FALSE

if (test_fit_pom) {
  cat("\n=== TESTING FIT_POM FUNCTION ===\n\n")

  # Check available categories in awareness data
  cat("Available categories in awareness.tb:\n")
  print(unique(awareness.tb$category))
  cat("\n")

  # Create test dataset using awareness data (use first available category)
  first_category <- unique(awareness.tb$category)[1]
  cat("Using category:", first_category, "\n\n")

  test_data <- awareness.tb %>%
    filter(category == first_category) %>%
    select(value.num, shown.infographic, age, sex) %>%
    drop_na() %>%
    mutate(
      # Ensure shown.infographic is a factor
      shown.infographic = factor(shown.infographic)
    )

  cat("Test data prepared: n =", nrow(test_data), "\n")
  cat("Outcome levels:", levels(factor(test_data$value.num)), "\n\n")

  # If still no data, skip test
  if (nrow(test_data) > 0) {
    # Test 1: Logit link (default)
    cat("TEST 1: Logit link\n")
    test_formula <- value.num ~ age + sex
    result_logit <- fit_pom(test_formula, test_data, link = "logit",
      confidence_level = 0.95, verbose = TRUE)

    cat("Coefficients:\n")
    print(result_logit$coefficients)
    cat("\n")

    # Test 2: Probit link
    cat("TEST 2: Probit link\n")
    result_probit <- fit_pom(test_formula, test_data, link = "probit",
      confidence_level = 0.95, verbose = TRUE)

    cat("Coefficients:\n")
    print(result_probit$coefficients)
    cat("\n")

    # Test 3: Complementary log-log link
    cat("TEST 3: Complementary log-log link\n")
    result_cloglog <- fit_pom(test_formula, test_data, link = "cloglog",
      confidence_level = 0.95, verbose = TRUE)

    cat("Coefficients:\n")
    print(result_cloglog$coefficients)
    cat("\n")

    # Compare AIC/BIC across link functions
    cat("LINK FUNCTION COMPARISON:\n")
    comparison <- tibble(
      link = c("logit", "probit", "cloglog"),
      AIC = c(result_logit$model_stats$aic,
        result_probit$model_stats$aic,
        result_cloglog$model_stats$aic),
      BIC = c(result_logit$model_stats$bic,
        result_probit$model_stats$bic,
        result_cloglog$model_stats$bic),
      logLik = c(result_logit$model_stats$log_likelihood,
        result_probit$model_stats$log_likelihood,
        result_cloglog$model_stats$log_likelihood)
    ) %>%
      arrange(AIC)

    print(comparison)
    cat("\nBest link function by AIC:", comparison$link[1], "\n")
    cat("\n=== TEST COMPLETE ===\n\n")
  } else {
    cat("ERROR: No data available for testing. Skipping test.\n\n")
  }
}

# REGRESSION DIAGNOSTIC & VISUALIZATION FUNCTIONS

# These functions create diagnostic plots and coefficient plots for regression models

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
    # Linear mapping: p=0.01 → 100%, p=0.25 → 25%, p>0.25 → 25%
    opacity <- if (row$p_value <= 0.01) {
      1.0
    } else if (row$p_value <= 0.25) {
      1.0 - ((row$p_value - 0.01) / (0.25 - 0.01)) * 0.75
    } else {
      0.25
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
plot_ppom_threshold_coefficients <- function(ppom_result, plot_title = NULL) {
  # Create plot showing how coefficients vary across thresholds in PPOM
  #
  # Args:
  #   ppom_result: Output from fit_ppom() containing threshold_specific_coefs
  #   plot_title: Optional title for the plot
  #
  # Returns:
  #   A ggplot object showing coefficients by threshold with confidence intervals

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for plotting.")
  }
  library(ggplot2)
  library(dplyr)

  # Extract threshold-specific coefficients
  coef_data <- ppom_result$threshold_specific_coefs

  if (nrow(coef_data) == 0) {
    stop("No threshold-specific coefficients found in ppom_result")
  }

  # Create threshold mapping for x-axis labels
  threshold_mapping <- coef_data %>%
    select(threshold_num, threshold) %>%
    distinct() %>%
    arrange(threshold_num)

  # Create plot showing coefficients across thresholds
  # Each variable gets its own facet
  threshold_plot <- ggplot(coef_data, aes(x = threshold_num, y = odds_ratio, group = variable)) +
    # Reference line at OR = 1
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +

    # Confidence interval ribbons
    geom_ribbon(aes(ymin = ci_lower_or, ymax = ci_upper_or),
      alpha = 0.2, fill = "#0072B2") +

    # Point estimates with line connecting them
    geom_line(color = "#0072B2", linewidth = 1) +
    geom_point(aes(color = ifelse(p_value < 0.05, "Significant", "Not significant")),
      size = 3) +

    # Facet by variable
    facet_wrap(~variable, scales = "free_y", ncol = 2) +

    # Styling
    scale_color_manual(
      values = c("Significant" = "#D55E00", "Not significant" = "#0072B2"),
      name = "p < 0.05"
    ) +
    scale_x_continuous(
      breaks = threshold_mapping$threshold_num,
      labels = threshold_mapping$threshold
    ) +
    labs(
      title = if (!is.null(plot_title)) plot_title else "Threshold-Specific Coefficients (PPOM)",
      x = "Threshold",
      y = "Odds Ratio"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )

  return(threshold_plot)
}
# EXPORT FUNCTIONS 

# CREATE TIMESTAMPED OUTPUT DIRECTORY
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


# SINGLE-MODEL ORCHESTRATION FUNCTIONS ----

run_single_pom_analysis <- function(
                                    config_row,
                                    run_brant_test = FALSE,
                                    verbose = TRUE
) {
  # Orchestrate complete proportional odds model analysis for a single config
  #
  # Args:
  #   config_row: Single row from regression.configs.tb
  #   run_brant_test: Whether to attempt Brant test
  #   verbose: Print progress messages
  #
  # Returns:
  #   List with model_info, data_prep, pom_result, vif_results, etc.

  if (verbose) {
    cat("\n========================================\n")
    cat("RUNNING POM ANALYSIS:", config_row$model_id, "\n")
    cat("========================================\n\n")
    cat("Model:", config_row$model_label, "\n")
    cat("Outcome:", config_row$outcome_var, "\n")
    cat("Data source:", config_row$data_source, "\n\n")
  }

  # Initialize results
  results <- list(
    model_info = list(
      model_id = config_row$model_id,
      model_label = config_row$model_label,
      outcome_var = config_row$outcome_var,
      data_source = config_row$data_source,
      interaction_terms = config_row$interaction_terms
    ),
    warnings = list(),
    errors = list()
  )

  # STEP 1: Prepare data
  if (verbose) cat("Step 1: Preparing data...\n")

  data_prep <- tryCatch({
    prepare_regression_data(
      config_row = config_row,
      data_tb = NULL,
      id.varnames = id.varnames
    )
  }, error = function(e) {
    results$errors$data_prep <<- e$message
    if (verbose) {
      cat("\n")
      cat(e$message)
      cat("\n")
    }
    return(NULL)
  })

  if (is.null(data_prep)) {
    results$status <- "FAILED: Data preparation"
    return(results)
  }

  results$data_prep <- data_prep

  if (verbose) {
    cat("  n_before_filter:", data_prep$n_final, "\n")
  }

  # STEP 1.5: Apply filter_condition if specified
  filter_applied <- FALSE
  n_before_filter <- data_prep$n_final

  if (!is.null(config_row$filter_condition) &&
    !is.na(config_row$filter_condition) &&
    nchar(trimws(config_row$filter_condition)) > 0) {

    filter_condition <- trimws(config_row$filter_condition)

    if (verbose) {
      cat("  Applying filter:", filter_condition, "\n")
    }

    # Apply filter using dplyr::filter with rlang::parse_expr
    filtered_data <- tryCatch({
      data_prep$data %>%
        filter(!!rlang::parse_expr(filter_condition))
    }, error = function(e) {
      warning(paste("Filter condition failed:", e$message))
      if (verbose) {
        cat("  WARNING: Filter failed, using unfiltered data\n")
      }
      return(data_prep$data)
    })

    data_prep$data <- filtered_data
    data_prep$n_final <- nrow(filtered_data)
    filter_applied <- TRUE

    # Store filter info in results
    results$filter_info <- list(
      filter_applied = TRUE,
      filter_condition = filter_condition,
      n_before_filter = n_before_filter,
      n_after_filter = data_prep$n_final,
      n_excluded = n_before_filter - data_prep$n_final
    )

    if (verbose) {
      cat("  n_after_filter:", data_prep$n_final, "\n")
      cat("  n_excluded:", n_before_filter - data_prep$n_final, "\n")

      # Report sex distribution after filtering if sex is a predictor
      if ("sex" %in% data_prep$predictors && "sex" %in% names(filtered_data)) {
        sex_counts <- table(filtered_data$sex, useNA = "no")
        cat("  Sex distribution after filtering:\n")
        for (sex_level in names(sex_counts)) {
          cat("    ", sex_level, ": n =", sex_counts[sex_level], "\n")
        }
      }
      cat("\n")
    }
  } else {
    # Store that no filter was applied
    results$filter_info <- list(
      filter_applied = FALSE
    )

    if (verbose) {
      cat("  No filter applied\n\n")
    }
  }

  # STEP 2: Build formula
  if (verbose) cat("Step 2: Building formula...\n")

  formula <- tryCatch({
    build_formula(
      outcome_var = data_prep$outcome_var,
      predictors = data_prep$predictors,
      interaction_terms = config_row$interaction_terms
    )
  }, error = function(e) {
    results$errors$formula <<- e$message
    return(NULL)
  })

  if (is.null(formula)) {
    results$status <- "FAILED: Formula"
    return(results)
  }

  if (verbose) cat("  Formula:", deparse(formula), "\n\n")

  # STEP 3: Fit Model
  if (verbose) cat("Step 3: Fitting model...\n")

  # Parse regression_method with robust type checking
  regression_methods <- if (!is.null(config_row$regression_method) &&
    !is.na(config_row$regression_method) &&
    is.character(config_row$regression_method) &&
    nchar(trimws(config_row$regression_method)) > 0) {
    # Split comma-separated methods and trim whitespace
    trimws(strsplit(config_row$regression_method, ",")[[1]])
  } else {
    # Default to pom_logit if not specified
    c("pom_logit")
  }

  # Parse confidence level with type checking
  conf_level <- if (!is.null(config_row$confidence_level) &&
    !is.na(config_row$confidence_level) &&
    is.numeric(config_row$confidence_level)) {
    config_row$confidence_level
  } else {
    0.95
  }

  pom_results_by_link <- list()

  for (model_spec in regression_methods) {
    # Use unified model fitting framework
    pom_result <- tryCatch({
      fit_model(
        model_spec = model_spec,
        formula = formula,
        data = data_prep$data,
        confidence_level = conf_level,
        verbose = verbose
      )
    }, error = function(e) {
      # Parse model type for error reporting
      model_info <- get_model_type(model_spec)
      results$errors[[paste0(model_info$model_type, "_", model_spec)]] <<- e$message
      if (verbose) {
        cat("  ERROR fitting", toupper(model_info$model_type), ":", e$message, "\n")
      }
      return(NULL)
    })

    if (!is.null(pom_result)) {
      pom_results_by_link[[model_spec]] <- pom_result
    }
  }

  if (length(pom_results_by_link) == 0) {
    results$status <- "FAILED: POM fitting"
    if (verbose) {
      cat("\n")
      cat("================================================================================\n")
      cat("POM FITTING ERROR\n")
      cat("================================================================================\n\n")
      cat("All link functions failed to fit the proportional odds model.\n\n")

      # DIAGNOSTIC CHECKS
      cat("DIAGNOSTIC CHECKS:\n")
      cat(strrep("-", 80), "\n\n", sep = "")

      # Check 1: Sample size
      min_n_threshold <- 30  # Generally recommended minimum
      min_n_per_category <- 10

      outcome_counts <- data_prep$data %>%
        count(.data[[data_prep$outcome_var]]) %>%
        arrange(n)

      n_categories <- nrow(outcome_counts)
      min_category_n <- min(outcome_counts$n)

      cat("1. SAMPLE SIZE CHECK\n")
      cat("   Total observations: n =", data_prep$n_final, "\n")
      cat("   Minimum recommended: n ≥", min_n_threshold, "\n")
      if (data_prep$n_final < min_n_threshold) {
        cat("   ✗ INSUFFICIENT DATA - Sample size below recommended minimum\n")
      } else {
        cat("   ✓ Sample size adequate\n")
      }
      cat("\n")

      # Check 2: Outcome variation
      cat("2. OUTCOME VARIATION CHECK\n")
      cat("   Outcome variable:", data_prep$outcome_var, "\n")
      cat("   Number of categories:", n_categories, "\n")
      cat("   Category distribution:\n")
      for (i in seq_len(nrow(outcome_counts))) {
        pct <- round(100 * outcome_counts$n[i] / data_prep$n_final, 1)
        cat("     ", outcome_counts[[data_prep$outcome_var]][i], ": n =",
          outcome_counts$n[i], paste0("(", pct, "%)"), "\n")
      }
      cat("   Minimum per category: n =", min_category_n,
        "(threshold: ≥", min_n_per_category, ")\n")

      if (n_categories < 2) {
        cat("   ✗ NO VARIATION - All observations in one category\n")
      } else if (min_category_n < min_n_per_category) {
        cat("   ✗ INSUFFICIENT VARIATION - Some categories have too few observations\n")
      } else {
        cat("   ✓ Outcome variation adequate\n")
      }
      cat("\n")

      # Check 3: Predictor variance
      cat("3. PREDICTOR VARIANCE CHECK\n")
      zero_var_predictors <- c()
      low_var_predictors <- c()

      for (pred in data_prep$predictors) {
        # Check unique values (removing NA)
        unique_vals <- length(unique(na.omit(data_prep$data[[pred]])))

        if (unique_vals == 1) {
          zero_var_predictors <- c(zero_var_predictors, pred)
          # Show the single value for context
          single_val <- unique(na.omit(data_prep$data[[pred]]))
          cat("   ", pred, ": unique values =", unique_vals,
            "✗ ZERO VARIANCE (all =", single_val, ")\n")
        } else if (unique_vals == 2) {
          # Check if binary has extreme imbalance
          val_counts <- table(data_prep$data[[pred]])
          min_prop <- min(val_counts) / sum(val_counts)
          if (min_prop < 0.05) {
            low_var_predictors <- c(low_var_predictors, pred)
            cat("   ", pred, ": unique values =", unique_vals,
              "⚠ LOW VARIANCE (", round(min_prop * 100, 1),
              "% in minority)\n", sep = "")
          } else {
            cat("   ", pred, ": unique values =", unique_vals, "✓\n")
          }
        } else {
          cat("   ", pred, ": unique values =", unique_vals, "✓\n")
        }
      }

      if (length(zero_var_predictors) > 0) {
        cat("\n   ✗ ZERO VARIANCE PREDICTORS FOUND: ",
          paste(zero_var_predictors, collapse = ", "), "\n")
        cat("   EXPLANATION: These variables have only one unique value after\n")
        cat("                filtering and NA removal. This may occur because:\n")
        cat("                • Filter conditions excluded variation\n")
        cat("                • All observations in this category have same value\n")
        cat("                • Variable is constant for this subset of data\n")
        cat("   ACTION: Remove these variables from model or adjust filters\n")
      } else if (length(low_var_predictors) > 0) {
        cat("\n   ⚠ Low variance predictors: ",
          paste(low_var_predictors, collapse = ", "), "\n")
        cat("   WARNING: Extreme imbalance may cause convergence issues\n")
      } else {
        cat("   ✓ All predictors have sufficient variance\n")
      }
      cat("\n")

      # Check 4: Separation issues
      cat("4. SEPARATION CHECK\n")
      cat("   Testing for perfect/quasi-complete separation...\n")

      # Simple check: For each categorical predictor, see if any level
      # perfectly predicts the outcome
      separation_issues <- c()

      for (pred in data_prep$predictors) {
        if (!is.numeric(data_prep$data[[pred]]) ||
          length(unique(data_prep$data[[pred]])) <= 5) {
          # Treat as categorical
          cross_tab <- table(data_prep$data[[pred]],
            data_prep$data[[data_prep$outcome_var]])

          # Check if any row (predictor level) has all observations in one outcome category
          for (i in seq_len(nrow(cross_tab))) {
            if (sum(cross_tab[i, ] > 0) == 1) {
              separation_issues <- c(separation_issues,
                paste0(pred, " (level '", rownames(cross_tab)[i],
                  "' → outcome '", colnames(cross_tab)[which(cross_tab[i, ] > 0)], "')"))
            }
          }
        }
      }

      if (length(separation_issues) > 0) {
        cat("   ✗ PERFECT SEPARATION DETECTED:\n")
        for (issue in separation_issues) {
          cat("     •", issue, "\n")
        }
      } else {
        cat("   ✓ No obvious separation detected\n")
        cat("   Note: Quasi-complete separation may still exist\n")
      }
      cat("\n")

      # Original error messages
      cat(strrep("-", 80), "\n\n", sep = "")
      cat("UNDERLYING ERROR MESSAGES:\n")
      for (err_name in names(results$errors)) {
        if (grepl("^pom_", err_name)) {
          cat("  ", err_name, ":\n    ", results$errors[[err_name]], "\n")
        }
      }
      cat("\n")

      # Recommendations based on findings
      cat(strrep("-", 80), "\n\n", sep = "")
      cat("RECOMMENDED ACTIONS:\n")

      if (data_prep$n_final < min_n_threshold) {
        cat("  • Increase sample size or relax filter conditions\n")
      }

      if (n_categories < 2 || min_category_n < min_n_per_category) {
        cat("  • Collapse outcome categories with low counts\n")
        cat("  • Check filter conditions - may be excluding too much data\n")
      }

      if (length(zero_var_predictors) > 0) {
        cat("  • Remove zero-variance predictors:",
          paste(zero_var_predictors, collapse = ", "), "\n")
      }

      if (length(low_var_predictors) > 0) {
        cat("  • Consider removing highly imbalanced predictors:",
          paste(low_var_predictors, collapse = ", "), "\n")
      }

      if (length(separation_issues) > 0) {
        cat("  • Remove predictors causing perfect separation\n")
        cat("  • Or: Collapse predictor categories to reduce separation\n")
      }

      cat("  • Try alternative regression methods (multinomial logistic, etc.)\n")
      cat("  • Consult with a statistician about your specific data structure\n\n")
      cat("================================================================================\n\n")
    }
    return(results)
  }

  # Select best by AIC
  aic_values <- sapply(pom_results_by_link, function(x) x$model_stats$aic)
  best_link <- names(which.min(aic_values))

  results$pom_result <- pom_results_by_link[[best_link]]
  results$link_comparison <- tibble(
    link_function = names(aic_values),
    aic = aic_values,
    selected = names(aic_values) == best_link
  ) %>% arrange(aic)

  if (verbose) {
    cat("  Best link:", best_link, "\n")
    cat("  AIC:", round(results$pom_result$model_stats$aic, 2), "\n\n")
  }

  # STEP 4: VIF
  if (verbose) cat("Step 4: Calculating VIF...\n")

  vif_results <- tryCatch({
    calculate_vif(results$pom_result$model)
  }, error = function(e) {
    results$warnings$vif <<- e$message
    return(NULL)
  })

  results$vif_results <- vif_results

  if (!is.null(vif_results) && verbose) {
    high_vif <- vif_results[vif_results$vif > 5, ]
    if (nrow(high_vif) > 0) {
      cat("  WARNING: High VIF (>5) detected\n")
    } else {
      cat("  VIF check passed\n")
    }
    cat("\n")
  }

  # STEP 5: Brant test
  if (run_brant_test) {
    if (verbose) cat("Step 5: Running Brant test...\n")

    brant_results <- tryCatch({
      perform_brant_test(results$pom_result$model, verbose = FALSE)
    }, error = function(e) {
      results$warnings$brant <<- e$message
      return(NULL)
    })

    results$brant_results <- brant_results
  } else {
    if (verbose) cat("Step 5: Skipping Brant test\n\n")
  }

  # STEP 6: Residuals
  if (verbose) cat("Step 6: Calculating residuals...\n")

  residuals <- tryCatch({
    calculate_pom_residuals(results$pom_result, verbose = FALSE)
  }, error = function(e) {
    results$errors$residuals <<- e$message
    return(NULL)
  })

  results$residuals <- residuals

  if (verbose && !is.null(residuals)) {
    cat("  Residuals calculated (n =", nrow(residuals), ")\n\n")
  }

  # STEP 7: Diagnostic plots
  if (verbose) cat("Step 7: Creating diagnostic plots...\n")

  if (!is.null(residuals)) {
    diagnostic_plot <- tryCatch({
      plot_pom_diagnostics(
        pom_result = results$pom_result,
        residuals_tb = residuals,
        plot_title = paste(config_row$model_id, "-", config_row$model_label)
      )
    }, error = function(e) {
      results$warnings$diagnostic_plot <<- e$message
      return(NULL)
    })

    results$diagnostic_plot <- diagnostic_plot

    if (!is.null(diagnostic_plot) && verbose) {
      cat("  Diagnostic plots created\n\n")
    }
  }

  # STEP 8: Coefficient plot (placeholder)
  if (verbose) {
    cat("Step 8: Coefficient plot (TODO)\n")
    cat("  Placeholder - will implement later\n\n")
  }

  results$coefficient_plot <- NULL
  results$status <- "SUCCESS"

  if (verbose) {
    cat("========================================\n")
    cat("ANALYSIS COMPLETE\n")
    cat("========================================\n\n")
  }

  return(results)
}

# RUN POM WITH CATEGORY SPLITS
run_pom_with_splits <- function(
                                config_row,
                                run_brant_test = FALSE,
                                verbose = TRUE
) {
  # Wrapper that handles split_by_category logic
  # If split_by_category=TRUE, runs separate analysis for each category
  # Otherwise runs single analysis
  #
  # Args:
  #   config_row: Single row from regression.configs.tb
  #   run_brant_test: Whether to attempt Brant test
  #   verbose: Print progress messages
  #
  # Returns:
  #   List with:
  #     - is_split: TRUE if multiple categories analyzed
  #     - categories: vector of category names (or NULL if not split)
  #     - results: list of results (one per category, or single result)

  # Check if split_by_category is requested
  should_split <- !is.null(config_row$split_by_category) &&
    !is.na(config_row$split_by_category) &&
    config_row$split_by_category == TRUE

  if (!should_split) {
    # Run single analysis
    single_result <- run_single_pom_analysis(
      config_row = config_row,
      run_brant_test = run_brant_test,
      verbose = verbose
    )

    return(list(
      is_split = FALSE,
      categories = NULL,
      results = list(single_result)
    ))
  }

  # SPLIT BY CATEGORY LOGIC
  if (verbose) {
    cat("\n################################################################################\n")
    cat("SPLIT BY CATEGORY ANALYSIS:", config_row$model_id, "\n")
    cat("################################################################################\n\n")
  }

  # Get the data source
  data_source_name <- config_row$data_source
  if (!exists(data_source_name)) {
    stop(paste("Data source", data_source_name, "not found"))
  }
  source_data <- get(data_source_name)

  # Check if data has category column
  if (!"category" %in% names(source_data)) {
    warning("split_by_category=TRUE but data does not have 'category' column. Running single analysis.")
    single_result <- run_single_pom_analysis(
      config_row = config_row,
      run_brant_test = run_brant_test,
      verbose = verbose
    )
    return(list(
      is_split = FALSE,
      categories = NULL,
      results = list(single_result)
    ))
  }

  # Get unique categories
  all_categories <- unique(source_data$category)
  all_categories <- sort(all_categories[!is.na(all_categories)])

  if (verbose) {
    cat("Found", length(all_categories), "categories to analyze:\n")
    for (cat_name in all_categories) {
      cat("  -", cat_name, "\n")
    }
    cat("\n")
  }

  # Run analysis for each category
  category_results <- list()

  for (i in seq_along(all_categories)) {
    cat_name <- all_categories[i]

    if (verbose) {
      cat("\n================================================================================\n")
      cat("CATEGORY", i, "of", length(all_categories), ":", cat_name, "\n")
      cat("================================================================================\n")
    }

    # Create modified config with filter for this category
    config_for_category <- config_row

    # Add or modify filter_conditions to include category filter
    category_filter <- paste0("category == '", cat_name, "'")

    if (!is.null(config_row$filter_conditions) &&
      !is.na(config_row$filter_conditions) &&
      is.character(config_row$filter_conditions) &&
      nchar(trimws(config_row$filter_conditions)) > 0) {
      # Combine existing filter with category filter
      config_for_category$filter_conditions <- paste0(
        "(",
        config_row$filter_conditions,
        ") & (",
        category_filter,
        ")"
      )
    } else {
      # Use only category filter
      config_for_category$filter_conditions <- category_filter
    }

    # Run analysis for this category
    cat_result <- tryCatch({
      run_single_pom_analysis(
        config_row = config_for_category,
        run_brant_test = run_brant_test,
        verbose = verbose
      )
    }, error = function(e) {
      if (verbose) {
        cat("ERROR for category", cat_name, ":", e$message, "\n")
      }
      return(list(
        status = "FAILED",
        error = e$message,
        model_info = list(
          model_id = config_row$model_id,
          category = cat_name
        )
      ))
    })

    # Store result with category name
    category_results[[cat_name]] <- cat_result
  }

  if (verbose) {
    cat("\n################################################################################\n")
    cat("SPLIT ANALYSIS COMPLETE\n")
    cat("################################################################################\n\n")
  }

  return(list(
    is_split = TRUE,
    categories = all_categories,
    results = category_results
  ))
}

# MULTI-MODEL PIPELINE FUNCTIONS ----

# These functions loop through config table and run multiple models
#   1. Filter for implement = TRUE
#   2. Loop through each config row
#   3. Call run_pom_with_splits() for each model
#   4. Collect results
#   5. Generate summary tables

run_implemented_models <- TRUE

if (run_implemented_models) {
  # Start timer for regression execution
  regression_start_time <- Sys.time()

  cat("\n=== RUNNING ALL IMPLEMENTED MODELS ===\n\n")

  # Filter for models with implement = TRUE
  models_to_run <- regression.configs.tb %>%
    filter(implement == TRUE)

  if (nrow(models_to_run) == 0) {
    cat("NOTE: No models have implement = TRUE in regression.configs.tb\n")
    cat("To run models, set implement = TRUE in the Google Sheets regression.configs tab\n\n")
  } else {
    cat("Found", nrow(models_to_run), "model(s) to run:\n")
    for (i in seq_len(nrow(models_to_run))) {
      cat("  -", models_to_run$model_id[i], ":", models_to_run$model_label[i], "\n")
    }
    cat("\n")

    # Create shared output directory for this run
    run_output_dir <- create_timestamped_output_dir("outputs")
    cat("Output directory:", run_output_dir, "\n\n")

    # Store results for all models
    all_results <- list()

    # Loop through each model with implement = TRUE
    for (i in seq_len(nrow(models_to_run))) {
      current_model <- models_to_run[i, ]
      model_id <- current_model$model_id

      cat("\n", strrep("=", 80), "\n", sep = "")
      cat("RUNNING MODEL:", model_id, "\n")
      cat(strrep("=", 80), "\n\n", sep = "")

      # Validate model configuration before running
      validation_failed <- FALSE
      validation_error <- NULL
      tryCatch({
        validate_model_config(current_model, verbose = TRUE)
      }, error = function(e) {
        validation_error <<- e$message
        validation_failed <<- TRUE
      })

      if (validation_failed) {
        cat("VALIDATION FAILED:", validation_error, "\n\n")
        cat("Skipping this model due to configuration errors.\n\n")
        # Store error result and skip to next model
        all_results[[model_id]] <- list(
          is_split = FALSE,
          categories = NULL,
          results = list(list(
            status = "ERROR",
            model_info = list(model_id = model_id),
            errors = list(validation = validation_error)
          ))
        )
        next
      }

      cat("Config details:\n")
      cat("  Model ID:", current_model$model_id, "\n")
      cat("  Model label:", current_model$model_label, "\n")
      cat("  Outcome:", current_model$outcome_var, "\n")
      cat("  Data source:", current_model$data_source, "\n")
      cat("  Predictors:", current_model$predictors, "\n")
      cat("  Split by category:", current_model$split_by_category, "\n\n")

      # Run the orchestrator with split handling
      current_results <- tryCatch({
        run_pom_with_splits(
          config_row = current_model,
          run_brant_test = FALSE,
          verbose = TRUE
        )
      }, error = function(e) {
        cat("ERROR running model", model_id, ":", e$message, "\n")
        return(list(
          is_split = FALSE,
          categories = NULL,
          results = list(list(
            status = "ERROR",
            model_info = list(model_id = model_id),
            errors = list(orchestrator = e$message)
          ))
        ))
      })

      # Store results
      all_results[[model_id]] <- current_results

      # Export PDF report (only if at least one analysis succeeded)
      if (current_results$is_split) {
        # Multi-category report - check if any categories succeeded
        success_count <- sum(sapply(current_results$results,
          function(r) r$status == "SUCCESS"))
        if (success_count > 0) {
          pdf_path <- export_multi_category_pdf_report(
            split_results = current_results,
            config_row = current_model,
            output_dir = run_output_dir,
            verbose = TRUE
          )
        } else {
          cat("No PDF generated - all categories failed\n")
        }
      } else {
        # Single analysis report (only if successful)
        single_result <- current_results$results[[1]]
        if (single_result$status == "SUCCESS") {
          pdf_path <- export_pdf_report(
            results = single_result,
            output_dir = run_output_dir,
            verbose = TRUE
          )
        } else {
          cat("No PDF generated - analysis failed\n")
        }
      }

      # Display summary of results
      cat("\n", strrep("-", 80), "\n", sep = "")
      cat("RESULTS SUMMARY FOR", model_id, "\n")
      cat(strrep("-", 80), "\n\n", sep = "")

      if (current_results$is_split) {
        # Split analysis summary
        cat("Split Analysis: YES\n")
        cat("Categories analyzed:", length(current_results$categories), "\n")
        cat("Categories:", paste(current_results$categories, collapse = ", "), "\n\n")

        # Summarize each category
        for (cat_name in current_results$categories) {
          cat_result <- current_results$results[[cat_name]]
          cat("Category:", cat_name, "\n")
          cat("  Status:", cat_result$status, "\n")

          if (cat_result$status == "SUCCESS") {
            cat("  Sample size:", cat_result$data_prep$n_final, "\n")
            cat("  AIC:", round(cat_result$pom_result$model_stats$aic, 2), "\n")
          }
          cat("\n")
        }
      } else {
        # Single analysis summary
        single_result <- current_results$results[[1]]
        cat("Split Analysis: NO\n")
        cat("Status:", single_result$status, "\n")

        if (single_result$status == "SUCCESS") {
          cat("Sample size:", single_result$data_prep$n_final, "\n")
          cat("Dropped observations:", single_result$data_prep$n_dropped, "\n\n")

          cat("Model fit:\n")
          cat("  Best link function:", single_result$pom_result$model_stats$link_function, "\n")
          cat("  AIC:", round(single_result$pom_result$model_stats$aic, 2), "\n")
          cat("  BIC:", round(single_result$pom_result$model_stats$bic, 2), "\n")
          cat("  Log-likelihood:", round(single_result$pom_result$model_stats$log_likelihood, 2), "\n\n")

          cat("Coefficients (first 10):\n")
          print(head(single_result$pom_result$coefficients, 10))
          cat("\n")

          if (!is.null(single_result$vif_results)) {
            cat("VIF diagnostics:\n")
            print(single_result$vif_results)
            cat("\n")
          }

          cat("Diagnostic plot: ", ifelse(!is.null(single_result$diagnostic_plot), "Created ✓", "Not created"), "\n")
          cat("Residuals table: ", ifelse(!is.null(single_result$residuals),
            paste(nrow(single_result$residuals), "observations"),
            "Not created"), "\n")

          cat("\nWarnings:", length(single_result$warnings), "\n")
          cat("Errors:", length(single_result$errors), "\n")

          if (length(single_result$warnings) > 0) {
            cat("\nWarning messages:\n")
            for (j in seq_along(single_result$warnings)) {
              cat("  -", names(single_result$warnings)[j], ":", single_result$warnings[[j]], "\n")
            }
          }

          if (length(single_result$errors) > 0) {
            cat("\nError messages:\n")
            for (j in seq_along(single_result$errors)) {
              cat("  -", names(single_result$errors)[j], ":", single_result$errors[[j]], "\n")
            }
          }
        }
      }

      cat("\n", strrep("=", 80), "\n", sep = "")
      cat("MODEL", model_id, "COMPLETE\n")
      cat(strrep("=", 80), "\n\n", sep = "")
    }

    # Overall summary
    cat("\n", strrep("#", 80), "\n", sep = "")
    cat("ALL MODELS COMPLETE\n")
    cat(strrep("#", 80), "\n\n", sep = "")

    cat("Total models run:", length(all_results), "\n")

    # Count successful models (handling split structure)
    successful <- 0
    failed <- 0
    for (model_id in names(all_results)) {
      result <- all_results[[model_id]]
      if (result$is_split) {
        # For split analyses, check if any category succeeded
        any_success <- any(sapply(result$results, function(x) x$status == "SUCCESS"))
        if (any_success) successful <- successful + 1 else failed <- failed + 1
      } else {
        # For single analyses
        if (result$results[[1]]$status == "SUCCESS") successful <- successful + 1 else failed <- failed + 1
      }
    }

    cat("Successful:", successful, "\n")
    cat("Failed:", failed, "\n\n")

    if (successful > 0) {
      cat("Successfully completed models:\n")
      for (model_id in names(all_results)) {
        result <- all_results[[model_id]]
        if (result$is_split) {
          any_success <- any(sapply(result$results, function(x) x$status == "SUCCESS"))
          if (any_success) {
            model_label <- result$results[[1]]$model_info$model_label
            cat("  ✓", model_id, "-", model_label, "\n")
          }
        } else {
          if (result$results[[1]]$status == "SUCCESS") {
            cat("  ✓", model_id, "-", result$results[[1]]$model_info$model_label, "\n")
          }
        }
      }
      cat("\n")
    }

    if (failed > 0) {
      cat("Failed models:\n")
      for (model_id in names(all_results)) {
        result <- all_results[[model_id]]
        if (result$is_split) {
          all_failed <- all(sapply(result$results, function(x) x$status != "SUCCESS"))
          if (all_failed) {
            # Get error details from first failed category
            first_failed <- result$results[[1]]
            cat("  ✗", model_id, "\n")
            if (!is.null(first_failed$errors) && length(first_failed$errors) > 0) {
              cat("    Reason:", first_failed$status, "\n")
              # Check if this is a configuration error (missing variables)
              if (!is.null(first_failed$errors$data_prep) &&
                grepl("CONFIGURATION ERROR", first_failed$errors$data_prep)) {
                cat("    ⚠  Configuration issue detected - see error details above\n")
              }
            }
          }
        } else {
          if (result$results[[1]]$status != "SUCCESS") {
            cat("  ✗", model_id, "\n")
            if (!is.null(result$results[[1]]$errors) && length(result$results[[1]]$errors) > 0) {
              cat("    Reason:", result$results[[1]]$status, "\n")
              if (!is.null(result$results[[1]]$errors$data_prep) &&
                grepl("CONFIGURATION ERROR", result$results[[1]]$errors$data_prep)) {
                cat("    ⚠  Configuration issue detected - see error details above\n")
              }
            }
          }
        }
      }
      cat("\n")
      cat("ACTION REQUIRED FOR FAILED MODELS:\n")
      cat("  • Review the error messages above for each failed model\n")
      cat("  • Fix configuration issues in the Google Sheets regression.configs tab\n")
      cat("  • Re-run the script after making corrections\n\n")
    }

    # Display execution time
    regression_end_time <- Sys.time()
    regression_duration <- as.numeric(difftime(regression_end_time, regression_start_time, units = "secs"))

    # Format duration in human-readable format
    if (regression_duration < 60) {
      duration_str <- sprintf("%.1f seconds", regression_duration)
    } else if (regression_duration < 3600) {
      minutes <- floor(regression_duration / 60)
      seconds <- regression_duration %% 60
      duration_str <- sprintf("%d min %.0f sec", minutes, seconds)
    } else {
      hours <- floor(regression_duration / 3600)
      minutes <- floor((regression_duration %% 3600) / 60)
      duration_str <- sprintf("%d hr %d min", hours, minutes)
    }

    cat(strrep("-", 80), "\n", sep = "")
    cat("EXECUTION TIME\n")
    cat(strrep("-", 80), "\n", sep = "")
    cat("Total time:", duration_str, "\n")
    cat("Started:", format(regression_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Finished:", format(regression_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat(strrep("-", 80), "\n\n", sep = "")
  }
}
