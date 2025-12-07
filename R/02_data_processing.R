# ==============================================================================
# 02_data_processing.R
# Data cleaning, reshaping, and recoding functions
# ==============================================================================

# RECODING FUNCTION
recode_from_theme <- function(varname, theme) {
  # Recode a variable based on response options for its theme
  #
  # Args:
  #   varname: Name of variable to recode
  #   theme: Theme category for the variable
  #
  # Returns:
  #   NULL (modifies data.tb in global environment)

  if (!varname %in% names(data.tb)) return(NULL)

  # Get response options for this theme
  options.tb <- response.options.tb %>%
    dplyr::filter(q.theme == theme) %>%
    dplyr::select(response.option, response.text)

  if (nrow(options.tb) == 0) return(NULL)

  # Ensure column is character for safe indexing
  data.tb[[varname]] <- as.character(data.tb[[varname]])

  # Build named vector for recoding
  recode.vector <- setNames(
    options.tb$response.text,
    as.character(options.tb$response.option)
  )

  # Perform recoding using named vector
  data.tb[[varname]] <<- recode.vector[data.tb[[varname]]]
}

# NUMERIC EXTRACTION FUNCTION
extract_numeric_from_ordinal <- function(x) {
  # Extract first numeric character from ordinal responses like "1. never heard"
  #
  # Args:
  #   x: Ordinal response string
  #
  # Returns:
  #   Numeric value (1-5) or NA

  if (is.na(x)) return(NA_real_)
  first_char <- substr(as.character(x), 1, 1)
  if (grepl("^[0-9]$", first_char)) {
    return(as.numeric(first_char))
  } else {
    return(NA_real_)
  }
}

# RESHAPE FUNCTION
ReshapeThemeTable <- function(theme, data_table, questions_table,
                               response_options_table,
                               drop_not_participating = FALSE,
                               extra_id_vars = NULL) {
  # Reshape wide data to long format for a specific theme
  #
  # Args:
  #   theme: Theme category to reshape (e.g., "awareness", "casualty.causes")
  #   data_table: Wide-format data tibble
  #   questions_table: Questions metadata
  #   response_options_table: Response options lookup
  #   drop_not_participating: Remove "not participating" rows
  #   extra_id_vars: Additional ID variables to include
  #
  # Returns:
  #   Long-format tibble with value.text and value.num columns

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
    dplyr::select(q.theme, var.name, q.num)

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
      value.text = ifelse(value.text == "0. not participating",
                          NA, value.text),
      value.num = ifelse(value.num == 0, NA_real_, as.numeric(value.num))
    )

  # 8. Optionally drop rows that were non-participants
  if (drop_not_participating) {
    long.tb <- long.tb %>%
      dplyr::filter(!is.na(value.num))
  }

  return(long.tb)
}

cat("Data processing functions loaded successfully.\n")
