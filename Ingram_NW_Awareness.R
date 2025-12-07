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

# LOAD MODULES
source("R/00_config.R")
source("R/01_validation.R")
source("R/02_data_processing.R")
source("R/03_regression_core.R")
source("R/04_regression_diagnostics.R")
source("R/05_plotting.R")
source("R/06_pdf_export.R")
source("R/07_orchestration.R")

# SECTION CLOCKING
section0.duration <- Sys.time() - section0.starttime
section0.duration

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
    shown.infographic == "A" ~ "Shown NW Iinfographic",
    shown.infographic == "B" ~ "No Infographic"
  ))

# RECODE TEXT VALUES FROM RESPONSE OPTIONS

# Step 1: Create a lookup table that connects var.name (from data.tb) to q.theme (in response.options.tb)
recode.mapping.tb <- questions.tb %>%
  dplyr::filter(q.theme %in% unique(response.options.tb$q.theme)) %>%
  dplyr::select(var.name, q.theme)

# Step 2: Apply to each variable that needs recoding
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

# 4B-REGRESSION EXECUTION -------------------------------------------------------------------------------

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

          cat("Diagnostic plot: ", ifelse(!is.null(single_result$diagnostic_plot), "Created", "Not created"), "\n")
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
            cat("  -", model_id, "-", model_label, "\n")
          }
        } else {
          if (result$results[[1]]$status == "SUCCESS") {
            cat("  -", model_id, "-", result$results[[1]]$model_info$model_label, "\n")
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
            cat("  -", model_id, "\n")
            if (!is.null(first_failed$errors) && length(first_failed$errors) > 0) {
              cat("    Reason:", first_failed$status, "\n")
              # Check if this is a configuration error (missing variables)
              if (!is.null(first_failed$errors$data_prep) &&
                grepl("CONFIGURATION ERROR", first_failed$errors$data_prep)) {
                cat("    Configuration issue detected - see error details above\n")
              }
            }
          }
        } else {
          if (result$results[[1]]$status != "SUCCESS") {
            cat("  -", model_id, "\n")
            if (!is.null(result$results[[1]]$errors) && length(result$results[[1]]$errors) > 0) {
              cat("    Reason:", result$results[[1]]$status, "\n")
              if (!is.null(result$results[[1]]$errors$data_prep) &&
                grepl("CONFIGURATION ERROR", result$results[[1]]$errors$data_prep)) {
                cat("    Configuration issue detected - see error details above\n")
              }
            }
          }
        }
      }
      cat("\n")
      cat("ACTION REQUIRED FOR FAILED MODELS:\n")
      cat("  - Review the error messages above for each failed model\n")
      cat("  - Fix configuration issues in the Google Sheets regression.configs tab\n")
      cat("  - Re-run the script after making corrections\n\n")
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
