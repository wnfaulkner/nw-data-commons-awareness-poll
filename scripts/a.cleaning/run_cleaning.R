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
source("scripts/c.helper_functions/00_config.R")
source("scripts/c.helper_functions/01_validation.R")
source("scripts/c.helper_functions/02_data_processing.R")
source("scripts/c.helper_functions/03_regression_core.R")
source("scripts/c.helper_functions/04_regression_diagnostics.R")
source("scripts/c.helper_functions/05_plotting.R")
source("scripts/c.helper_functions/06_pdf_export.R")
source("scripts/c.helper_functions/07_orchestration.R")

# SECTION CLOCKING
section0.duration <- Sys.time() - section0.starttime
section0.duration

# 1-IMPORT DATA & CONFIGS -------------------------------------------------------------------------------

cat("Importing data from local CSV files...\n")

# Load source data
data.tb <- read_csv(
  file.path(wd, "source_data/source_data_raw.csv"),
  show_col_types = FALSE
) %>%
  janitor::remove_empty("cols")

# Load config files
questions.tb <- read_csv(
  file.path(wd, "configs/questions.csv"),
  show_col_types = FALSE
) %>%
  janitor::remove_empty("cols") %>%
  # Convert q.id to character for matching with column names
  mutate(q.id = as.character(q.id))

response.options.tb <- read_csv(
  file.path(wd, "configs/response_options.csv"),
  show_col_types = FALSE
) %>%
  janitor::remove_empty("cols")

# Note: regression.configs.tb not loaded from CSV as it's not used in publication analyses
# The RQ-specific scripts (RQ1-RQ5) implement models directly rather than via config table
regression.configs.tb <- NULL

cat("  - Source data loaded:", nrow(data.tb), "rows\n")
cat("  - Questions config loaded:", nrow(questions.tb), "rows\n")
cat("  - Response options config loaded:", nrow(response.options.tb), "rows\n\n")

# Validate imported data structure (skip regression.configs.tb validation)
if (!is.null(regression.configs.tb)) {
  validate_google_sheets_data(
    data.tb = data.tb,
    questions.tb = questions.tb,
    response.options.tb = response.options.tb,
    regression.configs.tb = regression.configs.tb,
    verbose = TRUE
  )
} else {
  cat("Validation: Skipping regression.configs.tb (not used in publication analyses)\n\n")
}

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

# CREATE COLLAPSED CATEGORICAL VARIABLES FOR REGRESSION
data.tb <- create_collapsed_categories(data.tb)

# DEFINE EXTRA ID VARIABLES (derived variables to include in reshaped tables)
# Include calculated variables based on single columns (not multi-column aggregates like means)
extra.id.vars <- c(
  "age.group", "nationality.native", "language.native",
  # Collapsed categorical variables
  "political.affiliation.collapsed", "ethnicity.collapsed",
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

# Export cleaned main dataset to CSV for use by analysis scripts
cat("Exporting cleaned data to CSV...\n")
cleaned_data_path <- file.path(wd, "source_data/source_data_clean.csv")
write_csv(data.tb, cleaned_data_path)
cat("  ✓ Cleaned data saved to:", cleaned_data_path, "\n")
cat("  ✓ Sample size:", nrow(data.tb), "rows,", ncol(data.tb), "columns\n\n")

# Optional: Export to Excel for manual review
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

