#00000000000000000000000000000000000000000000000000000000000#
#0000       2024-11 NW Awareness Poll Data              0000#
#00000000000000000000000000000000000000000000000000000000000#

# 0-SETUP -------------------------------------------------------------------------------
	
  # INITIAL SETUP
    rm(list=ls()) #Remove lists
    gc()
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
    
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
    #library(ncdf4)
    library(janitor)
    library(lubridate)
    library(openxlsx2)
    #library(MASS)         # For polr (proportional odds logistic regression)
    library(ggplot2)
    #library(ggpubr)       # For statistical plots
    #library(psych)        # For descriptive stats
    #library(Hmisc)        # For rcorr if needed
    #library(broom)

  
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
        function(x){
          read_sheet(data_and_configs.ss, sheet = x)
        }
      )
    
    names(data.and.configs.ls) <- sheet.names.v
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(data.and.configs.ls) #Converts list elements to separate tibble objects names with their respective sheet names with ".tb" appended
    

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
        shown.infographic == "B" ~ "no infographic",
      ))

  # RECODE TEXT VALUES FROM RESPONSE OPTIONS

    #Step 1: Create a lookup table that connects var.name (from data.tb) to q.theme (in response.options.tb)
    recode.mapping.tb <- questions.tb %>%
      dplyr::filter(q.theme %in% unique(response.options.tb$q.theme)) %>%
      dplyr::select(var.name, q.theme)

    #Step 2: Recode each variable based on the response.options.tb
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

    #Step 3: Apply to each variable that needs recoding
    walk2(recode.mapping.tb$var.name, recode.mapping.tb$q.theme, recode_from_theme)

    # data.tb <- reduce2(
    #   recode.mapping.tb$var.name,
    #   recode.mapping.tb$q.theme,
    #   .init = data.tb,
    #   .f = function(dt, varname, theme) {
    #     # same recode logic, but return dt
    #     ...
    #     dt[[varname]] <- ...
    #     dt
    #   }
    # )

    # data.tb <- data.tb %>%
    #   mutate(across(
    #     everything(),
    #     ~ifelse(.x == "0. not participating", NA, .x)
    #   ))
  # RECODE POLITICAL AFFILIATION & ADD USEFUL VARIABLES -----
    data.tb %<>%
      mutate(
        political.affiliation = dplyr::case_when(
          political.affiliation %in% c("Don't know", "Dont know") ~ "Don't Know",
          political.affiliation %in% c("Other") ~ "Other",
          political.affiliation %in% c("Would not vote") ~ "Would not vote",
          political.affiliation %in% c("Democrat") ~ "USA-Democrat",
          political.affiliation %in% c("Republican") ~ "USA-Republican",
          political.affiliation %in% c("Independent","independent") & country.of.residence == "United States" ~ "USA-Independent",
          political.affiliation %in% c("Independent","independent") & country.of.residence == "United Kingdom" ~ "UK-Independent",
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
    extra.id.vars <- c("age.group", "nationality.native", "language.native")

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
    relocate(political.affiliation, .after = sex)

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
      )

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
    relocate(political.affiliation, .after = sex)

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
    relocate(political.affiliation, .after = sex)

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
      lapply(., function(x){x %>% select(-any_of(c("q.num", "q.id","q.theme")))}) # Remove q.num and q.id if they exist in any table
    
    names(export.ls) <- clean_table_names[clean_object_names %in% ls()]


# 3-EXPORT CLEANED DATA ------------------------------------------------------------------------------- 
  export.logical <- FALSE

  if(export.logical){
  
  # DEFINE & CREATE OUTPUT DIRECTORY

    output.base.name <-
      Sys.time() %>%
      gsub(":",".",.)

    output.dir <-
      paste(
        wd,
        "/outputs/",
        output.base.name,
        "_data/",
        sep = ""
      )
    
    if(output.dir %>% dir.exists %>% not){
      dir.create(output.dir, recursive = TRUE)
    }
    
  # WRITE CSV FILES INTO OUTPUT DIRECTORY
    
    # ExportCsvs <- 
    #   function(table, table_name){
    #     file.name <- paste(table_name,"_",output.base.name,".csv",sep="")
    #     write.csv(table, file.name, row.names = FALSE, na = "")
    #   }
    
    # setwd(output.dir)
    # Map(
    #   ExportCsvs,
    #   export.ls,
    #   names(export.ls)
    # )
    
  # WRITE TABLES INTO SINGLE EXCEL FILE IN OUTPUT DIRECTORY
    
    output.file.path <-
      paste(output.dir, "Reformatted Data_Analysis_",Sys.Date(),".xlsx", sep = "") %>%
      file.path(.)

    wb <- wb_workbook()

    for (i in seq_along(export.ls)) {
      sheet_name <- names(export.ls)[i]  # Get the name of the list element (if named)
      if (is.null(sheet_name) || sheet_name == "") {
        sheet_name <- paste0("Sheet", i)  # Assign default sheet names if missing
      }

      wb$add_worksheet(sheet_name)  # Create a new sheet
      wb$add_data(sheet = sheet_name, x = export.ls[[i]])  # Write data
    }

    wb$save(output.file.path, overwrite = TRUE)

    cat("Excel file successfully saved at:", output.file.path, "\n") # Print confirmation

  # CODE CLOCKING
    code.duration <- Sys.time() - sections.all.starttime
    code.duration
    
  }

# 4-ANALYSIS, STATISTICAL TESTS -------------------------------------------------------------------------------
# 4A-BIVARIATE TESTS
  run.bivariate.tests <- FALSE

  if(run.bivariate.tests){  
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
        gsub(":",".",.)

      bv.output.dir <-
        paste(
          wd,
          "/outputs/",
          bv.output.base.name,
          "_bv.tests/",
          sep = ""
        )

      if(bv.output.dir %>% dir.exists %>% not){
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


# 4B-REGRESSIONS

  #Regression Helper Functions ----

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
          formula_str <- paste(colnames(X)[i], "~",
                              paste(colnames(X)[-i], collapse = " + "))
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
        if (!is.na(config_row$filter_conditions) && config_row$filter_conditions != "") {
          source_data <- source_data %>%
            filter(eval(parse(text = config_row$filter_conditions)))
        }

        # Get outcome and predictor variables
        outcome_var <- config_row$outcome_var
        predictors <- strsplit(config_row$predictors, ",\\s*")[[1]]

        # Add control variables if specified
        if (!is.na(config_row$control_vars) && config_row$control_vars != "") {
          control_vars <- strsplit(config_row$control_vars, ",\\s*")[[1]]
          predictors <- c(predictors, control_vars)
        }

        # Select relevant columns
        all_vars <- unique(c(outcome_var, predictors))

        # For wide data, ensure all variables exist
        missing_vars <- setdiff(all_vars, names(source_data))
        if (length(missing_vars) > 0) {
          stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
        }

        # Create regression dataset
        regression_data <- source_data %>%
          select(all_of(all_vars)) %>%
          drop_na()  # Remove rows with any NA in the model variables

        return(list(
          data = regression_data,
          outcome_var = outcome_var,
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
        if (!is.na(interaction_terms) && interaction_terms != "") {
          interactions <- strsplit(interaction_terms, ",\\s*")[[1]]
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
        z_crit <- qnorm(1 - alpha/2)

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

    # TEST BRANT TEST FUNCTION
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


  #Regressions ----

  # SECTION 5A: DIAGNOSTIC & VISUALIZATION FUNCTIONS ----

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
        combined_plot <- grid.arrange(
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
      # TODO: Implement plot_pom_coefficients(pom_result, title)
      # Will create forest plot showing odds ratios with confidence intervals


  # SECTION 5B: SINGLE-MODEL ORCHESTRATION FUNCTIONS ----

    # These functions orchestrate the complete analysis for a single regression model

    # RUN SINGLE POM ANALYSIS
      # TODO: Implement run_single_pom_analysis(config_row)
      # Will:
      #   1. Prepare data using prepare_regression_data()
      #   2. Build formula using build_formula()
      #   3. Check cell counts
      #   4. Fit POM using fit_pom()
      #   5. Calculate VIF
      #   6. Run Brant test (if available)
      #   7. Calculate residuals
      #   8. Generate diagnostic plots
      #   9. Generate coefficient plot
      #  10. Return comprehensive results object


  # SECTION 5C: MULTI-MODEL PIPELINE FUNCTIONS ----

    # These functions loop through config table and run multiple models

    # RUN ALL CONFIGURED REGRESSIONS
      # TODO: Implement run_all_regressions(regression.configs.tb)
      # Will:
      #   1. Filter for implement = TRUE
      #   2. Loop through each config row
      #   3. Handle split_by_category logic
      #   4. Call run_single_pom_analysis() for each model
      #   5. Collect results
      #   6. Generate summary tables


  # SECTION 5D: EXPORT FUNCTIONS ----

    # These functions export results to files

    # EXPORT REGRESSION RESULTS
      # TODO: Implement export_regression_results(results_list, output_dir)
      # Will export:
      #   1. PDF with all diagnostic plots
      #   2. PDF with all coefficient plots
      #   3. Excel/CSV with coefficient tables
      #   4. Excel/CSV with model fit statistics
      #   5. Excel/CSV with assumption test results


  # SECTION 5E: EXAMPLE/TEST CODE ----

    # This section contains one-off example code for testing and development
    # Will be replaced with function calls as we build out the pipeline

    # EXAMPLE 1: OLS REGRESSION (original example - kept for reference)
    run_ols_example <- FALSE

    if (run_ols_example) {
      # Calculate mean awareness score per respondent
      example_outcome_data.tb <- awareness.tb %>%
        group_by(across(all_of(id.varnames))) %>%
        summarize(
          mean_outcome = mean(value.num, na.rm = TRUE),
          n_responses = n(),
          .groups = "drop"
        )

      # Define exclusions and base categories for all variables
      example_var_config <- list(
        mean_outcome = list(exclude = c(), base = NA),
        age = list(exclude = c(), base = NA),
        sex = list(exclude = c("Prefer not to say"), base = "Male"),
        ethnicity = list(exclude = c(), base = "White"),
        nationality.native = list(exclude = c(), base = NA),
        language.native = list(exclude = c(), base = NA),
        student.status = list(exclude = c(), base = NA),
        employment.status = list(exclude = c(), base = NA),
        country.of.residence = list(exclude = c("United Kingdom"), base = NA),
        political.affiliation = list(exclude = c("Don't Know", "Other", "Would not vote"), base = "USA-Democrat")
      )

      # Get all variable names from config (excluding outcome which is already in data)
      vars_to_join <- setdiff(names(example_var_config), "mean_outcome")

      # Join all variables from data.tb
      example_outcome_data.tb <- example_outcome_data.tb %>%
        left_join(
          data.tb %>% select(all_of(c(id.varnames, vars_to_join))),
          by = id.varnames
        )

      # Apply filtering for all variables
      example_data.tb <- example_outcome_data.tb

      for (var_name in names(example_var_config)) {
        if (var_name %in% names(example_data.tb)) {
          exclude_vals <- example_var_config[[var_name]]$exclude

          # Filter out excluded values and NAs
          example_data.tb <- example_data.tb %>%
            filter(!is.na(.data[[var_name]]))

          if (length(exclude_vals) > 0) {
            example_data.tb <- example_data.tb %>%
              filter(!(.data[[var_name]] %in% exclude_vals))
          }
        }
      }

      # Set base categories for factor variables
      for (var_name in names(example_var_config)) {
        base_val <- example_var_config[[var_name]]$base

        if (!is.na(base_val) && var_name %in% names(example_data.tb)) {
          # Get all unique values for this variable
          all_vals <- unique(example_data.tb[[var_name]])

          # Create factor with base category FIRST in levels (becomes reference)
          new_levels <- c(base_val, setdiff(all_vals, base_val))

          example_data.tb <- example_data.tb %>%
            mutate(!!var_name := factor(.data[[var_name]], levels = new_levels))
        }
      }

      # Remove variables with only 1 unique value (cannot be used in regression)
      vars_to_remove <- c()

      for (var_name in names(example_var_config)) {
        if (var_name %in% names(example_data.tb)) {
          n_unique <- length(unique(example_data.tb[[var_name]]))

          if (n_unique <= 1) {
            vars_to_remove <- c(vars_to_remove, var_name)
          }
        }
      }

      if (length(vars_to_remove) > 0) {
        cat("WARNING: Removing variables with ≤1 unique value:\n")
        for (var in vars_to_remove) {
          cat("  - ", var, "\n", sep = "")
        }
        cat("\n")

        example_data.tb <- example_data.tb %>%
          select(-all_of(vars_to_remove))
      }

      cat("Sample size for regression:", nrow(example_data.tb), "\n\n")

      # Fit the model
      example_model <- lm(mean_outcome ~
                      age + sex + ethnicity + nationality.native + language.native + student.status +
                      employment.status + political.affiliation,
                      data = example_data.tb)

      cat("MODEL SUMMARY:\n") # Display summary

      # Get the summary object
      example_summary <- summary(example_model)

      # Modify coefficient names to add underscores between variable names and values
      coef_names <- rownames(example_summary$coefficients)

      # Create a function to add underscores
      add_underscores_to_coefs <- function(coef_name) {
        # Skip intercept
        if (coef_name == "(Intercept)") {
          return(coef_name)
        }

        # Sort variable names by length (longest first) to match compound names first
        var_names_sorted <- names(example_var_config)[order(-nchar(names(example_var_config)))]

        # For each variable in example_var_config, check if coef_name starts with it
        for (var_name in var_names_sorted) {
          if (startsWith(coef_name, var_name)) {
            # Extract the value part (everything after the variable name)
            value_part <- substring(coef_name, nchar(var_name) + 1)

            # If there's a value part, add underscore
            if (nchar(value_part) > 0) {
              return(paste0(var_name, "_", value_part))
            } else {
              return(coef_name)  # No value part (e.g., continuous variable)
            }
          }
        }

        return(coef_name)  # No match found, return original
      }

      # Apply the function to all coefficient names
      new_coef_names <- sapply(coef_names, add_underscores_to_coefs, USE.NAMES = FALSE)
      rownames(example_summary$coefficients) <- new_coef_names

      # Print the modified summary
      print(example_summary)
    }  # End run_ols_example

    # EXAMPLE 2: POM TEST - Testing fit_pom() and calculate_pom_residuals()
      test_model_002 <- TRUE

      if (test_model_002) {
        cat("\n=== TESTING MODEL_002 WITH CALCULATE_POM_RESIDUALS ===\n\n")

        # Use model_002 configuration from regression.configs.tb
        # outcome_var: value.num
        # data_source: awareness.tb
        # predictors: age, sex, ethnicity, political.affiliation
        # NOTE: shown.infographic would be constant within a single category,
        # so we exclude it for this single-category test

        # Prepare test data for first category
        first_category <- unique(awareness.tb$category)[1]
        cat("Testing with category:", first_category, "\n\n")

        test_data_m002 <- awareness.tb %>%
          filter(category == first_category) %>%
          select(value.num, age, sex, ethnicity, political.affiliation) %>%
          drop_na() %>%
          mutate(
            sex = factor(sex),
            ethnicity = factor(ethnicity),
            political.affiliation = factor(political.affiliation)
          )

        cat("Test data prepared for model_002:\n")
        cat("  n =", nrow(test_data_m002), "\n")
        cat("  Outcome levels:", paste(sort(unique(test_data_m002$value.num)), collapse = ", "), "\n")
        cat("  Predictor levels:\n")
        cat("    sex:", paste(levels(test_data_m002$sex), collapse = ", "), "\n")
        cat("    ethnicity:", paste(levels(test_data_m002$ethnicity), collapse = ", "), "\n")
        cat("    political.affiliation:", length(levels(test_data_m002$political.affiliation)), "levels\n\n")

        if (nrow(test_data_m002) > 0) {
          # Fit POM model
          cat("Step 1: Fitting POM model...\n\n")
          test_formula_m002 <- value.num ~ age + sex + ethnicity + political.affiliation

          pom_result_m002 <- fit_pom(
            formula = test_formula_m002,
            data = test_data_m002,
            link = "logit",
            confidence_level = 0.95,
            verbose = TRUE
          )

          cat("\nModel fitted successfully!\n\n")
          cat("Coefficients table:\n")
          print(pom_result_m002$coefficients)
          cat("\n")

          # Calculate residuals
          cat("Step 2: Calculating residuals...\n\n")
          residuals_m002 <- calculate_pom_residuals(pom_result_m002, verbose = TRUE)

          cat("\nResiduals calculated!\n\n")
          cat("First 10 rows of residuals table:\n")
          print(head(residuals_m002, 10))
          cat("\n")

          cat("Summary statistics for residuals:\n")
          cat("  Pearson residuals:\n")
          cat("    Mean:", round(mean(residuals_m002$pearson_residual), 4), "\n")
          cat("    Median:", round(median(residuals_m002$pearson_residual), 4), "\n")
          cat("    SD:", round(sd(residuals_m002$pearson_residual), 4), "\n")
          cat("  Deviance residuals:\n")
          cat("    Mean:", round(mean(residuals_m002$deviance_residual), 4), "\n")
          cat("    Median:", round(median(residuals_m002$deviance_residual), 4), "\n")
          cat("    SD:", round(sd(residuals_m002$deviance_residual), 4), "\n")
          cat("\n")

          # Test diagnostic plots
          cat("Step 3: Creating diagnostic plots...\n\n")
          diagnostic_plot_m002 <- plot_pom_diagnostics(
            pom_result = pom_result_m002,
            residuals_tb = residuals_m002,
            plot_title = paste("Model 002 Diagnostics:", first_category)
          )

          cat("Diagnostic plots created successfully!\n")
          cat("(View the plot in your R graphics device)\n\n")

          cat("=== TEST COMPLETE ===\n\n")

        } else {
          cat("ERROR: No data available for testing model_002.\n\n")
        }
      }
