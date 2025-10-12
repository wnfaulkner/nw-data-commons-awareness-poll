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
    library(ncdf4)
    library(janitor)
    library(lubridate)
    library(openxlsx)
    library(MASS)         # For polr (proportional odds logistic regression)
    library(ggplot2)
    library(ggpubr)       # For statistical plots
    library(psych)        # For descriptive stats
    library(Hmisc)        # For rcorr if needed
    library(broom)

  
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
      dplyr::select(-started.at, -reviewed.at, -archived.at, -completion.code, -total.approvals, -status, -submission.id)

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

    data.tb <- data.tb %>%
      mutate(across(
        everything(),
        ~ifelse(.x == "0. not participating", NA, .x)
      ))

  # RESHAPE - DEFINE ABSTRACTED FUNCTION
    ReshapeThemeTable <- function(theme, data_table, questions_table, response_options_table, drop_not_participating = FALSE) {  
      # 1. Identify ID vars
      id.vars <- questions_table %>%
        dplyr::filter(var.category == "id.var") %>%
        dplyr::pull(var.name)
      
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

      # 6. Convert "0. not participating" → NA
      long.tb <- long.tb %>%
        mutate(value.text = ifelse(value.text == "0. not participating", NA, value.text))
      
      # 7. Optionally drop rows that were non-participants (after conversion to NA)
      if (drop_not_participating) {
        long.tb <- long.tb %>%
          dplyr::filter(shown.infographic == "shown infographic")
      }

      return(long.tb)
    }

  # RESHAPE - AWARENESS
    awareness.tb <- ReshapeThemeTable(
      theme = "awareness",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb,
      drop_not_participating = TRUE
    )

  # RESHAPE - CASUALTY CAUSES
    casualty.causes.tb <- ReshapeThemeTable(
      theme = "casualty.causes",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb,
      drop_not_participating = TRUE
    )

  # RESHAPE - SUPPORT REACTION
    support.reaction.tb <- ReshapeThemeTable(
      theme = "support.reaction",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb
    )

  # RESHAPE - DECISION FACTORS
    decision.factors.tb <- ReshapeThemeTable(
      theme = "decision.factors",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb
    )

# 3-ANALYSIS, STATISTICAL TESTS -------------------------------------------------------------------------------
  
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

  # NUCLEAR WINTER AWARENESS

    data.awareness.tb <- data.tb %>%
    dplyr::filter(
      !is.na(nw.awareness.1980s)
    ) %>%
    mutate(across(
      c(
        nw.awareness.1980s,
        nw.awareness.recent.media,
        nw.awareness.recent.academic
      ),
      convert_character_var_to_factor
    )) 

    # Data Summaries
    cor.test(as.numeric(data.awareness.tb$nw.awareness.1980s), data.awareness.tb$age, method = "spearman")

    # ggplot(data.awareness.tb %>% dplyr::filter(!is.na(nw.awareness.1980s)), aes(x = sex, y = nw.awareness.1980s)) +
    #   geom_jitter(width = 0.3, height = 0.1) +
    #   stat_smooth(method = "loess") +
    #   labs(title = "Awareness vs Sex")

    # awareness_colors <- c(
    #   "never heard"     = "#d4f0ff",  # lightest
    #   "heard a little"  = "#88ccee",
    #   "know something"  = "#4477aa",
    #   "know a lot"      = "#223366"   # darkest
    # )

    # ggplot(data.awareness.tb %>% dplyr::filter(!is.na(nw.awareness.1980s)),
    #   aes(x = sex, fill = nw.awareness.1980s)) +
    #   geom_bar(position = "fill") +
    #   scale_y_continuous(labels = percent_format()) +
    #   scale_fill_manual(values = awareness_colors) +
    #   labs(
    #     title = "Awareness of Nuclear Weapons in the 1980s by Sex",
    #     x = "Sex",
    #     y = "Proportion of Respondents",
    #     fill = "Awareness Level"
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     text = element_text(size = 14),
    #     legend.position = "right"
    #   )

    # Bivariate Statistical Tests
      kruskal.test(as.numeric(nw.awareness.1980s) ~ sex, data = data.awareness.tb)
      kruskal.test(as.numeric(nw.awareness.1980s) ~ ethnicity, data = data.awareness.tb)
      

    # Regression
    data.awareness.polr.tb <- data.awareness.tb %>%
      filter(sex %in% c("Female","Male")) %>%
      mutate(sex = relevel(factor(sex), ref = "Male"))
      
    model <- polr(
      nw.awareness.1980s ~ sex + age + ethnicity + political.affiliation +
      nationality + employment.status + student.status + language,
      data = data.awareness.polr.tb, Hess = TRUE
    )

    summary(model)

    coef_df <- broom::tidy(model) %>%
    filter(term == "sexFemale")  # focus on just the 'sex' coefficient

    ggplot(coef_df, aes(x = term, y = estimate)) + # Plot
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = estimate - std.error * 1.96,
                        ymax = estimate + std.error * 1.96),
                    width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(
        title = "Effect of Sex (Female vs Male) on Awareness",
        x = "",
        y = "Log-Odds Coefficient"
      ) +
      theme_minimal(base_size = 14)












    ctable <- coef(summary(model))
    pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    cbind(ctable, "p value" = pvals)

    ggplot(data.awareness.tb, aes(x = awareness, fill = sex)) +
      geom_bar(position = "fill") +
      facet_wrap(~nationality) +
      labs(title = "Awareness Distribution by Sex and Nationality")
    ggplot(data.awareness.tb, aes(x = awareness, y = age)) +
      geom_boxplot() +
      labs(title = "Age by Awareness Level")


# 3-EXPORT -------------------------------------------------------------------------------
  
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
      purrr::compact() # Remove NULL entries for non-existent tibbles
    
    names(export.ls) <- clean_table_names[clean_object_names %in% ls()]
  
  # DEFINE & CREATE OUTPUT DIRECTORY
    
    #setwd(paste(wd, "\\2. Reformatted Source Data", sep=""))
    
    output.base.name <- 
      Sys.time() %>% 
      gsub(":",".",.) 
      
    output.dir <- 
      paste(
        wd,
        "/outputs/",
        output.base.name,
        "/",
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
      
    wb <- createWorkbook()
    
    for (i in seq_along(export.ls)) {
      sheet_name <- names(export.ls)[i]  # Get the name of the list element (if named)
      if (is.null(sheet_name) || sheet_name == "") {
        sheet_name <- paste0("Sheet", i)  # Assign default sheet names if missing
      }
      
      addWorksheet(wb, sheet_name)  # Create a new sheet
      writeData(wb, sheet = sheet_name, export.ls[[i]])  # Write data
    }
    
    saveWorkbook(wb, output.file.path, overwrite = TRUE)
    
    cat("Excel file successfully saved at:", output.file.path, "\n") # Print confirmation

  # CODE CLOCKING
    code.duration <- Sys.time() - sections.all.starttime
    code.duration
