#00000000000000000000000000000000000000000000000000000000000#
#0000       2024-11 NW Awareness Poll Data              0000#
#00000000000000000000000000000000000000000000000000000000000#

# 0-SETUP --------------------------------------------------------------------------------
	
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
    drive_auth(email = "william@fluxrme.com")
    library(purrr)
    library(ncdf4)
    library(janitor)
    library(lubridate)
    library(openxlsx)
  
  # SECTION CLOCKING
    section0.duration <- Sys.time() - section0.starttime
    section0.duration
    
# 1-IMPORT DATA & CONFIGS --------------------------------------------------------------------------------
    
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
    

# 2-CLEANING & RESHAPING --------------------------------------------------------------------------------

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
      filter(var.category == "id.var") %>%
      pull(var.name)
  
  # RECODE INFOGRAPHIC
    data.tb %<>%
      mutate(shown.infographic = case_when(
        shown.infographic == "A" ~ "shown infographic",
        shown.infographic == "B" ~ "no infographic",
      ))

  # RESHAPE - DEFINE ABSTRACTED FUNCTION
    ReshapeThemeTable <- function(theme, data_table, questions_table, response_options_table) {  
      # 1. Identify ID vars
      id.vars <- questions.tb %>%
        filter(var.category == "id.var") %>%
        pull(var.name)
      
      # 2. Identify columns for the given theme
      theme.vars <- questions.tb %>%
        filter(q.theme == theme) %>%
        select(q.theme, var.name, q.num)  # keep both var name and q.num for later join
      
      # 3. Reshape data from wide to long
      long.tb <- data.tb %>%
        select(all_of(id.vars), all_of(theme.vars$var.name)) %>%
        pivot_longer(
          cols = -all_of(id.vars),
          names_to = "category",
          values_to = "value"
        )
      
      # 4. Attach q.theme to each category (for joining with response options)
      long.tb <- long.tb %>%
        left_join(theme.vars, by = c("category" = "var.name"))
      
      # 5. Join with response.options.tb using q.theme and value
      long.tb <- long.tb %>%
        left_join(
          .,
          response.options.tb,
          by = c("q.theme", "value" = "response.option")
        ) %>%
        select(-q.theme, -q.num)
      
      # 6. Rename response.text column for clarity
      long.tb <- long.tb %>%
        rename(value.text = response.text)
      
      return(long.tb)
    }

  # RESHAPE - AWARENESS
    awareness.tb <- ReshapeThemeTable(
      theme = "awareness",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb
    )

  # RESHAPE - CASUALTY CAUSES
    casualty.causes.tb <- ReshapeThemeTable(
      theme = "casualty.causes",
      data_table = data.tb,
      questions_table = questions.tb,
      response_options_table = response.options.tb
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

  
# 3-EXPORT --------------------------------------------------------------------------------
  
  # CREATE FINAL LIST OF CLEANED & REFORMATTED TABLES FOR EXPORT
    
    clean_object_names <- 
      c(
        "data.tb",
        "awareness.tb",
        "casualty.causes.tb",
        "support.reaction.tb",
        "decision.factors.tb"
      )
    
    clean_table_names <- 
      c(
        "1.wide.data",
        "2.awareness",
        "3.casualty.causes",
        "4.support.reaction",
        "5.decision.factors"
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
