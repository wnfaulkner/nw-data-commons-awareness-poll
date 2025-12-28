# ==============================================================================
# 00_config.R
# Configuration and setup for NW Awareness Poll Analysis
# ==============================================================================

# INITIAL SETUP
# Note: rm(list = ls()) should be called from main script, not here
options(java.parameters = "-Xmx8g")  # helps r not to fail when importing large xlsx files

# SET WORKING DIRECTORY
wd <- "/home/wnf/code/nw-data-commons-awareness-poll"

# GOOGLE SHEETS CONFIGURATION - DEPRECATED
# This project now uses local CSV files instead of Google Sheets
# The configuration below is retained for reference only
# sheets.id <- Sys.getenv("NW_SHEETS_ID")
# if (sheets.id == "") {
#   local_config_file <- file.path(wd, ".sheets_id")
#   if (file.exists(local_config_file)) {
#     sheets.id <- readLines(local_config_file, n = 1, warn = FALSE)
#   } else {
#     warning("sheets.id not configured. Set NW_SHEETS_ID environment variable or create .sheets_id file in project root.")
#   }
# }

# LOAD LIBRARIES/PACKAGES
library(wnf.utils)
LoadCommonPackages()
# Google Drive/Sheets libraries - DEPRECATED (no longer needed for CSV workflow)
# library(googledrive)
# drive_auth(cache = "~/.R/gargle_cache", email = "william@fluxrme.com")
library(purrr)
library(janitor)
library(lubridate)
library(openxlsx2)
library(ggplot2)
library(ggalluvial)   # For Sankey/alluvial diagrams

cat("Configuration loaded successfully.\n")
