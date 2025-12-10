# ==============================================================================
# 00_config.R
# Configuration and setup for NW Awareness Poll Analysis
# ==============================================================================

# INITIAL SETUP
# Note: rm(list = ls()) should be called from main script, not here
options(java.parameters = "-Xmx8g")  # helps r not to fail when importing large xlsx files

# SET WORKING DIRECTORY
wd <- "/home/wnf/code/nw-data-commons-awareness-poll"

# LOAD LIBRARIES/PACKAGES
library(wnf.utils)
LoadCommonPackages()
library(googledrive)
drive_auth(cache = "~/.R/gargle_cache", email = "william@fluxrme.com")
library(purrr)
library(janitor)
library(lubridate)
library(openxlsx2)
library(ggplot2)
library(ggalluvial)   # For Sankey/alluvial diagrams

cat("Configuration loaded successfully.\n")
