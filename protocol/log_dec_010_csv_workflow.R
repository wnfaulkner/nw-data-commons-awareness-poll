# Log DEC-010: CSV-based Data Workflow (Eschewing Google Sheets)
#
# This script documents the substantive decision to transition from Google Sheets
# authentication to a local CSV-based data workflow.

source("protocol/log_decision.R")

log_decision(
  stage = "Data Infrastructure - CSV-based Workflow",

  trigger = "Google Sheets authentication failure prevented data loading and analysis execution. Error: 'sheets.id' environment variable not configured, causing authentication loop.",

  evidence = list(
    authentication_error = "sheets.id not found in environment or .sheets_id file",
    script_affected = "scripts/c.helper_functions/00_config.R attempted Google authentication",
    workflow_blocked = "Unable to run run_cleaning.R or run_analyses.R",
    authentication_dependency = "Required manual browser authentication for Google Drive API",
    reproducibility_issue = "Google Sheets authentication requires user-specific credentials, hindering automated pipeline execution"
  ),

  options = c(
    "Option A: Fix Google Sheets authentication (restore environment variables, update credentials)",
    "Option B: Transition to CSV-based workflow (export Google Sheet tabs as CSV files in repository)",
    "Option C: Hybrid approach (maintain both Google Sheets and CSV, sync manually)"
  ),

  decision = "Option B: Transition to CSV-based workflow",

  rationale = paste(
    "CSV-based workflow provides several advantages over Google Sheets authentication:",
    "(1) Version control: CSV files tracked in git provide complete audit trail of data changes.",
    "(2) Reproducibility: No user-specific credentials required; anyone with repository access can run analyses.",
    "(3) Portability: Analysis pipeline can run offline without internet connection or API access.",
    "(4) Simplicity: Eliminates Google Drive/Sheets R package dependencies and complex authentication setup.",
    "(5) Stability: CSV files are static snapshots; Google Sheets API changes won't break pipeline.",
    "Option A would restore functionality but maintains authentication complexity.",
    "Option C creates maintenance burden of keeping two data sources synchronized.",
    "Since data collection is complete and dataset is finalized, static CSV files are appropriate."
  ),

  protocol_sections = c(
    "Section 0.2 - Data Sources and Loading",
    "Section 1.1 - Data Import and Initial Processing",
    "All analysis scripts (load data from CSV instead of Google Sheets)"
  ),

  implementation = list(
    csv_files_created = c(
      "configs/questions.csv (exported from Questions tab)",
      "configs/response_options.csv (exported from Response Options tab)",
      "source_data/source_data_raw.csv (exported from main data tab)"
    ),
    scripts_modified = c(
      "scripts/a.cleaning/run_cleaning.R - replaced read_sheet() with read_csv(), added janitor::remove_empty('cols')",
      "scripts/run_analyses.R - load data from source_data/source_data_clean.csv",
      "scripts/c.helper_functions/00_config.R - commented out Google Sheets configuration code"
    ),
    data_processing_changes = c(
      "Added janitor::remove_empty('cols') to handle trailing commas in CSV files",
      "Added suppressMessages() to CSV loading to suppress 'New names' warnings",
      "Converted q.id to character for proper column name matching"
    ),
    workflow_change = paste(
      "Old: Google Sheets API → read_sheet() → in-memory data",
      "New: CSV files (version controlled) → read_csv() → in-memory data →",
      "run_cleaning.R produces source_data/source_data_clean.csv →",
      "run_analyses.R loads cleaned CSV for all RQ analyses"
    ),
    gitignore_updates = c(
      "Added .sheets_id to .gitignore (no longer used)",
      "Added source_data/source_data_clean.csv to .gitignore (generated file, not version controlled)"
    ),
    testing_status = "Full pipeline tested successfully: run_cleaning.R produces cleaned CSV, run_analyses.R runs all RQs (2025-12-27)"
  ),

  change_type = "minor"
)

cat("\n✓ Decision DEC-010 logged successfully\n")
cat("  - Documented transition from Google Sheets to CSV-based workflow\n")
cat("  - Updated protocol sections and implementation details\n\n")
