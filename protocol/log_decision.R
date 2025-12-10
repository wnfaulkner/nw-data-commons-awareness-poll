# Protocol Decision Logger
#
# Automated tracking of substantive methodological changes for:
# - Audit trail and reproducibility
# - Transparent decision documentation
# - Protocol evolution tracking
# - Compliance (PRISMA-S, peer review)
#
# Usage:
#   source("protocol/log_decision.R")
#
#   log_decision(
#     stage = "RQ2 - Awareness as Predictor",
#     trigger = "Categorical variables cause model convergence failure",
#     evidence = list(
#       political_affiliation_levels = 11,
#       ethnicity_levels = 5,
#       convergence_failures = 3
#     ),
#     options = c(
#       "Option A: Remove categorical variables",
#       "Option B: Collapse high-cardinality variables",
#       "Option C: Use penalized regression"
#     ),
#     decision = "Option B: Collapse high-cardinality variables",
#     rationale = "Preserves important covariates while addressing convergence. Theoretically justified groupings.",
#     protocol_sections = c("Section 2.2 - Covariates", "Section 5 - RQ2"),
#     implementation = list(
#       function_created = "create_collapsed_categories() in R/02_data_processing.R",
#       political_affiliation = "11 levels -> 4 levels",
#       ethnicity = "5 levels -> 3 levels"
#     ),
#     change_type = "minor"
#   )

library(jsonlite)

#' Log a substantive protocol decision
#'
#' @param stage Character. Which stage/phase (e.g., "RQ2 - Awareness as Predictor")
#' @param trigger Character. What prompted this decision
#' @param evidence List. Data supporting the need for decision
#' @param options Character vector. List of options considered
#' @param decision Character. Final decision made
#' @param rationale Character. Why this decision was made
#' @param protocol_sections Character vector. Which protocol sections are affected
#' @param implementation List. Implementation details (scripts, functions, parameters, etc.)
#' @param change_type Character. "major" or "minor" (default: "minor")
#'
#' @return Character. Decision ID (e.g., "DEC-001")
log_decision <- function(stage,
                        trigger,
                        evidence,
                        options,
                        decision,
                        rationale,
                        protocol_sections,
                        implementation = list(),
                        change_type = "minor") {

  # Path to decision log
  log_path <- "protocol/decision_log.json"

  # Load existing log or create new one
  if (file.exists(log_path)) {
    log <- fromJSON(log_path, simplifyVector = FALSE)
  } else {
    stop("Decision log not found at protocol/decision_log.json")
  }

  # Generate decision ID
  count <- log$metadata$total_decisions + 1
  decision_id <- sprintf("DEC-%03d", count)

  # Determine version change
  old_version <- log$metadata$protocol_current_version
  version_parts <- as.numeric(strsplit(old_version, "\\.")[[1]])
  major <- version_parts[1]
  minor <- if (length(version_parts) > 1) version_parts[2] else 0

  if (change_type == "major") {
    new_version <- sprintf("%d.0", major + 1)
  } else {
    new_version <- sprintf("%d.%d", major, minor + 1)
  }

  # Create decision entry
  decision_entry <- list(
    id = decision_id,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stage = stage,
    trigger = list(
      description = trigger,
      evidence = evidence
    ),
    options_considered = lapply(options, function(opt) list(option = opt)),
    decision = decision,
    rationale = rationale,
    protocol_impact = list(
      sections_modified = protocol_sections,
      version_change = sprintf("%s → %s", old_version, new_version),
      methodology_change = decision
    ),
    implementation = implementation
  )

  # Add to log
  log$decisions[[count]] <- decision_entry
  log$metadata$total_decisions <- count
  log$metadata$protocol_current_version <- new_version
  log$metadata$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Save updated log
  write_json(log, log_path, pretty = TRUE, auto_unbox = TRUE)

  # Update CHANGELOG
  update_changelog(decision_entry, new_version)

  # Archive protocol version if sections modified
  if (length(protocol_sections) > 0) {
    archive_protocol_version(old_version, new_version)
  }

  # Print summary
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("SUBSTANTIVE DECISION LOGGED:", decision_id, "\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("Stage:", stage, "\n")
  cat("Trigger:", trigger, "\n")
  cat("Decision:", decision, "\n")
  cat("Protocol version:", old_version, "→", new_version, "\n")
  cat("Sections affected:", paste(protocol_sections, collapse = ", "), "\n")
  cat(rep("=", 80), "\n\n", sep = "")

  return(decision_id)
}

#' Update CHANGELOG.md with new decision
#' @noRd
update_changelog <- function(decision, new_version) {
  changelog_path <- "protocol/CHANGELOG.md"

  if (!file.exists(changelog_path)) {
    return(invisible(NULL))
  }

  # Read existing changelog
  changelog <- readLines(changelog_path)

  # Create new entry
  sections_str <- paste(decision$protocol_impact$sections_modified, collapse = ", ")
  entry <- c(
    "",
    "### Changed",
    sprintf("- %s ([%s])", decision$trigger$description, decision$id),
    sprintf("  - **Decision:** %s", decision$decision),
    sprintf("  - **Rationale:** %s", decision$rationale),
    sprintf("  - **Impact:** Protocol sections %s", sections_str),
    ""
  )

  # Find [Unreleased] section
  unreleased_line <- which(grepl("^## \\[Unreleased\\]", changelog))

  if (length(unreleased_line) > 0) {
    # Find next section
    next_section <- which(grepl("^## \\[", changelog))[2]

    if (!is.na(next_section)) {
      # Insert before next section
      changelog <- c(
        changelog[1:(next_section - 1)],
        entry,
        changelog[next_section:length(changelog)]
      )
    } else {
      # Append to end
      changelog <- c(changelog, entry)
    }

    writeLines(changelog, changelog_path)
    cat("✓ Updated CHANGELOG.md with decision", decision$id, "\n")
  }

  invisible(NULL)
}

#' Archive protocol version before changes
#' @noRd
archive_protocol_version <- function(old_version, new_version) {
  protocol_path <- "analysis_protocol_v3.md"

  if (!file.exists(protocol_path)) {
    cat("Note: Protocol file not found for archiving\n")
    return(invisible(NULL))
  }

  # Archive to versions directory
  archive_path <- sprintf("protocol/versions/analysis_protocol_v%s.md", old_version)
  file.copy(protocol_path, archive_path, overwrite = FALSE)

  cat("✓ Archived protocol v", old_version, " to ", archive_path, "\n", sep = "")

  invisible(NULL)
}
