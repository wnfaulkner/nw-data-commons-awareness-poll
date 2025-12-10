# Protocol Version Control System

This directory contains the analysis protocol and automated tracking of all substantive methodological changes.

## Purpose

- **Audit trail**: Complete history of analytical decisions
- **Reproducibility**: Document exactly why and when analysis methods were changed
- **Transparency**: Evidence for peer review and stakeholders
- **Compliance**: Supports PRISMA-S and other reporting standards

## Files

### Core Documents

**`analysis_protocol_v3.0.md`** - Locked baseline (NEVER MODIFIED)
- Original protocol document from 2025-12-07
- Reference point for tracking protocol evolution
- Located in parent directory as `analysis_protocol_v3.md`

**`decision_log.json`** - Structured decision history (MACHINE-READABLE)
- Every substantive decision logged with:
  - Timestamp and decision ID
  - Stage/phase of analysis
  - Trigger (what prompted the decision)
  - Evidence (data supporting the decision)
  - Options considered
  - Final decision and rationale
  - Protocol sections affected
  - Implementation details
- Queryable for analysis and reporting
- Updated using R helper functions

**`CHANGELOG.md`** - Human-readable change summary
- Follows "Keep a Changelog" format
- Chronological list of changes
- Links to decision log entries for details
- Quick overview of protocol evolution

### Archive

**`versions/`** - Archived protocol versions
- Previous versions saved before each update
- Named by version number (e.g., `analysis_protocol_v3.1.md`)
- Allows point-in-time reconstruction
- Supports tracing decisions to specific protocol states

## Version Numbering

**Semantic versioning:** `MAJOR.MINOR`

- **Major (X.0)**: Fundamental changes
  - Examples: Complete statistical approach overhaul, different outcome measures
  - Rare, requires strong justification

- **Minor (3.X)**: Substantive but incremental changes
  - Examples: Variable transformations, model specification changes, diagnostic criteria changes
  - Most decisions fall into this category

## What Gets Logged?

### Substantive Changes (LOGGED)

Changes that alter methodology, statistical approach, or affect reproducibility:

1. **Statistical methods:**
   - Changes to regression specifications
   - Addition or removal of covariates
   - Changes to model diagnostics criteria
   - Modifications to assumption testing procedures

2. **Data processing:**
   - Variable transformations or recoding
   - Handling of missing data
   - Sample inclusion/exclusion criteria changes
   - Creation of composite indices

3. **Deviations from protocol:**
   - Solutions to convergence issues
   - Adaptations based on diagnostic findings
   - Methodological refinements from empirical results
   - Changes from discovered data limitations

### Non-Substantive Changes (NOT LOGGED)

- Typo corrections in protocol document
- Formatting improvements
- Clarifications without meaning changes
- Addition of examples/elaborations
- Code refactoring without methodological changes

## How to Use

### Logging a Decision (R)

```r
source("protocol/log_decision.R")

log_decision(
  stage = "RQ2 - Awareness as Predictor",
  trigger = "Visual inspection of POM diagnostics shows proportional odds violation",
  evidence = list(
    residual_patterns = "Non-parallel across thresholds",
    brant_test_p = 0.02,
    diagnostic_plots = "outputs/RQ2_diagnostics.pdf pages 1-3"
  ),
  options = c(
    "Option A: Retain POM despite violation",
    "Option B: Use PPOM with all predictors flexible",
    "Option C: Use PPOM with only violating predictors flexible"
  ),
  decision = "Option B: PPOM with all predictors flexible",
  rationale = "Visual inspection shows multiple predictors violate assumption. Conservative approach: allow all coefficients to vary across thresholds. Simplifies interpretation compared to partial flexibility.",
  protocol_sections = c("Section 2.1.4 - Model Escalation", "Section 5 - RQ2"),
  implementation = list(
    scripts_modified = c("analysis/RQ2_awareness_support.R"),
    final_model = "PPOM with parallel=FALSE for all predictors"
  )
)
```

### Reviewing Decisions

- Read `CHANGELOG.md` for quick summary
- Check `decision_log.json` for full details
- Compare current `analysis_protocol_v3.md` to `protocol_v3.0.md` (baseline) for cumulative changes
- Review archived versions in `versions/` for historical states

## Example Workflow

1. **Identify issue:** "Categorical variables cause model convergence failure"
2. **Log decision:** Call `log_decision()` with all details
3. **System automatically:**
   - Generates decision ID (e.g., DEC-001)
   - Adds entry to decision_log.json
   - Updates CHANGELOG.md
   - Archives protocol if version increments
   - Prints confirmation
4. **Result:** Complete audit trail maintained

## Integration with Analysis Scripts

Decision logging is called from within analysis scripts when substantive choices are made:

```r
# At the point of decision in analysis script
if (brant_test$omnibus_p < 0.05) {
  # Log the decision to use PPOM
  source("protocol/log_decision.R")
  log_decision(...)

  # Proceed with PPOM
  model_ppom <- fit_ppom(...)
}
```

## Querying Decisions

Read all decisions:
```r
library(jsonlite)
log <- fromJSON("protocol/decision_log.json")

# View all decision summaries
for (i in seq_along(log$decisions)) {
  dec <- log$decisions[[i]]
  cat(sprintf("%s: %s\n", dec$id, dec$decision))
}

# Filter by stage
rq2_decisions <- log$decisions[grepl("RQ2", sapply(log$decisions, function(d) d$stage))]
```

## Current Protocol Status

- **Baseline Version:** 3.0 (2025-12-07)
- **Current Version:** See `metadata.protocol_current_version` in decision_log.json
- **Total Decisions:** See `metadata.total_decisions` in decision_log.json
- **Last Updated:** See `metadata.last_updated` in decision_log.json

## Maintenance

- Review decisions quarterly
- Archive old decisions (>2 years) if project is long-running
- Ensure all substantive analysis decisions are logged
- Keep CHANGELOG.md synchronized with decision_log.json

---

**For detailed implementation guide, see PROTOCOL_SYSTEM_PROMPT.md in project root.**
