# Protocol Changelog

All substantive changes to the analysis protocol are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html) for protocol versions.

---

## [Unreleased]

### Changed
- Protocol version control system implemented (2025-12-10)

---


### Changed
- High-cardinality categorical variables (political.affiliation=11 levels, ethnicity=5 levels) caused convergence issues and unstable coefficient estimates in ordinal regressions ([DEC-001])
  - **Decision:** Option B: Collapse high-cardinality variables into theoretically justified groups
  - **Rationale:** Preserves important demographic covariates while addressing convergence. Groupings based on political alignment theory (left/right/unaffiliated) and sample size. Option A loses important information. Option C adds complexity without addressing root cause. Option D oversimplifies politically meaningful distinctions.
  - **Impact:** Protocol sections Section 2.2 - Covariates, Section 0.3 - Key covariates for adjusted models


### Changed
- Automated proportional odds tests (Brant test, ordinal::nominal_test) unreliable with complex categorical models, returning NA/p=1.0 even after variable collapsing ([DEC-002])
  - **Decision:** Option B: Use visual inspection of diagnostic plots as primary method
  - **Rationale:** Visual inspection is standard practice in ordinal regression literature and more trustworthy than automated tests for complex models. Automated tests are fundamentally unreliable with categorical predictors. Option A risks false negatives. Option C loses information about categorical predictors. Option D is overly conservative and computationally expensive.
  - **Impact:** Protocol sections Section 2.1.3 - Proportional odds assumption testing, Section 2.1 - Ordinal modeling workflow


### Changed
- Visual inspection of POM diagnostic plots revealed proportional odds assumption violations for RQ2 Model 1 (separate awareness items) ([DEC-003])
  - **Decision:** Option B: Use PPOM with all predictors flexible (parallel=FALSE for all)
  - **Rationale:** Visual inspection shows violation affects multiple predictors, not just one. Conservative approach: allow all coefficients to vary across thresholds. Simplifies interpretation compared to partial flexibility. Exploratory analysis phase justifies flexibility over parsimony.
  - **Impact:** Protocol sections Section 2.1.4 - Model escalation: Partial Proportional Odds Model, Section 5 - RQ2: Using awareness as associational predictor


### Changed
- Complex timestamped output directories made it difficult to locate current results and track individual RQ outputs. Pipeline reorganization needed for publication preparation. ([DEC-004])
  - **Decision:** Option B: Single outputs/ directory with overwriting, archive old runs manually
  - **Rationale:** Direct file outputs (outputs/RQ1_*.md) provide predictable, stable paths for downstream use. Overwriting encourages treating outputs as reproducible artifacts. Old timestamped directories archived to outputs/archive/ for historical reference. Directory reorganization (R/ → scripts/helper_functions/, analysis/ → scripts/analysis/) improves code discoverability and clarifies project structure.
  - **Impact:** Protocol sections Section 7 - Output Management, Section 8 - Directory Structure, All RQ sections (RQ1-RQ5) - output paths updated


### Changed
- DEC-002 established visual inspection as primary method for PO assumption testing, but protocol lacks operational details for implementation. Analysts need concrete criteria for evaluating diagnostic plots and making escalation decisions. ([DEC-005])
  - **Decision:** Option B: Comprehensive protocol - detailed inspection criteria, decision flowchart, threshold guidelines
  - **Rationale:** Visual inspection requires subjective judgment; comprehensive written protocol ensures consistency across analyses. Option A insufficient for reproducibility. Option C conflicts with DEC-002 rationale that automated methods are unreliable. Detailed guidelines balance flexibility (visual assessment) with rigor (systematic criteria).
  - **Impact:** Protocol sections Section 2.1.3 - Proportional odds assumption testing, Section 2.1.2 - Diagnostic phase (cross-reference to visual criteria), All ordinal regression sections (RQ2, RQ3, RQ5) - implementation guidance


### Changed
- RQ2 analysis compared two model specifications: Model 1 (separate awareness items) vs Model 2 (awareness mean index). After fitting both POM models and examining diagnostics, decision needed on which specification to use for final inference. ([DEC-006])
  - **Decision:** Option B: Use Model 1 (separate awareness items)
  - **Rationale:** Separate awareness items provide richer theoretical interpretation by distinguishing 1980s awareness (historical knowledge) from recent academic awareness (scientific literature) and recent media awareness (public discourse). Though mean index has acceptable reliability, temporal/source distinctions are substantively meaningful for understanding nuclear winter awareness structure. PPOM will be fitted for Model 1 to address proportional odds violations.
  - **Impact:** Protocol sections Section 5 - RQ2: Using awareness as associational predictor, Section 5.3 - Check B: Item-wise vs mean model comparison, Section 5.5 - Outputs (Model 2 no longer reported)


### Changed
- DEC-005 established comprehensive visual inspection criteria but maintained Brant test in workflow. In practice, visual inspection by trained analyst provides definitive assessment of PO violations. Automated Brant test adds unnecessary computational overhead and potential confusion when results conflict with visual assessment. ([DEC-007])
  - **Decision:** Option B: Bypass Brant test entirely; rely solely on human visual inspection decision
  - **Rationale:** When trained analyst performs systematic visual inspection per DEC-005 protocol and reaches definitive conclusion (≥2 plots show clear violation), Brant test provides no additional information. Brant test is unreliable with categorical predictors (DEC-002) and cannot override expert visual assessment. Workflow efficiency improved by eliminating redundant computation. Protocol updated to: (1) Pause after POM fitting for human visual inspection, (2) Analyst documents findings per Section 2.1.3a template, (3) Analyst decides POM vs PPOM, (4) Log decision and proceed with selected model.
  - **Impact:** Protocol sections Section 2.1.3 - Proportional odds assumption testing, Section 2.1.3a - Visual Inspection Protocol, Section 2.1.3b - Brant Test (now optional/deprecated for complex models), All RQ sections with ordinal regression (RQ2, RQ3, RQ5)


### Changed
- Current protocol specifies separate comparison files (e.g., RQ2_model_comparison_items_vs_mean.md) and extensive PDF diagnostics with side-by-side comparisons. During iterative model development, maintaining multiple synchronized files creates overhead and potential inconsistencies. Single linear narrative documents model progression more naturally. ([DEC-008])
  - **Decision:** Option B: Single markdown file with linear narrative (POM → PPOM progression)
  - **Rationale:** Linear narrative structure naturally documents model development sequence: (1) POM fitting and diagnostics, (2) Visual inspection findings, (3) PPOM escalation rationale, (4) PPOM results, (5) Final model selection. Single file simplifies maintenance during iterative refinement and provides complete analytical story. PDF diagnostics retained for detailed visual inspection but referenced from markdown rather than duplicating content. Eliminates archival comparison files (e.g., model_comparison_items_vs_mean.md) that duplicate information.
  - **Impact:** Protocol sections Section 2.1.7 - PDF diagnostic report requirements, Section 5.5 - RQ2 Outputs (file structure), Section 6.5 - RQ3 Outputs (file structure), Section 8.3 - RQ5 Outputs (file structure), All RQ output specifications

## [3.0] - 2025-12-07

### Added
- Initial protocol baseline (analysis_protocol_v3.md)
- Modular R pipeline architecture (R/00_config.R through R/07_orchestration.R)
- Five research questions (RQ1-RQ5) with analysis scripts
- POM→PPOM workflow for ordinal regressions
- Multiple comparison correction procedures (Benjamini-Hochberg FDR)

---

## Version Number Scheme

**Major version (X.0):** Fundamental changes (e.g., complete methodology overhaul, statistical approach change)

**Minor version (3.X):** Substantive but incremental changes (e.g., adding models, modifying diagnostics, variable transformations)

---

## Change Categories

Changes are categorized using these tags:
- **Added:** New sections, methods, or procedures
- **Changed:** Modifications to existing methods
- **Deprecated:** Methods marked for future removal
- **Removed:** Deleted sections or methods
- **Fixed:** Corrections to errors in the protocol
- **Security:** Changes related to data security/privacy

---

## Decision Log Reference

For detailed rationale and evidence supporting each change, see `decision_log.json`.

Each decision includes:
- Trigger (problem or finding)
- Options considered
- Decision made
- Rationale
- Evidence
- Implementation details

---

**Note:** This changelog is updated when substantive protocol decisions are logged.
