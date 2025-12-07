# Nuclear Winter Polling Data – Analysis Protocol v3

This version updates **v2** to make the protocol as **LLM-executable** and unambiguous as possible, given the current main script `Ingram_NW_Awareness.R`. It is written so that:

- A human analyst can follow it step-by-step in R (VS Code + WSL).
- An LLM can safely generate analysis code in **separate R scripts** without breaking the data-cleaning pipeline.
- Results from earlier steps directly inform later analyses via explicit **decision rules** and **bookmark points** for human input.

---

## 0. GLOBAL SETUP & GUARDRAILS

### 0.1 Core scripts and objects

- **Main data script**: `publication_analysis.R`
  - Purpose: **data cleaning + reshaping** only.
  - Do **not** modify existing cleaning / reshaping code.
  - Legacy script `Ingram_NW_Awareness.R` contains old config-based analyses (to be removed in final refactor).

Running `publication_analysis.R` to completion should create (in the active R session):

- `data.tb` – wide table, one row per respondent.  
- Long tables created via `ReshapeThemeTable()`:
  - `awareness.tb` (`theme = "awareness"`) – includes `awareness.text`, `awareness.num`, `category`, demographics, etc.
  - `support.reaction.tb` (`theme = "support.reaction"`) – includes `support.text`, `support.num`, `category`, demographics, etc.
  - `decision.factors.tb` (`theme = "decision.factors"`) – includes various `decision.*` categories, plus demographics.
  - `casualty.causes.tb` (`theme = "casualty.causes"`) – includes `casualty.text`, `casualty.num`, interval bounds and midpoints.

- Export list:
  - `export.ls` – list of tables with names `c("1.wide.data", "2.awareness", "3.casualty.causes", "4.support.reaction", "5.decision.factors", "questions")`.

**Treatment variable**

- `shown.infographic` – as coded in raw data.  
  - `"A"` = **treatment (infographic shown)**  
  - `"B"` = **control**

**Key covariates** for adjusted models:

- `age`, `sex`, `ethnicity`, `education`, `political.affiliation`, `income`  
- `country.of.residence` (omit in country-specific models)

### 0.2 Awareness items in `data.tb`

Numeric 1–4:

- `nw.awareness.1980s`
- `nw.awareness.recent.academic`
- `nw.awareness.recent.media`

### 0.3 Decision-factor items in `data.tb`

Numeric 1–5 columns with prefix `decision.` including:

- `decision.reduce.russian.retaliation_numeric`
- `decision.punish.russia.signal.aggressors_numeric`
- `decision.limit.diplomatic.support.for.russia_numeric`
- `decision.avoid.killing.civilians.global.famine_numeric`
- `decision.avoid.escalation_numeric`

### 0.4 Primary nuclear-retaliation outcome

- In `support.reaction.tb`: `support.num`  
- In `data.tb`:  
  - **Primary**: `support.nuclear.strike.on.russia_numeric` (ordinal)

### 0.5 LLM guardrails

**Do not modify**:

- Cleaning logic  
- Reshape logic  
- Coding of variables  
- `export.ls`

**Do**:

- Add analysis code only in separate R scripts under `analysis/`  
- Follow naming conventions supplied here  
- Always produce both `.md` and `.csv` outputs  

---

## 1. SCRIPT ARCHITECTURE

### 1.1 Cleaning script

Run first:

```r
source("publication_analysis.R")
```

Produces all clean objects in the R session.

### 1.2 New analysis scripts

Under `analysis/` create:

- `RQ1_awareness_structure.R`
- `RQ2_awareness_support.R`
- `RQ3_treatment_effects.R`
- `RQ4_decision_factors_structure.R`
- `RQ5_integration_exploratory.R`

Each script assumes the cleaning script has already run.

### 1.3 Output directory logic

Use the existing helper:

```r
output_dir <- create_timestamped_output_dir("outputs")
```

Each RQ script must create a subfolder, e.g.:

```r
rq1_dir <- file.path(output_dir, "RQ1_awareness_structure")
dir.create(rq1_dir, recursive = TRUE, showWarnings = FALSE)
```

Output formats:

- **Markdown** summary file (`.md`)
- Supporting **CSV** tables

---

## 2. MODELING ENGINE & ASSUMPTION TESTING

### 2.1 Ordinal modeling workflow

Start with POM → test PO → escalate to PPOM only if needed:

1. **Fit POM** via helper: `fit_pom()`  
2. **Test PO assumption**:
   - Brant test or equivalent POA tests already in script  
   - Use residual diagnostics if needed  
3. **If violated**, fit PPOM via helper: `fit_ppom()` (likely wraps `VGAM::vglm()`)  
4. Retain both models and explain divergence in `.md` output

### 2.2 Covariates

Default covariates in all adjusted models:

- `age`, `sex`, `ethnicity`, `education`, `political.affiliation`, `income`

---

## 3. RQ1 – STRUCTURE OF NUCLEAR-WINTER AWARENESS

### 3.1 Input construction

```r
rq1_awareness_items <- data.tb %>%
  select(respondent.id,
         nw.awareness.1980s,
         nw.awareness.recent.academic,
         nw.awareness.recent.media)
```

### 3.2 Diagnostics

- **Cronbach’s alpha**  
- **Pearson correlation matrix**  
- **Polychoric correlation matrix**

### 3.3 Factor analysis

```r
rq1_fa <- psych::fa(rq1_awareness_items[-1], nfactors = 1, fm = "minres")
```

Extract:

- Loadings  
- Communalities  
- Variance explained  

### 3.4 Awareness mean index

```r
rq1_awareness_mean <- rq1_awareness_items %>%
  mutate(awareness_mean = rowMeans(across(-respondent.id), na.rm=TRUE))
```

Standardized version:

```r
awareness_mean_z = scale(awareness_mean)
```

### 3.5 Outputs

- `RQ1_awareness_alpha.csv`  
- `RQ1_awareness_cor_pearson.csv`  
- `RQ1_awareness_cor_polychoric.csv`  
- `RQ1_awareness_fa_loadings.csv`  
- `RQ1_awareness_mean_index.csv`  
- Markdown summary: `RQ1_awareness_structure.md`

---

## 4. RQ2 – USING AWARENESS AS ASSOCIATIONAL PREDICTOR

Only **treatment-group respondents** (`shown.infographic == "A"`) have awareness items.

### 4.1 Data assembly

```r
rq2_data <- data.tb %>%
  left_join(rq1_awareness_mean, by="respondent.id") %>%
  filter(shown.infographic == "A",
         !is.na(support.nuclear.strike.on.russia_numeric))
```

Include covariates.

### 4.2 Check A – Distribution & monotonicity

- Plot distribution of `awareness_mean`  
- Examine support means across quartiles  

### 4.3 Check B – Item-wise vs mean model comparison

Fit two models:

- **Model 1:** separate items  
- **Model 2:** mean index  

Extract predicted probabilities for representative covariate sets.

Compute:

```
delta_p_max = max(abs(p_hat_items - p_hat_mean))
```

Threshold:

- **Use mean index if** `delta_p_max ≤ 0.03`.

### 4.4 Final decision flag

```r
rq2_awareness_mean_ok_overall <- 
  (rq1_alpha$total$raw_alpha >= 0.7) &
  (delta_p_max <= 0.03)
```

### 4.5 Outputs

- `RQ2_awareness_mean_model_comparison.csv`  
- `RQ2_awareness_mean_effects.csv`  
- Markdown: `RQ2_awareness_support.md`  
  - Includes flag explaining whether `awareness_mean` is used downstream  

---

## 5. RQ3 – TREATMENT EFFECT OF NUCLEAR-WINTER INFORMATION

### 5.1 Construct dataset

```r
rq3_data <- data.tb %>%
  filter(!is.na(shown.infographic),
         !is.na(support.nuclear.strike.on.russia_numeric))
```

### 5.2 Unadjusted effects

Compute mean support and proportion ≥4 by treatment vs control.

### 5.3 POM → PPOM escalation

1. Fit **unadjusted POM**  
2. Fit **adjusted POM**  
3. Test PO assumption  
4. If violated, fit **adjusted PPOM**  
5. Retain all models  

### 5.4 Predicted probabilities

Compute predicted probabilities for:

- Treatment vs control  
- Subgroups (e.g., political affiliation)  
- Country-specific subsets as needed  

### 5.5 Outputs

- `RQ3_treatment_models_summary.csv`  
- `RQ3_treatment_effects_predictions.csv`  
- Markdown: `RQ3_treatment_effects.md`  

---

## 6. RQ4 – DECISION FACTORS: STRUCTURE VIA EFA

### 6.1 Items construction

```r
rq4_decision_items <- data.tb %>%
  select(respondent.id, ends_with("_numeric")) %>%
  filter(if_any(ends_with("_numeric"), ~ !is.na(.)))
```

### 6.2 Descriptive profiles

Plot decision-factor means by retaliation-support level (1–5).

### 6.3 Polychoric EFA

```r
decision_poly <- psych::polychoric(rq4_decision_items[-1])$rho
fa1 <- psych::fa(..., nfactors=1)
fa2 <- psych::fa(..., nfactors=2)
fa3 <- psych::fa(..., nfactors=3)
```

### 6.4 Human bookmark

Do **not** automatically generate factor-based indices.

Instead:

- Output all loadings  
- Provide commentary in Markdown  
- Insert:

```
BOOKMARK FOR HUMAN INTERPRETATION:
Review whether 2-factor “deterrence vs risk-avoidance” structure is defensible.
Do not create indices until instructed.
```

### 6.5 Outputs

- `RQ4_decision_factors_loadings_1factor.csv`  
- `RQ4_decision_factors_loadings_2factor.csv`  
- `RQ4_decision_factors_loadings_3factor.csv`  
- Markdown: `RQ4_decision_factors_structure.md`  

---

## 7. RQ5 – EXPLORATORY INTEGRATION (AWARENESS × DECISION FACTORS × SUPPORT)

### 7.1 Conditional logic

Proceed only after:

- RQ2 flag indicates whether to use `awareness_mean`  
- RQ4 human interpretation specifies factor structure or item-wise approach  

### 7.2 Example exploratory models

- PPOM:  
  - awareness + decision factors/indices + covariates  
  - interaction terms (awareness × political affiliation)  

### 7.3 Outputs

- Markdown: `RQ5_integration.md`  
- Supporting CSV(s)

---

## 8. FINAL REFACTOR & CODE CLEANUP

**BOOKMARK FOR HUMAN APPROVAL**

This section should be executed **only after** all RQ analyses (RQ1–RQ5) are complete and approved by the human analyst.

### 8.1 Purpose

Remove legacy config-based analysis code from `Ingram_NW_Awareness.R` to prepare the repository for publication alongside the research paper.

### 8.2 Files to review for removal

From `Ingram_NW_Awareness.R`:

1. **Section 4A** – Special visualizations (Sankey diagram code)
   - **Decision**: Keep or remove? (May be useful for descriptive reporting)

2. **Section 4B** – Regression execution loop (lines ~578–890)
   - **Decision**: REMOVE (replaced by RQ-specific analysis scripts)

3. **Commented-out code** – Bivariate tests and other legacy analysis
   - **Decision**: REMOVE

### 8.3 Files to potentially archive or remove

- Old output folders in `outputs/` with config-based results
- Any temporary or intermediate analysis files not part of final publication workflow

### 8.4 Verification checklist

After refactoring:

- [ ] `publication_analysis.R` runs without errors
- [ ] All RQ scripts (RQ1–RQ5) run successfully after sourcing `publication_analysis.R`
- [ ] No broken dependencies on removed code
- [ ] All helper functions in `R/` directory are still functional
- [ ] Repository is clean and ready for public release

### 8.5 Documentation updates

Update `README.md` (if exists) or create one to document:

- Purpose of the analysis
- How to run `publication_analysis.R`
- How to run each RQ analysis script in sequence
- Description of output files and their locations

---

## 9. LLM DO/DON'T CHECKLIST

### DO:

- Assume `publication_analysis.R` was sourced first
- Add code only to new scripts under `analysis/`
- Write `.md` + `.csv` results to timestamped folders
- Follow naming rules (`rq1_*`, `rq2_*`, etc.)
- Update this protocol document as decisions are made during implementation

### DO NOT:

- Modify cleaning code in `publication_analysis.R`
- Modify reshaping code
- Recode variables in the cleaning script
- Make substantive decisions where the protocol places a **BOOKMARK FOR HUMAN INPUT**
- Execute Section 8 (Final Refactor) without explicit human approval

---

END OF DOCUMENT
