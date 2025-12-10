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

- **Markdown** summary file (`.md`) - comprehensive single file with all results
- **PDF** diagnostic report - comprehensive single file with all regression diagnostics
- **No CSV files** - all results integrated into markdown and PDF

---

## 2. MODELING ENGINE & ASSUMPTION TESTING

### 2.1 Ordinal modeling workflow: POM → PPOM escalation

**MANDATORY PROCEDURE FOR ALL ORDINAL REGRESSIONS**

Every ordinal regression analysis (including RQ2, RQ3, and any exploratory models) must follow this comprehensive workflow. The protocol ensures rigorous assumption testing and principled model escalation.

#### 2.1.1 Initial model: Proportional Odds Model (POM)

All ordinal regression analyses **must** begin by fitting a Proportional Odds Model using one of:

- `ordinal::clm()` (preferred for full diagnostics)
- `fit_pom()` helper function (wraps `MASS::polr()`)

Example:
```r
model_pom <- fit_pom(
  formula = outcome ~ predictor + age + sex + ...,
  data = analysis_data
)
```

#### 2.1.2 Diagnostic phase: Complete visual and quantitative inspection

**REQUIRED DIAGNOSTIC PLOTS** (4-panel layout):

1. **Residuals vs Fitted**
   - Purpose: Detect systematic patterns, heteroscedasticity
   - Ideal: Random scatter around zero
   - Warning signs: Curvature, fanning, clustering

2. **Normal Q-Q Plot**
   - Purpose: Assess normality of residuals
   - Ideal: Points closely follow diagonal reference line
   - Warning signs: Heavy tails, systematic deviations

3. **Scale-Location Plot** (√|residuals| vs fitted)
   - Purpose: Check homoscedasticity assumption
   - Ideal: Horizontal trend line, even spread
   - Warning signs: Upward/downward trend, uneven spread

4. **Observed vs Predicted**
   - Purpose: Assess model calibration
   - Ideal: Points cluster near diagonal with even spread
   - Warning signs: Systematic over/underprediction, poor calibration

**Generate diagnostics using:**
```r
residuals_tb <- calculate_pom_residuals(model_pom, verbose = FALSE)
diag_plot <- plot_pom_diagnostics(
  pom_result = model_pom,
  residuals_tb = residuals_tb,
  plot_title = "Model Name"
)
```

**QUANTITATIVE INSPECTION RULES:**

For deviance and Pearson residuals:

- **Acceptable range**: Most residuals should fall within [-3, +3]
- **Warning threshold**: Any residuals > |4| indicate potential outliers
- **Critical threshold**: Multiple residuals > |5| suggest serious model misspecification

**DECISION RULE FOR DIAGNOSTICS:**

Proceed to proportional odds testing (2.1.3) if diagnostics are acceptable. If diagnostics reveal serious issues:
- Document the issues in markdown output
- Consider data transformations or covariate adjustments before proceeding
- Flag for human review if issues persist

#### 2.1.3 Proportional odds assumption testing

**PRIMARY METHOD: Visual Inspection** (DEC-002, DEC-005)

Visual inspection of diagnostic plots is the PRIMARY method for assessing the proportional odds assumption. Automated tests (Brant test) serve as confirmatory evidence or tiebreakers when visual assessment is ambiguous.

##### 2.1.3a Visual Inspection Protocol for Proportional Odds Assumption

**OVERVIEW**

The proportional odds assumption requires that predictor effects remain constant across all outcome thresholds. Violations manifest as threshold-specific patterns in diagnostic plots. This protocol provides systematic criteria for identifying assumption violations through visual inspection.

**RATIONALE** (from DEC-002):
- Automated tests unreliable with complex categorical models
- Visual inspection is standard practice in ordinal regression literature
- Allows nuanced judgment of violation severity
- More trustworthy than automated tests for this project's models

**INSPECTION CRITERIA BY PLOT TYPE:**

**1. Residuals vs Fitted Values Plot**

*Purpose:* Detect non-proportional effects across outcome thresholds

*What to look for:*
- **NO VIOLATION**: Random scatter with no systematic patterns; residuals evenly distributed around zero across all fitted values
- **MILD VIOLATION**: Subtle non-parallel trends; slight clustering at specific fitted values; patterns present but not pronounced
- **CLEAR VIOLATION**: Obvious non-parallel patterns; distinct clustering or fanning at specific threshold regions; systematic curvature varying by outcome level

*Threshold-specific indicators:*
- If low support levels (1-2) show positive residual clusters while high support (4-5) shows negative clusters → suggests non-proportional effects
- If residual spread varies systematically across predicted probability regions → indicates threshold-specific heteroscedasticity

**2. Normal Q-Q Plot**

*Purpose:* Assess whether proportional odds assumption holds for residual distribution

*What to look for:*
- **NO VIOLATION**: All points closely follow diagonal reference line; minor deviations at extreme tails acceptable
- **MILD VIOLATION**: Moderate deviations from diagonal at one tail; some systematic departure but < 30% of points clearly off line
- **CLEAR VIOLATION**: Large systematic deviations affecting > 30% of points; distinct threshold-specific patterns (e.g., different curvature for different outcome levels); heavy tails with extreme outliers

*Threshold-specific indicators:*
- If different outcome levels show distinct Q-Q patterns → suggests effects vary by threshold
- If lower and upper tails deviate in opposite directions → may indicate non-proportional odds

**3. Scale-Location Plot (√|residuals| vs fitted)**

*Purpose:* Check for threshold-specific heteroscedasticity

*What to look for:*
- **NO VIOLATION**: Horizontal trend line (smoothed fit approximately flat); relatively even vertical spread across fitted values
- **MILD VIOLATION**: Slight upward or downward trend; moderate variation in spread (√|residuals| range varies by < 50% across fitted value range)
- **CLEAR VIOLATION**: Strong upward or downward trend in smoothed line; dramatic variation in spread (√|residuals| range varies by > 50%); distinct threshold-specific patterns

*Threshold-specific indicators:*
- If √|residuals| systematically increase or decrease with fitted values → indicates heteroscedasticity varying by threshold
- If distinct regions show different spread patterns → suggests non-proportional odds

**4. Observed vs Predicted Plot**

*Purpose:* Assess model calibration and detect systematic misprediction at specific support levels

*What to look for:*
- **NO VIOLATION**: Points cluster near diagonal (y = x line); even scatter above and below diagonal; no systematic patterns
- **MILD VIOLATION**: Slight systematic over/underprediction at one or two outcome levels; points generally near diagonal but with consistent bias in one region
- **CLEAR VIOLATION**: Large systematic over/underprediction at multiple outcome levels; distinct S-curve or inverted S-curve pattern; points far from diagonal in systematic way

*Threshold-specific indicators:*
- If model systematically overpredicts low support and underpredicts high support (or vice versa) → suggests threshold-varying effects
- If calibration is good for middle outcome levels but poor at extremes → may indicate non-proportional odds

**SEVERITY CLASSIFICATION RULES:**

Classify each of the 4 plots independently:

- **Clear violation**: Obvious patterns as described above; no ambiguity in interpretation
- **Mild violation**: Subtle patterns; requires careful judgment; may be borderline
- **No violation**: Random scatter; no threshold-specific patterns detected

**DECISION FLOWCHART:**

```
Step 1: Generate 4-panel POM diagnostic plots using plot_pom_diagnostics()
        ↓
Step 2: Inspect each panel independently using criteria above
        ↓
Step 3: Count number of panels with clear violations
        ↓
        ├─ ≥2 plots show CLEAR violation? → YES → ESCALATE TO PPOM (Section 2.1.4)
        │                                          Document visual findings in markdown
        │                                          Skip Brant test (decision clear)
        │
        ├─ 1 plot shows CLEAR violation? → Use Brant test as tiebreaker:
        │                                  ├─ Brant p ≤ 0.05 → ESCALATE TO PPOM
        │                                  └─ Brant p > 0.05 → RETAIN POM
        │                                     Document: "Visual inspection showed mild
        │                                     violation, but Brant test non-significant"
        │
        └─ 0 plots show CLEAR violation? → RETAIN POM
                                           Run Brant test as confirmation
                                           Document: "Visual inspection shows no violation,
                                           confirmed by Brant test"
```

**INTEGRATION WITH AUTOMATED TESTS:**

The Brant test serves three roles:

1. **Confirmatory role**: When visual inspection shows no violation, Brant test confirms (expected: p > 0.05)
2. **Tiebreaker role**: When visual inspection is ambiguous (1 panel mild violation), Brant test makes final call
3. **Secondary role**: When visual inspection shows clear violation (≥2 panels), Brant test is optional but may be run for documentation

**Priority order:**
1. Visual inspection FIRST
2. Brant test SECOND (confirmatory or tiebreaker)
3. Never rely on Brant test alone when it returns p = 1.0 or NA (known failure mode with categorical predictors)

**DOCUMENTATION REQUIREMENTS:**

Every ordinal regression markdown output MUST include:

```markdown
### Proportional Odds Assumption Testing

**Visual Inspection Results:**

Examined 4-panel diagnostic plots for POM:
- Residuals vs Fitted: [No violation | Mild violation | Clear violation] - [Brief description]
- Q-Q Plot: [No violation | Mild violation | Clear violation] - [Brief description]
- Scale-Location: [No violation | Mild violation | Clear violation] - [Brief description]
- Observed vs Predicted: [No violation | Mild violation | Clear violation] - [Brief description]

**Visual Inspection Decision:** [Retain POM | Escalate to PPOM]

**Brant Test Results:** [Confirmatory | Tiebreaker | Not run]
- Omnibus p-value: [value]
- Interpretation: [Confirms visual assessment | Used as tiebreaker | etc.]

**Final Decision:** [Retain POM | Fit PPOM]
```

Reference diagnostic PDF for visual evidence.

##### 2.1.3b Brant Test for Proportional Odds Assumption

**REQUIRED TEST:** Brant test for proportional odds assumption

Execute using:
```r
brant_result <- perform_brant_test(model_pom$model, verbose = TRUE)
```

The Brant test examines whether the proportional odds assumption holds by testing if coefficients are constant across outcome thresholds.

**INTERPRETATION:**

- **Omnibus p-value > 0.05**: Proportional odds assumption holds → retain POM
- **Omnibus p-value ≤ 0.05**: Assumption violated → must escalate to PPOM
- **Individual item p-values**: Identify which predictors violate the assumption

**ERROR HANDLING:**

If Brant test fails (due to package conflicts or data issues):
```r
brant_result <- tryCatch({
  perform_brant_test(model_pom$model, verbose = FALSE)
}, error = function(e) {
  cat("WARNING: Brant test failed. Defaulting to POM.\n")
  list(omnibus_p_value = 0.99, brant_test_failed = TRUE)
})
```

Document any test failures in the markdown output.

**KNOWN LIMITATION: ordinal::nominal_test() with categorical predictors**

The `ordinal::nominal_test()` function may return NA values (manifesting as p=1.0) when models include multiple categorical predictors, even after collapsing high-cardinality variables. This occurs because `nominal_test()` attempts to fit very flexible models by adding nominal effects (separate coefficients) for each predictor at each outcome threshold, which can lead to:

- Rank-deficient design matrices
- Convergence issues in likelihood optimization
- Insufficient degrees of freedom for the test

**Mitigation strategy:**

To reduce this issue, categorical variables with high cardinality have been collapsed:
- `political.affiliation`: 11 levels → 4 levels (Left-leaning, Right-leaning, Unaffiliated/Uncertain, Other)
- `ethnicity`: 5 levels → 3 levels (White, Black, Asian & Other)

Reference: `create_collapsed_categories()` in `R/02_data_processing.R`

**Conservative interpretation when test fails:**

When `nominal_test()` returns NA for all predictors (p=1.0), the analysis pipeline treats this as "PO assumption holds" and defaults to the simpler POM model. This is a conservative approach:
- If we cannot test the assumption, we use the more restrictive (proportional odds) model
- The POM model is more parsimonious and easier to interpret
- Results remain valid under the PO assumption even if it is mildly violated

**Alternative approaches if needed:**

1. Test PO assumption for continuous predictors only (age, awareness scores)
2. Fit PPOM models based on theoretical considerations rather than statistical tests
3. Use alternative PO tests (e.g., score test, Wald test) for specific predictors of interest
4. Accept POM as the default model specification for complex categorical models

#### 2.1.4 Model escalation: Partial Proportional Odds Model (PPOM)

**WHEN REQUIRED:** If Brant test omnibus p-value ≤ 0.05

**Fit PPOM using:**
```r
model_ppom <- fit_ppom(
  formula = outcome ~ predictor + age + sex + ...,
  data = analysis_data,
  parallel = FALSE ~ predictor_that_violated_assumption
)
```

The PPOM relaxes the proportional odds constraint for specific predictors that violated the assumption, allowing their coefficients to vary across outcome thresholds.

**RETAIN BOTH MODELS:** Always keep both POM and PPOM results for comparison and transparency.

#### 2.1.5 Model comparison: Predicted probability differences

**REQUIRED COMPARISON:** Compute predicted probabilities from both models for representative covariate profiles

```r
# Generate predictions for both models
pred_pom <- predict(model_pom$model, type = "probs", newdata = test_profiles)
pred_ppom <- predict(model_ppom, type = "response", newdata = test_profiles)

# Calculate maximum absolute difference
delta_p_max <- max(abs(pred_pom - pred_ppom))
```

**DECISION RULE:**

- **If Δp_max ≤ 0.03**: Models yield substantively similar predictions → may use simpler POM for parsimony
- **If Δp_max > 0.03**: Meaningful differences exist → must use PPOM and report both

**DOCUMENTATION REQUIREMENT:** Report Δp_max in markdown output regardless of which model is chosen.

#### 2.1.6 Final model selection and justification

**SELECTION CRITERIA:**

1. **If PO assumption holds** (Brant p > 0.05):
   - Use POM
   - Document: "Proportional odds assumption satisfied (Brant p = X.XX)"

2. **If PO assumption violated AND Δp_max > 0.03**:
   - Use PPOM
   - Document: "Proportional odds assumption violated (Brant p = X.XX). PPOM used due to meaningful prediction differences (Δp_max = X.XX)"

3. **If PO assumption violated BUT Δp_max ≤ 0.03**:
   - May use POM for parsimony
   - Document: "Proportional odds assumption violated (Brant p = X.XX), but prediction differences minimal (Δp_max = X.XX). POM retained for parsimony"

**MANDATORY MARKDOWN DOCUMENTATION:**

Every ordinal regression output must include a "Model Diagnostics" section with:

- Residual statistics (deviance range, Pearson range)
- Brant test results (omnibus and per-predictor p-values)
- Note about diagnostic plot availability in separate PDF
- If PPOM fitted: Δp_max and model comparison summary
- Final model selection justification

#### 2.1.7 PDF diagnostic report requirements

**REQUIRED OUTPUT:** Single comprehensive PDF file containing all regression diagnostics

**Page structure for each model:**

- **Page 1:** Coefficient table (text table with all regression coefficients)
- **Page 2:** Forest plot (odds ratios with confidence intervals)
- **Page 3:** 4-panel diagnostic plots (residuals vs fitted, Q-Q, scale-location, observed vs predicted)

**If both POM and PPOM fitted:**

- Pages 1-3: POM (coefficient table, forest plot, diagnostics)
- Pages 4-6: PPOM (coefficient table, forest plot, diagnostics)
- Page 7: Side-by-side predicted probability comparison

**Generate using:**
```r
pdf_file <- file.path(output_dir, "MODEL_NAME_diagnostics.pdf")
pdf(pdf_file, width = 11, height = 8.5, onefile = TRUE)
# Page 1: grid.arrange coefficient table
# Page 2: grid.arrange forest plot
# Page 3: grid.arrange 4-panel diagnostics
# ... repeat for additional models
dev.off()
```

**Note:** All numerical results (coefficients, p-values, etc.) must also appear in the markdown file for easy reference.

### 2.2 Covariates

Default covariates in all adjusted models:

- `age`, `sex`, `ethnicity`, `education`, `political.affiliation`, `income`

When analyzing country-specific subsets, `country.of.residence` should be omitted.

### 2.3 Application to specific research questions

The POM→PPOM workflow in Section 2.1 applies to:

- **RQ2**: Both model 1 (separate awareness items) and model 2 (awareness mean)
- **RQ3**: Unadjusted and adjusted treatment effect models
- **RQ5**: All exploratory integration models
- **Any ad-hoc ordinal regressions**

Each RQ section below references this workflow but does not repeat the full procedure.

---

## 3. MULTIPLE COMPARISON CORRECTIONS

### 3.1 Rationale

When conducting multiple statistical tests across related outcome variables and predictors, the probability of false positives (Type I errors) inflates. Multiple comparison correction methods control this inflation while preserving statistical power.

This protocol adopts the **Benjamini-Hochberg False Discovery Rate (FDR)** correction, which is:
- More powerful than Bonferroni correction (reduces Type II errors)
- Appropriate for exploratory research with correlated outcome variables
- Well-established in survey research with multiple related items

### 3.2 Analysis families

**Family definition:** A family consists of all statistical tests conducted on conceptually related outcome variables within a research domain.

**Defined families for this project:**

1. **Awareness Items** (RQ1, RQ2)
   - Tests involving: `nw.awareness.1980s`, `nw.awareness.recent.academic`, `nw.awareness.recent.media`
   - Includes: factor analysis, regressions predicting awareness items

2. **Support Reactions** (RQ2, RQ3, RQ5)
   - Tests involving: `support.nuclear.strike.on.russia` and related items
   - Includes: treatment effects, awareness→support associations, exploratory models

3. **Decision Factors** (RQ4, RQ5)
   - Tests involving: `decision.reduce.russian.retaliation`, `decision.punish.russia.signal.aggressors`, `decision.limit.diplomatic.support.for.russia`, `decision.avoid.killing.civilians.global.famine`, `decision.avoid.escalation`
   - Includes: EFA, regressions with decision factors as predictors or outcomes

4. **Casualty Estimates** (if analyzed)
   - Tests involving casualty estimation variables
   - Includes: any regressions with casualty estimates as outcomes or predictors

**Sub-family considerations:**

Within a family, further subdivision into sub-families by research question may be appropriate when:
- Research questions test distinct hypotheses on the same outcome domain
- The questions use non-overlapping predictor sets
- Treating the entire family as one unit would be excessively conservative

Example: Within "Support Reactions", RQ3 (treatment effects) tests a specific causal hypothesis, while RQ5 explores demographic associations. These could be treated as separate sub-families.

### 3.3 Unit of testing: Coefficient vs regression

**Decision rule:**

- **Exploratory analyses**: Treat **each coefficient** as an independent test
  - Rationale: All predictors are substantively interesting and merit individual interpretation
  - Example: "Which demographic variables correlate with nuclear winter awareness?" (RQ1 demographic models, if conducted)
  - Example: "Which decision factors predict support for retaliation?" (RQ5 exploratory models)

- **Hypothesis-driven analyses**: Treat **each regression** as a single test
  - Rationale: One primary predictor is hypothesis-driven; other variables serve as statistical controls
  - Example: RQ2 (awareness→support): Primary interest is awareness coefficients; age, sex, ethnicity, political affiliation are controls
  - Example: RQ3 (treatment effects): Primary interest is treatment coefficient; covariates are controls

### 3.4 Benjamini-Hochberg FDR procedure

**Within each analysis family:**

1. **Identify substantive coefficients**
   - For exploratory models: all predictor coefficients
   - For hypothesis-driven models: primary predictor coefficient(s) only

2. **Collect all raw p-values** for substantive coefficients across all models in the family

3. **Apply Benjamini-Hochberg correction:**
   ```r
   adjusted_p <- p.adjust(raw_p_values, method = "BH")
   ```

4. **Interpret using adjusted p-values (q-values)**
   - Report significance based on q-value ≤ α (typically α = 0.05)
   - q-value represents the expected proportion of false discoveries among all discoveries at this threshold

### 3.5 Reporting requirements

**For all regression coefficients in markdown output:**

Present a table with the following columns:
- **Variable name**
- **Effect size** (log odds, OR, or standardized coefficient)
- **95% Confidence Interval**
- **Raw p-value** (uncorrected)
- **Adjusted p-value (q)** (Benjamini-Hochberg FDR within family)
- **Significance indicator** (based on q-value)

**Example table format:**

| Variable | Log Odds | 95% CI | Raw p | FDR q | Sig |
|----------|----------|--------|-------|-------|-----|
| awareness_1980s | 0.317 | [0.173, 0.461] | 0.0000 | 0.0001 | *** |
| awareness_recent_academic | -0.086 | [-0.243, 0.071] | 0.2826 | 0.3532 | ns |
| awareness_recent_media | -0.126 | [-0.281, 0.029] | 0.1109 | 0.1663 | ns |

**Interpretation statement template:**

"Within the [Family Name] analysis family, we conducted [N] tests on [K] substantive coefficients. Applying Benjamini-Hochberg FDR correction (α = 0.05), [M] coefficients remained statistically significant (q ≤ 0.05)."

### 3.6 Implementation notes

**Timing of correction:**

- FDR correction should be applied **after** all models within a family have been fitted
- This may require iterative updates to markdown files as additional models are added

**Cross-family tests:**

- Do **not** correct across families (e.g., do not pool p-values from Awareness and Support families)
- Each family maintains its own FDR correction

**Interaction terms:**

- If testing interactions exploratively, include interaction term p-values in the FDR correction pool
- If testing a specific pre-specified interaction (hypothesis-driven), treat as primary coefficient

**Model selection tests:**

- Likelihood ratio tests for model comparison (e.g., POM vs PPOM) are **not** included in FDR correction
- These are diagnostic tests for model specification, not substantive hypothesis tests

### 3.7 Application to specific research questions

**RQ1 (Awareness Structure):**
- Family: Awareness Items
- No FDR correction needed (no hypothesis tests; only factor analysis and descriptive statistics)

**RQ2 (Awareness→Support):**
- Family: Support Reactions
- Unit of testing: Regression (primary interest is awareness coefficients; covariates are controls)
- Collect p-values for: awareness coefficients only (items or mean, depending on model selection)
- Apply FDR across all awareness coefficients in final selected models

**RQ3 (Treatment Effects):**
- Family: Support Reactions (sub-family: Treatment Effects may be appropriate)
- Unit of testing: Regression (primary interest is treatment coefficient; covariates are controls)
- Collect p-values for: treatment effect coefficient only
- FDR correction: If treating as sub-family, only one test (no correction needed). If pooled with RQ2, include in Support Reactions family FDR correction.

**RQ4 (Decision Factors Structure):**
- Family: Decision Factors
- No FDR correction needed (no hypothesis tests; only EFA and descriptive statistics)

**RQ5 (Exploratory Integration):**
- Family: Support Reactions
- Unit of testing: Coefficient (all predictors substantively interesting)
- Collect p-values for: all predictor coefficients (awareness, decision factors, interactions)
- Apply FDR across all substantive coefficients in exploratory models

---

## 4. RQ1 – STRUCTURE OF NUCLEAR-WINTER AWARENESS

### 4.1 Input construction

```r
rq1_awareness_items <- data.tb %>%
  select(respondent.id,
         nw.awareness.1980s,
         nw.awareness.recent.academic,
         nw.awareness.recent.media)
```

### 4.2 Diagnostics

- **Cronbach’s alpha**  
- **Pearson correlation matrix**  
- **Polychoric correlation matrix**

### 4.3 Factor analysis

```r
rq1_fa <- psych::fa(rq1_awareness_items[-1], nfactors = 1, fm = "minres")
```

Extract:

- Loadings  
- Communalities  
- Variance explained  

### 4.4 Awareness mean index

```r
rq1_awareness_mean <- rq1_awareness_items %>%
  mutate(awareness_mean = rowMeans(across(-respondent.id), na.rm=TRUE))
```

Standardized version:

```r
awareness_mean_z = scale(awareness_mean)
```

### 4.5 Outputs

**Single markdown file:** `RQ1_awareness_structure.md`

Contains:
- Cronbach's alpha results
- Pearson correlation matrix
- Polychoric correlation matrix
- Factor analysis loadings
- Variance explained
- Awareness mean index summary statistics

**No PDF required** for RQ1 (no regression models)

---

## 4. RQ2 – USING AWARENESS AS ASSOCIATIONAL PREDICTOR

Only **treatment-group respondents** (`shown.infographic == "A"`) have awareness items.

### 5.1 Data assembly

```r
rq2_data <- data.tb %>%
  left_join(rq1_awareness_mean, by="respondent.id") %>%
  filter(shown.infographic == "A",
         !is.na(support.nuclear.strike.on.russia_numeric))
```

Include covariates.

### 5.2 Check A – Distribution & monotonicity

- Plot distribution of `awareness_mean`
- Examine support means across quartiles

### 5.3 Check B – Item-wise vs mean model comparison

**MANDATORY WORKFLOW:** Follow Section 2.1 (POM→PPOM) for both models.

#### 5.3.1 Fit initial POM models

- **Model 1 (POM):** separate awareness items
  ```r
  model1_pom <- fit_pom(
    formula = support.nuclear.strike.on.russia_numeric ~
              nw.awareness.1980s_numeric +
              nw.awareness.recent.academic_numeric +
              nw.awareness.recent.media_numeric +
              age + sex + ethnicity + political.affiliation +
              employment.status + student.status,
    data = rq2_data
  )
  ```

- **Model 2 (POM):** awareness mean index
  ```r
  model2_pom <- fit_pom(
    formula = support.nuclear.strike.on.russia_numeric ~
              awareness_mean +
              age + sex + ethnicity + political.affiliation +
              employment.status + student.status,
    data = rq2_data
  )
  ```

#### 5.3.2 Diagnostics and assumption testing

**For each POM model, execute:**

1. Calculate residuals: `calculate_pom_residuals()`
2. Generate diagnostic plots: `plot_pom_diagnostics()`
3. Generate forest plots: `plot_pom_coefficients()`
4. **Run Brant test**: `perform_brant_test()`
   - Document omnibus p-value
   - Document per-predictor p-values

#### 5.3.3 PPOM escalation (if needed)

**If Brant omnibus p ≤ 0.05 for Model 1:**

```r
model1_ppom <- fit_ppom(
  formula = support.nuclear.strike.on.russia_numeric ~
            nw.awareness.1980s_numeric +
            nw.awareness.recent.academic_numeric +
            nw.awareness.recent.media_numeric +
            age + sex + ethnicity + political.affiliation +
            employment.status + student.status,
  data = rq2_data,
  parallel = FALSE ~ [predictors_that_violated]
)
```

- Calculate residuals and generate diagnostics for PPOM
- **Compute Δp_max** between Model 1 POM and Model 1 PPOM
- **Select final Model 1** per Section 2.1.6 criteria

**If Brant omnibus p ≤ 0.05 for Model 2:**

```r
model2_ppom <- fit_ppom(
  formula = support.nuclear.strike.on.russia_numeric ~
            awareness_mean +
            age + sex + ethnicity + political.affiliation +
            employment.status + student.status,
  data = rq2_data,
  parallel = FALSE ~ [predictors_that_violated]
)
```

- Calculate residuals and generate diagnostics for PPOM
- **Compute Δp_max** between Model 2 POM and Model 2 PPOM
- **Select final Model 2** per Section 2.1.6 criteria

#### 5.3.4 Cross-model comparison (Items vs Mean)

After selecting final versions of Model 1 and Model 2:

Extract predicted probabilities for representative covariate sets from both final models.

Compute:

```r
delta_p_max_cross = max(abs(p_hat_model1_final - p_hat_model2_final))
```

Threshold:

- **Use mean index if** `delta_p_max_cross ≤ 0.03`

### 5.4 Final decision flag

```r
rq2_awareness_mean_ok_overall <-
  (rq1_alpha$total$raw_alpha >= 0.7) &
  (delta_p_max <= 0.03)
```

If this flag is `FALSE`, downstream analyses (RQ5) should use **separate awareness items** rather than the mean index.

### 5.5 Outputs

**Required files (2 total):**

**1. Markdown file:** `RQ2_awareness_support.md`

Contains:
- Overview and sample description
- Distribution and monotonicity check
- Full coefficient tables for both models
- Residual statistics (deviance and Pearson ranges)
- Brant test results for each model
- Model comparison (Δp_max between Model 1 and Model 2)
- Final decision flag and interpretation
- Reference to diagnostic PDF

**2. PDF file:** `RQ2_POM_diagnostics.pdf`

**Minimum page structure (POM only):**
- **Pages 1-3: Model 1 POM (Separate Items)**
  - Page 1: Coefficient table
  - Page 2: Forest plot
  - Page 3: 4-panel diagnostic plots
- **Pages 4-6: Model 2 POM (Awareness Mean)**
  - Page 4: Coefficient table
  - Page 5: Forest plot
  - Page 6: 4-panel diagnostic plots

**If PPOM fitted for Model 1:**
- **Pages 7-9: Model 1 PPOM**
  - Page 7: Coefficient table
  - Page 8: Forest plot
  - Page 9: 4-panel diagnostic plots
- **Page 10: Model 1 POM vs PPOM comparison** (predicted probabilities)

**If PPOM fitted for Model 2:**
- **Pages 11-13: Model 2 PPOM** (or 7-9 if Model 1 PPOM not fitted)
  - Coefficient table
  - Forest plot
  - 4-panel diagnostic plots
- **Page 14 (or 10): Model 2 POM vs PPOM comparison** (predicted probabilities)

**Maximum pages:** 14 (if both models require PPOM)  

---

## 6. RQ3 – TREATMENT EFFECT OF NUCLEAR-WINTER INFORMATION

### 6.1 Construct dataset

```r
rq3_data <- data.tb %>%
  filter(!is.na(shown.infographic),
         !is.na(support.nuclear.strike.on.russia_numeric))
```

### 6.2 Unadjusted effects

Compute descriptive statistics by treatment vs control:

- Mean support (1-5 scale)
- Proportion with high support (≥4)
- Standard deviations

### 6.3 Ordinal regression models

**MANDATORY WORKFLOW:** Follow Section 2.1 (POM→PPOM) for all models.

**Model sequence:**

1. **Unadjusted POM**
   ```r
   model_unadjusted <- fit_pom(
     formula = support.nuclear.strike.on.russia_numeric ~ shown.infographic,
     data = rq3_data
   )
   ```

2. **Adjusted POM** (with covariates)
   ```r
   model_adjusted <- fit_pom(
     formula = support.nuclear.strike.on.russia_numeric ~
               shown.infographic + age + sex + ethnicity +
               education + political.affiliation + income,
     data = rq3_data
   )
   ```

**For each model, execute:**

1. Calculate residuals: `calculate_pom_residuals()`
2. Generate diagnostic plots: `plot_pom_diagnostics()`
3. Generate forest plots: `plot_pom_coefficients()`
4. Run Brant test: `perform_brant_test()`
5. **If PO assumption violated:**
   - Fit corresponding PPOM model
   - Compare predicted probabilities (Δp_max)
   - Retain both POM and PPOM
   - Document model selection justification per Section 2.1.6

### 6.4 Predicted probabilities

Compute predicted probabilities for:

- **Treatment vs control** (primary comparison)
- **Subgroups by political affiliation** (e.g., Democrat, Republican, Labour, Conservative)
- **Country-specific subsets** if sample sizes permit (USA, UK)

Use representative covariate profiles (e.g., median age, modal categories).

### 6.5 Outputs

**Required files (2 total):**

**1. Markdown file:** `RQ3_treatment_effects.md`

Contains:
- Overview and sample description
- Descriptive statistics (mean support by treatment/control)
- Full coefficient tables for unadjusted and adjusted models
- Residual statistics (deviance and Pearson ranges)
- Brant test results for each model
- PPOM results if fitted, including Δp_max
- Predicted probabilities for treatment vs control
- Model selection justifications
- Reference to diagnostic PDF

**2. PDF file:** `RQ3_treatment_diagnostics.pdf`

Page structure:
- **Pages 1-3: Unadjusted Model**
  - Page 1: Coefficient table
  - Page 2: Forest plot
  - Page 3: 4-panel diagnostic plots
- **Pages 4-6: Adjusted Model**
  - Page 4: Coefficient table
  - Page 5: Forest plot
  - Page 6: 4-panel diagnostic plots
- **Additional pages if PPOM fitted for either model** (following same 3-page structure per model)  

---

## 7. RQ4 – DECISION FACTORS: STRUCTURE VIA EFA

### 7.1 Items construction

```r
rq4_decision_items <- data.tb %>%
  select(respondent.id, ends_with("_numeric")) %>%
  filter(if_any(ends_with("_numeric"), ~ !is.na(.)))
```

### 7.2 Descriptive profiles

Plot decision-factor means by retaliation-support level (1–5).

### 7.3 Polychoric EFA

```r
decision_poly <- psych::polychoric(rq4_decision_items[-1])$rho
fa1 <- psych::fa(..., nfactors=1)
fa2 <- psych::fa(..., nfactors=2)
fa3 <- psych::fa(..., nfactors=3)
```

### 7.4 Human bookmark

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

### 7.5 Outputs

**Single markdown file:** `RQ4_decision_factors_structure.md`

Contains:
- Overview and sample description
- Decision factor descriptive profiles by support level
- Polychoric correlation matrix
- 1-factor solution (loadings, variance explained)
- 2-factor solution (loadings, variance explained)
- 3-factor solution (loadings, variance explained)
- Human interpretation bookmark

**No PDF required** for RQ4 (no regression models)  

---

## 8. RQ5 – EXPLORATORY INTEGRATION (AWARENESS × DECISION FACTORS × SUPPORT)

### 8.1 Conditional logic

**BOOKMARK FOR HUMAN APPROVAL**

Proceed only after:

- RQ2 flag indicates whether to use `awareness_mean` or separate awareness items
- RQ4 human interpretation specifies factor structure (2-factor, 1-factor, or item-wise approach)

### 8.2 Exploratory models

**MANDATORY WORKFLOW:** Follow Section 2.1 (POM→PPOM) for all models.

**Example model specifications:**

1. **Awareness + Decision Factors** (no interactions)
   ```r
   model_additive <- fit_pom(
     formula = support.nuclear.strike.on.russia_numeric ~
               awareness_predictor + decision_factor_predictors +
               age + sex + ethnicity + political.affiliation + income,
     data = integration_data
   )
   ```

2. **With interaction terms**
   ```r
   model_interaction <- fit_pom(
     formula = support.nuclear.strike.on.russia_numeric ~
               awareness_predictor * political.affiliation +
               decision_factor_predictors +
               age + sex + ethnicity + income,
     data = integration_data
   )
   ```

**For each model, execute:**

1. Calculate residuals: `calculate_pom_residuals()`
2. Generate diagnostic plots: `plot_pom_diagnostics()`
3. Generate forest plots: `plot_pom_coefficients()`
4. Run Brant test: `perform_brant_test()`
5. If PO assumption violated, fit PPOM and compare predictions

**Model comparison:**

- Compare AIC/BIC across models
- Assess improvement from interaction terms
- Document final model selection

### 8.3 Outputs

**Required files (2 total):**

**1. Markdown file:** `RQ5_integration.md`

Contains:
- Overview and conditional logic applied (awareness and decision factor approach used)
- Model specifications and justifications
- Full coefficient tables for all fitted models
- Residual statistics for each model
- Brant test results
- PPOM results if fitted, including Δp_max
- Model comparison table (AIC, BIC)
- Final model selection and interpretation
- Reference to diagnostic PDF

**2. PDF file:** `RQ5_integration_diagnostics.pdf`

Page structure (3 pages per model):
- **Pages 1-3: First Model**
  - Page 1: Coefficient table
  - Page 2: Forest plot
  - Page 3: 4-panel diagnostic plots
- **Pages 4-6: Second Model** (if applicable)
  - Page 4: Coefficient table
  - Page 5: Forest plot
  - Page 6: 4-panel diagnostic plots
- **Additional pages if PPOM fitted for any model** (following same 3-page structure per model)

---

## 9. FINAL REFACTOR & CODE CLEANUP

**BOOKMARK FOR HUMAN APPROVAL**

This section should be executed **only after** all RQ analyses (RQ1–RQ5) are complete and approved by the human analyst.

### 9.1 Purpose

Remove legacy config-based analysis code from `Ingram_NW_Awareness.R` to prepare the repository for publication alongside the research paper.

### 9.2 Files to review for removal

From `Ingram_NW_Awareness.R`:

1. **Section 4A** – Special visualizations (Sankey diagram code)
   - **Decision**: Keep or remove? (May be useful for descriptive reporting)

2. **Section 4B** – Regression execution loop (lines ~578–890)
   - **Decision**: REMOVE (replaced by RQ-specific analysis scripts)

3. **Commented-out code** – Bivariate tests and other legacy analysis
   - **Decision**: REMOVE

### 9.3 Files to potentially archive or remove

- Old output folders in `outputs/` with config-based results
- Any temporary or intermediate analysis files not part of final publication workflow

### 9.4 Verification checklist

After refactoring:

- [ ] `publication_analysis.R` runs without errors
- [ ] All RQ scripts (RQ1–RQ5) run successfully after sourcing `publication_analysis.R`
- [ ] No broken dependencies on removed code
- [ ] All helper functions in `R/` directory are still functional
- [ ] Repository is clean and ready for public release

### 9.5 Documentation updates

Update `README.md` (if exists) or create one to document:

- Purpose of the analysis
- How to run `publication_analysis.R`
- How to run each RQ analysis script in sequence
- Description of output files and their locations

---

## 10. LLM DO/DON'T CHECKLIST

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
