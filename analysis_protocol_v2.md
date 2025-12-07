# Nuclear Winter Polling Data – Updated Analysis Protocol (v2)

## 0. Cross-cutting conventions

These apply to all four RQs unless stated otherwise.

### 0.1 Units of analysis and samples

- **Country:**  
  - **Primary analyses:** UK and US analysed **separately**, as per your preference and Ingram’s reporting.  
  - **Secondary/robustness:** pooled models with `country` covariate and `country × treatment` interactions where useful.

- **Treatment vs control:**
  - `shown.infographic == 1`: saw awareness questions + infographic + scenario items.
  - `shown.infographic == 0`: skipped awareness section; saw scenario items only.
  - **Round 1:** treat as information treatment.
  - **Round 2:** model as compound treatment.

- **Main ordinal items:** awareness sources, casualty expectations, support options, decision-factor ratings.

### 0.2 Modelling defaults

- **Primary model:** PPOM (partial proportional odds).
- **Interpretation:** predicted probabilities first; odds ratios secondary.
- **Multiple testing:** families defined now; coefficients chosen later.
- **Core covariates:** age, sex, ethnicity, political affiliation, education.


---

# 1. RQ1 — STRUCTURE OF NUCLEAR-WINTER AWARENESS

## Research Question (RQ) 1
How ‘aware’ is the general UK & US public of nuclear winter as a potential consequence of nuclear weapons use?

## 1.1 Objectives
- Measure public awareness through three items: 1980s debates, recent media, recent academic studies.
- Assess whether the three awareness items (`aware_1980s`, `aware_media`, `aware_academic`) form a single latent “awareness” construct.
- Justify creation of a **simple awareness index** for downstream use.

## 1.2 Data
- Only **treatment-group respondents** completed these items.
- Awareness items coded 1–4 (“never heard” → “know a lot”).

## 1.3 Analytic Steps

### Step 1 — Basic Descriptives
- Frequencies per category per country.
- Correlation matrix (Pearson + polychoric).
- Cronbach’s alpha.

### Step 2 — Factor Analysis (light-touch)
- Fit **one-factor EFA** (principal axis or ML).
- Fit **one-factor CFA**:
  - Model: `aware_i ~ λ_i * F + ε_i`  
  - Constraints: factor variance fixed at 1.
- Assess:
  - Loadings: expect λ > 0.5 for all items.
  - Communalities.
  - Model fit (CFI, RMSEA; note CFA on 3 items is saturated—interpret cautiously).

### Step 3 — Create Awareness Index
- Compute **unweighted mean**:  
  `awareness_index = rowMeans(c(aware_1980s, aware_media, aware_academic))`
- Standardize version if needed.

### Step 4 — Robustness Checks
Conduct *before using the index in later models*:

#### (a) Distribution & monotonicity
- Plot histogram of `awareness_index`.
- Bin into quartiles → plot mean support for retaliation by awareness quartile.

#### (b) Polychoric–Pearson agreement
- Compare item correlations across methods.

#### (c) Sensitivity models
Fit:
- Model A: PPOM on `support_retaliation ~ awareness_index + covariates`.
- Model B: PPOM on `support_retaliation ~ aware_1980s + aware_media + aware_academic + covariates`.
- Compare direction, magnitude, and significance.

### Decision Rule
If Models A & B show consistent patterns:
- **Use `awareness_index` for RQ3/RQ5 secondary analyses.**

If inconsistent:
- Use **item-wise predictors** instead.

---

# 2. RQ2 — EFFECT OF AWARENESS ON SUPPORT FOR RETALIATION (ASSOCIATIONAL)

*(Note: Awareness is **post-treatment**; no causal claims.)*

## Research Question (RQ) 2
How well does awareness of nuclear winter correlate with support for nuclear retaliation, especially for those that were not shown the infographic?

## 2.1 Objectives
- Examine associations between awareness and support among **treated respondents only**, using the validated awareness index.
- Confirm robustness of treating the index as approximately continuous.

## 2.2 Steps
- Fit PPOM:  
  `support_retaliation ~ awareness_index + age + sex + ethnicity + political_id + education`
- Plot predicted probabilities by awareness level.
- Run sensitivity checks from RQ1 (Step 4).

## 2.3 Decision Rule
If awareness associations differ strongly by subgroup (e.g., political ID):
- Add interaction terms in an exploratory model.

---

# 3. RQ3 — TREATMENT EFFECT OF NUCLEAR-WINTER INFORMATION

*(This remains the primary causal analysis.)*

## Research Question (RQ) 3
To what degree does showing respondents the infographic about nuclear winter (i.e. ‘informing’ them) correspond with significant changes in levels of support for different government responses, with special focus on nuclear retaliation?

## 3.1 Objectives
- Estimate effect of infographic treatment on support for retaliation and other policy responses.

## 3.2 Models
### Step 1 — Raw differences
- Tabulate support by treatment status.
- Compute difference in means and “% supporting retaliation.”

### Step 2 — PPOM (Unadjusted)
`support_retaliation ~ shown_infographic`

### Step 3 — PPOM (Adjusted)
`support_retaliation ~ shown_infographic + age + sex + ethnicity + political_id + education`

### Step 4 — Threshold Diagnostics
- Test PO assumption (Brant test-like checks).
- If violation: allow `shown_infographic` & political ID to vary across thresholds → PPOM.

### Step 5 — Predicted Probabilities
- Compute marginal effects for:
  - full sample,
  - subgroups (political ID, awareness quartiles).

### Step 6 — Secondary Outcomes
Repeat for:
- sanctions support,  
- ceasefire support,  
- conventional response support.

---

# 4. RQ4 — STRUCTURE OF DECISION FACTORS AND THEIR RELATION TO RETALIATION SUPPORT

## Research Question (RQ)
How do the patterns of decision factors judged as important change along the increasing/decreasing levels of support for nuclear retaliation? 

## 4.1 Objectives
- Understand how individuals weigh different “decision factors.”
- Identify whether these factors cluster into 1–2 latent dimensions.
- Explore how these dimensions vary by support level for retaliation.

## 4.2 Data
Items include (names approximate):
- `decision.punish.russia.signal.aggressors`
- `decision.reduce.russian.retaliation`
- `decision.avoid.escalation`
- `decision.avoid.killing.civilians.global.famine`
- `decision.limit.diplomatic.support.for.russia`

## 4.3 Analytic Steps

### Step 1 — Descriptive Profiles
- For each support-retaliation level (1–5):
  - Compute mean importance rating for each decision factor.
  - Visualize via barplots or parallel coordinate plots.

### Step 2 — Exploratory Factor Analysis (EFA)
- Conduct EFA using polychoric correlations.
- Extract 1–3 factors.
- Examine:
  - factor loadings,
  - cross-loadings,
  - factor interpretability.

### EFA Decision Rules
- If EFA suggests **two interpretable clusters** (“deterrence” vs “risk avoidance”):
  - Create two indices (mean of items per factor).
- If EFA suggests **one general factor**:
  - Create a single “decision constraint/priority index.”
- If EFA is **ambiguous**:
  - Use **item-wise analyses** only; do *not* build indices.

### Step 3 — Optional CFA
- Only if EFA yields a clean two-factor solution.
- Fit CFA with:
  - Factor 1: deterrence-related items
  - Factor 2: risk-avoidance items
  - Diplomatic-support item loads where EFA suggested.

### Step 4 — Derived Indices (if justified)
- Compare index values across retaliation-support levels.
- Optionally, include indices in PPOM predicting retaliation support.

---

# 5. RQ5 — AWARENESS × DECISION-FACTOR STRUCTURE (EXPLORATORY)

## Objectives
- Examine how awareness and decision-factor structure jointly correlate with retaliation support.

## Steps
- Fit PPOM including:
  - awareness_index (if validated)
  - decision-factor indices (if validated)
  - demographics  
- Examine interactions:
  - awareness × decision-factor index  
  - awareness × political ID  

---

# 6. MULTIPLE COMPARISON STRATEGY

Families:
- Awareness analyses (RQ1 & RQ2)
- Support outcomes (RQ3)
- Decision-factor analyses (RQ4 & RQ5)

Adjustment:
- Use FDR (Benjamini–Hochberg) for families.
- Exploratory models emphasize effect sizes rather than p-values.

---

# 7. IMPLEMENTATION ORDER FOR CODE UPDATES

1. **Data Cleaning**  
   - Ensure awareness and decision variables properly coded.
   - Create treatment vs control subsets.

2. **RQ1 Awareness Analysis**
   - correlations → alpha → EFA/CFA → index → robustness checks.

3. **RQ3 Treatment Effects**
   - raw → PPOM unadjusted → PPOM adjusted → diagnostics → predictions.

4. **RQ2 Awareness–Support Association**
   - PPOM within treated respondents → sensitivity models.

5. **RQ4 Decision-Factor Analysis**
   - descriptives → EFA → decision on indices → optional CFA.

6. **RQ5 Integration**
   - exploratory PPOMs.

7. **Outputs**
   - save tables, figures, factor-loading plots.

8. **Multiple Comparison Corrections**

---

# 8. SCRIPT STRUCTURE

Recommended:
- `01_clean_data.R`
- `02_awareness_RQ1.R`
- `03_treatment_RQ3.R`
- `04_awareness_support_RQ2.R`
- `05_decision_factors_RQ4.R`
- `06_decision_factor_models_RQ4.R`
- `07_integration_RQ5.R`

Each script must:
- Load dependencies
- Define functions
- Produce and save intermediate outputs

---

**End of Updated Analysis Protocol (v2)**