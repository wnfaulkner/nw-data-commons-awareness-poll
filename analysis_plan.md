# Analysis Plan for Nuclear Winter Awareness Poll

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

## 1. RQ1 – Levels & structure of nuclear-winter awareness

### 1.1 Focus
Measure public awareness through three items: 1980s debates, recent media, recent academic studies.

### 1.2 Variables
Awareness items (ordinal 1–4), demographic subgrouping variables.

### 1.3 Analysis Steps
1. Clean & verify coding; use treatment group only.
2. Descriptive distributions via charts and tables.
3. Structural measures: correlations, Cronbach’s alpha, factor analysis.
4. Create awareness index(es) for later modelling.

### 1.4 Outputs
Stacked bar charts, descriptive table, narrative of awareness structure.

---

## 2. RQ3 – Effect of infographic on support for responses

### 2.1 Focus
Estimate causal effect of infographic on support for nuclear retaliation and alternative responses.

### 2.2 Variables
Primary outcome: support for nuclear retaliation (ordinal 1–5).  
Treatment: `shown.infographic`.  
Covariates: demographics.

### 2.3 Analysis Steps

#### Step 1: Raw comparisons
Cross-tabs, mean differences, share supporting (4–5).

#### Step 2: PPOM for nuclear retaliation
- Unadjusted model: outcome ~ treatment.  
- Adjusted model: outcome ~ treatment + demographics.  
- PPOM constraints:
  - Parallel effects for age, sex unless diagnostics suggest violation.
  - Non-parallel effects for political affiliation, possibly treatment.

- Outputs: predicted probabilities under treatment vs control.

#### Step 3: Secondary outcomes
PPOMs for sanctions, ceasefire ultimatum, non-nuclear attack, military supplies.

#### Step 4: Round-2 planning
Assess compound-treatment and awareness-question effects.

---

## 3. RQ5 – Awareness and support for retaliation

### 3.1 Focus
Study how naturally occurring awareness levels correlate with support, especially in control group.

### 3.2 Samples
Primary: control group.  
Secondary: treatment group for comparison.

### 3.3 Analysis Steps
1. Descriptive associations: mean support by awareness levels.
2. PPOM within control group:
   - Predictors: awareness items or index + demographics.
   - Output: predicted probability shifts from low → high awareness.
3. Compare slopes via treatment × awareness interaction models.

---

## 4. RQ9 – Decision factors and support profiles

### 4.1 Focus
Identify how importance ratings of decision factors vary across support levels.

### 4.2 Models
Descriptive grouping + predictive model linking decision factors to support.

### 4.3 Analysis Steps

#### Step 1: Descriptive profiles
Mean ratings of each decision factor within support categories (1–5). Radar or bar charts.

#### Step 2: Dimensionality assessment
Correlations; factor analysis; construct indices (restraint; punish/deter).

#### Step 3: PPOM with decision factors as predictors
Outcome: support for retaliation.  
Predictors: decision-factor ratings + treatment + demographics.

#### Step 4: Interactions
Decision-factor × treatment effects.

---

## 5. Deferred Decisions (Return Later)

1. **Substantive coefficients for FDR** – finalize once model list is fixed.  
2. **Pooled UK+US analysis** – run for key models later.  
3. **Model 3 brainstorming** – explore ordinal ML models as robustness.

