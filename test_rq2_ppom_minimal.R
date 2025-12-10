# Minimal PPOM test for RQ2 data
# Testing if PPOM can fit with real RQ2 data

# Load minimal requirements
source("scripts/helper_functions/00_config.R")
source("scripts/helper_functions/01_validation.R")
source("scripts/helper_functions/02_data_processing.R")
source("scripts/helper_functions/03_regression_core.R")

suppressMessages({
  library(dplyr)
  library(googlesheets4)
  library(VGAM)
})

cat("\n==============================================================================\n")
cat("MINIMAL RQ2 PPOM TEST\n")
cat("==============================================================================\n\n")

# Load data
cat("Loading data...\n")
gs4_auth(email = "w@wbnicholson.com")
data.tb <- read_sheet(sheets.id, sheet = "data")
data.tb <- prepare_data_for_analysis(data.tb)

# Create awareness mean (simplified from RQ1)
rq1_awareness_mean <- data.tb %>%
  filter(shown.infographic == "Shown NW Iinfographic") %>%
  select(participant.id, nw.awareness.1980s_numeric,
         nw.awareness.recent.academic_numeric,
         nw.awareness.recent.media_numeric) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(awareness_mean = mean(c(nw.awareness.1980s_numeric,
                                 nw.awareness.recent.academic_numeric,
                                 nw.awareness.recent.media_numeric), na.rm = TRUE)) %>%
  ungroup()

# Prepare minimal RQ2 dataset
rq2_data <- data.tb %>%
  left_join(rq1_awareness_mean, by = "participant.id") %>%
  filter(
    shown.infographic == "Shown NW Iinfographic",
    !is.na(support.nuclear.strike.on.russia_numeric),
    !is.na(awareness_mean)
  ) %>%
  select(
    participant.id,
    support.nuclear.strike.on.russia_numeric,
    nw.awareness.1980s_numeric,
    nw.awareness.recent.academic_numeric,
    nw.awareness.recent.media_numeric,
    age, sex,
    ethnicity.collapsed, political.affiliation.collapsed,
    employment.status, student.status
  ) %>%
  drop_na()

cat("Sample size:", nrow(rq2_data), "\n\n")

# Test 1: PPOM with just awareness variables (no categorical covariates)
cat("==============================================================================\n")
cat("TEST 1: PPOM with awareness variables only\n")
cat("==============================================================================\n\n")

formula1 <- support.nuclear.strike.on.russia_numeric ~
  nw.awareness.1980s_numeric +
  nw.awareness.recent.academic_numeric +
  nw.awareness.recent.media_numeric

ppom1 <- tryCatch({
  fit_ppom(formula = formula1, data = rq2_data, link = "logit", verbose = TRUE)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(ppom1)) {
  cat("\n✓ TEST 1 PASSED: PPOM works with awareness variables only\n\n")
} else {
  cat("\n✗ TEST 1 FAILED: PPOM failed even with minimal predictors\n\n")
}

# Test 2: Add continuous covariate (age)
cat("==============================================================================\n")
cat("TEST 2: PPOM with awareness + age (continuous)\n")
cat("==============================================================================\n\n")

formula2 <- support.nuclear.strike.on.russia_numeric ~
  nw.awareness.1980s_numeric +
  nw.awareness.recent.academic_numeric +
  nw.awareness.recent.media_numeric +
  age

ppom2 <- tryCatch({
  fit_ppom(formula = formula2, data = rq2_data, link = "logit", verbose = TRUE)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(ppom2)) {
  cat("\n✓ TEST 2 PASSED: PPOM works with continuous covariate\n\n")
} else {
  cat("\n✗ TEST 2 FAILED: Adding age broke PPOM\n\n")
}

# Test 3: Add one categorical (sex - binary)
cat("==============================================================================\n")
cat("TEST 3: PPOM with awareness + age + sex\n")
cat("==============================================================================\n\n")

formula3 <- support.nuclear.strike.on.russia_numeric ~
  nw.awareness.1980s_numeric +
  nw.awareness.recent.academic_numeric +
  nw.awareness.recent.media_numeric +
  age + sex

ppom3 <- tryCatch({
  fit_ppom(formula = formula3, data = rq2_data, link = "logit", verbose = TRUE)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(ppom3)) {
  cat("\n✓ TEST 3 PASSED: PPOM works with binary categorical\n\n")
} else {
  cat("\n✗ TEST 3 FAILED: Adding sex broke PPOM\n\n")
}

# Test 4: Add all collapsed categoricals
cat("==============================================================================\n")
cat("TEST 4: PPOM with all predictors (collapsed categoricals)\n")
cat("==============================================================================\n\n")

formula4 <- support.nuclear.strike.on.russia_numeric ~
  nw.awareness.1980s_numeric +
  nw.awareness.recent.academic_numeric +
  nw.awareness.recent.media_numeric +
  age + sex + ethnicity.collapsed + political.affiliation.collapsed +
  employment.status + student.status

ppom4 <- tryCatch({
  fit_ppom(formula = formula4, data = rq2_data, link = "logit", verbose = TRUE)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(ppom4)) {
  cat("\n✓ TEST 4 PASSED: PPOM works with full model\n\n")
} else {
  cat("\n✗ TEST 4 FAILED: Full model PPOM failed\n\n")
}

cat("==============================================================================\n")
cat("SUMMARY\n")
cat("==============================================================================\n\n")

cat("Test 1 (awareness only):", ifelse(!is.null(ppom1), "PASS", "FAIL"), "\n")
cat("Test 2 (+ age):", ifelse(!is.null(ppom2), "PASS", "FAIL"), "\n")
cat("Test 3 (+ sex):", ifelse(!is.null(ppom3), "PASS", "FAIL"), "\n")
cat("Test 4 (full model):", ifelse(!is.null(ppom4), "PASS", "FAIL"), "\n\n")
