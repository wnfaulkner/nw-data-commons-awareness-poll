# Check sample sizes for categorical variables in RQ2

suppressMessages({
  library(tidyverse)
  library(googlesheets4)
})

# Load config
source("R/00_config.R")

gs4_deauth()
data.tb <- read_sheet(sheets.id, sheet = "data")

# Filter to RQ2 sample (treatment group with complete data)
rq2_sample <- data.tb %>%
  filter(
    shown.infographic == "Shown NW Iinfographic",
    !is.na(support.nuclear.strike.on.russia_numeric)
  )

cat("RQ2 Sample Size:", nrow(rq2_sample), "\n\n")

cat("==============================================================================\n")
cat("CATEGORICAL VARIABLE FREQUENCIES\n")
cat("==============================================================================\n\n")

# Ethnicity
cat("ETHNICITY:\n")
ethnicity_counts <- rq2_sample %>%
  count(ethnicity, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100,
    per_category_n = n
  )
print(ethnicity_counts)
cat("Total levels:", nrow(ethnicity_counts), "\n\n")

# Political affiliation
cat("POLITICAL AFFILIATION:\n")
political_counts <- rq2_sample %>%
  count(political.affiliation, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100,
    per_category_n = n
  )
print(political_counts)
cat("Total levels:", nrow(political_counts), "\n\n")

# Sex
cat("SEX:\n")
sex_counts <- rq2_sample %>%
  count(sex, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100
  )
print(sex_counts)
cat("\n")

# Employment status
cat("EMPLOYMENT STATUS:\n")
employment_counts <- rq2_sample %>%
  count(employment.status, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100
  )
print(employment_counts)
cat("\n")

# Student status
cat("STUDENT STATUS:\n")
student_counts <- rq2_sample %>%
  count(student.status, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100
  )
print(student_counts)
cat("\n")

cat("==============================================================================\n")
cat("CATEGORIES WITH <50 OBSERVATIONS (RULE OF THUMB: PROBLEMATIC)\n")
cat("==============================================================================\n\n")

small_political <- political_counts %>% filter(n < 50)
cat("Political affiliation categories with n<50:\n")
print(small_political)
cat("\n")

small_ethnicity <- ethnicity_counts %>% filter(n < 50)
if (nrow(small_ethnicity) > 0) {
  cat("Ethnicity categories with n<50:\n")
  print(small_ethnicity)
} else {
  cat("All ethnicity categories have n>=50\n")
}
cat("\n")

cat("==============================================================================\n")
cat("RECOMMENDED COLLAPSING SCHEMES\n")
cat("==============================================================================\n\n")

cat("ETHNICITY OPTIONS:\n")
cat("  Option A (binary): White vs Non-White\n")
cat("  Option B (4-level): White, Black, Asian, Other (combine Mixed+Other)\n")
cat("  Option C (keep current): 5 levels\n\n")

cat("POLITICAL AFFILIATION OPTIONS:\n")
cat("  Option A (3-level ideology):\n")
cat("    - Left (Labour, Democrat, Green, SNP)\n")
cat("    - Right (Conservative, Republican)\n")
cat("    - Other (Independent, Don't Know, Would not vote, Other, Plaid Cymru)\n\n")
cat("  Option B (4-level UK/US split):\n")
cat("    - UK Left (Labour, Green, SNP)\n")
cat("    - UK Right (Conservative)\n")
cat("    - US (Democrat, Republican, Independent)\n")
cat("    - Other (Don't Know, Would not vote, Other parties)\n\n")
cat("  Option C (keep current): 11 levels\n\n")
