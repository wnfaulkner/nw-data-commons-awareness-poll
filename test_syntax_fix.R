# Test the syntax fix for case_when trailing comma issue
library(dplyr)
library(magrittr)

cat("\n================================================================================\n")
cat("TESTING SYNTAX FIX - case_when trailing comma removal\n")
cat("================================================================================\n\n")

# Create minimal test data
test_data <- data.frame(
  shown.infographic = c("A", "B", "A", "B", "A")
)

cat("Original data:\n")
print(test_data)
cat("\n")

# Test the FIXED code (no trailing comma)
cat("Testing FIXED case_when syntax (no trailing comma)...\n")
result <- tryCatch({
  test_data %<>%
    mutate(shown.infographic = dplyr::case_when(
      shown.infographic == "A" ~ "shown infographic",
      shown.infographic == "B" ~ "no infographic"
    ))

  cat("SUCCESS! Code executed without errors.\n\n")
  cat("Transformed data:\n")
  print(test_data)
  cat("\n")

  cat("================================================================================\n")
  cat("SYNTAX FIX VERIFIED - No trailing comma issues\n")
  cat("================================================================================\n\n")

  TRUE
}, error = function(e) {
  cat("ERROR:", e$message, "\n\n")
  cat("================================================================================\n")
  cat("SYNTAX FIX FAILED\n")
  cat("================================================================================\n\n")
  FALSE
})
