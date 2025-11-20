library(styler)

cat("Styling Ingram_NW_Awareness.R...\n")
cat("This may take a minute...\n\n")

# Style the file with strict = FALSE to be less aggressive
styler::style_file(
  "Ingram_NW_Awareness.R",
  strict = FALSE,
  scope = "indention",  # Only fix indentation, don't change other style
  indent_by = 2
)

cat("\nDone! Indentation fixed.\n")
