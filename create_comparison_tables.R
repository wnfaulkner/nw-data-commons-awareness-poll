# Create visual comparison tables
library(ggplot2)
library(gridExtra)
library(grid)

# casualties.blast
blast_data <- data.frame(
  "Forest.Plot.Variable" = c("sexMale", "ethnicityBlack", "ethnicityMixed", "political.affiliationUK-Independent"),
  "Forest.Order" = c(21, 23, 24, 25),
  "Interpretation.Variable" = c("sexMale", "ethnicityBlack", "ethnicityMixed", "political.affiliationUK-Independent"),
  "Interpretation.Order" = c(1, 2, 3, 4)
)

# casualties.radiation
radiation_data <- data.frame(
  "Forest.Plot.Variable" = c("political.affiliationUSA-Republican", "age", "sexMale", "political.affiliationUK-Independent"),
  "Forest.Order" = c(1, 11, 20, 25),
  "Interpretation.Variable" = c("political.affiliationUSA-Republican", "age", "sexMale", "political.affiliationUK-Independent"),
  "Interpretation.Order" = c(1, 2, 3, 4)
)

# casualties.starvation
starvation_data <- data.frame(
  "Forest.Plot.Variable" = c("political.affiliationUK-Plaid Cymru", "age", "sexMale"),
  "Forest.Order" = c(1, 10, 23),
  "Interpretation.Variable" = c("political.affiliationUK-Plaid Cymru", "age", "sexMale"),
  "Interpretation.Order" = c(1, 2, 3)
)

# Create PNG
png("/home/wnf/code/nw-data-commons-awareness-poll/comparison_tables.png",
    width = 1200, height = 1000, res = 100)

# Set up layout
par(mfrow = c(3, 1))

# Create theme for tables
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params = list(cex = 0.9)),
  colhead = list(fg_params = list(cex = 1.0, fontface = "bold")),
  rowhead = list(fg_params = list(cex = 0.9))
)

# Create tables
table1 <- tableGrob(blast_data, rows = NULL, theme = mytheme)
table2 <- tableGrob(radiation_data, rows = NULL, theme = mytheme)
table3 <- tableGrob(starvation_data, rows = NULL, theme = mytheme)

# Add titles
title1 <- textGrob("casualties.blast (p ≤ 0.1)", gp = gpar(fontsize = 14, fontface = "bold"))
title2 <- textGrob("casualties.radiation (p ≤ 0.1)", gp = gpar(fontsize = 14, fontface = "bold"))
title3 <- textGrob("casualties.starvation (p ≤ 0.1)", gp = gpar(fontsize = 14, fontface = "bold"))

# Arrange with titles
grid.arrange(
  title1, table1,
  title2, table2,
  title3, table3,
  ncol = 1,
  heights = c(0.5, 2, 0.5, 2, 0.5, 1.5)
)

dev.off()

cat("Table image saved to: /home/wnf/code/nw-data-commons-awareness-poll/comparison_tables.png\n")
