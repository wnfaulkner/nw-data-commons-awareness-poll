# 4-ANALYSIS, STATISTICAL TESTS -------------------------------------------------------------------------------
# 4A SPECIAL VISUALIZATIONS

# SANKEY DIAGRAM - NUCLEAR WINTER AWARENESS FLOWS
# This diagram visualizes how awareness levels flow across three sources:
# 1980s awareness, recent academic awareness, and recent media awareness

# Prepare data for Sankey diagram
sankey_data <- data.tb %>%
  select(
    nw.awareness.1980s,
    nw.awareness.recent.academic,
    nw.awareness.recent.media
  ) %>%
  # Remove any rows with missing data
  filter(
    !is.na(nw.awareness.1980s),
    !is.na(nw.awareness.recent.academic),
    !is.na(nw.awareness.recent.media)
  ) %>%
  # Count frequency of each unique pattern
  group_by(
    nw.awareness.1980s,
    nw.awareness.recent.academic,
    nw.awareness.recent.media
  ) %>%
  summarise(freq = n(), .groups = 'drop') %>%
  # Ensure awareness levels are ordered factors (reversed so "4. know a lot" appears on top)
  mutate(
    nw.awareness.1980s = factor(
      nw.awareness.1980s,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    ),
    nw.awareness.recent.academic = factor(
      nw.awareness.recent.academic,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    ),
    nw.awareness.recent.media = factor(
      nw.awareness.recent.media,
      levels = c("4. know a lot", "3. know something", "2. heard a little", "1. never heard"),
      ordered = TRUE
    )
  )

# Calculate total number of respondents
total_n <- sum(sankey_data$freq)

# Define color palette for awareness levels
awareness_colors <- c(
  "4. know a lot" = "#313695",        # Darkest blue
  "3. know something" = "#4575b4",   # Medium blue
  "2. heard a little" = "#91bfdb",   # Light blue
  "1. never heard" = "#e0f3f8"       # Lightest blue
)

# Add flow color column to sankey_data
sankey_data <- sankey_data %>%
  rowwise() %>%
  mutate(
    flow_color = get_flow_color(nw.awareness.1980s, nw.awareness.recent.academic, nw.awareness.recent.media)
  ) %>%
  ungroup()

# Create Sankey/alluvial diagram
awareness_sankey <- ggplot(
  sankey_data,
  aes(
    y = freq,
    axis1 = nw.awareness.1980s,
    axis2 = nw.awareness.recent.academic,
    axis3 = nw.awareness.recent.media
  )
) +
  # Add flows (alluvium) - colored based on connected awareness levels
  geom_alluvium(
    aes(fill = flow_color),
    width = 1/6,
    alpha = 0.7,
    curve_type = "cubic"
  ) +
  # Add stacked bars (strata) - colored by awareness level
  geom_stratum(
    aes(fill = c("4. know a lot" = "#313695", "3. know something" = "#4575b4",
                 "2. heard a little" = "#91bfdb", "1. never heard" = "#e0f3f8")[after_stat(stratum)]),
    width = 1/6,
    color = "grey30",
    size = 0.3
  ) +
  # Add labels to strata with percentages
  geom_text(
    stat = "stratum",
    aes(label = paste0(after_stat(stratum), "\n(",
                       round(after_stat(count) / total_n * 100, 1), "%)")),
    size = 4,
    fontface = "bold"
  ) +
  # Use identity scale to apply exact colors
  scale_fill_identity() +
  # Axis labels - reduced expand to bring labels closer
  scale_x_discrete(
    limits = c("1980s", "Recent\nAcademic", "Recent\nMedia"),
    expand = c(0.05, 0.05)
  ) +
  # Labels and theme
  labs(
    title = "Nuclear Winter Awareness Flows Across Information Sources",
    subtitle = "Each flow represents respondents with the same awareness pattern",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  )

# Display the plot
print(awareness_sankey)

# Save the plot as PNG
ggsave(
  filename = "outputs/awareness_sankey_diagram.png",
  plot = awareness_sankey,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nSankey diagram saved to: outputs/awareness_sankey_diagram.png\n")