# install.packages(c("ggplot2","dplyr")) # if needed
library(ggplot2)
library(dplyr)

# Example data: each row = one bubble
df <- tibble::tribble(
  ~pathway,                           ~group,               ~value, ~category,
  "Glycan",                           "NonSpor_Lytic",      10,    "Sporulation homologs",
  "Glycan",                           "NonSpor_Temp",       2,     "Sporulation homologs",
  "Glycan",                           "Spor_Lytic",         25,    "Sporulation homologs",
  "Glycan",                           "Spor_Temp",          40,    "Sporulation homologs",
  "Glycerophospholipid",              "NonSpor_Lytic",      5,     "Lipid",
  "Glycerophospholipid",              "Spor_Temp",          8,     "Lipid",
  "Fatty acid biosynthesis",          "Spor_Lytic",         6,     "Lipid",
  "Lipopolysaccharide biosynthesis",  "NonSpor_Temp",       3,     "Lipid",
  "Alanine/aspartate/glutamate",      "NonSpor_Lytic",      1,     "AA",
  "Glycine/serine/threonine",         "Spor_Temp",          12,    "AA"
)

# Make pathway an ordered factor so the plot y-order is logical
pathway_levels <- rev(unique(df$pathway)) # reverse so top item plotted at top
df <- df %>% mutate(
  pathway = factor(pathway, levels = pathway_levels),
  group_f = factor(group, levels = c("NonSpor_Lytic","NonSpor_Temp","Spor_Lytic","Spor_Temp")),
  # numeric x positions for tile/strip placement
  xpos = as.numeric(group_f)
)

# Choose a color for each 'category' — you can map to a palette or use a named vector
cat_colors <- c("Sporulation homologs" = "#1f78b4",
                "Lipid" = "#33a02c",
                "AA" = "#b15928")

# Basic bubble plot
p <- ggplot(df, aes(x = xpos, y = pathway)) +
  geom_point(aes(size = value), shape = 21, stroke = 0.6, fill = "#0D5F85", colour = "black", alpha = 0.9) +
  scale_size_area(max_size = 18) +                      # control max bubble diameter
  scale_x_continuous(breaks = 1:4,
                     labels = c("NonSpor\nLytic", "NonSpor\nTemp", "Spor\nLytic", "Spor\nTemp"),
                     expand = expansion(add = c(0.5, 1.5))) +  # add space at right for category strip
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
p

# Add a colored strip at the right that corresponds to pathway categories
# Make one tile per pathway at an x coordinate beyond the last group (here 4.6)
strip_x <- max(df$xpos) + 0.8
strip_df <- df %>% select(pathway, category) %>% distinct() %>%
  mutate(x = strip_x, xend = strip_x + 0.4)

p <- p +
  geom_tile(data = strip_df, aes(x = x, y = pathway, fill = category), width = 0.4, height = 0.9, inherit.aes = FALSE) +
  scale_fill_manual(values = cat_colors, name = NULL) +
  coord_cartesian(clip = "off")  # allow strip to extend into margin

# Add legend and show
p + guides(size = guide_legend(title = "Value")) +
  theme(plot.margin = margin(5, 40, 5, 5))  # give room on the right for the strip


