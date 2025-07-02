# importing data
liver_enzymes <- read.csv("https://raw.githubusercontent.com/gungorMetehan/my_df/refs/heads/main/dfs/liver_enzymes.csv", header = T, sep = ",")

# required packages
library(ggplot2)
install.packages("tidyr")
library(tidyr)

liver_enzymes$date <- as.Date(liver_enzymes$date, format = "%d.%m.%Y")

# data manipulation
liver_long <- pivot_longer(
  liver_enzymes,
  cols = c(AST, ALT, GGT, ALP),
  names_to = "Enzyme",
  values_to = "Value"
)

# limits for the rectangles
limits <- data.frame(
  Enzyme = c("ALP", "ALT", "AST", "GGT"),
  ymin = c(40, 0, 0, 10),
  ymax = c(130, 50, 50, 71)
)

# line plots
ggplot(liver_long, aes(x = factor(date), y = Value, group = 1, color = Enzyme)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_rect(data = limits, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "#00c287", alpha = 0.15) +
  labs(x = "", y = "", title = "Liver Enzymes Over Time with Ideal Ranges", color = "Enzyme") +
  geom_text(aes(label = Value), vjust = -1, color = "black", size = 3, show.legend = FALSE, family = "AvantGarde") +
  facet_wrap(~ Enzyme, scales = "fixed") +
    theme_minimal() +
  theme(
    text = element_text(family = "AvantGarde"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  coord_cartesian(ylim = c(0, 250))

