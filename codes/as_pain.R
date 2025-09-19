# importing data
as <- read.csv("https://raw.githubusercontent.com/gungorMetehan/my_df/refs/heads/main/dfs/as.csv")

# data manipulation
## converting date format
as$date <- as.Date(as$date, format = "%d.%m.%Y")
## ordering
as <- as[order(as$date), ]
## creating id variable for the x-axis
as$id <- seq_along(as$date)
## values
as$measure <- "Pain \nLevel"

# plotting
library(ggplot2)
ggplot(as, aes(x = id, y = measure, fill = pain)) +
  geom_vline(xintercept = 108, color = "grey80", size = .4, linetype = "dashed") +
  geom_tile(color = "white", size = .5, height = .15, width = 1) +
  geom_text(aes(label = pain), col = "grey100", size = 3.25, angle = 90) +
  scale_x_continuous(
    breaks = as$id,
    labels = format(as$date, "%d %b"),
    expand = expansion(add = 0.5)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(size = 8),
    legend.key.height = grid::unit(0.75, "cm"),
    legend.key.width = grid::unit(0.3, "cm"),
    axis.text.x = element_text(size = 10, color = "grey30", angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(vjust = .5, color = "grey30", hjust = 0, size = 10),
    axis.ticks = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0.7, 0.1, 0.1, 0.2, "cm"),
    text = element_text(family = "AvantGarde")
  ) +
  labs(x = "", y = "") +
  scale_fill_gradient(low = "white", high = "#c20000", limits = c(0, 10)) +
  geom_curve(aes(x = 1, y = 1.5, xend = 1, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0) +
  annotate(geom = "text", x = 0.5, y = 1.55, hjust = 0,
           label = "the day before \nthe hospital visit", color = "black", size = 3, family = "AvantGarde") +
  
  geom_curve(aes(x = 3, y = 1.25, xend = 2, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 2.75, y = 1.3, hjust = 0,
           label = "first morning after \nmelox fort (15 mg)", color = "black", size = 3, family = "AvantGarde") +
  
  geom_curve(aes(x = 15, y = 1.5, xend = 14, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 14.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 3, family = "AvantGarde") +
  
  geom_curve(aes(x = 17, y = 1.25, xend = 16, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 16.75, y = 1.30, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 3, family = "AvantGarde") +
  
  geom_curve(aes(x = 77, y = 1.5, xend = 76, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 76.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 3, family = "AvantGarde") +
  
  annotate(geom = "text", x = 109, y = 1.5, label = "no treatment", color = "#c20000", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 5)
