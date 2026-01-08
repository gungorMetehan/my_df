# importing data
as <- read.csv("https://raw.githubusercontent.com/gungorMetehan/my_df/refs/heads/main/dfs/as.csv")

# data manipulation
## converting date format
as$date <- as.Date(as$date, format = "%d.%m.%Y")
## ordering

## timeline index (dates may not be strictly ordered by year)
as$id <- seq_len(nrow(as))
## values
as$measure <- "Pain \nLevel"

# plotting
library(ggplot2)
ggplot(as, aes(x = id, y = measure, fill = pain)) +
  geom_vline(xintercept = 108, color = "grey80", size = .4, linetype = "dashed") +
  geom_vline(xintercept = 111, color = "grey80", size = .4, linetype = "dashed") +
  geom_vline(xintercept = 121, color = "grey80", size = .4, linetype = "dashed") +
  geom_vline(xintercept = 125, color = "grey80", size = .4, linetype = "dashed") +
  geom_tile(color = "white", size = .5, height = .15, width = 1) +
  geom_text(aes(label = pain), col = "grey100", size = 1, angle = 90) +
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
    axis.text.x = element_text(size = 3.5, color = "grey30", angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(vjust = .5, color = "grey30", hjust = 0, size = 10),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0.7, 0.1, 0.1, 0.2, "cm"),
    text = element_text(family = "AvantGarde"),
    panel.grid.major.x = element_line(color = "grey99", linewidth = 0.2)
  ) +
  labs(x = "", y = "") +
  scale_fill_gradient(low = "white", high = "#c20000", limits = c(0, 10)) +
  geom_curve(aes(x = 1, y = 1.5, xend = 1, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0) +
  annotate(geom = "text", x = 0.5, y = 1.55, hjust = 0,
           label = "the day before \nthe hospital visit", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 2, y = 0.7, xend = 2, yend = 0.9),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0) +
  annotate(geom = "text", x = 1, y = 0.65, hjust = 0,
           label = "first morning after \nmelox fort (15 mg)", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 15, y = 1.5, xend = 14, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 14.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 17, y = 1.25, xend = 16, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 16.75, y = 1.30, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 77, y = 1.5, xend = 76, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 76.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 2, family = "AvantGarde") +
  
  annotate(geom = "text", x = 109.5, y = 1.5, label = "no treatment", color = "#c20000", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 3) +
  annotate("rect",
           xmin = 108, xmax = 111,
           ymin = -Inf, ymax = Inf,
           fill = "#c20000", alpha = 0.03) +
  annotate(geom = "text", x = 123, y = 1.5, label = "no treatment", color = "#c20000", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 3) +
  annotate("rect",
           xmin = 121, xmax = 125,
           ymin = -Inf, ymax = Inf,
           fill = "#c20000", alpha = 0.03) +
  
  geom_curve(aes(x = 132, y = 1.5, xend = 131, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 131.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 147, y = 1.5, xend = 146, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0.3) +
  annotate(geom = "text", x = 146.75, y = 1.55, hjust = 0,
           label = "i forgot to \ntake the medicine \nlast night", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 192, y = 1.5, xend = 192, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0) +
  annotate(geom = "text", x = 191.5, y = 1.55, hjust = 0,
           label = "in dubai", color = "black", size = 2, family = "AvantGarde") +
  
  geom_curve(aes(x = 199, y = 1.5, xend = 199, yend = 1.1),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "black", linewidth = 0.1, curvature = 0) +
  annotate(geom = "text", x = 198.5, y = 1.55, hjust = 0,
           label = "in ankara \nagain", color = "black", size = 2, family = "AvantGarde")
