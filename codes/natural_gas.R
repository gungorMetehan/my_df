# verinin ice aktarilmasi
natural_gas <- read.csv("https://raw.githubusercontent.com/gungorMetehan/my_df/refs/heads/main/dfs/natural_gas.csv", header = T, sep = ",")

# verinin kontrol edilmesi
natural_gas

# veri gorsellestirme icin gerekli paketin calistirilmasi
library(ggplot2)

# gorsellestirmede kullanilmak uzere gerekli duzenlemeler (iki farkli eksen icin)
max_tuketim_m3 <- max(natural_gas$tuketim_m3)
max_tutar <- max(natural_gas$tutar)
scale_ratio <- max_tuketim_m3 / max_tutar

# gorsellestirmede kullanilmak uzere gerekli duzenlemeler (aylarin siralamasini yapmak icin)
natural_gas$id <- seq_len(nrow(natural_gas))

# veri gorsellestirme
ggplot(natural_gas, aes(x = factor(id))) +
  # sutun grafigi ve degerlerin sutunların icine yazilmasi
  geom_bar(aes(y = tuketim_m3), stat = "identity", fill = "#31efb5") +
  geom_text(aes(y = tuketim_m3, label = round(tuketim_m3, 1)), 
            vjust = 1.5, size = 3, color = "white", family = "AvantGarde", fontface = "bold") +
  # cizgi grafigi ve grafigin uzerindeki noktalar
  geom_line(aes(y = tutar * scale_ratio, group = 1), color = "#00704e", size = 1.25) +
  geom_point(aes(y = tutar * scale_ratio), color = "#00704e", size = 1.25) +
  # x eksenindeki aylari duzenleme
  scale_x_discrete(labels = natural_gas$donem) +
  # ikinci eksenin hazirlanmasi (ggplot2 normalde buna imkan tanimiyor)
  scale_y_continuous(
    name = expression("Hacim " (m^3)),
    sec.axis = sec_axis(~ . / scale_ratio, name = "Tutar (₺)"), limits = c(0, 500)
  ) +
  # grafik basligi
  labs(title = "Aylara Göre Toplam Doğal Gaz Tüketimi ve Fatura Tutarı") +
  # tema ayarlari
  theme_minimal() +
  theme(
    axis.text.y.left = element_text(size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
    axis.title.y.right = element_text(color = "#00704e", size = 10),
    axis.text.y.right = element_text(color = "#00704e", size = 10),
    axis.title.x = element_blank(),
    text = element_text(family = "AvantGarde", size = 10),
    plot.title = element_text(size = 30),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  # dikey cizgiler
  geom_vline(xintercept = 12.5, color = "grey80", size = .4, linetype = "dashed") +
  geom_vline(xintercept = 24.5, color = "grey80", size = .4, linetype = "dashed") +
  geom_vline(xintercept = 36.5, color = "grey80", size = .4, linetype = "dashed") +
  # grafik uzerindeki 2022, 2023, 2024 ve 2025 yazilari
  annotate(geom = "text", x = 1.5, y = 450, label = "2022", color = "#00704e", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 20) +
  annotate(geom = "text", x = 13.5, y = 450, label = "2023", color = "#00704e", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 20) +
  annotate(geom = "text", x = 25.5, y = 450, label = "2024", color = "#00704e", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 20) +
  annotate(geom = "text", x = 37.5, y = 450, label = "2025", color = "#00704e", 
           fontface = "bold", family = "AvantGarde", alpha = .6, angle = 90, size = 20)
