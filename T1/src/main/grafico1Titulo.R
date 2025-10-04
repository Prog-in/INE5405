if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)


movies <- read_csv("../resources/data.csv")
movies$title_length <- nchar(movies$title)

max_len <- max(movies$title_length, na.rm = TRUE)
breaks <- seq(0, max_len + 1, by = 5)
labels <- paste0(breaks[-length(breaks)] + 1, "-", breaks[-1])

movies$title_class <- cut(movies$title_length, breaks = breaks, labels = labels, include.lowest = TRUE)

df_length <- movies %>%
  count(title_class)

area_chart <- ggplot(df_length, aes(x = title_class, y = n, group = 1)) +
  geom_area(fill = "grey30", alpha = 0.5) +
  geom_point(size = 3, color = "grey30") +
  geom_text(aes(label = n), vjust = -0.5, size = 7) +
  theme_minimal(base_size = 20) +
  labs(
    x = "Tamanho do Título (em caracteres)",
    y = "Número de Filmes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(area_chart)
ggsave("titulo_contagem_caracteres.png", plot = area_chart, width = 14, height = 8)

