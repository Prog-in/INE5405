library(readr)
library(ggplot2)
library(dplyr)


movies <- read_csv("../resources/data.csv")

movies$title_length <- nchar(movies$title)

quartile_breaks_dec <- quantile(movies$title_length, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

rounded_quartile_breaks <- unique(round(quartile_breaks_dec))

labels <- c()
labels[1] <- paste0(rounded_quartile_breaks[1], " - ", rounded_quartile_breaks[2])
for (i in 2:(length(rounded_quartile_breaks) - 1)) {
  start <- rounded_quartile_breaks[i] + 1
  end <- rounded_quartile_breaks[i+1]
  labels <- c(labels, paste0(start, " - ", end))
}

movies$title_class_quartile <- cut(movies$title_length, breaks = rounded_quartile_breaks,labels = labels, include.lowest = TRUE, right = TRUE)


bar_chart_final <- ggplot(data = movies, aes(x = title_class_quartile, fill = title_class_quartile)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 4) +
  labs(
    title = "Distribuição de Filmes por Tamanho do Título",
    x = "Tamanho do Título (em caracteres)",
    y = "Número de Filmes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(bar_chart_final)
ggsave("titulo_contagem_caracteres.png", plot = bar_chart_final, width = 10, height = 7)