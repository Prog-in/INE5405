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

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)


movies <- read_csv("../resources/data.csv")

movies_por_ano <- movies %>%
  filter(!is.na(release_date)) %>%
  mutate(ano_lancamento = year(release_date)) %>%
  filter(ano_lancamento > 1920 & ano_lancamento <= 2024) %>%
  count(ano_lancamento)

lancamentos_por_ano_plot <- ggplot(data = movies_por_ano, aes(x = ano_lancamento, y = n)) +
  geom_line(size = 1, color = "grey30") +
  geom_point(size = 2, color = "grey30") +
  coord_cartesian(xlim = c(NA, 2024), expand = FALSE) +
  labs(
    x = "Ano de Lançamento",
    y = "Número de Filmes Lançados"
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(lancamentos_por_ano_plot)
ggsave("grafico_lancamentos_por_ano.png", plot = lancamentos_por_ano_plot, width = 12, height = 7)