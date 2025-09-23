library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)


movies <- read_csv("../resources/data.csv")


movies_com_contagem <- movies %>%
  filter(!is.na(release_date) & !is.na(title)) %>%
  mutate(
    ano_lancamento = year(release_date),
    word_count = str_count(title, "\\S+")
  ) %>%
  filter(ano_lancamento > 1920 & ano_lancamento <= 2024)

-
media_palavras_por_ano <- movies_com_contagem %>%
  group_by(ano_lancamento) %>%
  summarise(media_palavras = mean(word_count, na.rm = TRUE))



evolucao_palavras_plot <- ggplot(data = media_palavras_por_ano, aes(x = ano_lancamento, y = media_palavras)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linetype = "dashed") +
  labs(
    title = "Evolução do Número Médio de Palavras nos Títulos de Filmes",
    subtitle = "Análise da quantidade de palavras por título ao longo dos anos",
    x = "Ano de Lançamento",
    y = "Média de Palavras no Título"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(evolucao_palavras_plot)

ggsave("grafico_evolucao_palavras_titulo.png", plot = evolucao_palavras_plot, width = 12, height = 7)