library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)


movies <- read_csv("/home/lucas/INE5405/T1/src/resources/data.csv") # Está com o caminho absoluto pois estava dando algum bug na minha maquina

movies_com_contagem <- movies %>%
  filter(!is.na(release_date) & !is.na(title)) %>%
  mutate(
    ano_lancamento = year(release_date),
    word_count = str_count(title, "\\S+")
  ) %>%
  filter(ano_lancamento > 1920 & ano_lancamento <= 2024)


media_palavras_por_ano <- movies_com_contagem %>%
  group_by(ano_lancamento) %>%
  summarise(media_palavras = mean(word_count, na.rm = TRUE))



evolucao_palavras_plot <- ggplot(data = media_palavras_por_ano, aes(x = ano_lancamento, y = media_palavras)) +
  geom_line(size = 1, color = "grey30") +
  geom_point(size = 2, color = "grey30") +
  labs(
    x = "Ano de Lançamento",
    y = "Média de Palavras no Título"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(evolucao_palavras_plot)

ggsave("grafico_evolucao_palavras_titulo.png", plot = evolucao_palavras_plot, width = 12, height = 7)