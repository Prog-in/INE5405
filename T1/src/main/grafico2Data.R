library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(lubridate)

movies <- read_csv("/home/lucas/INE5405/T1/src/resources/data.csv") # Está com o caminho absoluto pois estava dando algum bug na minha maquina

movies_por_decada <- movies %>%
  filter(!is.na(release_date)) %>%
  mutate(ano_lancamento = year(release_date)) %>%
  filter(ano_lancamento >= 1970 & ano_lancamento <= 2024) %>%
  mutate(decada = floor(ano_lancamento / 10) * 10) %>%
  count(decada) %>%
  mutate(decada_label = paste0(decada, "s"))

line_chart_decada <- ggplot(data = movies_por_decada, aes(x = decada_label, y = n, group = 1)) +
  geom_line(linewidth = 1, color = "grey30") +
  geom_point(size = 4, color = "grey30") +
  geom_text(aes(label = n), vjust = -0.8, size = 4) +
  theme_minimal(base_size = 16) +
  labs(
    x = "Década",
    y = "Número de Filmes Lançados"
  )

print(line_chart_decada)
ggsave("grafico_lancamentos_por_decada.png", plot = line_chart_decada, width = 12, height = 7)

