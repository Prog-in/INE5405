# --- CARREGAR PACOTES E DADOS ---
if (!require(readr)) install.packages("readr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

movies <- read_csv("../resources/data.csv")


# --- ANÁLISE POR DÉCADA ---
movies_por_decada <- movies %>%
  filter(!is.na(release_date)) %>%
  mutate(ano_lancamento = year(release_date)) %>%
  filter(ano_lancamento >= 1970 & ano_lancamento <= 2024) %>%
  mutate(decada = floor(ano_lancamento / 10) * 10) %>%
  count(decada) %>%
  mutate(decada_label = paste0(decada, "s")) 

lancamentos_por_decada_plot <- ggplot(data = movies_por_decada, aes(x = decada_label, y = n, fill = decada_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  labs(
    title = "Número de Filmes Lançados por Década",
    subtitle = "A produção cresce exponencialmente nas décadas mais recentes do dataset",
    x = "Década",
    y = "Número de Filmes Lançados"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(lancamentos_por_decada_plot)
ggsave("grafico_lancamentos_por_decada.png", plot = lancamentos_por_decada_plot, width = 12, height = 7)