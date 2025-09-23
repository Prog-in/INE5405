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


# --- ANÁLISE POR ANO ---
movies_por_ano <- movies %>%
  filter(!is.na(release_date)) %>%
  mutate(ano_lancamento = year(release_date)) %>%
  filter(ano_lancamento > 1920 & ano_lancamento <= 2024) %>%
  count(ano_lancamento)

lancamentos_por_ano_plot <- ggplot(data = movies_por_ano, aes(x = ano_lancamento, y = n)) +
  geom_line(color = "dodgerblue", size = 1) +
  geom_point(color = "dodgerblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linetype = "dashed") +
  # --- AJUSTE DEFINITIVO: Remove a expansão automática do eixo ---
  coord_cartesian(xlim = c(NA, 2024), expand = FALSE) +
  labs(
    title = "Evolução do Número de Lançamentos de Filmes por Ano",
    subtitle = "Observa-se um crescimento expressivo a partir da década de 90",
    x = "Ano de Lançamento",
    y = "Número de Filmes Lançados"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(lancamentos_por_ano_plot)
ggsave("grafico_lancamentos_por_ano.png", plot = lancamentos_por_ano_plot, width = 12, height = 7)