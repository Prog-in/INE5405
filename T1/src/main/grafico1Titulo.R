library(readr)
library(ggplot2)
library(dplyr)


movies <- read_csv("../resources/data.csv")

movies$title_length <- nchar(movies$title)


# --- CRIAÇÃO DOS INTERVALOS CUSTOMIZADOS (LARGURA 5) ---

# 1. Encontrar o valor mínimo e máximo para os limites
min_len <- min(movies$title_length, na.rm = TRUE)
max_len <- max(movies$title_length, na.rm = TRUE)

# 2. Criar os pontos de corte para os primeiros 8 intervalos de 5 em 5
# Começamos do mínimo e vamos somando 5, 8 vezes.
# Isso nos dá os 9 primeiros pontos de corte.
breaks_inicio <- floor(min_len) -1 + (0:8 * 5)

# 3. Construir o vetor final de cortes: os 9 pontos iniciais + o valor máximo no final
breaks <- c(breaks_inicio, max_len)
breaks <- unique(breaks) # Garantir que não haja duplicatas
breaks <- breaks[breaks <= max_len]
if (tail(breaks, 1) < max_len) {
  breaks <- c(breaks, max_len)
}


# 4. Criar os rótulos dinamicamente
labels <- c()
# Loop para criar os primeiros 8 rótulos
num_intervalos_fixos <- length(breaks) - 2
for (i in 1:num_intervalos_fixos) {
  start <- breaks[i] + 1
  end <- breaks[i+1]
  labels <- c(labels, paste0(start, "-", end))
}
# Adicionar o rótulo final para o restante
start_final <- breaks[length(breaks)-1] + 1
labels <- c(labels, paste0(start_final, "+"))


# 5. Criar a nova coluna de classes
movies$title_class_custom_5 <- cut(movies$title_length,
                                   breaks = breaks,
                                   labels = labels,
                                   include.lowest = TRUE,
                                   right = TRUE)


# --- GRÁFICO DE BARRAS COM OS NOVOS INTERVALOS ---

bar_chart_custom_5 <- ggplot(data = movies, aes(x = title_class_custom_5, fill = title_class_custom_5)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3.5) +
  labs(
    title = "Distribuição de Filmes por Tamanho do Título",
    x = "Tamanho do Título (em caracteres)",
    y = "Número de Filmes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(bar_chart_custom_5)

ggsave("titulo_contagem_caracteres.png", plot = bar_chart_custom_5, width = 14, height = 8)