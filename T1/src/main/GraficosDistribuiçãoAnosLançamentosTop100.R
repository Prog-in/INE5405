library(dplyr)
library(ggplot2)

# 1. Pegar os 100 filmes mais populares
top_100_filmes <- data %>%
  arrange(desc(popularity)) %>%
  head(100)

# 2. Extrair o ano da coluna release_date
top_100_filmes <- top_100_filmes %>%
  mutate(ano = as.numeric(substr(release_date, 1, 4)))

# 3. Contar quantos filmes por ano
df_anos <- top_100_filmes %>%
  group_by(ano) %>%
  summarise(frequencia = n(), .groups = "drop")

# 4. Gráfico de barras com fonte maior em tudo
ggplot(df_anos, aes(x = ano, y = frequencia)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(
    title = "",
    x = "Ano de Lançamento",
    y = "Número de Filmes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20)
  )

