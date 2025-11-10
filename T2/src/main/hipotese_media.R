library(dplyr)
library(readr)

data <- read_csv("../resources/data.csv")


drama_apenas <- data %>%
  filter(genre == "Drama")

comedia_apenas <- data %>%
  filter(genre == "Comedy")

teste_drama_vs_comedia <- t.test(
  x = drama_apenas$vote_average,
  y = comedia_apenas$vote_average,
  alternative = "greater",
  var.equal = FALSE 
)

print(teste_drama_vs_comedia)

cat(
  "\n[Drama Puro]:", nrow(drama_apenas), 
  "filmes  |  [Comédia Pura]:", nrow(comedia_apenas), 
  "filmes\n"
)

cat("Média Drama Puro =", mean(drama_apenas$vote_average), 
    " | Média Comédia Pura =", mean(comedia_apenas$vote_average), "\n")

library(ggplot2)

drama_df <- data.frame(
  Genero = "Drama",
  Nota = drama_apenas$vote_average
)

comedia_df <- data.frame(
  Genero = "Comédia",
  Nota = comedia_apenas$vote_average
)

dados_grafico <- rbind(drama_df, comedia_df)

ggplot(dados_grafico, aes(x = Genero, y = Nota, fill = Genero)) +

  geom_violin(trim = FALSE, alpha = 0.8) +

  geom_boxplot(
    width = 0.1, fill = "white", color = "black", alpha = 0.7
  ) +

  stat_summary(
    fun = mean, geom = "point", shape = 23, size = 4, fill = "red"
  ) +

  labs(
    title = "Comparação das Notas Médias: Drama vs. Comédia",
    subtitle = "Filmes com gênero único (Drama n=611, Comédia n=744)",
    x = "Gênero do Filme",
    y = "Média de Votos (vote_average)"
  ) +

  theme_minimal() +

  coord_flip()

ggsave("grafico_drama_comedia.png", width = 10, height = 6)