if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

tabela_idiomas <- read.csv("../resources/data.csv")

idiomas_count <- as.data.frame(table(tabela_idiomas$original_language))
colnames(idiomas_count) <- c("original_language", "count")

# Pega a contagem de filmes em inglês da nossa tabela original de idiomas
contagem_en <- idiomas_count[idiomas_count$original_language == "en", "count"]

# Soma a contagem de TODOS os outros idiomas
contagem_outros <- sum(idiomas_count[idiomas_count$original_language != "en", "count"])

# Cria uma nova tabela com apenas essas duas informações
pizza_principal_dados <- data.frame(
  Categoria = c("Inglês (en)", "Outros Idiomas"),
  Contagem = c(contagem_en, contagem_outros)
)

# Adiciona a coluna de porcentagem
pizza_principal_dados$Porcentagem <- scales::percent(
  pizza_principal_dados$Contagem / sum(pizza_principal_dados$Contagem),
  accuracy = 0.1)

# Gera o gráfico de pizza principal
ggplot(pizza_principal_dados, aes(x = "", y = Contagem, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Porcentagem),
      position = position_stack(vjust = 0.5),
      color = "white", size = 7) +
  theme_void() +
  scale_fill_manual(values=c("skyblue", "gray")) +
    theme(
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
    )

