library(ggplot2)

tabela_idiomas <- read.csv("../resources/data.csv")

idiomas_count <- as.data.frame(table(tabela_idiomas$original_language))
colnames(idiomas_count) <- c("original_language", "count")

# Preparação dos dados
idiomas_nao_en <- subset(idiomas_count, original_language != 'en')
ordenados_nao_en <- idiomas_nao_en[order(-idiomas_nao_en$count), ]
top_5_nao_en <- head(ordenados_nao_en, 5)
soma_resto <- sum(tail(ordenados_nao_en$count, -5))
linha_outros <- data.frame(original_language = "Outros", count = soma_resto)
pizza_detalhada_dados <- rbind(top_5_nao_en, linha_outros)

# Coluna de porcentagem
pizza_detalhada_dados$Porcentagem <- scales::percent(
  pizza_detalhada_dados$count / sum(pizza_detalhada_dados$count),
  accuracy = 0.1)

pizza_detalhada_dados$Idioma <- as.character(pizza_detalhada_dados$original_language)

# Criação dos nomes descritivos
mapa_nomes <- c(
  "fr" = "Francês",
  "ja" = "Japonês",
  "it" = "Italiano",
  "es" = "Espanhol",
  "de" = "Alemão"
)
pizza_detalhada_dados$Idioma_Descritivo <- ifelse(
  pizza_detalhada_dados$Idioma == "Outros",
  "Outros",
  paste0(mapa_nomes[pizza_detalhada_dados$Idioma], " (", pizza_detalhada_dados$Idioma, ")")
)

# Gráfico de rosca final
ggplot(pizza_detalhada_dados, aes(x = 2, y = count, fill = reorder(Idioma_Descritivo, -count))) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = Porcentagem),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5.8) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.5, 2.5)) +
  labs(fill = "Idioma") +
  theme_void() +
    theme(
        legend.title = element_text(size = 17), # Altera o tamanho do TÍTULO "Idioma"
        legend.text = element_text(size = 15)   # Altera o tamanho dos ITENS (Francês (fr), etc.)
    )

