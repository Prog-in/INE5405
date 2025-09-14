setwd("/home/sofia/Sofia/UFSC/5º semestre/Probabilidade e Estatística/Trabalho")
filmes <- read.csv("top10K-TMDB-movies.csv")

library(ggplot2)

# --- GRÁFICO 1: PIZZA PRINCIPAL ---

# Pega a contagem de filmes em inglês da nossa tabela original de idiomas
contagem_en <- tabela_idiomas$Contagem[tabela_idiomas$Idioma == 'en']

# Soma a contagem de TODOS os outros idiomas
contagem_outros <- sum(tabela_idiomas$Contagem[tabela_idiomas$Idioma != 'en'])

# Cria uma nova tabela com apenas essas duas informações
pizza_principal_dados <- data.frame(
  Categoria = c("Inglês (en)", "Outros Idiomas"),
  Contagem = c(contagem_en, contagem_outros)
)

# Adiciona a coluna de porcentagem
pizza_principal_dados$Porcentagem <- scales::percent(pizza_principal_dados$Contagem / sum(pizza_principal_dados$Contagem), accuracy = 0.1)


# Gera o gráfico de pizza principal
ggplot(pizza_principal_dados, aes(x = "", y = Contagem, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Porcentagem), position = position_stack(vjust = 0.5), color = "white", size = 6) +
  labs(title = "Proporção de Filmes: Inglês vs. Outros Idiomas") +
  theme_void() +
  scale_fill_manual(values=c("skyblue", "gray")) # Define cores manualmente






# --- GRÁFICO 2: COROA CIRCULAR DOS OUTROS IDIOMAS ---

# Preparação dos dados
idiomas_nao_en <- subset(tabela_idiomas, Idioma != 'en')
top_5_nao_en <- head(idiomas_nao_en, 5)
soma_resto <- sum(idiomas_nao_en$Contagem[6:nrow(idiomas_nao_en)])
linha_outros <- data.frame(Idioma = "Outros", Contagem = soma_resto)
pizza_detalhada_dados <- rbind(top_5_nao_en, linha_outros)

# Coluna de porcentagem
pizza_detalhada_dados$Porcentagem <- scales::percent(pizza_detalhada_dados$Contagem / sum(pizza_detalhada_dados$Contagem), accuracy = 0.1)

pizza_detalhada_dados$Idioma <- as.character(pizza_detalhada_dados$Idioma)


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
ggplot(pizza_detalhada_dados, aes(x = 2, y = Contagem, fill = reorder(Idioma_Descritivo, -Contagem))) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = Porcentagem), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.5, 2.5)) + 
  labs(title = "Composição dos 'Outros Idiomas'", fill = "Idioma") +
  theme_void()
