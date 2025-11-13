# 1. Carregar os dados (como no seu código original)
dados <- read.csv("top10K-TMDB-movies.csv")

# 2. Filtrar os dados para incluir apenas filmes em inglês
# Assumimos que a coluna de idioma se chama 'original_language' 
# e que o código para inglês é 'en'.
dados_ingles <- subset(dados, original_language == "en")

# 3. Calcular a proporção de filmes em inglês com média (vote_average) > 8
# A função mean() em um vetor lógico (TRUE/FALSE) calcula a proporção de TRUEs.
# TRUE (se > 8) é tratado como 1, e FALSE (se <= 8) como 0.
# A média desses 1s e 0s é a proporção de filmes que atendem ao critério.
proporcao_media_maior_8 <- mean(dados_ingles$vote_average > 8)

# 4. Mostrar o resultado
print(paste("Total de filmes em inglês na amostra:", nrow(dados_ingles)))
print(paste("Proporção de filmes em inglês com média > 8:", proporcao_media_maior_8))

# Opcional: Mostrar em porcentagem formatada
proporcao_pct <- round(proporcao_media_maior_8 * 100, 2)
print(paste0(proporcao_pct, "% dos filmes em inglês têm nota média superior a 8."))
