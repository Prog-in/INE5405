# 1. Carregar os dados (Se ainda não estiverem carregados)
dados <- read.csv("top10K-TMDB-movies.csv")

# 2. Definir o valor da hipótese nula (H0)
# (Baseado na sua observação dos filmes em inglês)
p_h0 <- 0.018

# 3. Preparar a amostra que será testada (filmes NÃO ingleses)
# Esta será a amostra da qual extrairemos 'x' e 'n'
dados_teste <- subset(dados, original_language != "en")

# 4. Calcular 'x' e 'n' para a amostra de teste
# n = total de filmes não ingleses
n_teste <- nrow(dados_teste)

# x = número de "sucessos" (vote_average > 8) nos filmes não ingleses
x_teste <- sum(dados_teste$vote_average > 8)

# 5. Executar o Teste de Proporção (prop.test)
# H0: p = 0.018
# H1: p > 0.018
# Nível de significância (alpha) = 0.05

teste_resultado <- prop.test(
  x = x_teste,
  n = n_teste,
  p = p_h0,
  alternative = "greater",
  correct = FALSE
)

# 6. Exibir os resultados
print(paste("Total de filmes NÃO ingleses (n):", n_teste))
print(paste("Sucessos em filmes NÃO ingleses (x):", x_teste))
print(paste("Proporção observada (x/n):", x_teste / n_teste))

print(teste_resultado)

