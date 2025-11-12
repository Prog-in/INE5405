# --- Preparação dos dados ---
library(readr)

data <- read_csv("../resources/data.csv")

# --- Coeficiente de Correlação: Popularidade x Média de Votos ---
# (Testa a relação linear entre as duas variáveis)

# Cálculo simples do coeficiente r de Pearson
r <- cor(data$popularity, data$vote_average, method = "pearson", use = "complete.obs")

# Teste de significância (H0: ρ = 0)
teste_cor <- cor.test(data$popularity, data$vote_average, method = "pearson")

# --- Resultados ---
cat("Coeficiente de correlação de Pearson (r):", round(r, 4), "\n")
cat("p-valor:", format.pval(teste_cor$p.value, digits = 4), "\n")

# --- Visualização (opcional) ---
plot(
  data$popularity, data$vote_average,
  main = "Relação entre Popularidade e Média de Votos",
  xlab = "Popularidade", ylab = "Média de Votos",
  pch = 19, col = rgb(0.2, 0.4, 0.6, 0.3)
)
abline(lm(vote_average ~ popularity, data = data), col = "red", lwd = 2)
