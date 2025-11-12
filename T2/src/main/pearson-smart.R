# --- Preparação dos dados ---
library(readr)
library(dplyr)

data <- read_csv("../resources/data.csv")

# --- Tratamento: remoção de outliers extremos de Popularidade ---

# cálculo dos quartis e IQR (faixa interquartílica)
Q1 <- quantile(data$popularity, 0.25, na.rm = TRUE)
Q3 <- quantile(data$popularity, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# filtrando apenas filmes dentro de 1.5 * IQR
data_central <- data %>%
  filter(popularity >= (Q1 - 1.5 * IQR) & popularity <= (Q3 + 1.5 * IQR))

cat("Tamanho original:", nrow(data), "\n")
cat("Tamanho após filtragem:", nrow(data_central), "\n")

# --- Cálculo do coeficiente de correlação (centrado) ---
r <- cor(data_central$popularity, data_central$vote_average,
         method = "pearson", use = "complete.obs")

teste_cor <- cor.test(data_central$popularity, data_central$vote_average,
                      method = "pearson")

# --- Resultados ---
cat("\nCoeficiente de correlação (r) após filtragem:", round(r, 4), "\n")
cat("p-valor:", format.pval(teste_cor$p.value, digits = 4), "\n")


# --- Visualização (opcional) ---
plot(
  data_central$popularity, data_central$vote_average,
  main = "Correlação entre Popularidade e Média de Votos (sem outliers)",
  xlab = "Popularidade (centrada)", ylab = "Média de Votos",
  pch = 19, col = rgb(0.2, 0.4, 0.6, 0.3)
)
abline(lm(vote_average ~ popularity, data = data_central), col = "red", lwd = 2)
