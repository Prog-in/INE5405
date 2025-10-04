# Pacotes necessários
library(tidyverse)
library(moments)   # para skewness e kurtosis

# Ler dataset
dados <- read.csv("../resources/data.csv")

# Selecionar só a coluna Popularidade
pop <- dados$popularity

# --- TABELA DE FREQUÊNCIA ---
# Definir classes (aqui fiz faixas de 0-20, 20-40... até o máx do dataset)
breaks <- seq(0, max(pop), by=100)
tabela_freq <- table(cut(pop, breaks = breaks, right = FALSE))
tabela_freq_rel <- prop.table(tabela_freq) * 100

pop_log <- log1p(pop)

# Definir faixas de 0,1,2,... até o teto do valor máximo
breaks <- seq(0, ceiling(max(pop_log)), by = 1)

# Tabela de frequência usando essas faixas
tabela_freq <- table(cut(pop_log, breaks = breaks, right = FALSE))
tabela_freq_rel <- prop.table(tabela_freq) * 100

tabela_pop <- data.frame(
  Faixa = names(tabela_freq),
  Frequencia = as.numeric(tabela_freq),
  Percentual = round(as.numeric(tabela_freq_rel), 2)
)

print("Tabela de Frequência - Popularidade (faixas inteiras)")
print(tabela_pop)


# --- MEDIDAS DE RESUMO (usando log1p) ---
media <- mean(pop_log)
mediana <- median(pop_log)
moda <- as.numeric(names(sort(table(round(pop_log, 1)), decreasing = TRUE)[1])) # aproximação
desvio_padrao <- sd(pop_log)
variancia <- var(pop_log)
amplitude <- max(pop_log) - min(pop_log)
quartis <- quantile(pop_log, probs = c(0.25, 0.5, 0.75))
assimetria <- skewness(pop_log)
curtose <- kurtosis(pop_log)

resumo_pop <- list(
  media = media,
  mediana = mediana,
  moda = moda,
  desvio_padrao = desvio_padrao,
  variancia = variancia,
  amplitude = amplitude,
  quartis = quartis,
  assimetria = assimetria,
  curtose = curtose
)

print("Medidas resumo - Popularidade (log1p)")
print(resumo_pop)
