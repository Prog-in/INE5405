library(tidyverse)
library(lubridate)

# Ler dataset
dados <- read.csv("../resources/data.csv")

# Extrair o ano de release_date
dados$release_date <- as.Date(dados$release_date, format = "%Y-%m-%d")
dados$ano <- year(dados$release_date)

# --- TABELA DE FREQUÊNCIA POR DÉCADA ---
dados$decada <- floor(dados$ano / 10) * 10
tabela_freq <- table(dados$decada)
tabela_freq_rel <- prop.table(tabela_freq) * 100

tabela_ano <- data.frame(
  Decada = names(tabela_freq),
  Frequencia = as.numeric(tabela_freq),
  Percentual = round(as.numeric(tabela_freq_rel), 2)
)

print("Tabela de Frequência - Ano de Lançamento")
print(tabela_ano)

# --- MEDIDAS DE RESUMO ---
media <- mean(dados$ano, na.rm = TRUE)
mediana <- median(dados$ano, na.rm = TRUE)
moda <- as.numeric(names(sort(table(dados$ano), decreasing = TRUE)[1]))
desvio_padrao <- sd(dados$ano, na.rm = TRUE)
variancia <- var(dados$ano, na.rm = TRUE)
amplitude <- max(dados$ano, na.rm = TRUE) - min(dados$ano, na.rm = TRUE)
quartis <- quantile(dados$ano, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

resumo_ano <- list(
  media = media,
  mediana = mediana,
  moda = moda,
  desvio_padrao = desvio_padrao,
  variancia = variancia,
  amplitude = amplitude,
  quartis = quartis
)

print("Medidas resumo - Ano de Lançamento")
print(resumo_ano)
