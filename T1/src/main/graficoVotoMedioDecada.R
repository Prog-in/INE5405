# Define diretório
setwd("/home/rafael/Documents/teste")

# Lê os dados
data <- read.csv("top10K-TMDB-movies.csv")

# Extrai ano e década
data$release_year <- as.numeric(substr(data$release_date, 1, 4))
data$decade <- floor(data$release_year / 10) * 10

# Remove linhas sem ano válido
data <- data[!is.na(data$decade), ]

# Calcula a média do voto médio por década
avg_decada <- aggregate(vote_average ~ decade, data = data, mean)

# Gera gráfico de barras com fontes maiores
bp <- barplot(avg_decada$vote_average,
              names.arg = avg_decada$decade,
              col = "steelblue",
              border = "black",
              main = "",
              xlab = "Década",
              ylab = "Voto Médio",
              ylim = c(0, max(avg_decada$vote_average) + 1),
              cex.main = 2.0, # Tamanho do título (antes: 1.5)
              cex.lab = 1.7,  # Tamanho dos rótulos dos eixos (antes: 1.2)
              cex.axis = 1.6) # Tamanho dos números nos eixos (antes: 1.1)

# Adiciona os valores exatos acima das barras com fonte maior
text(x = bp,
     y = avg_decada$vote_average,
     labels = round(avg_decada$vote_average, 2),
     pos = 3,
     cex = 1.3, # Tamanho dos valores sobre as barras (antes: 1.5)
     col = "black")