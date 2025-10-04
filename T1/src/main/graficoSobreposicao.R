# Define diretório
setwd("/home/rafael/Documents/teste")

# Lê os dados
data <- read.csv("top10K-TMDB-movies.csv")

# Extrai ano e década
data$release_year <- as.numeric(substr(data$release_date, 1, 4))
data$decade <- floor(data$release_year / 10) * 10
data <- data[!is.na(data$decade), ]

# Frequência por década
count_decada <- table(data$decade)

# Média de votos por década
avg_decada <- aggregate(vote_average ~ decade, data = data, mean)

# Cria gráfico de barras (quantidade de filmes)
bp <- barplot(count_decada,
              col = "steelblue",
              border = "black",
              main = "",
              xlab = "Década",
              ylab = "Número de Filmes",
              cex.main = 1.7, # Tamanho do título (antes: 1.5)
              cex.lab = 1.5,  # Tamanho dos rótulos dos eixos (antes: 1.2)
              cex.axis = 1.5)

# Ajusta eixo secundário para média de votos
par(new = TRUE)
plot(bp, avg_decada$vote_average, type = "o", pch = 16, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,10))

# Eixo da direita (para votos)
axis(side = 4, at = seq(0, 10, 1))
mtext("Média de Votos", side = 4, line = 3, col = "red")

# Legenda
legend("topleft",
       legend = c("Quantidade de Filmes", "Média de Votos"),
       fill = c("steelblue", NA),
       border = c("black", NA),
       lty = c(NA, 1),
       pch = c(NA, 16),
       col = c("black", "red"),
       bty = "n")
