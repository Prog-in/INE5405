setwd("/home/rafael/Documents/teste")
data <- read.csv("top10K-TMDB-movies.csv")
# Create data 
popularity <- data$vote_average
hist(popularity, xlab = "Média de Votos",
     col = "gray", border = "black",     main = paste(""),ylab = "Número de Filmes",
     cex.main = 2.0, # Tamanho do título 
     cex.lab = 1.5,  # Tamanho dos rótulos 
     cex.axis = 1.8) # Tamanho dos números



