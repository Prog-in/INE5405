# Define diretório
setwd("/home/rafael/Documents/teste")

# Lê os dados
data <- read.csv("top10K-TMDB-movies.csv")

# Ordena pelos maiores votos médios e seleciona os 100 melhores
top100 <- data[order(-data$vote_average), ][1:100, ]

# Cria a variável de década a partir do ano de lançamento
# Supondo que a coluna se chame "release_date"
top100$release_year <- as.numeric(substr(top100$release_date, 1, 4))
top100$decade <- floor(top100$release_year / 10) * 10

# Calcula frequências por década
freq_decada <- table(top100$decade)

# Gera gráfico de barras
barplot(freq_decada,
        col = "steelblue",
        border = "black",
        main = "",
        xlab = "Década",
        ylab = "Número de Filmes",
        cex.main = 1.5, # Tamanho do título (antes: 1.5)
        cex.lab = 1.4,  # Tamanho dos rótulos dos eixos (antes: 1.2)
        cex.axis = 1.4)
