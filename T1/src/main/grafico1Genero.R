if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

if (!requireNamespace("UpSetR", quietly = TRUE)) {
  install.packages("UpSetR")
}
library(UpSetR)


movies <- read_csv("../resources/data.csv")

# limpar NAs e separar por vírgula (ignora espaços)
movies$genre[is.na(movies$genre)] <- ""
all_genres <- strsplit(movies$genre, "\\s*,\\s*")

# vetor de gêneros únicos
genre_vector <- trimws(unlist(all_genres))
unique_genres <- sort(unique(genre_vector))

# matriz binária (cada coluna = 1 se o filme tem o gênero)
genre_matrix <- sapply(unique_genres, function(g) sapply(all_genres, function(movie) as.integer(g %in% movie)))
genre_matrix <- as.data.frame(genre_matrix)

# caminho do arquivo de saída
outfile <- "upset_genres.png"
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

# abrir dispositivo PNG (use type='cairo' se estiver em headless e quiser melhor render)
png(filename = outfile, width = 2000, height = 1200, res = 150, type = "cairo")
suppressWarnings(
  upset(genre_matrix,
        nsets = 10,
        nintersects = 10,
        order.by = "freq",
        text.scale = 2.5,
        mb.ratio = c(0.5, 0.5),
        main.bar.color = "grey30",
        sets.bar.color = "grey30")
)
dev.off()

