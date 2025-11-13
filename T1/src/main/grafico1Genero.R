# ==============================================================================
# 1. Instalação e Carregamento (Igual ao anterior)
# ==============================================================================
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("UpSetR", quietly = TRUE)) install.packages("UpSetR")
library(readr)
library(UpSetR)

# ==============================================================================
# 2. Leitura e Preparação (Igual ao anterior)
# ==============================================================================
movies <- read_csv("../resources/data.csv")
movies$genre[is.na(movies$genre)] <- ""
all_genres <- strsplit(movies$genre, "\\s*,\\s*")
genre_vector <- trimws(unlist(all_genres))
unique_genres <- sort(unique(genre_vector))
unique_genres <- unique_genres[unique_genres != ""]

# ==============================================================================
# 3. Criação da Matriz (TOTAL)
# ==============================================================================
contains_df <- as.data.frame(sapply(unique_genres, function(g)
  sapply(all_genres, function(movie) as.integer(g %in% movie))
))
colnames(contains_df) <- unique_genres

# ==============================================================================
# 4. AGRUPAMENTO DOS GÊNEROS PEQUENOS (O TRUQUE)
# ==============================================================================
# Lista dos gêneros que você quer esconder da lista principal
genres_to_hide <- c("TV Movie", "Western", "Music", "War", "History")

# Criamos uma nova coluna "Outros"
# Se o filme tiver QUALQUER um dos gêneros acima, ele ganha 1 em "Outros"
contains_df$Outros <- as.integer(rowSums(contains_df[, genres_to_hide]) > 0)

# Removemos as colunas individuais dos gêneros pequenos
# Mantemos apenas os gêneros principais + a nova coluna "Outros"
cols_to_keep <- setdiff(colnames(contains_df), genres_to_hide)
contains_df_final <- contains_df[, cols_to_keep]

# ==============================================================================
# 5. Geração do Gráfico
# ==============================================================================
png("upset_final_agrupado.png", width = 2000, height = 1200, res = 150, type = "cairo")

upset(
  contains_df_final,
  nsets = ncol(contains_df_final), # Usa todas as colunas (incluindo 'Outros')
  nintersects = 20,
  order.by = "freq",
  text.scale = 2.3,
  mb.ratio = c(0.5, 0.5),
  main.bar.color = "grey30",
  sets.bar.color = "grey30",
  keep.order = FALSE
)

dev.off()