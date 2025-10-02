library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

movies <- read_csv("/home/lucas/INE5405/T1/src/resources/data.csv") # Está com o caminho absoluto pois estava dando algum bug na minha maquina

movies$word_count <- str_count(movies$title, "\\S+")

category_levels <- c(
  "1 palavra", "2 palavras", "3 palavras", "4 palavras", 
  "5 palavras", "6 palavras", "7 palavras ou +"
)

movies$word_count_class <- case_when(
  movies$word_count == 1 ~ "1 palavra",
  movies$word_count == 2 ~ "2 palavras",
  movies$word_count == 3 ~ "3 palavras",
  movies$word_count == 4 ~ "4 palavras",
  movies$word_count == 5 ~ "5 palavras",
  movies$word_count == 6 ~ "6 palavras",
  movies$word_count >= 7 ~ "7 palavras ou +"
)

movies$word_count_class <- factor(movies$word_count_class, levels = category_levels)

df_words <- movies %>%
  count(word_count_class)

lollipop_chart <- ggplot(df_words, aes(x = n, y = word_count_class)) +
  geom_segment(aes(x = 0, xend = n, y = word_count_class, yend = word_count_class),
               linewidth = 1, color = "grey30") +
  geom_point(size = 5, color = "grey30") +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  theme_minimal(base_size = 16) +
  labs(
    x = "Número de Filmes",
    y = "Número de Palavras"
  )

print(lollipop_chart)
ggsave("titulo_contagem_palavras.png", plot = lollipop_chart, width = 11, height = 7)
