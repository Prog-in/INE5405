if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)

movies <- read_csv("../resources/data.csv")

movies$word_count <- str_count(movies$title, "\\S+")

category_levels <- c(
  "1", "2", "3", "4", 
  "5", "6", "7 ou +"
)

movies$word_count_class <- case_when(
  movies$word_count == 1 ~ "1",
  movies$word_count == 2 ~ "2",
  movies$word_count == 3 ~ "3",
  movies$word_count == 4 ~ "4",
  movies$word_count == 5 ~ "5",
  movies$word_count == 6 ~ "6",
  movies$word_count >= 7 ~ "7 ou +"
)

movies$word_count_class <- factor(movies$word_count_class, levels = category_levels)

df_words <- movies %>%
  count(word_count_class)

lollipop_chart <- ggplot(df_words, aes(x = word_count_class, y = n)) +
  geom_segment(aes(x = word_count_class, xend = word_count_class, y = 0, yend = n),
               linewidth = 1, color = "grey30") +
  geom_point(size = 5, color = "grey30") +
  geom_text(aes(label = n), vjust = -0.5, size = 7) +
  theme_minimal(base_size = 20) +
  labs(
    x = "Número de Palavras",
    y = "Número de Filmes"
  )


print(lollipop_chart)
ggsave("titulo_contagem_palavras.png", plot = lollipop_chart, width = 11, height = 8.5)
