library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

movies <- read_csv("../resources/data.csv")


movies$word_count <- str_count(movies$title, "\\S+")

mean_words <- mean(movies$word_count, na.rm = TRUE)

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


word_count_chart_horizontal <- ggplot(data = movies, aes(y = word_count_class, fill = word_count_class)) +
  geom_bar(show.legend = FALSE) +
  geom_text(
    stat = 'count', 
    aes(label = ..count..), 
    hjust = -0.2, 
    size = 3.5
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Distribuição de Filmes por Número de Palavras no Título",
    x = "Número de Filmes",
    y = "Número de Palavras"
  ) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5))

print(word_count_chart_horizontal)
ggsave("titulo_contagem_palavras.png", plot = word_count_chart_horizontal, width = 11, height = 7)