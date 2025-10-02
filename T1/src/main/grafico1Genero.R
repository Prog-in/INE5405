if (!require(UpSetR)) install.packages("UpSetR")

movies <- read.csv("../resources/data.csv")

# Split genres into list
all_genres <- strsplit(movies$genre, "\\,")

# Flatten into one vector
genre_vector <- unlist(all_genres)

# Get unique genres
unique_genres <- unique(genre_vector)

# Build binary matrix
genre_matrix <- sapply(unique_genres, function(g) sapply(all_genres,
    function(movie) as.integer(g %in% movie)))
genre_matrix <- as.data.frame(genre_matrix)

# UpSet plot (shows intersections)
upset(genre_matrix,
      nsets = 10,
      nintersects = 10,
      order.by = "freq",
      text.scale = 2.5,
      mb.ratio = c(0.5, 0.5),
      main.bar.color = "grey30",
      sets.bar.color = "grey30")

