library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

movie_data <- read.csv("movies.csv")
rating_data <- read.csv("ratings.csv")

str(movie_data)
str(rating_data)


summary(movie_data)
head(movie_data)

summary(rating_data)
head(rating_data)
View(movie_data)

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
head(movie_genre)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE)
View(movie_genre2)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
View(genre_mat1)
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}


genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) 
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
}
str(genre_mat2)

SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])

head(SearchMatrix)

ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

