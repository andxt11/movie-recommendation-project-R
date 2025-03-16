install.packages("recommenderlab")
install.packages("ggplot2")
install.packages("data.table")
install.packages("reshape2")
install.packages("readxl")
library(recommenderlab) #for recommendation
library(ggplot2) #visualization
library(data.table)
library(readxl)
library(reshape2)
#Retrieving the data
movie_data <- read.csv("C:/Users/movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("C:/Users/ratings.csv")

#Structure
str(movie_data)
str(rating_data)

#summary statistics
summary(movie_data)
head(movie_data)
summary(rating_data)
head(rating_data)

#tabular view
data.table(movie_data)
data.table(rating_data)

#Data preprocessing
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1],"[|]", type.convert = TRUE), stringsAsFactors = FALSE) #DataFlair

colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
                "Sci-Fi", "Thriller", "war", "western")

genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
   gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
   genre_mat1[index+1, gen_col] <- 1
  }
  
}

genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors= FALSE)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)


#Create a search matrix that gives us films based on genres 

SearchMovie <- cbind(movie_data[,1:2], genre_mat2[])

head(SearchMovie)

#how many movies have several genres
#let's create sparse matrix for recommendation 

ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE) #basically our sparse matrix 
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
recommendation_model$IBCF_realRatingMatrix$parameters
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")
movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings

#Exploring the most viewed movies in the dataset 

#Counting the number of views in a film and arranging in descending order. 

library(ggplot2)
movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (index in 1:10325) {
  subset_result <- subset(movie_data, movie_data$movieId == table_views[index, 1])
  if (nrow(subset_result) > 0) {
    table_views[index, 3] <- as.character(subset_result$title)
  } else {
    table_views[index, 3] <- NA  # or any other value you want to assign for missing titles
  }
}
table_views[1:6,]
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size= 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")

#Heatmap of movie ratings 

image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

#selecting useful data

movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

#Delineate the matrix
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#Visualize the distribution of average ratings per user

average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

#Data Normalization 

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

#Performing Data Binarization 

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

#Collaborative Filtering System 

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#Building the Recommendation System 

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                             method = "IBCF",
                             parameter = list(k = 30))
recommen_model
class(recommen_model)

model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]

#Calculating RMSE (Root Mean Squared Error)

# Predict ratings for testing data
predicted_ratings <- predict(object = recommen_model, newdata = testing_data)

# Extract actual ratings from testing data
actual_ratings <- as.matrix(testing_data@data)

# Calculate squared errors
squared_errors <- (as(predicted_ratings, "matrix") - actual_ratings)^2

# Compute RMSE
rmse <- sqrt(mean(squared_errors, na.rm = TRUE))
print(paste("RMSE:", rmse))


