## server.R
library(gsubfn)
library(tidyverse)
library(dplyr)
library(recommenderlab)

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  dat$MovieID = paste0('m', dat$MovieID)
  
  return(dat)
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = as.data.frame(ratings)
ratings = ratings[, !(names(ratings)) %in% c("Timestamp")]
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
grouped_genres = unique(movies$Genres)
all_genres = strsplit(grouped_genres, "\\|")
unique_genres = sort(unique(unlist(all_genres, recursive = FALSE)))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# Create columns with split genres
movies = movies %>% separate(Genres, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "genre_6"), sep = "\\|")

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
train = Rmat[1:1000, ]

# Train the model
recommender.UBCF <- Recommender(train, method = "UBCF",
                                parameter = list(normalize = 'center', 
                                                 method = 'Cosine', 
                                                 nn = 20))

# Get all movies that contain the genre inputted by the user
get_movies_in_genre = function(genre) {
  #filter dataset on genre in any of the genre columns
  movies_with_selected_genre = movies %>% filter(genre_1 == genre | genre_2 == genre | genre_3 == genre | genre_4 == genre | genre_5 == genre | genre_6 == genre)
  
  return(movies_with_selected_genre)
}

# Gets movie recommendation depending if user selects by popularity or by highest ratings
get_most_popular_or_ratings = function(movies_with_selected_genre, movie_rating_criteria) {
  
  # Join our movies that pertain to the selected genre to our ratings dataset
  joined_data = inner_join(movies_with_selected_genre, ratings, by="MovieID")
  
  if(movie_rating_criteria == "rating") {
    # Get all the mean reviews for each movies as well as total reviews
    grouped_data = joined_data %>%
      group_by(MovieID, Title, image_url) %>%
      dplyr::summarize(Ratings_Mean = mean(Rating, na.rm=TRUE), Count_of_Reviews = sum(Rating, na.rm=TRUE))
    
    # Order movies with highest average ratings
    ordered_by_rating <- grouped_data[with(grouped_data,order(-Ratings_Mean)),]
    # Remove movies with less than 1000 total ratings
    ordered_by_rating <- ordered_by_rating %>% filter(Count_of_Reviews > 1000)
    # Select movies with top 10 average ratings
    top_10 <- ordered_by_rating[1:10,]
    
  } else if(movie_rating_criteria == "popular") {
    # Get all the mean reviews for each movies as well as total reviews
    grouped_data = joined_data %>%
      group_by(MovieID, Title, image_url) %>%
      dplyr::summarize(Ratings_Mean = mean(Rating, na.rm=TRUE), Count_of_Reviews = sum(Rating, na.rm=TRUE))
    
    # Order movies by number of reviews
    ordered_by_popularity <- grouped_data[with(grouped_data,order(-Count_of_Reviews)),]
    # Select movies with highest number of reviews
    top_10 <- ordered_by_popularity[1:10,]
  }
  
  return(top_10)
}

shinyServer(function(input, output, session) {
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  output$genre_list <- renderUI({
    selectInput("genre", "Genre:",
                unique_genres)
  })
  
  output$top5 <- renderUI({
    radioButtons("top10", "Top 10:",
                 c("Highest Rating" = "rating",
                   "Most Popular" = "popular"))
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df_top10_popular_rating <- eventReactive(input$top10_popular_ratings_btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # Get top 10 movies based on user selected popularity or ratings
      top_10_movies <- get_most_popular_or_ratings(get_movies_in_genre(input$genre), input$top10)
      
      user_predicted_ids = 1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = top_10_movies$MovieID[user_predicted_ids], 
                                  Title = top_10_movies$Title[user_predicted_ids],
                                  image_url = top_10_movies$image_url[user_predicted_ids]) 
    }) # still busy
    
  }) # clicked on button
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # If user didnt rate any movies return empty list
      if(dim(user_ratings)[1] == 0) {
        return(user_ratings)
      }
      
      #create dumby user
      test_i = 9999
      # List of user rated MovieIDs
      test_j_with_m = user_ratings$MovieID
      # List of user ratings
      test_x = user_ratings$Rating
      # List of user rated MovieIDs without "m" at the beginning
      test_j = sub('.', '', test_j_with_m)
      
      # Creating duplicate ratings df to modify
      test_ratings = as.data.frame(ratings)
      
      # Create df with userID, movies, and ratings
      test_tmp = data.frame(UserID = test_i, MovieID = test_j, Rating = test_x)
      
      # Bind our df to the duplicate ratings df 
      test_rbind = rbind(test_ratings, test_tmp)
      
      i = paste0('u', test_rbind$UserID)
      j = paste0('m', test_rbind$MovieID)
      x = test_rbind$Rating
      
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      # Create sparseMatrix
      test_Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(test_Rmat) = levels(tmp$i)
      colnames(test_Rmat) = levels(tmp$j)
      # Create realRatingMatrix
      test_Rmat = new('realRatingMatrix', data = test_Rmat)
      # Select our user inputted data
      new_user_rmat = test_Rmat[dim(test_Rmat)[1],]
      
      p.UBCF <- predict(recommender.UBCF, new_user_rmat, type="ratings")
      
      p.UBCF <- as.numeric(as(p.UBCF, "matrix"))
      
      # Get indexes of all movies in order of top similarities
      all_movie_ind = order(p.UBCF, decreasing = TRUE)
      # Get movieID from indices
      sorted_recommended_movies = colnames(test_Rmat)[all_movie_ind]
      
      # Remove any movie the user rated
      for(movie in test_j_with_m) {
        top_recommended_movies_no_user_ratings = sorted_recommended_movies[sorted_recommended_movies != movie]
        sorted_recommended_movies = top_recommended_movies_no_user_ratings
      }
      
      # Select top 10 movies
      top_recommended_movies = top_recommended_movies_no_user_ratings[1:10]
      
      # Remove "m" from each movie
      top_recommended_movies = sub('.', '', top_recommended_movies)
      
      top_10_movies = data.frame(matrix(ncol = 4, nrow = 0))
      
      # Join movie dataset to our list of movies to access all columns
      for(movie_id in top_recommended_movies){
        top_10_movies = rbind(top_10_movies, movies %>% filter(MovieID == movie_id))
      }

      user_predicted_ids = 1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = top_10_movies$MovieID[user_predicted_ids], 
                                  Title = top_10_movies$Title[user_predicted_ids],
                                  image_url = top_10_movies$image_url[user_predicted_ids])
    }) # still busy
    
  }) # clicked on button
  
  output$results_top10_popular_rating <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_top10_popular_rating()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    if(dim(recom_result)[1] == 0) {
      div(style = "text-align:center", strong("No movies rated. Please rate movies"))
    } else {
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", 
                  a(img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150))
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(recom_result$Title[(i - 1) * num_movies + j])
              )
              
          )        
        }))) # columns
      }) # rows
    }
    
  }) # renderUI function
  
}) # server function
