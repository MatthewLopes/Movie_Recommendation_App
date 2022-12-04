## server.R

# load functions
# source('functions/cf_algorithm.R') # collaborative filtering
# source('functions/similarity_measures.R') # similarity measures

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

  # p.IBCF <- predict(recommender.IBCF, test, type="ratings")
  # p.IBCF <- as.numeric(as(p.IBCF, "matrix"))
  # 
  # # randomly sample for training data
  # 
  # user_movie_ratings = data.frame(levels(tmp$j), p.IBCF, stringsAsFactors = T)
  # colnames(user_movie_ratings) = c('MovieID', 'Rating')
  # user_movie_ratings_without_na = na.omit(user_movie_ratings)
  # 
  # sorted_pred = user_movie_ratings_without_na[order(user_movie_ratings_without_na$Rating, decreasing = TRUE),]
  # 
  # top10 = sorted_pred[1:10,]
  # 
  # top10$MovieID = as.numeric(substr(top10$MovieID, 2, nchar(top10['MovieID'])-1))
  # 
  # top10_joined_data = inner_join(top10, movies, by="MovieID")
  # 
  # print(top10_joined_data)
  # 
  # return(top10_joined_data)
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


movies = movies %>% separate(Genres, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "genre_6"), sep = "\\|")

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
train = Rmat[1:500, ]

recommender.IBCF <- Recommender(train, method = "IBCF",
                                parameter = list(normalize = 'center',
                                                 method = 'Cosine',
                                                 k = 30))

get_movies_in_genre = function(genre) {
  movies_with_selected_genre = movies %>% filter(genre_1 == genre | genre_2 == genre | genre_3 == genre | genre_4 == genre | genre_5 == genre | genre_6 == genre)
  
  i = paste0('u', ratings$UserID)
  j = paste0('m', ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  train = Rmat[1:500, ]

  recommender.IBCF <- Recommender(train, method = "IBCF",
                                  parameter = list(normalize = 'center',
                                                   method = 'Cosine',
                                                   k = 30))
  print(recommender.IBCF)
  
  return(movies_with_selected_genre)
}

get_most_popular_or_ratings = function(movies_with_selected_genre, movie_rating_criteria) {
  joined_data = inner_join(movies_with_selected_genre, ratings, by="MovieID")
  
  if(movie_rating_criteria == "rating") {
    grouped_data = joined_data %>%
      group_by(MovieID, Title, image_url) %>%
      dplyr::summarize(Ratings_Mean = mean(Rating, na.rm=TRUE), Count_of_Reviews = sum(Rating, na.rm=TRUE))
    
    ordered_by_rating <- grouped_data[with(grouped_data,order(-Ratings_Mean)),]
    ordered_by_rating <- ordered_by_rating %>% filter(Count_of_Reviews > 1000)
    top_10 <- ordered_by_rating[1:10,]
    
  } else if(movie_rating_criteria == "popular") {
    grouped_data = joined_data %>%
      group_by(MovieID, Title, image_url) %>%
      dplyr::summarize(Ratings_Mean = mean(Rating, na.rm=TRUE), Count_of_Reviews = sum(Rating, na.rm=TRUE))
    
    ordered_by_popularity <- grouped_data[with(grouped_data,order(-Count_of_Reviews)),]
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
      
      # get the user's rating data
      #value_list <- reactiveValuesToList(input)
      #user_ratings <- get_user_ratings(value_list)
      
      top_10_movies <- get_most_popular_or_ratings(get_movies_in_genre(input$genre), input$top10)
      
      #print(top_10_movies)
      
      user_predicted_ids = 1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = top_10_movies$MovieID[user_predicted_ids], 
                                  Title = top_10_movies$Title[user_predicted_ids],
                                  image_url = top_10_movies$image_url[user_predicted_ids]) 
                                  #Predicted_rating =  user_results)
      
      #user_results = (1:10)/10
      #user_predicted_ids = 1:10
      #recom_results <- data.table(Rank = 1:10, 
                                  #MovieID = movies$MovieID[user_predicted_ids], 
                                  #Title = movies$Title[user_predicted_ids], 
                                  #Predicted_rating =  user_results)
      
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
      
      print(user_ratings)
      
      #To convert test to right format
      # get vector
      p.IBCF <- predict(recommender.IBCF, test, type="ratings")
      p.IBCF <- as.numeric(as(p.IBCF, "matrix"))
      
      # Make prediciton here
      
      
      user_results = (1:10)/10
      user_predicted_ids = 1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  output$results_top10_popular_rating <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_top10_popular_rating()
    
    print(recom_result)
    
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
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
