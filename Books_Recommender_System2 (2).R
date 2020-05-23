# install the following packages
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)
library(reshape2)
library(dplyr)



# read the dataset
b <-fread(file.choose(),header=TRUE, strip.white = T)

## ensure that file imported correctly
head(b)
b <- na.omit(b) # Remove rows with N/A
head(b)
class(b) # class is data.frame
str(b) # check structure of data

# remove users who have rated fewer than 3 books
b[, N := .N, .(UserID, ISBN)]
cat('Number of duplicate ratings: ', nrow(b[N > 1])) 
# Number of duplicate ratings:  172
b <- b[N == 1]

# remove users who rated fewer than 3 books
b[, N := .N, .(UserID)]
cat('Number of users who rated fewer than 3 books: ', uniqueN(b[N <= 2, UserID])) 
# Number of users who rated fewer than 3 books:  50166
b <- b[N > 2]
str(b)

# Select a subset of users 
#To reduce calculation times in this kernel, I select only a subset of users. (e.g., 20%)
set.seed(1)
user_fraction <- 0.2
users <- unique(b$UserID)
sample_users <- sample(users, round(user_fraction * length(users)))
cat('Number of ratings (before): ', nrow(b))
# Number of ratings (before):  338004

b <- b[UserID %in% sample_users]
cat('Number of ratings (after): ', nrow(b))
# Number of ratings (after):  69083

# distribution of ratings
b %>% 
  ggplot(aes(x = Rating, fill = factor(Rating))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)

# number of ratings per user
b %>% 
  group_by(UserID) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

# distribution of mean user ratings
b %>% 
  group_by(UserID) %>% 
  summarize(mean_user_rating = mean(Rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(fill = "cadetblue3", color = "grey20")


# Collaborative Filtering
dimension_names <- list(UserID = sort(unique(b$UserID)), book_id = sort(unique(b$ISBN)))
ratingmat <- spread(select(b, ISBN, UserID, Rating), ISBN, Rating) %>% select(-UserID)

ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]

dim(ratingmat) # Our Matrix has 6089 rows X 105229 columns

# Find similar users
current_user <- "254"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
head(selected_users, 40)

# pearson’s correlation to calculate similarity of ratings for the user
user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat[current_user,]) %>% filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat),rating=ratingmat["30711",]) %>% filter(!is.na(rating))
tmp<-merge(user1, user2, by="item")
tmp

cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs") # cor = -1

user2 <- data.frame(item = colnames(ratingmat), rating = ratingmat["15408", ]) %>% filter(!is.na(rating))
tmp <- merge(user1, user2, by="item")
tmp

cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs") # cor = 1


# reduce the influence of interindividual differences in mean ratings
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

# calculate the similarity of all others users with the current user
# and sort them according to the highest similarity.

similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40) # this tells uses user ids 80061, 132031 and 191707 have the highest similarity scores

# Visualizing similarities between users
sim_mat <- cor(t(rmat), use = 'pairwise.complete.obs')
random_users <- selected_users[1:20]
qgraph(sim_mat[c(current_user, random_users), c(current_user, random_users)], layout = "spring", vsize = 5, theme = "TeamFortress", labels = c(current_user, random_users))

# Step 2: Get predictions for other books
similar_users <- names(res[1:4])

similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), rating = c(t(as.data.frame(rmat[similar_users,])))) %>% filter(!is.na(rating))

current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[current_user,]) %>% filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

predictions %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


# Step 3: Recommend the best 5 predictions

#Given the results, we would sort the predictions with respect to their mean rating
#and recommend the highest rated books to our current user. 
#Please note that ratings are still normalized.

predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(5, wt = mean_rating) %>% 
  mutate(ISBN = as.numeric(as.character(item))) %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
# Top 5 predicted books are 261102214, 3518396315, 3518380796, 3548255582, 3936018022

# Using recommenderlab

# Creating a Sparse Matrix
ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
sparse_ratings <- na.omit(sparse_ratings) 
rm(ratingmat0)
gc()

# Covert to real Rating Matrix class
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings # 4094 x 47090 rating matrix of class ‘realRatingMatrix’ with 69083 ratings.

# Running the UBCF algorithm
model <- Recommender(real_ratings, method = "UBCF", param = list(method = "pearson", nn = 4))

# Making predictions 
prediction <- predict(model, real_ratings[current_user, ], type = "ratings")

# Looking at the best predictions for our current user
as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 

# Evaluating the predictions
#10-fold crossvalidation. The given parameter determines how many ratings are given to create 
#the predictions and in turn on how many predictions per user remain for the evaluation of the 
#prediction. In this case -1 means that the predictions are calculated from all but 1 ratings, 
#and performance is evaluated for 1 for each user.

scheme <- evaluationScheme(real_ratings[1:500,], method = "cross-validation", k = 10, given = -1, goodRating = 10)

# Compare all algorithms using a random set of users
algorithms <- list("random" = list(name = "RANDOM", param = NULL),
                  "UBCF_05" = list(name = "UBCF", param = list(nn = 5)),
                   "UBCF_10" = list(name = "UBCF", param = list(nn = 10)),
                   "UBCF_30" = list(name = "UBCF", param = list(nn = 30)),                   
                   "UBCF_50" = list(name = "UBCF", param = list(nn = 50))
)
# evaluate the alogrithms with the given scheme            
results <- evaluate(scheme, algorithms, type = "ratings")

# restructure results output into a visual view
tmp <- lapply(results, function(x) slot(x, "results"))
res <- tmp %>% 
  lapply(function(x) unlist(lapply(x, function(x) unlist(x@cm[ ,"RMSE"])))) %>% 
  as.data.frame() %>% 
  gather(key = "Algorithm", value = "RMSE")

res %>% 
  ggplot(aes(Algorithm, RMSE, fill = Algorithm)) +
  geom_bar(stat = "summary") + geom_errorbar(stat = "summary", width = 0.3, size = 0.8) +
  coord_cartesian(ylim = c(0.6, 5)) + guides(fill = FALSE)


# Server #

# reshape to books x user matrix 
#ratingmatrix <- sparseMatrix(books$Rating, books$UserID, books$ISBN) # book x user matrix
#ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
#dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$Rating <- renderUI({
    num_rows <- 10
    num_books <- 5 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", strong(b$ISBN)),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$ISBN), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
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
      
      # add user's ratings as first column to rating matrix
      rmat <- cbind(Ratings, sparse_ratings)
      
      # get the indices of which cells in the matrix should be predicted
      # predict all books the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      # run the ubcf-alogrithm
      res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
      
      # sort, organize, and return the results
      user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
      user_predicted_ids <- as.numeric(names(user_results))
      recom_results <- data.table(Rank = 1:20, 
                                  Book_id = user_predicted_ids, 
                                  Author = books$authors[user_predicted_ids], 
                                  Title = books$title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_books <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_books + j),
            
            div(style = "text-align:center", 
                a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]), 
                  target='blank', 
                  img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
            ),
            div(style = "text-align:center; color: #999999; font-size: 80%", 
                books$authors[recom_result$Book_id[(i - 1) * num_books + j]]
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(books$title[recom_result$Book_id[(i - 1) * num_books + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function

## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
#library(ShinyRatingInput)
library(shinyjs)

#source('functions/helpers.R')

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Book Recommender"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(includeCSS("books.css"),
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover books you might like",
                      br(),
                      tableOutput("results")
                    )
                  )
    )
  )
) 

# Run the application 
shinyApp
