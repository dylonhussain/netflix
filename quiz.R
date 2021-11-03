##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx %>% filter(rating == 3) %>% nrow()

edx %>% select(userId) %>% distinct()

genres = c('Drama', 'Comedy', 'Thriller', 'Romance')

sapply(genres, function (g){
  edx %>% filter(str_detect(genres, g)) %>% count()
})


edx %>% filter(title == 'Forrest Gump (1994)'| title == 'Jurassic Park (1993)'|title == 'Pulp Fiction (1994)'
               |title == 'Shawshank Redemption, The (1994)'|title == 'Speed 2: Cruise Control (1997)')%>% group_by(title) %>% summarise(n())


edx %>% filter(str_detect(title, 'Forrest Gump') | str_detect(title, 'Jurassic Park') | str_detect(title,'Pulp Fiction') 
               | str_detect(title,'Shawshank Redemption, The') | str_detect(title,'Speed 2: Cruise Control'))%>% group_by(title) %>% summarise(n())
                
edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(n)


