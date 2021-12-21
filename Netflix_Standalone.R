library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

#####################################################
#Helper code to save and load objects if code is
#is run in segments
#####################################################
saveRDS(b_m, file = 'b_m.Rda')
saveRDS(b_u, file = 'b_u.Rda')
saveRDS(edx, file = 'edx.Rda')
saveRDS(genrelist, file = 'genrelist.Rda')
saveRDS(mu, file = 'mu.Rda')
saveRDS(knnfit, file = 'knnfactorfit.Rda')
saveRDS(validation, file = 'validation.Rda')
edx = readRDS('edx.Rda')
mu = readRDS('mu.Rda')
b_m = readRDS('b_m.Rda')
b_u = readRDS('b_u.Rda')
b_g = readRDS('b_g.Rda')
genrelist = readRDS('genrelist.Rda')
knnfit = readRDS('knnfit.Rda')
validation = readRDS('validation.Rda')

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# 
# library(tidyverse)
# library(caret)
# library(data.table)

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
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
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

################################################
#End Copied Code Begin Dylon's Code
#To make prediction algorithm based on Netflix dataset
################################################


#Code to find all genres in dataset
ngenres = 9

#Splits genres into columns with one genre each
genresplit = edx %>% select(genres) %>% separate(genres, into = as.character(1:ngenres), sep = '\\|')

#Verified that all rows in edx have <ngenres of genres (returns a <NA> if true)
distinct(genresplit[,..ngenres])

#Adds all distinct genres to genrelist
genrelist = as.vector(distinct(genresplit[,1]))
for (i in 2:ngenres){
      i = 1
      temp = as.vector(distinct(genresplit[, ..i]))
      names(temp) = '1'
      genrelist = bind_rows(genrelist, temp)
}

#Removes all redundancies from genrelist
genrelist = distinct(genrelist)

# #Determine mu, the average movie rating 
mu = edx %>% summarise(mu = mean(rating)) %>% .[[1]]

#Determine b_m, the movie effect: mean deviation from prediction by user
b_m = edx %>% mutate(rating = rating - mu) %>% group_by(movieId)  %>% summarise(bm = mean(rating))
edx = left_join(edx, b_m, by = 'movieId')

#Determine b_u, the user effect: mean deviation from prediction by movie 
b_u = edx %>% mutate(rating = rating - mu - bm) %>% group_by(userId) %>% summarise(bu = mean(rating))
edx = left_join(edx, b_u, by = 'userId')

#creates data frame of user-genre effect: mean deviation from prediction 
# by genre-user combination
for (genre in unlist(genrelist)){
    temp = edx %>% mutate(rating = rating - mu - bm - bu) %>%
      filter(str_detect(edx$genre, genre)) %>% group_by(userId) %>%
      summarise(thisgenre = mean(rating))
    colnames(temp) = c('userId', paste(genre))
    b_u = left_join(b_u, temp, by = 'userId')
}

#finds all genres in observation and sums the individual
#genres effect to find user-genre effect and returns that and number of genres
calcGE = function(arg){
  thisuser = b_u %>% filter(userId == as.numeric(arg['userId']))
  genreeff = 0
  genrecount = 0
  for (genre in unlist(genrelist)){
    if(str_detect(arg['genres'], genre) & !is.na(thisuser[1,genre])){
      genrecount = genrecount + 1
      genreeff = genreeff + thisuser[1,genre]
    }
  }
  c(as.numeric(genreeff), as.numeric(genrecount))
}

#binds b_g and number of genres to edx
#############################WARNING!!!!!! Takes a VERY long time to run################
b_g = edx %>% select(genres, userId) %>% apply(1, calcGE) %>% t() %>% data.frame() 
colnames(b_g) = c('bg', 'genrecount')
edx = bind_cols(edx, b_g)

#round timestamp to date
edx = edx %>% mutate(date = floor_date(as_datetime(timestamp), unit = 'day')) 

#Power Tune
#Uses 10-fold cross validation to find
#expected RMSE for given power, x
powerrmse = function(x){
  rmse = 0
  for (i in c(1:10)) {
    
    #calculates sum of error squared
    folderr = edx[as.vector(fold[[i]]),] %>% 
      mutate(bg = ifelse(genrecount != 0, weight * bg/(genrecount)^x, 0)) %>%
      mutate(errsq = (as.numeric(rating) - mu - bu - bm - bg)^2) %>% 
      select(errsq) %>% sum()
    
    #divides sum of error squared by N and roots it for RMSE and adds to sum
    rmse = rmse + (folderr/length(fold[[i]]))^(.5)
  }
  
  #divides sum of RMSE by N to get mean RMSE across folds
  rmse = rmse/10
  return(rmse)
}

#Applies tuneval to powerrmse to determine optimal power
fold = createFolds(edx$rating, k = 10) 
weight = 1
power = 1
tuneval = seq(30, 45,1)/100
tuneresults = sapply(tuneval, powerrmse)
exptune = data.frame(power = tuneval, rmse = tuneresults)
plot(exptune$power, exptune$rmse)
#power = exptune %>% top_n(-1, rmse) %>% .$power
power = .41

#Weight Tune
#Uses 10-fold cross validation to find
#expected RMSE for given weight, y
weightrmse = function(y){
  rmse = 0
  for (i in c(1:10)) {
    
    #calculates sum of error squared
    folderr = edx[as.vector(fold[[i]]),] %>% 
      mutate(bg = ifelse(genrecount != 0, y*bg/(genrecount)^power, 0)) %>%
      mutate(errsq = (rating - mu - bu - bm - bg)^2) %>% 
      select(errsq) %>% sum()
    
    #divides sum of error squared by N and roots it for RMSE and adds to sum    
    rmse = rmse + (folderr/length(fold[[i]]))^(.5)
  }
  
  #divides sum of RMSE by N to get mean RMSE across folds
  rmse = rmse/10
  return(rmse)
}

#Applies tuneval to weightrmse to determine optimal weight
fold = createFolds(edx$rating, k = 10) 
tuneval = c(8:12)/10
tuneresults = sapply(tuneval, weightrmse)
weighttune = data.frame(weight = tuneval, rmse = tuneresults)
plot(weighttune$weight, weighttune$rmse)

#makes prediction rhat
edx = edx %>% mutate(rhat = mu+bm+bu+ifelse(genrecount != 0, bg/genrecount^power,0))


#######################################################################
#Code to determine optimal span for time effect
#Commented out because it takes forever to run and not used in final calculations
#######################################################################

#finds error from prediction with mu, bu, bm, and bg
#.41 came from optimizing powerrmse
# edx = edx %>% 
#   mutate(err = rating - mu - bu - bm - ifelse(genrecount != 0, bg/(genrecount)^ power, 0))

#Span Tune
#Uses 10-fold cross validation to find
#expected RMSE for given span, i
# foldcount = 10
# spanerr = data.frame()
# fold = createFolds(edx$rating, k = foldcount)
# 
# for (i in seq(.5, 5, .5)){
#   rmse = 0
#   
#   #makes timefit for given span
#   timefit =  edx %>% mutate(date = as.numeric(as.Date(date))) %>% 
#     group_by(date) %>% summarise(err = mean(err)) %>% 
#     loess(.$err ~ .$date, data =., 
#           span = i, control=loess.control(surface="direct"))
#   
#   #calculates RMSE for each fold and adds to sum
#   for(j in c(1:foldcount)){
#     thisfold = edx[as.vector(fold[[j]]),]
#     temp = thisfold %>% mutate(bt = predict(timefit,as.numeric(as.Date(date)))) %>% 
#       mutate(rhat = mu + bu + bm + ifelse(genrecount != 0, bg/(genrecount)^ 0.41, 0) + bt) %>%
#       mutate(errsq = (rhat - rating)^2) %>% select(errsq) %>% sum()
#     temp = sqrt(temp/nrow(thisfold))
#     rmse= rmse +temp
#   }
#   
#   #calculates mean rmse and saves to data frame
#   rmse = rmse/foldcount
#   temp = data.frame(i, rmse)
#   spanerr = rbind(spanerr, temp)
#   saveRDS(spanerr, file = 'spanerr.Rda')
# }
#    #creates time fit on the error and uses it to predict bt on edx
# span = 5
# timefit =  edx %>% mutate(date = as.numeric(as.Date(date))) %>% 
#   group_by(date) %>% summarise(err = mean(err)) %>% 
#   loess(.$err ~ .$date, data =., 
#         span = span, control=loess.control(surface="direct"))
# saveRDS(timefit, file = 'timefit.Rda')
# 
# edx = edx %>% mutate(bt = predict(timefit,as.numeric(as.Date(date)))) %>% 
#   mutate(rhat = mu + bu + bm + bg + bt) 

#######################################################################
#Code for making a knn model
#commented out because it is not used in calculations
#######################################################################


#for loop finding time required to make knnmodel for different portions of edx
# edx = edx %>%  mutate(rhat = mu + bu + bm + ifelse(genrecount != 0, bg/(genrecount)^ power, 0))
# for (p in seq(.004, .006, .001)){
#   p = .005
#   small = createDataPartition(edx$rating, times = 1, p = p)
#   small = as.vector(small$Resample1)
#   small = edx[small,] %>% select(rating, rhat)
#   start = Sys.time()
#   knnfit = train(rating ~ rhat, data = small, method = 'knn')
#   duration = as.numeric(Sys.time() - start) *60
#   knnduration = rbind(knnduration, data.frame(p, duration))
# }
# saveRDS(knnduration, file = 'knnduration.Rda')
# plot(knnduration)

#Creates knn model (knnfit) for .005 sample of edx
#RMSE 0.836469 for (3.984527 when rating is a factor)
#p = .005 so scrapped the knn idea
p = .005
small = createDataPartition(edx$rating, times = 1, p = p)
small = as.vector(small$Resample1)
small = edx[small,]  %>%
  select(rating, rhat)
knnfit = train(rating ~ rhat, data = small, method = 'knn')

#Uses knn model (knnfit) to make final prediction for edx
prediction = data.frame(rhat = predict(knnfit, newdata = edx))
edx = edx %>% select(-rhat)
edx = bind_cols(edx, prediction)

#Uses 10 fold validation to estimate the RMSE 
foldcount = 10
fold = createFolds(edx$rating, k = foldcount)
rmse = 0
for (i in c(1:foldcount)){
  thisfold = edx[as.vector(fold[[i]]),]
  errsum = thisfold %>% mutate(errsq = (as.numeric(rating) - as.numeric(rhat))^2) %>%
    select(errsq) %>% sum()
  rmse = rmse + sqrt(errsum/length(fold[[i]]))
}
rmse = rmse/foldcount

#############################################################
#End of training portion, begin validation
#############################################################

#Join columns needed for calc
b_u_small = b_u %>% select(userId, bu)
validation = left_join(validation, b_u_small, by = 'userId')
validation = left_join(validation, b_m, by = 'movieId')
b_g = validation %>% select(genres, userId) %>% apply(1, calcGE) %>% t() %>% data.frame() 
colnames(b_g) = c('bg', 'genrecount')
validation = bind_cols(validation, b_g)

#Sum of error squared
errsq = validation %>%
  mutate(rhat = mu+bm+bu+ifelse(genrecount != 0, bg/genrecount^power,0)) %>%
  mutate(errsq = (as.numeric(rating) - as.numeric(rhat))^2) %>%
    select(errsq) %>% sum()

#root mean
rmse = sqrt(errsq/nrow(validation))
# rmse = 0.8601549 WOOT!!!!!!