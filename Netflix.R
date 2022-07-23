library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

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

setwd('Rda')
edx2 = readRDS('edx.Rda')
##################################DONT RUN The comment out code, kills RAM. used to get all genres.###########################
# genresplit = edx %>% select(genres) %>% separate(genres, into = as.character(1:8), sep = '\\|')  
# colnames(genresplit) = c('1', '1', '1', '1', '1', '1', '1', '1')

#Verified that no rows in edx have >8genres
#distinct(genresplit[,8])

# genrelist = as.vector(genresplit[,1])
# for (i in 2:8){
#       print(i)
#       genrelist = bind_rows(genrelist, genresplit[, ..i])
# }
# > distinct(genrelist)
# 1
# 1:             Comedy
# 2:             Action
# 3:           Children
# 4:          Adventure
# 5:          Animation
# 6:              Drama
# 7:              Crime
# 8:             Sci-Fi
# 9:             Horror
# 10:           Thriller
# 11:          Film-Noir
# 12:            Mystery
# 13:            Western
# 14:        Documentary
# 15:            Romance
# 16:            Fantasy
# 17:            Musical
# 18:                War
# 19:               IMAX
# 20: (no genres listed)
# 21:               <NA>
#   1
########################################################^^output above vvresulting variable below######################
genrelist = c('Comedy', 'Action','Children','Adventure','Animation','Drama','Crime','Sci-Fi','Horror',
              'Thriller',
              'Film-Noir',
              'Mystery',
              'Western',
              'Documentary',
              'Romance',
              'Fantasy',
              'Musical',
              'War',
              'IMAX')
saveRDS(genrelist, file = 'genrelist.Rda')

# #Determine mu, the average movie rating
# mu = edx2 %>% summarise(mu = mean(rating)) %>% .[[1]]
# saveRDS(mu, file = 'mu.Rda')
# #Determine b_m, the movie effect
# b_m = edx2 %>% group_by(movieId) %>% summarise(bm = mean(rating) - mu)
# saveRDS(b_m, file = 'b_m.Rda')
# #Determine b_u, the user effect
# b_u = edx2 %>% group_by(userId) %>% summarise(bu = mean(rating) - mu)
# saveRDS(b_u, file = 'b_u.Rda')
# #Add columns to validation set and find expected rating.
# edx2 = left_join(edx2, b_u, by = 'userId')
# edx2 = left_join(edx2, b_m, by = 'movieId')

# #Determine mu, the average movie rating but corrects for previous prediction
mu = edx2 %>% summarise(mu = mean(rating)) %>% .[[1]]
saveRDS(mu, file = 'mu.Rda')
#Determine b_m, the movie effect
b_m = edx2 %>% mutate(rating = rating - mu) %>% group_by(movieId)  %>% summarise(bm = mean(rating))
edx2 = left_join(edx2, b_m, by = 'movieId')
#Determine b_u, the user effect
b_u = edx2 %>% mutate(rating = rating - mu - bm) %>% group_by(userId) %>% summarise(bu = mean(rating))
edx2 = left_join(edx2, b_u, by = 'userId')
#Add columns to validation set and find expected rating.



#creates data frame of user-genre effect 

# for (i in 1:length(genrelist)){
#   temp = edx2 %>% filter(str_detect(edx2$genre, genrelist[i])) %>% group_by(userId) %>% 
#                                                 summarise(thisgenre = mean(rating))
#   colnames(temp) = c('userId', genrelist[i])
#   temp = left_join(b_u, temp, by = 'userId')
#   temp2 = temp[i+2] - temp[2] - mu  #user-genre_effect = avg_genre_rating - bu - mu
#   b_u = bind_cols(b_u, temp2)
# }
# rm(temp, temp2)
#### corrected user genre effect
for (i in 1:length(genrelist)){
  temp = edx2 %>% mutate(rating = rating - mu - bm - bu) %>%
    filter(str_detect(edx2$genre, genrelist[i])) %>% group_by(userId) %>% 
    summarise(thisgenre = mean(rating))
  colnames(temp) = c('userId', genrelist[i])
  temp = left_join(b_u, temp, by = 'userId')
  b_u = bind_cols(b_u, temp[, i + 2])
}
rm(temp)


#finds all genres in observation and calculates user-genre effect b_g
calcGE = function(g, u){
  user = b_u %>% filter(userId == u)
  genreeff = 0
  genrecount = 0
  for (i in 1:length(genrelist)){
    if(str_detect(g, genrelist[i]) & !is.na(user[[1,i+2]])){
      genrecount = genrecount + 1
      genreeff = genreeff + user[[1,i+2]]
    }
  }
  if(genrecount == 0){
    genreeff = 0
  }
  c(genreeff, genrecount)
}

#binds b_g to edx
temp = mapply(calcGE, edx2$genres, edx2$userId)   
b_g = data.frame(bg = temp[1,], gcount = temp[2,])
edx2 = bind_cols(edx2, b_g)


#round timestamp to date
edx2 = edx2 %>% mutate(date = floor_date(as_datetime(timestamp), unit = 'day')) 
#saveRDS(edx2, file = 'edx2.Rda')
saveRDS(edx2, file = 'edx2corrected.Rda')
setwd('..')

setwd('Rda')
edx2 = readRDS('edx2.Rda')
#b_u = readRDS('b_u.Rda')
#b_m = readRDS('b_m.Rda')
mu = readRDS('mu.Rda')
#validation = readRDS('validation.Rda')
#genrelist = readRDS('genrelist.Rda')

fold = createFolds(edx2$rating, k = 10) 
weight = 1
power = 1
#################################Power Tune
powerrmse = function(x){
  rmse = 0
  for (i in c(1:10)) {
    folderr = edx2[as.vector(fold[[i]]),] %>% 
      mutate(bg = ifelse(gcount != 0, bg/(gcount)^x, 0)) %>%
      mutate(errsq = (rating - mu - bu - bm - weight * bg)^2) %>% 
      select(errsq) %>% sum()
    rmse = rmse + (folderr/length(fold[[i]]))^(.5)
  }
  rmse = rmse/10
  return(rmse)
}

tuneval = c(30:45)/100
tuneresults = sapply(tuneval, powerrmse)
exptune = data.frame(power = tuneval, rmse = tuneresults)
plot(exptune$power, exptune$rmse)
power = exptune %>% top_n(-1, rmse) %>% .$power

#########################################Weight Tune
weightrmse = function(y){
  rmse = 0
  for (i in c(1:10)) {
    folderr = edx2[as.vector(fold[[i]]),] %>% 
      mutate(bg = ifelse(gcount != 0, bg/(gcount)^power, 0)) %>%
      mutate(errsq = (rating - mu - bu - bm - y*bg)^2) %>% 
      select(errsq) %>% sum()
    rmse = rmse + (folderr/length(fold[[i]]))^(.5)
  }
  rmse = rmse/10
  return(rmse)
}

tuneval = c(8:12)/10
tuneresults = sapply(tuneval, weightrmse)
weighttune = data.frame(weight = tuneval, rmse = tuneresults)
plot(weighttune$weight, weighttune$rmse)

edx3 = edx2 %>% mutate(bg = ifelse(gcount != 0, bg/(gcount)^ 0.41, 0))  #.41 came from optimizing powerrmse
edx3 = edx3 %>% mutate(err = rating - mu - bu - bm - bg)


#time effect on 10 folds
#                           
# timefit =  edx3 %>% mutate(date = as.numeric(as.Date(date))) %>% 
#   group_by(date) %>% summarise(err = mean(err)) %>% 
#   loess(.$err ~ .$date, data =., 
#         span = 2, control=loess.control(surface="direct"))
  

#############tunes span based on error
foldcount = 10
spanerr = data.frame()
fold = createFolds(edx3$rating, k = foldcount)
for (i in seq(.5, 5, .5)){
  rmse = 0
  timefit =  edx3 %>% mutate(date = as.numeric(as.Date(date))) %>% 
    group_by(date) %>% summarise(err = mean(err)) %>% 
    loess(.$err ~ .$date, data =., 
          span = i, control=loess.control(surface="direct"))
  for(j in c(1:foldcount)){
    thisfold = edx3[as.vector(fold[[j]]),]
    temp = thisfold %>% mutate(bt = predict(timefit,as.numeric(as.Date(date)))) %>% 
      mutate(rhat = mu + bu + bm + bg + bt) %>%
      mutate(errsq = (rhat - rating)^2) %>% select(errsq) %>% sum()
    temp = sqrt(temp/nrow(thisfold))
    rmse= rmse +temp
  }
  rmse = rmse/foldcount
  temp = data.frame(i, rmse)
  spanerr = rbind(spanerr, temp)
  saveRDS(spanerr, file = 'currentspanerr.Rda')
  print(i)
}




plot(timefit$x, timefit$fitted)
plot(spanerr)


span = 5
timefit =  edx3 %>% mutate(date = as.numeric(as.Date(date))) %>% 
  group_by(date) %>% summarise(err = mean(err)) %>% 
  loess(.$err ~ .$date, data =., 
        span = span, control=loess.control(surface="direct"))
saveRDS(timefit, file = 'timefit.Rda')

edx3 = edx3 %>% mutate(bt = predict(timefit,as.numeric(as.Date(date)))) %>% 
  mutate(rhat = mu + bu + bm + bg + bt) 
saveRDS(edx3, file = 'edx3.Rda')


#####for knning rating ~rhat
for (p in seq(.004, .006, .001)){
  p = .005
  small = createDataPartition(edx3$rating, times = 1, p = p)
  small = as.vector(small$Resample1)
  small = edx3[small,] %>% select(rating, rhat)
  start = Sys.time()
  knnfit = train(rating ~ rhat, data = small, method = 'knn')
  duration = as.numeric(Sys.time() - start) *60
  knnduration = rbind(knnduration, data.frame(p, duration))
}
saveRDS(knnduration, file = 'knnduration.Rda')
plot(knnduration)

prediction = data.frame(prediction = predict(knnfit, newdata = small))
small = bind_cols(small, prediction)

small %>% mutate(hit = prediction == rating) %>% summarise(mean(hit))

#alternative to knn
view = edx3 %>% mutate(prediction = round(2 * rhat,0)) %>% 
  mutate(prediction = ifelse(prediction <0, 0, prediction)) %>%
  mutate(prediction = ifelse(prediction > 10, 10, prediction)) %>%
  mutate(prediction = prediction/2) %>% mutate(hit = prediction == rating) %>%
  select(rating, rhat, prediction, hit)
edx3 %>% mutate(hit = prediction == rating) %>% summarise(mean(hit))
edx3 %>% mutate(errsq = (as.numeric(rating) - rhat)^2) %>% summarise(mean(errsq))
# last = slice_max(edx3, order_by = prediction, n = 1)
# first = slice_min(edx3, order_by = prediction, n = 1)
# edxsmall3 = bind_rows(edxsmall3, last)
# edxsmall3 = bind_rows(edxsmall3, first)
# edxsmall3$rating = factor(edxsmall3$rating)
#edxsmall3 = edxsmall3 %>% select(rating, prediction)

#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#knnfit = train(rating ~ ., data = edxsmall3, method = 'knn', trControl=trctrl,
#               preProcess = c("center", "scale"), tuneLength = 10)
knnfit = train(rating ~ rhat, data = edx3, method = 'knn')
saveRDS(knnfit, file = 'knnfit.Rda')
final = data.frame(predict(knnfit, newdata = edx3))
colnames(final)= 'final'
edxsmall3 = bind_cols(edx3, final)

edx3 %>% mutate(hit = (final == rating)) %>% summarise(accuracy = mean(hit))
saveRDS(knnfit, file = 'knnfit.Rda')

fold = createFolds(edx3$pred, k = 10)

start = 2
end = 6
number = subset(edx3, select = c(rating, bu, bm, bg, bt)) %>% mutate(mu = mu)
number = number[,c(1,6,2,3,4,5)] %>% mutate(error = as.numeric(rating))
errors = data.frame(errsq = double())
for (j in c(start:end)){
  number[,'error'] = number[,'error'] - number[,..j]
  rmse = 0
  for (i in c(1:10)) {

    folderr = number[as.vector(fold[[i]]),] %>% mutate(errsq = error*error) %>%
      summarise(errsq = sum(errsq)) %>% .[[1]]
    rmse = rmse + (folderr/length(fold[[i]]))^(.5)
    #temp = temp %>% mutate(hit = (rating == predict(knnfit, pred))) %>% summarise(mean(hit))
  }
  rmse = rmse/10
  errors[nrow(errors)+1,]=rmse
}
rownames(errors) = c('mu', 'bu', 'bm', 'bg', 'bt')
  
#date %>% ggplot(aes(date, rating)) + geom_point(size = 1) + geom_line(data = fitdf, size = 2, color = 'blue') + 
#  scale_y_log10()

# 
# foldindx = createFolds(y = edx$rating, k = 50)
# fold = edx[as.vector(foldindx[[i]]),]

# span = timespan/timebin

# 
# train = train %>% mutate(pred = mu + bu + bm) %>% mutate(pred = ifelse(pred < .5, .5, ifelse(pred > 5, 5, pred))) %>% 
#   mutate(pred = round(pred*2,0)/2)
# 
# train %>% mutate(sqer = (pred - rating)*(pred-rating)) %>% summarise(msqe = mean(sqer)) %>% sqrt()
