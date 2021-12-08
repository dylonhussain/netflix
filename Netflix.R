library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(parallel)
library(MASS)

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
