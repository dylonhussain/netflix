library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

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



#Makes smaller subset of edx2 for time effect fitting
small = createDataPartition(edx3$rating, times = 1, p = .1)
small = as.vector(small$Resample1)
small = edx3[small,]
last = slice_max(edx3, order_by = timestamp, n = 1)
first = slice_min(edx3, order_by = timestamp, n = 1)
small = bind_rows(small, last)
small = bind_rows(small, first)
small = small %>% mutate(err = rating - mu - bu - bm - bg)
#############tunes span based on error
spanerr = data.frame()
fold = createFolds(small$rating, k = 10)
for (i in c(15:45)){
  span = i/10
  rmse = 0
  for(j in c(1:10)){
    thisfold = small[as.vector(fold[[j]]),]
    thisfold = bind_rows(thisfold, last)
    thisfold = bind_rows(thisfold, first)
    timefit2 = loess(err ~ timestamp, data = thisfold, span = span)
    rmse = thisfold %>% mutate(bt = predict(timefit2,timestamp)) %>% 
      mutate(rhat = mu + bu + bm + bg + bt) %>%
      mutate(errsq = (rhat - rating)^2) %>% select(errsq) %>% sum()
    rmse = sqrt(rmse/nrow(thisfold))
    rmse= rmse +temp
  }
  rmse = rmse/10
  temp = data.frame(span, rmse)
  spanerr = rbind(spanerr, temp)
  
}
saveRDS(spanerr, file = 'spanerr2 2p5-3p5.Rda')
plot(timefit$x, timefit$fitted)
plot(spanerr)

#################################Finds optimal span for smoothing = 3.3
# spanerr = data.frame()
# for (i in c(26:35)){
#   span = i/10
#   timefit = loess(rating ~ timestamp, data = edxsmall2, span = span)
#   errsq = edx2 %>% mutate(bt = predict(timefit,timestamp) - mu) %>% mutate(pred = mu + bu + bm + bg + bt) %>%
#   mutate(errsq = (pred - rating)^2) %>% summarise(error = sum(errsq))
#   temp = data.frame(span, errsq)
#   spanerr = rbind(spanerr, temp) 
# }
# saveRDS(spanerr, file = 'spanerr2p6-3p5.Rda')
# plot(timefit$x, timefit$fitted)
# plot(spanerr)
##############################################################
span = 3.3
timefit = loess(rating ~ timestamp, data = edxsmall2, span = span)
saveRDS(timefit, file = 'timefit.Rda')

edx3 = edx2 %>% mutate(bt = predict(timefit,timestamp) - mu) %>% 
  mutate(prediction = mu + bu + bm + bg + bt) 

edxsmall3 = createDataPartition(edx3$rating, times = 1, p = .001)
edxsmall3 = as.vector(edxsmall3$Resample1)
edxsmall3 = edx3[edxsmall3,]
last = slice_max(edx3, order_by = prediction, n = 1)
first = slice_min(edx3, order_by = prediction, n = 1)
edxsmall3 = bind_rows(edxsmall3, last)
edxsmall3 = bind_rows(edxsmall3, first)
edxsmall3$rating = factor(edxsmall3$rating)
#edxsmall3 = edxsmall3 %>% select(rating, prediction)

#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#knnfit = train(rating ~ ., data = edxsmall3, method = 'knn', trControl=trctrl,
#               preProcess = c("center", "scale"), tuneLength = 10)
knnfit = train(rating ~ prediction, data = edxsmall3, method = 'knn')
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
