library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(purr)

setwd('Rda')
edx2 = readRDS('edx2.Rda')
#b_u = readRDS('b_u.Rda')
#b_m = readRDS('b_m.Rda')
mu = readRDS('mu.Rda')
#validation = readRDS('validation.Rda')
#genrelist = readRDS('genrelist.Rda')

#Makes smaller subset of edx2 for time effect fitting
edxsmall2 = createDataPartition(edx2$rating, times = 1, p = .01)
edxsmall2 = as.vector(edxsmall2$Resample1)
edxsmall2 = edx2[edxsmall2,]
last = slice_max(edx2, order_by = timestamp, n = 1)
first = slice_min(edx2, order_by = timestamp, n = 1)
edxsmall2 = bind_rows(edxsmall2, last)
edxsmall2 = bind_rows(edxsmall2, first)


#########################################################################

#date = edx2 %>% mutate(rating = rating - mu) %>% group_by(date) %>% summarise(rating = mean(rating))


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
