library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

setwd('Rda')
edx2 = readRDS('edx2.Rda')
b_u = readRDS('b_u.Rda')
b_m = readRDS('b_m.Rda')
mu = readRDS('mu.Rda')
validation = readRDS('validation.Rda')
genrelist = readRDS('genrelist.Rda')

#Makes smaller subset of edx2 for time effect fitting
edxsmall = createDataPartition(edx2$rating, times = 1, p = .01)
edxsmall = as.vector(edxsmall$Resample1)
edxsmall = edx2[edxsmall,]
edxsmall = bind_rows(edxsmall, slice_max(edx2, order_by = timestamp, n = 1))
edxsmall = bind_rows(edxsmall, slice_min(edx2, order_by = timestamp, n = 1))

rm(test)
#########################################################################

#date = edx2 %>% mutate(rating = rating - mu) %>% group_by(date) %>% summarise(rating = mean(rating))

#calculates the fits for span from .1 to 10
#######################Tidy######################################333
# fitdf = data.frame()
# 
# for (i in c(1:10)){
#   span = i/10
#   fit = loess(rating ~ timestamp, data = edx2, span = span)
#   temp = bind_cols(as.data.frame(fit$x), as.data.frame(fit$fitted))
#   colnames(temp) = c('timestamp', 'rating')
#   temp = temp %>% mutate(rating = rating - mu, span = span)
#   fitdf = bind_rows(fitdf, temp)
# }
# #function to get time effect
# getbt = function (timestamp, span){
#   temp = fitdf %>% filter(span == span, timestamp == timestamp)
#   temp[1]$rating
# }
##########################Tidy######################################
##################not Tidy###############
# 
# 
# first = TRUE
# for (i in c(1:10)){
#   span = i/10
#   fit = loess(rating ~ timestamp, data = edx2, span = span)
#   temp = bind_cols(as.data.frame(fit$x), as.data.frame(fit$fitted))
#   colnames(temp) = c('timestamp', as.character(span))
#   temp[,2] = temp[,2] - mu
#   if (first){
#     fitdf = temp
#   }else{
#     fitdf = left_join(fitdf, temp, by = 'timestamp')
#   }
#   first = FALSE
# }
# rownames(fitdf) = as.list(fitdf$timestamp)
# getbt = function(timestamp, span){
#   fitdf[]
# }
###############################Not Tidy$$####################333
spanerr = data.frame()
for (i in c(16:25)){
  span = i/10
  fit = loess(rating ~ timestamp, data = edxsmall, span = span)
  errsq = edx2 %>% mutate(bt = predict(fit,timestamp) - mu) %>% mutate(pred = mu + bu + bm + bg + bt) %>%
  mutate(errsq = (pred - rating)^2) %>% summarise(error = sum(errsq))
  temp = data.frame(span, errsq)
  spanerr = rbind(spanerr, temp) 
}
saveRDS(spanerr, file = 'spanerr1p6-2p5.Rda')
# plot(fit$x, fit$fitted)
# plot(spanerr)
#edx2 %>% mutate(bt = getbt(timestamp, span)) %>% mutate(pred = mu + bu + bm + bg + bt) %>% 
#  mutate(errsq = (pred - rating)^2) %>% summarise(result = sum(errsq))


#date %>% ggplot(aes(date, rating)) + geom_point(size = 1) + geom_line(data = fitdf, size = 2, color = 'blue') + 
  scale_y_log10()

# 
# foldindx = createFolds(y = edx$rating, k = 50)
# fold = edx[as.vector(foldindx[[i]]),]

# span = timespan/timebin

# 
# train = train %>% mutate(pred = mu + bu + bm) %>% mutate(pred = ifelse(pred < .5, .5, ifelse(pred > 5, 5, pred))) %>% 
#   mutate(pred = round(pred*2,0)/2)
# 
# train %>% mutate(sqer = (pred - rating)*(pred-rating)) %>% summarise(msqe = mean(sqer)) %>% sqrt()
