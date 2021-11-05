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
#added to wrangling
#edx2 = edx2 %>% mutate(date = floor_date(as_datetime(timestamp), unit = 'day'))
date = edx2 %>% mutate(rating = rating - mu) %>% group_by(date) %>% summarise(rating = mean(rating))

#calculates the fits for span from .1 to 10
fitdf = data.frame(row.names = c('timestamp', 'rating', 'span'))
for (i in c(1:10)){
  span = i/10
  fit = loess(rating ~ timestamp, data = edx2, span = span)
  temp = bind_cols(as.data.frame(fit$x), as.data.frame(fit$fitted))
  colnames(temp) = c('timestamp', 'rating')
  temp = temp %>% mutate(rating = rating - mu, span = span)
  fitdf = bind_rows(fitdf, temp)
}
#function to get time effect
getbt = function (timestamp, span){
  temp = fitdf %>% filter(span == span, timestamp == timestamp)
  temp[1]$rating
}

date %>% ggplot(aes(date, rating)) + geom_point(size = 1) + geom_line(data = fitdf, size = 2, color = 'blue') + 
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
