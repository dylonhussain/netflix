library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

setwd('Rda')
edx = readRDS('edx.Rda')
b_u = readRDS('b_u.Rda')
b_m = readRDS('b_m.Rda')
mu = readRDS('mu.Rda')
validation = readRDS('validation.Rda')
genrelist = readRDS('genrelist.Rda')

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
  }else{
    genreeff = genreeff / genrecount
  }
  genreeff
}

#binds b_g to edx
  temp = mapply(calcGE, fold$genres, fold$userId) %>% enframe() %>% select('value')  
  colnames(temp) = 'bg'
  fold = bind_cols(fold, temp)
  fold = fold %>% mutate(pred = mu + bg + bu + bm)
# 
# foldindx = createFolds(y = edx$rating, k = 50)
# fold = edx[as.vector(foldindx[[i]]),]
# timebin = 1000
# timespan = diff(range(edx$timestamp))
# span = timespan/timebin
# fit = loess(rating ~ timestamp, span = span, data = edx)
#temp = predict(fit, timestamp)
# 
# train = train %>% mutate(pred = mu + bu + bm) %>% mutate(pred = ifelse(pred < .5, .5, ifelse(pred > 5, 5, pred))) %>% 
#   mutate(pred = round(pred*2,0)/2)
# 
# train %>% mutate(sqer = (pred - rating)*(pred-rating)) %>% summarise(msqe = mean(sqer)) %>% sqrt()
