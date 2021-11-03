library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

load(edx.Rda)
load(b_u.Rda)
load(b_m.Rda)
load(mu.Rda)
load(validation.Rda)




calcGE = function(g, u){
  user = b_u %>% filter(userId == u)
  genreeff = 0
  for (i in 1:length(genrelist)){
    if(str_detect(g, genrelist[i]) & !is.na(user[[1,i+2]])){
      genreeff = genreeff + genreweight * user[[1,i+2]]
    }
  }
  genreeff
}
temp = mapply(calcGE, edx$genres, edx$userId) %>% enframe() %>% select('value')  
colnames(temp) = 'bg'
edx = bind_cols(edx, temp)
timebin = 1000
timespan = diff(range(edx$timestamp))
span = timespan/timebin
fit = loess(rating ~ timestamp, span = span, data = edx)
#temp = predict(fit, timestamp)
# 
# train = train %>% mutate(pred = mu + bu + bm) %>% mutate(pred = ifelse(pred < .5, .5, ifelse(pred > 5, 5, pred))) %>% 
#   mutate(pred = round(pred*2,0)/2)
# 
# train %>% mutate(sqer = (pred - rating)*(pred-rating)) %>% summarise(msqe = mean(sqer)) %>% sqrt()
