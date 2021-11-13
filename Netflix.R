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

#Makes smaller subset of edx2 for time effect fitting
edxsmall = createDataPartition(edx2$rating, times = 1, p = .01)
edxsmall = as.vector(edxsmall$Resample1)
edxsmall = edx2[edxsmall,]
last = slice_max(edx2, order_by = timestamp, n = 1)
first = slice_min(edx2, order_by = timestamp, n = 1)
edxsmall = bind_rows(edxsmall, last)
edxsmall = bind_rows(edxsmall, first)


#########################################################################

#date = edx2 %>% mutate(rating = rating - mu) %>% group_by(date) %>% summarise(rating = mean(rating))


#################################Finds optimal span for smoothing = 3.3
# spanerr = data.frame()
# for (i in c(26:35)){
#   span = i/10
#   timefit = loess(rating ~ timestamp, data = edxsmall, span = span)
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
timefit = loess(rating ~ timestamp, data = edxsmall, span = span)
saveRDS(timefit, file = 'timefit.Rda')
edx3 = edx2 %>% mutate(bt = predict(timefit,timestamp) - mu) %>% mutate(pred = mu + bu + bm + bg + bt) 

edxsmall3 = createDataPartition(edx3$rating, times = 1, p = .1)
edxsmall3 = as.vector(edxsmall3$Resample1)
edxsmall3 = edx3[edxsmall3,]
last = slice_max(edx3, order_by = timestamp, n = 1)
first = slice_min(edx3, order_by = timestamp, n = 1)
edxsmall3 = bind_rows(edxsmall3, last)
edxsmall3 = bind_rows(edxsmall3, first)

knnfit = train(rating ~ pred, data = edxsmall3, method = 'knn')
saveRDS(knnfit, file = 'knnfit.Rda')
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
