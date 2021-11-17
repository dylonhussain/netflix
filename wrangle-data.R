library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(purrr)


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

#Determine mu, the average movie rating
mu = edx2 %>% summarise(mu = mean(rating)) %>% .[[1]]
saveRDS(mu, file = 'mu.Rda')
#Determine b_m, the movie effect
b_m = edx2 %>% group_by(movieId) %>% summarise(bm = mean(rating) - mu)
saveRDS(b_m, file = 'b_m.Rda')
#Determine b_u, the user effect
b_u = edx2 %>% group_by(userId) %>% summarise(bu = mean(rating) - mu)
saveRDS(b_u, file = 'b_u.Rda')
#Add columns to validation set and find expected rating.
edx2 = left_join(edx2, b_u, by = 'userId')
edx2 = left_join(edx2, b_m, by = 'movieId')

#creates data frame of user-genre effect 

for (i in 1:length(genrelist)){
  temp = edx2 %>% filter(str_detect(edx2$genre, genrelist[i])) %>% group_by(userId) %>% 
                                                summarise(thisgenre = mean(rating))
  colnames(temp) = c('userId', genrelist[i])
  temp = left_join(b_u, temp, by = 'userId')
  temp2 = temp[i+2] - temp[2] - mu  #user-genre_effect = avg_genre_rating - bu - mu
  b_u = bind_cols(b_u, temp2)
}
rm(temp, temp2)


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
b_g = data.frame(b_g = temp[1,], gcount = temp[2,])
edx2 = bind_cols(edx2, temp)

#round timestamp to date
edx2 = edx2 %>% mutate(date = floor_date(as_datetime(timestamp), unit = 'day')) 
saveRDS(edx2, file = 'edx2.Rda')
setwd('..')