library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)


setwd('Rda')
load(edx.Rda)
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

#Determine mu, the average movie rating
mu = edx %>% summarise(mu = mean(rating)) %>% .[[1]]

#Determine b_m, the movie effect
b_m = edx %>% group_by(movieId) %>% summarise(bm = mean(rating) - mu)

#Determine b_u, the user effect
b_u = edx %>% group_by(userId) %>% summarise(bu = mean(rating) - mu)

#Add columns to validation set and find expected rating.
edx = left_join(edx, b_u, by = 'userId')
edx = left_join(edx, b_m, by = 'movieId')

#creates data frame of user-genre effect 

for (i in 1:length(genrelist)){
  temp = edx %>% filter(str_detect(edx$genre, genrelist[i])) %>% group_by(userId) %>% summarise(thisgenre = mean(rating))
  colnames(temp) = c('userId', genrelist[i])
  temp = left_join(b_u, temp, by = 'userId')
  temp2 = temp[i+2] - temp[2] - mu  #user-genre_effect = avg_genre_rating - bu - mu
  b_u = bind_cols(b_u, temp2)
}
rm(temp, temp2)
save(b_u)
save(b_m)
save(mu)
setwd('..')