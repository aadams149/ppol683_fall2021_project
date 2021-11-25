setwd("C:/Users/alexi/Desktop/PPOL683/final_project/data")

library(tidyverse)

counties = readr::read_csv('counties.csv')
tweets = readr::read_csv('most_recent_tweets.csv')

newdf = data.frame()
for (ii in unique(tweets$username)){
  print(ii)
  rows = tweets %>%
    filter(username == ii) %>%
    filter(date == max(date)) %>%
    filter(time == max(time))
  newdf = rbind.data.frame(newdf, rows)
}

newdf1 <-
  newdf %>%
  distinct(across(c(date,
                    time,
                    user_id,
                    username,
                    name,
                    tweet)),
           .keep_all = TRUE)

newdf1 = 
  newdf1 %>%
  select(user_id,
         username,
         tweet,
         date,
         time,
         link)

counties$twitter = tolower(counties$twitter)

counties = 
  counties %>%
  left_join(newdf1,
            by = c('twitter' = 'username'))

#counties$date = as.character(counties$date)
counties$year = lubridate::year(counties$date)
counties$month = lubridate::month(counties$date) %>%
  str_pad(width = '2',pad = '0')
counties$day = lubridate::day(counties$date) %>%
  str_pad(width = '2',pad = '0')

write_csv(counties, 'counties_with_tweets.csv')