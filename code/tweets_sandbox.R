library(tidyverse)

counties = readr::read_csv('data/raw/counties_with_tweets.csv')
tweets = readr::read_csv('data/most_recent_tweets/most_recent_tweets_2021_11_30.csv')

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

counties <-
  counties %>%
  select(fips:district)

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