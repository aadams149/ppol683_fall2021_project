library(tidyverse)

counties = vroom('data/raw/counties_with_tweets.csv')
tweets = vroom('data/most_recent_tweets/most_recent_tweets_2021_11_30.csv')
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

df = counties %>%
  left_join(
    population_counties,
    by = c('fips' = 'fips',
           'state_full' = 'STNAME')
  ) %>%
  select(fips:twitter,
         population,
         twitterYN:filename)

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