library(tidyverse)
library(vroom)

# Read in previous county data ----------------------------------------

counties = vroom::vroom('data/raw/counties_with_tweets.csv') %>%
  mutate(fips = 
           as.character(fips))


# Find most recent file of scraped tweets and read that in ------------

input_stem <- 'data/raw/most_recent_tweets/'
input_name <- 'most_recent_tweets_'

## Look for all files in the inputs folder that start with Primary_Scraping_input
list_of_input_files <- list.files(path = input_stem,pattern = input_name)

## Strip everything but the dates in the file names, and store the date as a vector of type date, sorted from greatest to least
input_file_dates <- substr(list_of_input_files,20,29) %>% as.Date(format="%Y-%m-%d") %>% sort(decreasing = TRUE)

## Take the first item in the sorted vector(The most recent input file date) and convert back to a string
latest_date <- input_file_dates[1] %>% as.character(format="%Y-%m-%d")

## Combine the resulting date as a string with the rest of the path for the input csv. 
input_path <- paste0(input_stem,input_name,latest_date,".csv") 

## Read in the csv 
tweets = vroom::vroom(input_path)


# Read in population data ---------------------------------------------

population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)


# Left join population data to county data ----------------------------

df = counties %>%
  left_join(
    population_counties,
    by = c('fips' = 'fips',
           'state_full' = 'STNAME')
  ) %>%
  select(fips:twitter,
         population,
         twitterYN:filename)


# EDA w/ tweets -------------------------------------------------------

#Note: Tweets were most recently scraped on 2021-12-10

tweets_per = 
  data.frame(table(tweets$username)) %>%
  rename('total_tweets' = 'Freq')

tweets_per_COVID = 
  data.frame(table(tweets[tweets$date >= '2020-03-10',]$username)) %>%
  rename('tweets_COVID' = 'Freq')

tweets_per_last60 = 
  data.frame(table(tweets[tweets$date >= '2021-10-10',]$username)) %>%
  rename('tweets_last60' = 'Freq')

tweet_counts = 
  tweets_per %>%
  left_join(
    tweets_per_COVID,
    by = 'Var1'
  ) %>%
  left_join(
    tweets_per_last60,
    by = 'Var1'
  ) 

all_accounts = counties %>%
  select(twitter) %>%
  left_join(
    tweet_counts,
    by = c('twitter' = 'Var1')
  ) %>%
  drop_na(
    twitter
  ) %>%
  distinct()

all_accounts[is.na(all_accounts)] <- 0

df <-
  df %>%
  left_join(
    all_accounts,
    by = 'twitter'
  )



# newdf = data.frame()
# for (ii in unique(tweets$username)){
#   print(ii)
#   rows = tweets %>%
#     filter(username == ii) %>%
#     filter(date == max(date)) %>%
#     filter(time == max(time))
#   newdf = rbind.data.frame(newdf, rows)
# }
# 
# newdf1 <-
#   newdf %>%
#   distinct(across(c(date,
#                     time,
#                     user_id,
#                     username,
#                     name,
#                     tweet)),
#            .keep_all = TRUE)

# counties$twitter = tolower(counties$twitter)
# 
# counties <-
#   counties %>%
#   select(fips:district)
# 
# counties = 
#   counties %>%
#   left_join(newdf1,
#             by = c('twitter' = 'username'))

#counties$date = as.character(counties$date)
counties$year = lubridate::year(counties$date)
counties$month = lubridate::month(counties$date) %>%
  str_pad(width = '2',pad = '0')
counties$day = lubridate::day(counties$date) %>%
  str_pad(width = '2',pad = '0')