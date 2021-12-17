#This script generates a dataframe of social media data for 
#multicounty health districts which can be connected to the main, county-level
#data set using rbind

#Load libraries
library(sf)
library(tidyverse)
library(vroom)

#Some of the necessary info is contained in the shapefile
us_counties <-
  st_read('data/spatial/counties_with_mc_districts_small.shp') %>%
  #Rename GEOID to fips for clarity
  rename(fips = GEOID) %>%
  #Transform to 4326 so it plays nice w/ tmap
  st_transform(crs = 4326)

#Read in social media data
tw_fb_data <-
  vroom(
    'https://raw.githubusercontent.com/aadams149/ppol683_fall2021_project/main/data/raw/counties_with_tweets.csv'
  ) %>%
  #Change the fips column to character so it plays nice with the other data sets
  mutate(fips =
           as.character(fips)) %>%
  #Add leading zeroes where necessary
  mutate(
    fips =
      str_pad(fips,
              5,
              pad = '0'),
    #Create a column with county names and state abbreviations together
    #(This is just for aesthetic benefit later.)
    place =
      paste0(name, ", ", state_full),
    #Create a column recording the presence of a facebook and/or twitter
    #(I ran into some issues trying to get tmap/leaflet to play nice
    #with multiple polygon sets, so this ended up being an adequate 
    #alternative solution)
    socmed = case_when(
      facebookYN == 1 & twitterYN == 1 ~ "Both Facebook and Twitter",
      facebookYN == 0 &
        twitterYN == 0 ~ "No Facebook or Twitter",
      facebookYN == 1 &
        twitterYN == 0 ~ "Facebook but no Twitter",
      facebookYN == 0 &
        twitterYN == 1 ~ "Twitter but no Facebook"
    )
  ) %>%
  rename('tweet_date' = date)

#Read in pop
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

#Swap out a bogus population column (only done on first use)
tw_fb_data <-
  tw_fb_data %>%
  select(!population) %>%
  left_join(population_counties,
            by = c('fips',
                   'state_full' = 'STNAME')) %>%
  select(
    fips:twitter,
    population,
    twitterYN:socmed
  )

#Create new data frame
district_data = data.frame()
#Loop over district names
for(ii in unique(tw_fb_data$district)) {
  #If district is not NA,
  if (!is.na(ii)) {
    #Subset data to just counties in district
    df_subset <-
      tw_fb_data %>%
      filter(district == ii)
    
    #Create new row w/ same columns as original data frame
    newrow <-
      data.frame(
        fips = us_counties[us_counties$NAME == ii, ]$fips,
        name = ii,
        state = us_counties[us_counties$NAME == ii, ]$STNAME,
        state_full = us_counties[us_counties$NAME == ii, ]$STNAME,
        twitter = unique(df_subset$twitter)[1],
        population = sum(df_subset$population),
        twitterYN = mean(df_subset$twitterYN),
        twitterNotes = NA,
        twitter_active_COVID = mean(df_subset$twitter_active_COVID),
        twitter_active_last60 = mean(df_subset$twitter_active_last60),
        facebookYN = mean(df_subset$facebookYN),
        facebookNotes = NA,
        notes = NA,
        district = ii,
        id = df_subset$id[1],
        conversation_id = df_subset$conversation_id[1],
        created_at = df_subset$created_at[1],
        tweet_date = df_subset$tweet_date[1],
        time = df_subset$time[1],
        timezone = df_subset$timezone[1],
        user_id = df_subset$user_id[1],
        twitter_display_name = df_subset$twitter_display_name[1],
        place = us_counties[us_counties$NAME == ii, ]$name_1,
        tweet = df_subset$tweet[1],
        language = df_subset$language[1],
        mentions = df_subset$mentions[1],
        urls = df_subset$urls[1],
        photos = df_subset$photos[1],
        replies_count = df_subset$replies_count[1],
        retweets_count = df_subset$retweets_count[1],
        likes_count = df_subset$likes_count[1],
        hashtags = df_subset$hashtags[1],
        cashtags = df_subset$cashtags[1],
        link = df_subset$link[1],
        retweet = df_subset$retweet[1],
        quote_url = df_subset$quote_url[1],
        video = df_subset$video[1],
        thumbnail = df_subset$thumbnail[1],
        near = df_subset$near[1],
        geo = df_subset$geo[1],
        source = df_subset$source[1],
        user_rt_id = df_subset$user_rt_id[1],
        user_rt = df_subset$user_rt[1],
        retweet_id = df_subset$retweet_id[1],
        reply_to = df_subset$reply_to[1],
        retweet_date = df_subset$retweet_date[1],
        translate = df_subset$translate[1],
        trans_src = df_subset$trans_src[1],
        trans_dest = df_subset$trans_dest[1],
        filename = df_subset$filename[1],
        total_tweets = df_subset$total_tweets[1],
        tweets_COVID = df_subset$tweets_COVID[1],
        tweets_last60 = df_subset$tweets_last60[1],
        mean_likes = df_subset$mean_likes[1],
        mean_retweets = df_subset$mean_retweets[1],
        mean_replies = df_subset$mean_replies[1],
        socmed = df_subset$socmed[1]
      )
    district_data <-
      rbind.data.frame(district_data,
                       newrow)
  }
}

#Round a couple of columns
district_data <-
  district_data %>%
  mutate(twitterYN = 
           round(twitterYN),
         facebookYN = 
           round(facebookYN))

#Export as csv
#write_csv(district_data,
#         'data/raw/mc_district_socmed_data.csv')

# write_csv(tw_fb_data,
#           'data/raw/counties_with_tweets.csv')
