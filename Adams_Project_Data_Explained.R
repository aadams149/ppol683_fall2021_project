#Since I'm loading many of my data sources from the web, rather than
#files bundled with the app code, I decided it made sense to include this
#script. The code here loads in all required data sets and lists the 
#transformations or modifications made to each.

library(tidyverse)
library(vroom)

# COVID-19 Case/Death Data --------------------------------------------

#Data Source: NY Times COVID-19 GitHub
covid <-
  vroom(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  ) %>%
  # Pivot and rename the data
  pivot_longer('cases':'deaths',
               names_to = 'metric',
               values_to = 'n')

#The `vroom` package from the tidyverse makes it possible to read in large
#files very quickly. From there, I apply the same transformations used in 
#previous versions of this app; convert cases and deaths to `metric` and 
#values to `n`. The other columns are "fips" (my primary key across 
#different data sets), county name, and date.

# Public Health Department Social Media Data --------------------------


# Read in my project data from my project GitHub
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
  ) 

#Relevant columns:
#   -fips: primary key
#   -name: county name
#   -state: state abbreviation
#   -state_full: full state name
#   -place: [name, state_full] (used for map labels)
#   -twitterYN: Is there a twitter for this county's health department
#   -facebookYN: Is there a facebook for the department
#   -twitter_active_COVID: was twitter active post-declaration of COVID as a pandemic
#   -twitter_active_last60: was twitter active w/in last 60 days (defined relative to 2021-12-01)
#    (the most recent day I scraped the accounts)
#   -date: the date of the most recent retrievable tweet (note: not all accounts had
#    retrievable tweets)
#   -tweet: the content of the tweet
#   -link: the link to the tweet
#   -likes_count/retweets_count/replies_count: # of that kind of engagement metric on the retrieved tweet
#   -socmed: used for map labeling, constructed from twitterYN and facebookYN


# Race Data -----------------------------------------------------------

#Source: U.S. Census Bureau, 2020 U.S. Census

racedata <-
  vroom('data/raw/county_race_data.csv') %>%
  select(id,
         Total_Hispanic:Total_Multiracial) %>%
  mutate(fips = 
           str_sub(id,-5,-1)) %>%
  select(!id)

#Relevant columns:
#   -fips: primary key
#   -Total_[Racial Group]: # of people identifying as that racial group in the
#    county

#I divide each racial column by the population variable listed below to
#generate a proportion, which is the main racial attribute used in my code.


# Population Data -----------------------------------------------------

#Source: Provided in class/U.S. Census Bureau, 2020 U.S. Census

population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

#Relevant columns:
#   -fips: primary key
#   -STNAME: state name (made redundant by state_full above)
#   -population: county population as of the 2020 census


# Vaccination Data ----------------------------------------------------

#Source: Centers for Disease Control and Prevention

#Read in vaccination data from CDC API
#I'm only incorporating the most recent vax numbers in this project
#This data set updates each day, same as the COVID-19 one from the NYTimes
#Rows are imported in reverse chronological order
#I use the system date to check if there are at least 3143 rows where the date
#is today (the number of counties in the US). If so, subset the data to 
#rows from today. Otherwise, subtract 1 from the date and subset to that date
#(yesterday). This way, no matter when the app is run, it should have 
#1 vaccine data observation for each county that is no more than 24 hours old
vax_data <-
  vroom('https://data.cdc.gov/resource/8xkx-amqh.csv?$limit=7000')

today <- Sys.Date()

if (length(vax_data[vax_data$date == today,]) >= 3143){
  vax_data = vax_data %>%
    filter(date == today)
}else{
  vax_data = vax_data %>%
    filter(date == today-1)
}

vax_data <-
  vax_data %>%
  select(fips,
         date,
         series_complete_pop_pct,
         administered_dose1_pop_pct
  )

#Relevant columns:
#   -fips: primary key
#   -date: the date of the data
#   -series_complete_pop_pct: the percent of the county which has received
#   a full vaccination sequence
#   -administered_dose1_pop_pct: the percent of the county which has received
#   at least one COVID-19 vaccine dose

#There are other variables included in this data set which break down rates
#and proportions by different age groups. I'm not sure if I'll have time to
#fully incorporate that into my project, especially since I don't have 
#age data for any other metrics. 

#I'm considering implementing more than the most recent day's worth of data
#for this set of variables, but there are limitations to what I can 
#retrieve using the API like this.


# Shapefile -----------------------------------------------------------

#Shapefile of U.S. counties. Original CRS is 5070 (NAD83)

us_counties <-
  st_read('data/spatial/us_counties.shp') %>%
  #Rename GEOID to fips for clarity
  rename(fips = GEOID) %>%
  #Transform to 4326 so it plays nice w/ tmap
  st_transform(crs = 4326) %>%
  #Maybe this left join is unnecessary.
  left_join(population_counties,
            by = 'fips')

#GEOID is equivalent to fips, and renaming it makes matching easier. 