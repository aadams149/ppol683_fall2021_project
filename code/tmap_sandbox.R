covid <-
  vroom(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  ) 

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
  ) %>%
  rename('tweet_date' = date)

racedata <-
  vroom('data/raw/county_race_data.csv') %>%
  select(id,
         Total_Hispanic:Total_Multiracial) %>%
  mutate(fips = 
           str_sub(id,-5,-1)) %>%
  select(!id)

#Read in county population data
#Yes, this is the same file we used in class, I'm just excluding the 
#state totals instead of filtering to only them
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

#Read in vaccination data from CDC API
#I'm incorporating vaccine data from the past two months into this app.
#(I've tested different lengths of time, and this is the most I can get
#without it taking an absurdly long time to download.)

#61 days * 3143 counties is 191723 rows of data
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


#Drop rows where date is min date, since there might not be an observation
#for all 3143 counties
vax_data <-
  vax_data %>%
  select(date,
         fips,
         series_complete_yes,
         administered_dose1_recip)

#Left join into covid data, treat as metric
covid <-
  covid %>%
  left_join(
    vax_data,
    by = c('fips' = 'fips',
           'date' = 'date')
  ) %>%
  select(
    date:deaths,
    series_complete_yes,
    administered_dose1_recip
  ) %>%
  # Pivot and rename the data
  pivot_longer('cases':'administered_dose1_recip',
               names_to = 'metric',
               values_to = 'n')

# Read in shapefiles:

#Side note: thank you for providing the U.S. county shapefile!
#I was going to go try and track one down, so thank you for saving me
#the trouble
us_counties <-
  st_read('data/spatial/counties_with_mc_districts.shp') %>%
  #Rename GEOID to fips for clarity
  rename(fips = GEOID) %>%
  #Transform to 4326 so it plays nice w/ tmap
  st_transform(crs = 4326) 

#Define rescaling function
range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#Set tmap mode
tmap_mode('view')

#Test case plotting w/ multi-county districts
covid_filtered <-
  covid %>%
  filter(metric == 'cases',
         date >= '2021-11-01',
         date <= '2021-12-01') %>%
  select(!metric)


covid_adjusted <-
  covid_filtered %>%
  left_join(population_counties,
            by = c('fips',
                   'state' = 'STNAME')) %>%
  mutate(n =
           n / population * 100)


summary_data <-
  covid_adjusted %>%
  #Left join with social media data
  left_join(tw_fb_data,
            by = 'fips') %>%
  #Select certain columns and rename where necessary
  select(fips,
         state_full,
         name,
         place,
         Twitter = twitterYN,
         Facebook = facebookYN,
         socmed,
         date,
         n = n,
         district
  ) %>%
  #Add in population data
  left_join(population_counties,
            by = c('fips' = 'fips',
                   'state_full' = 'STNAME')) %>%
  left_join(vax_data,
            by = c('fips',
                   'date' = 'date')) %>%
  rename('Fully Vaccinated' = series_complete_yes,
         'First Dose' = administered_dose1_recip) %>%
  #Remove cases where data is recorded for units other than county
  filter(!is.na(fips)) %>%
  left_join(
    racedata,
    by = 'fips'
  ) %>%
  mutate(
    hispanic_prop = 
      Total_Hispanic/population,
    white_prop =
      Total_White/population,
    black_prop = 
      Total_Black/population,
    AmInd_prop = 
      Total_AmIndian/population,
    asian_prop = 
      Total_Asian/population,
    nhawaiian_prop = 
      Total_NativeHawaiian/population,
    other_prop = 
      Total_Other/population,
    multi_prop = 
      Total_Multiracial/population
  ) %>%
  select(!c(Total_Hispanic,
            Total_White,
            Total_Black,
            Total_Asian,
            Total_AmIndian,
            Total_NativeHawaiian,
            Total_Other,
            Total_Multiracial))

district_df_full = data.frame()
district_fips = us_counties %>%
  filter(is_dstr == 1) %>%
  select(distrct,
         fips)
for (ii in unique(summary_data$district)) {
  district_df = data.frame()
  row_subset <-
    summary_data %>%
    filter(district == ii)
  
  row_subset$date =
    as.character(row_subset$date)
  
   for (date in unique(row_subset$date)) {
    newrow <-
      data.frame(
        fips = district_fips[district_fips$distrct == ii,]$fips,
        state_full = unique(row_subset$state_full)[1],
        name = ii,
        place = ii,
        Twitter = mean(row_subset[row_subset$date == date,]$Twitter),
        Facebook = mean(row_subset[row_subset$date == date,]$Facebook),
        socmed = NA,
        date = as.character(date),
        n = mean(row_subset[row_subset$date == date,]$n),
        district = ii,
        population = sum(row_subset[row_subset$date == date,]$population),
        `Fully Vaccinated` = mean(row_subset[row_subset$date == date,]$`Fully Vaccinated`),
        `First Dose` = mean(row_subset[row_subset$date == date,]$`First Dose`),
        hispanic_prop = mean(row_subset[row_subset$date == date,]$hispanic_prop),
        white_prop = mean(row_subset[row_subset$date == date,]$white_prop),
        black_prop = mean(row_subset[row_subset$date == date,]$black_prop),
        AmInd_prop = mean(row_subset[row_subset$date == date,]$AmInd_prop),
        asian_prop = mean(row_subset[row_subset$date == date,]$asian_prop),
        nhawaiian_prop = mean(row_subset[row_subset$date == date,]$nhawaiian_prop),
        other_prop = mean(row_subset[row_subset$date == date,]$other_prop),
        multi_prop = mean(row_subset[row_subset$date == date,]$multi_prop)
      )
  district_df = rbind.data.frame(district_df,
                                newrow)

  }
  district_df_full = rbind.data.frame(district_df_full,
                                     district_df)
  
  district_df_full$date = as.Date(district_df_full$date, format = 'YYYY-mm-dd')
  
  

}


colnames(district_df_full) <-
  colnames(summary_data)

summary_data = rbind.data.frame(summary_data, district_df_full)

summary_data_WV = summary_data %>%
  filter(state_full %in% c('Ohio','Indiana','Michigan','Illinois',
                           'Iowa','Wisconsin'))

us_counties %>%
  filter(STNAME %in% c('Ohio','Indiana','Michigan','Illinois',
                  'Iowa','Wisconsin'),
         in_dstr == 0) %>%
  left_join(summary_data_WV,
            by = 'fips') %>%
  mutate(name_1 = 
           case_when(is.na(name_1) ~ name,
                     !is.na(name_1) ~ name_1)) %>%
  rename('Jurisdiction' = name_1) %>%
  tm_shape() +
  #Fill with the COVID-19 indicator
  tm_polygons(col = 'n',
              id = 'place',
              #Tooltip = county name and state abbreviation
              popup.vars = c('Jurisdiction: ' = 'Jurisdiction',
                             'Case Rate: ' = 'n'),
              #Legend title = currently active metric
              title = 'Cases',
              alpha = 0.3) 



# Address invalid geometries ------------------------------------------

#For some reason using rmapshaper on my edited shapefile produced 
#an invalid polygon for Hitchcock County, Nebraska. The code below was used
#to assess the issue and fix it.

# us_counties <-
#   st_read('data/spatial/counties_with_mc_districts_small.shp')
# 
# test <-
#   st_is_valid(us_counties, reason = TRUE)
# 
# test = data.frame(test)
# 
# test = cbind.data.frame(us_counties %>%
#                           select(GEOID,
#                                  name_1,
#                                  distrct,
#                                  in_dstr,
#                                  is_dstr),
#                         test)
# 
# hitchcock <- 
#   us_counties %>%
#   filter(GEOID == 31087)
# 
# hitchcock <-
#   st_make_valid(hitchcock)
# 
# us_counties[us_counties$GEOID == 31087,] <- hitchcock
#
# st_write(
#      us_counties,
#      'data/spatial',
#      'counties_with_mc_districts_small',
#      driver = "ESRI Shapefile",
#      append = FALSE
#    )
