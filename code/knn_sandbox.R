#I used this script to test out code related to my KNN implementation
#in my app, using arbitrary dates and a randomly selected FIPS
#code to see if I could output a dataframe of similar counties


# Read in data --------------------------------------------------------

#COVID
covid <-
  vroom(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  ) %>%
  filter(date <= '2021-12-14') %>%
  # Pivot and rename the data
  pivot_longer('cases':'deaths',
               names_to = 'metric',
               values_to = 'n')

#Population
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = 
           paste0(STATE,
                  COUNTY)) %>%
  select(fips, 
         STNAME,
         population)

covid$n[is.na(covid$n)] <- 0

#Vaccine data
vax_data <-
  vroom('data/raw/vax_data.csv') %>%
  mutate(date = 
           lubridate::mdy(date)) %>%
  filter(date == max(date))


#Race Data
racedata <-
  vroom('data/raw/county_race_data.csv') %>%
  select(id,
         Total_Hispanic:Total_Multiracial) %>%
  mutate(fips = 
           str_sub(id,-5,-1)) %>%
  select(!id)

#Social Media Data
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

#Recreate summary data from the app
abc <-
  covid %>%
  filter(metric == 'cases',
         date >= '2021-11-01',
         date <= '2021-11-30') %>%
  select(!metric) %>%
  left_join(population_counties,
            by = 'fips') %>%
  mutate(n =
           n / population * 100)%>%
  group_by(fips) %>%
  summarise(n = max(n) - min(n)) %>%
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
         population,
         n = n
  ) %>%
  #Remove cases where data is recorded for units other than county
  filter(!is.na(fips)) %>%
  #Drop unwanted columns
  # select(!c(fips,
  #           place,
  #           socmed)) %>%
  # #Rename columns to look neater/more professional
  select(fips,
         State = state_full,
         County = name,
         '2020 Population' = population,
         !!str_to_title('cases') := n,
         Facebook,
         Twitter
  ) %>%
  #Drop rows w/ bad data
  filter(!is.na(State)) %>%
  left_join(vax_data,
            by = 'fips') %>%
  left_join(
    racedata,
    by = 'fips'
  ) %>%
  mutate(
    hispanic_prop = 
      Total_Hispanic/`2020 Population`,
    white_prop =
      Total_White/`2020 Population`,
    black_prop = 
      Total_Black/`2020 Population`,
    AmInd_prop = 
      Total_AmIndian/`2020 Population`,
    asian_prop = 
      Total_Asian/`2020 Population`,
    nhawaiian_prop = 
      Total_NativeHawaiian/`2020 Population`,
    other_prop = 
      Total_Other/`2020 Population`,
    multi_prop = 
      Total_Multiracial/`2020 Population`
  ) %>%
  select(!c(Total_Hispanic,
            Total_White,
            Total_Black,
            Total_Asian,
            Total_AmIndian,
            Total_NativeHawaiian,
            Total_Other,
            Total_Multiracial))

abc <-
  abc %>% drop_na()

#Create dataframe of just indicator variables
abc_subset <-
  abc %>%
  select(!c(fips,
            State,
            County)) %>%
  drop_na()

#Run KNN model
model <- 
  RANN::nn2(abc_subset)

model_output <-
  as.data.frame(model$nn.idx)

model_output <-
  cbind.data.frame(abc,model_output)

#Output is matrix of indexes, so easily accessible row
#numbers are important
model_output <- model_output %>%
  mutate(ID = row_number())

#Test code: Brevard County, FL
fips_code = '12009'

#Select 1 row as target 
target_row = model_output %>%
  filter(fips == fips_code)

#For some reason the first most similar county is always
#itself, so rows 2-6 are the 5 most similar

#Proof of concept works!
output_table = 
  bind_rows(model_output[model_output$ID == target_row$V1,],
          model_output[model_output$ID == target_row$V2,],
          model_output[model_output$ID == target_row$V3,],
          model_output[model_output$ID == target_row$V4,],
          model_output[model_output$ID == target_row$V5,],
          model_output[model_output$ID == target_row$V6,]) %>%
  select(!c(V1:V10,ID))
