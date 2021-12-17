#Test code for making plotly lineplot

#Read in libraries
library(plotly)
library(tidyverse)
library(vroom)

#read in covid data
covid <-
  vroom(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  ) %>%
  filter(date <= '2021-12-14')

#read in social media data
tw_fb_data <-
  read_csv('data/raw/counties_with_tweets.csv') %>% 
  mutate(fips =
           as.character(fips)) %>%
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

#read in population data
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

#read in vaccine data
vax_data <-
  vroom('data/raw/vax_data.csv') %>%
  select(date = date,
         fips = fips,
         Series_Complete_Yes,
         Administered_Dose1_Recip) %>%
  mutate(date = 
           lubridate::mdy(as.character(date)))

#left join
covid_all <-
  covid %>%
  left_join(
    vax_data,
    by = c('fips',
           'date')
  ) %>%
  left_join(
    population_counties,
    by = c('fips',
           'state' = 'STNAME')
  ) %>%
  drop_na(fips)

#fill na with 0
covid_all[c('deaths',
            'Series_Complete_Yes',
            'Administered_Dose1_Recip')][is.na(covid_all[c('deaths',
                                                           'Series_Complete_Yes',
                                                           'Administered_Dose1_Recip')])] <- 0

#select social media columns for grouping
socmed <-
  tw_fb_data %>%
  select(
    fips,
    twitter,
    twitterYN,
    facebookYN,
    socmed
  )

#left join again
covid_all <-
  covid_all %>%
  left_join(
    socmed,
    by = 'fips'
  )

#adjust for population
covid_all <-
  covid_all %>%
  mutate(
    cases_adjusted = 
      cases/population,
    deaths_adjusted = 
      deaths/population,
    comp_vax_adjusted = 
      Series_Complete_Yes/population,
    first_dose_adjusted = 
      Administered_Dose1_Recip/population
  )
#group by
covid_mean_data <-
  covid_all %>%
  group_by(
    date,
    socmed
  ) %>%
  summarise(
    case_rate = 
      mean(cases_adjusted, na.rm = TRUE),
    death_rate = 
      mean(deaths_adjusted, na.rm = TRUE),
    vax_rate = 
      mean(comp_vax_adjusted, na.rm = TRUE),
    dose1_rate = 
      mean(first_dose_adjusted, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  drop_na()

#make and test plotly
lineplot <- covid_mean_data
lineplot <- lineplot %>% plot_ly(
  type = 'scatter',
  mode = 'lines',
  x = ~date, 
  y = ~case_rate, 
  color = ~socmed, 
  legendgroup = ~socmed
) %>%
  layout(
    title = 'COVID-19 and Social Media',
    xaxis = list(
      title = 'Date',
      rangeslider = list(type = "date",
                         label = 'Date')),
    yaxis = list(
      title = 'Case Rate'
    ))

  
lineplot
