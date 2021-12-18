#This script is just a large, unorganized sandbox for testing
#different snippets of code. I ran a lot of linear models here
#and made my correlation map, but overall this script isn't 
#really an example of data processing


# Read in libraries ---------------------------------------------------

library(sjPlot)
library(tidyverse)
library(vroom)


# Read in data --------------------------------------------------------

#Read in counties
counties <-
  vroom('data/raw/counties_with_tweets.csv') %>%
  #Change the fips column to character so it plays nice with the other data sets
  mutate(fips =
           as.character(fips)) %>%
  #Add leading zeroes where necessary
  mutate(
    fips =
      str_pad(fips,
              5,
              pad = '0'))

#Read in raw tweets
tweets = readr::read_csv(
  'data/raw/most_recent_tweets/most_recent_tweets_2021-12-10.csv')

#Select most recent tweets from each account
newdf <-
  data.frame()
for (ii in unique(tweets$username)) {
  print(ii)
  rows =
    tweets %>%
    filter(username == ii) %>%
    filter(date == max(date)) %>%
    filter(time == max(time))
  newdf =
    rbind.data.frame(newdf, rows)
}

#Read in race data
racedata <-
  vroom('data/raw/county_race_data.csv') %>%
  select(id,
         Total_Hispanic:Total_Multiracial) %>%
  mutate(fips = 
           str_sub(id,-5,-1)) %>%
  select(!id)

#Read in vaccine data
vax_data <-
  vroom('data/raw/vax_data.csv') %>%
  mutate(date = 
           lubridate::mdy(date)) %>%
  filter(date == '2021-12-14')

#Distinct rows
newdf1 <-
  newdf %>%
  distinct(across(c(date,
                    time,
                    user_id,
                    username,
                    name,
                    tweet)),
           .keep_all = TRUE)

#Make twitter handles lowercase
counties$twitter <-
  tolower(counties$twitter)

#Select relevant counties
counties <-
  counties %>%
  select(fips:district)

#Merge in most recent tweets
counties <- 
  counties %>%
  left_join(newdf1,
            by = c('twitter' = 'username')) %>%
  distinct(
    across(fips),
    .keep_all = TRUE) %>%
  rename('name' = name.x,
         'twitter_display_name' = name.y)

#Read in population data
population = readr::read_csv('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

#Read in COVID data
covid <-
  readr::read_csv(
    url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
  ) %>%
  #Use cumulative metrics for modeling,
  #so only need most recent date
  filter(date == '2021-12-14') %>%
  mutate(
    fips = 
      as.character(fips)
  )
  
counties <-
  counties %>%
  left_join(
    covid,
    by = 'fips'
  ) %>%
  left_join(
    vax_data,
    by = 'fips'
  ) %>%
  mutate(
    twitter_active_COVID = 
      ifelse(twitter_active_COVID == 'Yes',1,0),
    twitter_active_last60 = 
      ifelse(twitter_active_last60 == 'Yes',1,0),
    cases_normalized = cases/population,
    deaths_normalized = deaths/population,
    vax_normalized = Series_Complete_Yes/population,
    dose1_normalized = Administered_Dose1_Recip/population)
 

# Is twitter associated with population -------------------------------


twittermodel = lm(
  population ~ twitterYN,
  data = counties
)

tab_model(twittermodel)



# Is facebook associated with population ------------------------------


facebookmodel = lm(
  population ~ facebookYN,
  data = counties
)

tab_model(facebookmodel)



# Is twitter activity associated w/ population ------------------------

#Did more populous areas have more twitter activity during COVID
covid_pop_model =
  lm(
    population ~ twitter_active_COVID,
    data = counties
  )

tab_model(covid_pop_model)

#Did more populous areas have more twitter activity in the last 2 months
covid_recent_model = 
  lm(population ~ twitter_active_last60,
     data = counties
     )

tab_model(covid_recent_model)

#Multivariate model of population----
multivariate_model_population = 
  lm(
    population ~ facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = counties
  )

tab_model(multivariate_model_population)

# covid outcomes ------------------------------------------------------

#Effect of social media on cases (normalized by population)
covid_model_cases <-
  lm(
    cases_normalized ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = counties
  )

tab_model(covid_model_cases)

#Results: health department twitter account being active 
#in last 60 days is associated with 0.8% lower COVID case rates

#Effect of social media on deaths normalized by population
covid_model_deaths <-
  lm(
    deaths_normalized ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = counties
  )

tab_model(covid_model_deaths)

#Results: no comparable effect for deaths

# Evaluate effects of race data ---------------------------------------

#Add in race data and calculate proportions
counties <-
  counties %>%
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
  )

#Add race to case rates model
cases_w_race <-
  lm(
    cases_normalized ~ twitterYN +
      facebookYN +
      twitter_active_COVID+
      twitter_active_last60+
      hispanic_prop +
      white_prop +
      black_prop +
      AmInd_prop +
      asian_prop +
      nhawaiian_prop+
      other_prop+
      multi_prop,
    data = counties
  )

tab_model(cases_w_race)

#Results: twitter is barely significant, almost all
#race data is significant

deaths_w_race <-
  lm(
    deaths_normalized ~ twitterYN +
      facebookYN +
      twitter_active_COVID+
      twitter_active_last60+
      hispanic_prop +
      white_prop +
      black_prop +
      AmInd_prop +
      asian_prop +
      nhawaiian_prop+
      other_prop+
      multi_prop,
    data = counties
  )

tab_model(deaths_w_race)
#effect sizes are all basically zero
#print summary in console to see more detail
summary(deaths_w_race)

# Correlation matrix --------------------------------------------------

#Subset variables of interest
corr_df <-
  counties %>%
  select(
    twitterYN,
    facebookYN,
    population,
    cases_normalized,
    deaths_normalized,
    vax_normalized,
    dose1_normalized,
    hispanic_prop:multi_prop,
    twitter_active_COVID,
    twitter_active_last60
  )

#Preprocess for correlation plot
corr = round(cor(corr_df, method = 'pearson', use = 'complete.obs'),1)

melted = reshape2::melt(corr)

get_lower_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

melted_upper <-
  get_lower_tri(corr)

melted_upper <-
  reshape2::melt(melted_upper)


#Correlation plot
ggplot(data = melted_upper,
       aes(x=Var2,
           y=Var1,
           fill=value)) + 
  geom_tile(color = 'white') +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_y_discrete(labels = c(
    'twitter_active_last60' = 'Twitter Active Last 60 Days',
    'twitter_active_COVID' = 'Twitter Active During COVID',
    'multi_prop' = 'Multiracial Percent',
    'other_prop' = 'Other Race Percent',
    'nhawaiian_prop' = 'Native Hawaiian Percent',
    'asian_prop' = 'Asian Percent',
    'AmInd_prop' = 'Native American Percent',
    'black_prop' = 'Black Percent',
    'hispanic_prop' = 'Hispanic Percent',
    'white_prop' = 'White Percent',
    'deaths_normalized' = 'Deaths, Pop. Adjusted',
    'cases_normalized' = 'Cases, Pop. Adjusted',
    'population' = 'Population',
    'facebookYN' = 'Facebook',
    'twitterYN' = 'Twitter',
    'vax_normalized' = '% Fully Vaccinated',
    'dose1_normalized' = '% First Dose'
  ))+
  scale_x_discrete(labels = c(
    'twitter_active_last60' = 'Twitter Active Last 60 Days',
    'twitter_active_COVID' = 'Twitter Active During COVID',
    'multi_prop' = 'Multiracial Percent',
    'other_prop' = 'Other Race Percent',
    'nhawaiian_prop' = 'Native Hawaiian Percent',
    'asian_prop' = 'Asian Percent',
    'AmInd_prop' = 'Native American Percent',
    'black_prop' = 'Black Percent',
    'hispanic_prop' = 'Hispanic Percent',
    'white_prop' = 'White Percent',
    'deaths_normalized' = 'Deaths, Pop. Adjusted',
    'cases_normalized' = 'Cases, Pop. Adjusted',
    'population' = 'Population',
    'facebookYN' = 'Facebook',
    'twitterYN' = 'Twitter',
    'vax_normalized' = '% Fully Vaccinated',
    'dose1_normalized' = '% First Dose'
  ))+
  ggtitle('Pearson Correlation Coefficients for Relevant Variables') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0.5),
    #legend.position = c(0, 0.7),
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7,
                               title.position = "top", title.hjust = 0.5))





# polished LM models --------------------------------------------------

#Run some polished models and format nicely for presentation
cases_twfb_race <-
  lm(
    cases_normalized ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID +
      twitter_active_last60,
    data = counties
  )

tab_model(cases_twfb_race,
          pred.labels = c(
            'Intercept',
            'Twitter',
            'Facebook',
            'White Percent',
            'Black Percent',
            'Hispanic Percent',
            'Asian Percent',
            'Twitter Active During COVID-19',
            'Twitter Active in Last 60 Days'
          ),
          dv.labels = 'Cases Adjusted For Population')

deaths_twfb_race <-
  lm(
    deaths_normalized ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID +
      twitter_active_last60,
    data = counties
  )

tab_model(deaths_twfb_race,
          pred.labels = c(
            'Intercept',
            'Twitter',
            'Facebook',
            'White Percent',
            'Black Percent',
            'Hispanic Percent',
            'Asian Percent',
            'Twitter Active During COVID-19',
            'Twitter Active in Last 60 Days'
          ),
          dv.labels = 'Deaths Adjusted For Population')


vaxx_rates_model <-
  lm(
    vax_normalized ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID +
      twitter_active_last60,
    data = counties
  )


tab_model(list(cases_twfb_race,
               deaths_twfb_race,
               vaxx_rates_model),
          pred.labels = c(
            'Intercept',
            'Twitter',
            'Facebook',
            'White Percent',
            'Black Percent',
            'Hispanic Percent',
            'Asian Percent',
            'Twitter Active During COVID-19',
            'Twitter Active in Last 60 Days'
          ),
          dv.labels = c(
            'Cases, Pop. Adjusted',
            'Deaths, Pop. Adjusted',
            'Vaccinations, Pop. Adjusted'
            ))

dose1_rates_model <-
  lm(
    dose1_normalized ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID +
      twitter_active_last60,
    data = counties
  )


tab_model(list(cases_twfb_race,
               deaths_twfb_race,
               vaxx_rates_model,
               dose1_rates_model),
          pred.labels = c(
            'Intercept',
            'Twitter',
            'Facebook',
            'White Percent',
            'Black Percent',
            'Hispanic Percent',
            'Asian Percent',
            'Twitter Active During COVID-19',
            'Twitter Active in Last 60 Days'
          ),
          dv.labels = c(
            'Cases, Pop. Adjusted',
            'Deaths, Pop. Adjusted',
            'Vaccinations, Pop. Adjusted',
            'First Doses, Pop. Adjusted'
          ))
