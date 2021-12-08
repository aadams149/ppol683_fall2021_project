library(tidyverse)

counties = readr::read_csv('data/raw/counties_with_tweets.csv') %>%
  #Change the fips column to character so it plays nice with the other data sets
  mutate(fips =
           as.character(fips)) %>%
  #Add leading zeroes where necessary
  mutate(
    fips =
      str_pad(fips,
              5,
              pad = '0'))

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

racedata = readr::read_csv('data/raw/county_race_data.csv')

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

counties =
  counties %>%
  distinct(
    across(fips
    ),
    .keep_all = TRUE
  )

write_csv(counties, 'data/raw/counties_with_tweets.csv')

getwd()
counties = counties %>%
  rename('name' = name.x,
         'twitter_display_name' = name.y)

population = readr::read_csv('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, STNAME, population)

covid <-
  readr::read_csv(
    url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
  ) %>%
  # Pivot and rename the data
  pivot_longer('cases':'deaths',
               names_to = 'metric',
               values_to = 'n')

covid_today <-
  covid %>%
  filter(date == max(date)) %>%
  pivot_wider(
              names_from = 'metric',
              values_from = 'n')

rm(covid)

df = 
  counties %>%
  left_join(
    population,
    by = 'fips'
  ) %>%
  left_join(
    covid_today,
    by = 'fips'
  )

df$twitter_active_COVID =
  case_when(
            df$date.x >= '03-11-2020' ~ 1,
            df$date.x < '03-11-2020' ~ 0,
            !is.na(df$twitter) & is.na(df$date.x) ~ 0)

df$twitter_active_last60 = 
  case_when(
    df$date.x >= lubridate::mdy('10-01-2021') ~ 1,
    df$date.x < lubridate::mdy('10-01-2021') ~ 0,
    !is.na(df$twitter) & is.na(df$date.x) ~ 0
  )

df$twitter_active_COVID[df$twitter_active_COVID == 'Outdated-Add District'] <- NA

df$cases_normalized = df$cases/df$population
df$cases_per_100k = df$cases/(df$population/100000)
df$deaths_normalized = df$deaths/df$population
df$deaths_per_100k = df$deaths/(df$population/100000)

df$twitter_active_COVID_all = df$twitter_active_COVID
df$twitter_active_COVID_all[is.na(df$twitter_active_COVID_all)] <- 0

df$twitter_active_last60_all = df$twitter_active_last60
df$twitter_active_last60_all[is.na(df$twitter_active_last60_all)] <- 0


# Is twitter associated with population -------------------------------


twittermodel = lm(
  population ~ twitterYN,
  data = df
)

summary(twittermodel)



# Is facebook associated with population ------------------------------


facebookmodel = lm(
  population ~ facebookYN,
  data = df
)

summary(facebookmodel)



# Is twitter activity associated w/ population ------------------------

covid_pop_model =
  lm(
    population ~ twitter_active_COVID,
    data = df
  )

summary(covid_pop_model)

covid_recent_model = 
  lm(population ~ twitter_active_last60,
     data = df
     )

summary(covid_recent_model)

multivariate_model_population = 
  lm(
    population ~ facebookYN+twitter_active_COVID+twitter_active_last60,
    data = df
  )

summary(multivariate_model_population)



# covid outcomes ------------------------------------------------------

#Effect of social media on cases (normalized by population)
covid_model_cases <-
  lm(
    cases_normalized ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = df
  )

summary(covid_model_cases)

#Effect of social media on deaths normalized by population
covid_model_deaths <-
  lm(
    deaths_normalized ~ twitterYN+
      facebookYN+
      factor(twitter_active_COVID)+twitter_active_last60,
    data = df
  )

summary(covid_model_deaths)

#Effect of social media on cases (non-normalized)
covid_model_cases_nn <-
  lm(
    cases ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60+
      population,
    data = df
  )

summary(covid_model_cases_nn)

#Effect of social media on deaths (non-normalized)
covid_model_deaths_nn <-
  lm(
    deaths ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60+
      population,
    data = df
  )

summary(covid_model_deaths_nn)

library(sjPlot)

tab_model(covid_model_cases)

tab_model(covid_model_deaths)

tab_model(covid_model_cases_nn)

tab_model(covid_model_deaths_nn)

cases_100k <-
  lm(
    cases_per_100k ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = df
  )

deaths_100k <-
  lm(
    deaths_per_100k ~ twitterYN+
      facebookYN+
      twitter_active_COVID+
      twitter_active_last60,
    data = df
  )

summary(deaths_100k)

racedata = readr::read_csv('data/raw/county_race_data.csv') %>%
  select(id:Total_Multiracial) %>%
  mutate(fips = 
           str_sub(
             id,
             -5,
             -1
           )) %>%
  select(Total_Hispanic:fips)

df <-
  df %>%
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

cases_w_race <-
  lm(
    cases_normalized ~ twitterYN +
      facebookYN +
      twitter_active_COVID_all+
      twitter_active_last60_all+
      hispanic_prop +
      white_prop +
      black_prop +
      AmInd_prop +
      asian_prop +
      nhawaiian_prop+
      other_prop+
      multi_prop,
    data = df
  )

tab_model(cases_w_race)


deaths_w_race <-
  lm(
    cases_normalized ~ twitterYN +
      facebookYN, #+
      #twitter_active_COVID_all+
      #twitter_active_last60_all+
      #hispanic_prop +
      #white_prop +
      #black_prop +
      #AmInd_prop +
      #asian_prop +
      #nhawaiian_prop+
      #other_prop+
      #multi_prop,
    data = df
  )

tab_model(deaths_w_race)




# Correlation matrix --------------------------------------------------

corr_df <-
  df1 %>%
  select(
    twitterYN,
    facebookYN,
    population,
    cases_normalized,
    deaths_normalized,
    hispanic_prop:twitter_active_last60_all,
    Series_Complete_Pop_Pct,
    
  )

corr = round(cor(corr_df, method = 'pearson', use = 'complete.obs'),1)

melted = reshape2::melt(corr)

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
    'twitter_active_last60_all' = 'Twitter Active Last 60 Days',
    'twitter_active_COVID_all' = 'Twitter Active During COVID',
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
    'Series_Complete_Pop_Pct' = '% Fully Vaccinated'
  ))+
  scale_x_discrete(labels = c(
    'twitter_active_last60_all' = 'Twitter Active Last 60 Days',
    'twitter_active_COVID_all' = 'Twitter Active During COVID',
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
    'Series_Complete_Pop_Pct' = '% Fully Vaccinated'
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

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}

melted_upper <-
  get_upper_tri(corr)

melted_upper <-
  reshape2::melt(melted_upper)



# polished LM models --------------------------------------------------

deaths_twfb_race <-
  lm(
    deaths_normalized*100 ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID_all +
      twitter_active_last60_all,
    data = df
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
            'Native American Percent',
            'Pacific Islander Percent',
            'Twitter Active During COVID-19',
            'Twitter Active in Last 60 Days'
          ),
          dv.labels = 'Cases Adjusted For Population')


vaxx_rates_model <-
  lm(
    Series_Complete_Pop_Pct ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID_all +
      twitter_active_last60_all,
    data = df
  )

tab_model(list(vaxx_18_65_model,
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
            '% At Least One Dose',
            '% Fully Vaccinated'
          ))


tab_model(list(cases_twfb_race,
               deaths_twfb_race),
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
            'Deaths, Pop. Adjusted'
            ))


df$vaccination_young = df$Series_Complete_18PlusPop_Pct - df$Series_Complete_65PlusPop_Pct

vaxx_18_65_model <-
  lm(
      Administered_Dose1_Pop_Pct ~ twitterYN+
      facebookYN +
      white_prop +
      black_prop +
      hispanic_prop +
      asian_prop +
      twitter_active_COVID_all +
      twitter_active_last60_all,
    data = df
  )

summary(vaxx_18_65_model)
# vaccination data preprocessing --------------------------------------


# vaccination_data <-
#   readr::read_csv('vaccine_data.csv') %>%
#   mutate(Date = 
#            str_replace_all(Date,'/','-')) %>%
#   mutate(Date = 
#            as.Date(Date)) %>%
#   filter(lubridate::year(Date) == 2021) %>%
#   group_by('FIPS') %>%
#   filter(Date == max(Date))
# 
# newdf = data.frame()
# for (ii in df$fips){
#   data = vaccination_data %>%
#     filter(FIPS == ii)
#   data = head(data, 1)
#   newdf = rbind.data.frame(newdf, data)
#   
# }
# 
# write_csv(newdf, 'data/raw/vaccination_rates.csv')
# 
# rm(vaccination_data)

vaccination_rates <-
  readr::read_csv('data/raw/vaccination_rates.csv')

df = df %>%
  left_join(
    vaccination_rates,
    by = c('fips' = 'FIPS'))
  

cases_engagement <-
  lm(cases_normalized ~ twitterYN+
       twitter_active_COVID_all+
       twitter_active_last60_all+
       likes_count+
       retweets_count+
       replies_count,
     data = df1
       )
summary(cases_engagement)

deaths_engagement <-
  lm(deaths_normalized ~ twitterYN+
       twitter_active_COVID_all+
       twitter_active_last60_all+
       likes_count+
       retweets_count+
       replies_count,
     data = df1
  )
summary(deaths_engagement)

vaxx_engagement <-
  lm(Series_Complete_Pop_Pct ~ #twitterYN+
       #twitter_active_COVID_all+
       #twitter_active_last60_all+
       likes_count+
       retweets_count+
       replies_count+
       facebookYN+
       white_prop +
       black_prop +
       hispanic_prop +
       asian_prop,
     data = df1
  )
summary(vaxx_engagement)


vaxx_engagement <-
  lm(Series_Complete_Pop_Pct ~ twitterYN+
       twitter_active_COVID_all+
       twitter_active_last60_all+
       likes_count+
       retweets_count+
       replies_count+
       facebookYN +
       white_prop +
       black_prop +
       hispanic_prop +
       asian_prop+
       in_multicountydistrict,
     data = df1
  )
summary(vaxx_engagement)

racedata = racedata %>%
  rename(fips = id)

df1 = df1 %>%
  left_join(
    racedata,
    by = 'fips'
  ) %>%
  mutate(
    white_prop = 
      Total_White/population,
    black_prop = 
      Total_Black/population,
    hispanic_prop = 
      Total_Hispanic/population,
    asian_prop = 
      Total_Asian/population,
    AmInd_prop = 
      Total_AmIndian/population,
    nhawaiian_prop = 
      Total_NativeHawaiian/population,
    other_prop = 
      Total_Other/population,
    multi_prop = 
      Total_Multiracial/population
  )

vaxxadmin_engagement <-
  lm(Administered_Dose1_Pop_Pct ~ #twitterYN+
       #twitter_active_COVID_all+
       #twitter_active_last60_all+
       likes_count+
       retweets_count+
       replies_count+
       facebookYN+
       white_prop +
       black_prop +
       hispanic_prop +
       asian_prop+
       in_multicountydistrict,
     data = df1
  )
summary(vaxxadmin_engagement)


df1$in_multicountydistrict = ifelse(!is.na(df1$district),1,0)


# maps ----------------------------------------------------------------

library(RANN)

nn_data = df %>%
  select(fips,
         name = name.x,
         state_full,
         twitterYN,
         twitter_active_COVID_all,
         twitter_active_last60_all,
         facebookYN,
         population,
         cases_normalized,
         deaths_normalized,
         hispanic_prop:multi_prop,
         Series_Complete_Pop_Pct,
         Administered_Dose1_Pop_Pct
         ) %>%
  drop_na()

nn_info = nn_data %>% select(fips,name,state_full)

abc = RANN::nn2(data = nn_data %>% select(!c(fips,name,state_full)))

nn_output = as.data.frame(abc$nn.idx)
nn_output = cbind.data.frame(nn_info, nn_output)
