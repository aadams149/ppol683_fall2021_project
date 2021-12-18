#This script calculates Moran's I for my 4 COVID-19 metrics of 
#interest, to see if there is spatial autocorrelation
#present in the data


#Load required libraries
library(ape)
library(geodist)
library(sf)
library(tidyverse)

#I'm checking spatial autocorrelation using Moran's I for cumulative
#COVID-19 metrics


# Read in data --------------------------------------------------------

#Read in covid data
covid <-
  vroom(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  ) %>%
  filter(date == '2021-12-14')

#Read in vaccine data
vax_data <-
  vroom('data/raw/vax_data.csv') %>%
  mutate(date = 
           lubridate::mdy(date)) %>%
  filter(date == '2021-12-14')

#Left join
covid <-
  covid %>%
  left_join(
    vax_data,
    by = c('fips' = 'fips',
           'date' = 'date')
  )

#Read in population data
population_counties <-
  vroom('data/raw/co-est2020.csv') %>%
  #Exclude state total populations
  filter(COUNTY != '000') %>%
  #Rename population for convenience
  rename('population' = POPESTIMATE2020) %>%
  #Create a fips column for easy merging
  mutate('fips' = paste0(STATE,COUNTY)) %>%
  select(fips, 
         STNAME,
         population)

#Left join population data and adjust for population
covid <-
  covid %>%
  left_join(
    population_counties,
    by = c('fips',
           'state' = 'STNAME')
  ) %>%
  mutate(
    case_rate = 
      cases/population,
    death_rate = 
      deaths/population,
    vax_rate = 
      Series_Complete_Yes/population,
    dose1_rate = 
      Administered_Dose1_Recip/population
  ) %>%
  #Drop rows in PR/multicounty districts
  filter(fips <= 56999)

#Read in shapefile
us_counties <-
  st_read('data/spatial/counties_with_mc_districts_small.shp') %>%
  #Rename GEOID to fips for clarity
  rename(fips = GEOID) %>%
  #Transform to 4326 so it plays nice w/ tmap
  st_transform(crs = 4326)

#Generate county centroids
county_centroids <-
  us_counties %>%
  filter(is_dstr != 1,
         fips <= 56999) %>%
  st_centroid() %>%
  left_join(
    covid,
    by = 'fips'
  )

#Generate inverse distance matrix
inverse_distance_matrix <- 
  county_centroids %>% 
  st_coordinates() %>% 
  #Some distances are Very Big, so use measure = geodesic
  geodist::geodist(measure = 'geodesic') %>% 
  {1/.}

#Set diagonals to zero
diag(inverse_distance_matrix) <- 0


#Calculate Moran's I:

caserates <-
  ape::Moran.I(x = county_centroids$case_rate,
             weight = inverse_distance_matrix,
             na.rm = TRUE)

deathrates <-
  ape::Moran.I(x = county_centroids$death_rate,
               weight = inverse_distance_matrix,
               na.rm = TRUE)

vaxrates <-
  ape::Moran.I(x = county_centroids$vax_rate,
               weight = inverse_distance_matrix,
               na.rm = TRUE)

dose1rates <-
  ape::Moran.I(x = county_centroids$dose1_rate,
               weight = inverse_distance_matrix,
               na.rm = TRUE)

#Unlist 
caserates = data.frame(unlist(caserates))
deathrates = data.frame(unlist(deathrates))
vaxrates = data.frame(unlist(vaxrates))
dose1rates = data.frame(unlist(dose1rates))

#Bind into data frame for easy display in app 
moran_i <-
  bind_cols(
    caserates,
    deathrates,
    vaxrates,
    dose1rates
  ) %>%
  mutate(
    statistic = 
      rownames(.)
  ) %>%
  rename('Case Rates' = `unlist.caserates.`,
         'Death Rates' = `unlist.deathrates.`,
         'Completed Vaccination Rates' = `unlist.vaxrates.`,
         'First Dose Rates' = `unlist.dose1rates.`) %>%
  select(
    statistic,
    'Case Rates':'First Dose Rates'
  )

# #Export as .csv
# write.csv(moran_i,
#           'data/raw/moran_i.csv')  
