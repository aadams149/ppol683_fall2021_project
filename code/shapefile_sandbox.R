#I used this script to process the county shapefile. This script
#creates and writes a new shapefile called `counties_with_mc_districts`,
#which contains all the polygons of the original, plus 87 new polygons
#which cover different multi-county public health districts in the United 
#States. The multi-county district polygons are assigned arbitrary FIPS
#codes starting with 999, the 2-digit state fips code corresponding to the 
#state (since each district is located entirely within 1 state),
#the district name as the attribute 'district', and a string of constituent
#county names separated by commas as 'name'. I also create two additional
#variables for use in plotting: is_district = 1 if the polygon is a multi-
#county health district, while in_district = 1 if the polygon is a county
#which forms part of a multi-county health district.

#Read in needed libraries
library(sf)
library(tidyverse)

#Read in original shapefile
us_counties <-
  st_read('data/spatial/us_counties.shp')

#Read in social media dataset (this has the district names)
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
              pad = '0'))

#Select relevant columns, drop rows where district = NA
districts <-
  tw_fb_data %>%
  select(fips,
         name,
         district) %>%
  drop_na()

#Left join shapefile and district names
us_counties <-
  us_counties %>%
  left_join(
    districts,
    by = c('GEOID' = 'fips'))

#Make up a new FIPS code for each department
newfips = 99901
#Copy the shapefile in case something goes wrong
us_counties1 = us_counties

#For each district
for (ii in unique(districts$district)){

  #Filter counties in that district
  needed_counties <-
    us_counties %>%
    filter(district == ii)
  
  #Join them into 1 big polygon
  unified <-
    st_union(needed_counties)
  
  #Convert vector of names into single string
  countynames <-
    paste(needed_counties$name, collapse = ', ')
  
  #Define new row of data frame
  newrow <-
    st_as_sf(
      data.frame(
        STATEFP = unique(needed_counties$STATEFP)[1],
        COUNTYFP = NA,
        COUNTYNS = NA,
        AFFGEOID = NA,
        GEOID = newfips,
        NAME = ii,
        LSAD = NA,
        ALAND = NA,
        AWATER = NA,
        name = countynames,
        district = ii,
        geometry = unified))
    
  #rbind
  us_counties1 = rbind(us_counties1,
                       newrow)
  
  #Arbitrary fips increments by 1
  newfips = newfips+1
}

#Create 2 additional variables
us_counties1 <-
  us_counties1 %>%
  mutate(is_district = 
           #Is the polygon a multi-county district or not
           case_when(GEOID >= 99000 ~ 1,
                     GEOID < 99000 ~ 0),
           #Is the polygon in a multi-county district or not
         in_district = 
           case_when(GEOID >= 99000 ~ 0,
                     is.na(district) ~ 0,
                     (GEOID <= 99000) & (!is.na(district)) ~ 1)) 

#Write the shapefile
st_write(
  us_counties,
  'data/spatial',
  'counties_with_mc_districts',
  driver = "ESRI Shapefile"
)
