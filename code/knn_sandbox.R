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
         n = n
  ) %>%
  #Add in population data
  left_join(population_counties,
            by = c('fips' = 'fips',
                   'state_full' = 'STNAME')) %>%
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
  #replace zeros and ones with nos and yeses 
  #(and NAs, where applicable)
  # mutate(Facebook = 
  #          ifelse(is.na(Facebook),
  #                 'NA',
  #                 Facebook)) %>%
  # mutate(Twitter = 
  #          case_when(Twitter == 1 ~ 'Yes',
  #                    Twitter == 0 ~ 'No'),
  #        Facebook =
  #          case_when(Facebook == 1 ~ 'Yes',
  #                    Facebook == 0 ~ 'No',
  #                    Facebook == 'NA' ~ 'NA')) %>%
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

abc = abc %>% drop_na()
abc_subset = abc %>%
  select(!c(fips,
            State,
            County)) %>%
  drop_na()

model <- RANN::nn2(abc_subset)
model_output = as.data.frame(model$nn.idx)
model_output = cbind.data.frame(abc,model_output)
model_output = model_output %>%
  mutate(ID = row_number())

target_row = model_output %>%
  filter(fips == fips_code)
fips_code = '12009'

output_table = 
  bind_rows(model_output[model_output$ID == target_row$V1,],
          model_output[model_output$ID == target_row$V2,],
          model_output[model_output$ID == target_row$V3,],
          model_output[model_output$ID == target_row$V4,],
          model_output[model_output$ID == target_row$V5,],
          model_output[model_output$ID == target_row$V6,]) %>%
  select(!c(V1:V10,ID))
