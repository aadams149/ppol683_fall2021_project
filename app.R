
# setup -------------------------------------------------------------------
library(leaflet)
library(lubridate)
library(RANN)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(vroom)
options(scipen = 999)

# data --------------------------------------------------------------------

# My final project for this class is also centered on COVID-19, so I ended up
# using some similar elements to our practice in class in building this app.
# I did incorporate the data I've been collecting for my own project, and I 
# tried to make this as distinct from the in-class practice as I could,
# including a small change to the CSS.

# Read in covid data from New York Times:

# For this app, I'm using county-level data on cases and deaths.

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
    ) 

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
    st_read('data/spatial/us_counties.shp') %>%
    #Rename GEOID to fips for clarity
    rename(fips = GEOID) %>%
    #Transform to 4326 so it plays nice w/ tmap
    st_transform(crs = 4326) %>%
    #Maybe this left join is unnecessary.
    left_join(population_counties,
              by = 'fips')

#Define rescaling function
range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }

#Set tmap mode
tmap_mode('view')

# user interface ----------------------------------------------------------

#I kept the code which instantiates the dashboard and sidebar,
#but I added new tabs and changed the content of the existing tabs.

ui <- 
    dashboardPage(
        
        dashboardHeader(title = 'Social Media and Public Health'),
        
        dashboardSidebar(
          radioButtons(
            inputId = 'date_or_daterange',
            label = 'Display',
            choiceNames = c(
              'Cumulative Total',
              'Total Over Selected Date Range'
            ),
            choiceValues = c(
              'cumulative',
              'daterange'
            )
          ),
          conditionalPanel(
            condition = "input.date_or_daterange == 'cumulative'",
            dateInput(
              inputId = 'single_date',
              label = 'Select a date:',
              min = min(covid$date),
              max = max(covid$date))
          ),
          conditionalPanel(
            condition = "input.date_or_daterange == 'daterange'",
            uiOutput('daterangeUI')
            # dateRangeInput(
            #   inputId = 'date_range',
            #   label = 'Select a range of dates:',
            #   start = min(covid$date),
            #   end = max(covid$date))
          ),
            
            selectInput(
              inputId = 'state_selector',
              label = 'Select a state',
              choices = c('Show All',sort(unique(tw_fb_data$state_full)))
            ),
            
            radioButtons(
                inputId = 'metric',
                label = 'View:',
                choiceNames = c('Cases',
                                'Deaths',
                                'Fully Vaccinated',
                                'At Least 1 Vaccine Dose'),
                choiceValues = c('cases',
                                 'deaths',
                                 'series_complete_yes',
                                 'administered_dose1_recip')),
            
            checkboxInput(
                inputId = 'population_adjust',
                label = 'Adjust for population?'),
            
            sidebarMenu(
                menuItem('Home',
                         icon = icon('home'),
                         tabName = 'home'),
                
                menuItem('Map',
                         icon = icon('map'),
                         tabName = 'maps'),
                
                menuItem('Table',
                         icon = icon('table'),
                         tabName = 'tables'),
      
                menuItem('Trend',
                         icon = icon('chart-line'),
                         tabName = 'charts'),
                
                menuItem('Social Media',
                         icon = icon('wifi'),
                         tabName = 'socialmedia'),
                
                menuItem('Help',
                         icon = icon('question-circle'),
                         tabName = 'help')
            )
        ),
        
        dashboardBody(
            tags$head(
                tags$link(
                    rel = 'stylesheet',
                    type = 'text/css',
                    href = 'dashboard_styles.css'
                )
            ),
            
            # COVID Map tab -------------------------------------------------------
            
            
            tabItems(
                tabItem(
                  tabName = 'home',
                  h2('Homepage'),
                  helpText('The purpose of this app is to visualize data
                           related to county health department social
                           media accounts in the United States. Try clicking
                           on one of the other tabs to see some of the 
                           visualizations. You can adjust the input values
                           using the control widgets on the sidebar and in
                           the tabs. For more information about any of the
                           tabs or visualizations included here, click on
                           the "Help" tab.')
                ),
                tabItem(
                    tabName = 'maps',
                    #Create a header
                    h2('Map of COVID-19 Outcomes'),
                    #Display some dynamic text about which county in the selected
                    #jurisdiction has the highest/lowest incidence of the selected
                    #metric for the specified interval
                    textOutput('covidmap_text'),
                    #Display a map of the desired COVID indicator, state, and interval
                    tmapOutput(outputId = 'covid_map')),
                
                # Summary Table Tab ---------------------------------------------------
                
                #This one's nice and simple
                tabItem(
                    tabName = 'tables',
                    h2('Summary Table'),
                    tabsetPanel(
                      tabPanel('Master Table',
                               helpText(
                                 textOutput('master_table_text')
                               ),
                               dataTableOutput(outputId = 'summary_table')),
                      tabPanel('Similarity Table',
                               helpText(
                                 h5(
                                   'Use the drop-down menu on the sidebar to 
                                   select a state. Once a state is selected,
                                   the drop-down menu on this tab will populate
                                   with the counties in that state. Selecting 
                                   a county will cause the app to calculate and
                                   identify the five most similar counties in the
                                   United States, and report data on those counties
                                   in the table below. Checking the "Within State?"
                                   box will tell the app to limit the calculations
                                   to the selected state, rather than the entire
                                   country. For more information about how these
                                   similar counties are identified, check the 
                                   "Similarity Table" page in the "Help" tab.'
                                 )
                               ),
                               checkboxInput('instate','Within State?'),
                               uiOutput('county_selector_table'),
                               conditionalPanel(
                                 condition = "input.state_selector != 'Show All'",
                                 dataTableOutput(outputId = 'similarity_table'))
                               )
                    )
                    ),
                
                # Scatterplot Tab -----------------------------------------------------
                
                
                tabItem(
                    tabName = 'charts',
                    #Create appropriate header
                    h2('Trends in COVID-19 Incidence'),
                    #Display selector for counties depending on selected state
                    uiOutput('county_selector_trend'),
                    #Display text indicating which COVID-19 metric is chosen,
                    #which state is chosen, and the current selected date interval
                    textOutput('plot_text'),
                    #Show a dynamic line plot
                    plotOutput(outputId = 'plot_output')),
                
                # Social Media Map tab ------------------------------------------------
                
                
                tabItem(
                    tabName = 'socialmedia',
                    h2('County Public Health Department Social Media Accounts'),
                    #Display text indicating number of counties w/ FB and/or Twitter
                    textOutput('socialmedia_text'),
                    #Display interactive map
                    tmapOutput(outputId = 'socialmedia_map')
                ),
                
                # Help tab ------------------------------------------------------------
                #This tab contains a tab subset which provides information for each of 
                #the other tabs, as well as some general information for the app
                #and some relevant links.
                tabItem(
                    tabName = 'help',
                    h2('Help'),
                    
                    tabsetPanel(
                        tabPanel('General Information',
                                 helpText(
                                     h5(
                                         'This web app is intended to allow users to 
                         visualize data related to COVID-19 and public
                         health departments across the United States.'),
                                     
                                     h5(
                                         paste(
                                             'The date-range widget on the left allows users
                           to select from a range of dates, spanning from ',
                                             min(covid$date),
                                             ' to ',
                                             max(covid$date),
                                             '. This interval reflects the data on
                 COVID-19 cases and deaths at the county level over the past
                 30 days. This data is retrieved from the New York Times 
                 COVID-19 data GitHub repository, accessible on the "Links"
                 tab below. The other data used in this app, such as the 
                 data on social media coverage and county population, will
                 be added to the `ppol683_final_project` repository 
                 accessible at my GitHub (labeled "Alex`s GitHub"
                 on the "Links" tab) in the weeks to come.')),
                                     
                                     h5(
                                         "The radio buttons can be used to switch between 
                         views of COVID-19 case counts and COVID-19 deaths.
                         Checking the 'Adjust for Population?' checkbox 
                         divides the number of cases or deaths in each 
                         county by that county's population."),
                                     
                                     h5("Some elements of this app, such as the maps on 
                       the Map and Social Media sidebar tabs or the county
                       drop-down menu on the Trend sidebar tab, may take a
                       few seconds to load. Don't worry! Unless you see an
                       error message (which you shouldn't), the app is 
                       working normally, and might just be a little slow.
                       When running this app in RStudio, there should be
                       three warning messages about the number of options
                       in certain selector widgets. These will not affect
                       the performance of the app, and can be safely
                       ignored."))
                        ),
                        
                        tabPanel('Map',
                                 helpText(
                                     h5('This tab shows the number of COVID-19 cases or 
                       deaths over the last 30 days in the United States
               at the county level. The map view can be changed using the 
               radio buttons and "Adjust for population?" checkbox on the 
               sidebar, as well as the drop-down menu on this tab. Choosing 
               "Show All" in the drop-down menu will display a map of the 
               United States. Choosing any other option will display a map 
               of the selected state. Moving your cursor over a county on 
               the map will show the county and state name. Clicking on a 
               county will produce a pop-up bubble showing the county name,
               state, and COVID-19 statistic based on the radio
               buttons and checkbox. This map is created using the 
               `tmap` package'))),
                        
                        tabPanel('Table',
                                 helpText(
                                     h5('This table shows the population as of the 2020 
                       census, existence of public health department 
                       Facebook and Twitter accounts, and COVID-19 rates for
                       each county in the United States. Like the map on the
                       "Map" tab, this table reacts to the radio buttons and
                       checkbox on the sidebar. Switching between "Cases" 
                       and "Deaths" or checking or un-checking the "Adjust
                       for Population?" box will alter the information 
                       displayed in the COVID-19 statistic column.'),
                                     
                                     h5("The table is also searchable: entering text in 
                       the box in the top-right corner will bring up entries
                       with those characters. Clicking the arrows next to 
                       each column name once will sort the table by 
                       ascending order of that column's values; clicking it
                       a second time will sort the table by descending order
                       of column values. The number of items displayed can 
                       be changed using the widget in the top-left corner, 
                       and it is possible to view additional pages of the 
                       table using the navigation buttons at the bottom."))),
                        
                        tabPanel('Scatterplot',
                                 helpText(
                                     paste0("This tab shows a line plot of changes in 
                       COVID-19 incidence for a selected metric and interval.
                       You can use the 'State' drop-down menu to choose a 
                       state, and then select up to five counties from the 
                       'County' drop-down menu. Each county will be added to
                       the line plot as a colored line, and the legend on 
                       the right side of the plot will update as counties 
                       are added and removed. The plot in this tab is 
                       generated using the `ggplot2`
                       package, from the `tidyverse` collection of packages.")
                                 )),
                        
                        tabPanel('Social Media',
                                 helpText(
                                     helpText(
                                         h5('Use the "State" drop-down selector to choose a 
                         state to view. The app will then create a map of 
                         that state, colored in based on whether each county
                         health department has a Facebook or Twitter account.
                         Moving your cursor over a county will show the 
                         county name. Select "Show All" to see a map of the
                         entire United States.'),
                                         
                                         h5('Note: Due to the ongoing data collection 
                         process, data for some states may be incomplete. 
                         The app will be updated with more complete data in
                         the coming weeks. Additionally, many county public
                         health departments which do not have individual 
                         social media accounts are part of multi-county or 
                         regional departments which do have accounts. 
                         Those counties are listed as having a Facebook or
                         Twitter profile here. There are also some county 
                         health department Twitter accounts which are 
                         inactive or have not tweeted. While those are 
                         included here, future versions of this app will 
                         identify those counties, as well as the ones which
                         are part of multi-county health departments.')
                                     ))),
                        
                        tabPanel('Links',
                                 tags$a(href = 'https://github.com/nytimes/covid-19-data',
                                        'New York Times COVID-19 Data GitHub'),
                                 hr(),
                                 tags$a(href = 'https://github.com/aadams149',
                                        "Alex's GitHub"),
                                 hr(),
                                 tags$a(href = 'https://fonts.googleapis.com/css2?family=Merriweather&display=swap',
                                        'Get the font used in this app')))
                    
                )
                
            )
        )
    )

# server ------------------------------------------------------------------

server <- 
    function(input, output) { 
        
        daterangeUI <-
          reactive({
            if(input$metric %in% c('completed vaccinations',
                                   'first vaccine doses administered')){
              dateRangeInput(
                inputId = 'date_range',
                label = 'Select a range of dates',
                min = min(vax_data$date),
                max = max(vax_data$date)
              )
            }else{
              dateRangeInput(
                inputId = 'date_range',
                label = 'Select a range of dates',
                min = min(covid$date),
                max = max(covid$date))
            }
          })
          
        output$daterangeUI <-
          renderUI(daterangeUI())
        
        
        # Code for COVID Map Output -------------------------------------------
        
        #This section contains all the code necessary to filter and instantiate
        #reactive data for the map on the 'Map' tab.
        
        #First, filter the COVID-19 data by metric and date range
        covid_filtered <-
            reactive({
              if(input$date_or_daterange == 'cumulative'){
                covid %>%
                  filter(metric == input$metric,
                         date == input$single_date) %>%
                  select(!metric)
              }else{
                covid %>%
                    filter(metric == input$metric,
                           date >= input$date_range[1],
                           date <= input$date_range[2]) %>%
                    select(!metric)
              }
            }) 
        
        #Check the status of the population adjustment checkbox to 
        #determine if necessary to compute by population
        covid_adjusted <-
            reactive({
                if (input$population_adjust) {
                    covid_filtered() %>%
                        left_join(population_counties,
                                  by = 'fips') %>%
                        mutate(n =
                                   n / population * 100)
                } else {
                    covid_filtered()
                }
            })
        
        #Create a summary table showing cumulative change over the selected
        #interval
        summary_data <-
            reactive({
              if(input$date_or_daterange == 'daterange'){
                covid_adjusted <-
                  covid_adjusted() %>%
                    group_by(fips) %>%
                    summarise(n = max(n) - min(n)) }
              else{
                covid_adjusted <-
                  covid_adjusted()
              }
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
                           n = n
                    ) %>%
                    #Add in population data
                    left_join(population_counties,
                              by = c('fips' = 'fips',
                                     'state_full' = 'STNAME')) %>%
                    left_join(vax_data,
                              by = 'fips') %>%
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
                
            })
        
        master_table_text <-
          reactive({
          if(input$state_selector == 'Show All'){
            'This table shows the data used in this app for all counties
            and county-equivalent jurisdictions in the United States.'
          }else{
            paste(
              'This table shows the data used in this app for all counties
              and county-equivalent jurisdictions in ',
              input$state_selector,'.')
          }})
        
        output$master_table_text <-
          renderText(
            master_table_text()
          )
        
        #Filter the summary data if the option selected is anything other than
        #"Show all" (this is only used for the dynamic text which accompanies
        #the map)
        summary_covidmap_subset <-
            reactive({
                if(input$state_selector != 'Show All'){
                    summary_data() %>%
                        filter(state_full == input$state_selector)
                }
            })
        
        #Filter the shapefile based on the selected state
        shapefile_covid <-
            reactive({
                if(input$state_selector == 'Show All'){
                    us_counties
                }else{
                    us_counties %>%
                        filter(STNAME == input$state_selector)
                }
            })
        
        
        
        # COVID Map Text ------------------------------------------------------
        
        #Create some reactive text to specify what the map is showing.
        #4 versions: U.S. vs specific state, Pop_adjust vs not
        covidmap_text <-
            reactive({
              max_metric = summary_data()$place[which.max(summary_data()$n)]
              max_pop = summary_data()$population[which.max(summary_data()$n)]
              min_metric = summary_data()$place[which.min(summary_data()$n)]
              min_pop = summary_data()$population[which.min(summary_data()$n)]
              
              if(input$date_or_daterange == 'daterange'){
                beginning = 
                  paste0('Between ',
                         input$date_range[1],
                         ' and ',
                         input$date_range[2],
                         ", ")
              }else{
                beginning = 
                  paste0('As of ',
                         input$single_date,
                         ", ")
              }
              if(input$state_selector == 'Show All'){
                  if(input$population_adjust){
                      
                      paste0(
                        beginning,
                        max_metric,
                        ' (Population: ',
                        max_pop,
                        ') ',
                        ' had the highest rate of COVID-19 ',
                        tolower(input$metric),
                        ' in the United States, adjusted for population. ',
                        min_metric,
                        ' (Population: ',
                        min_pop,
                        ') ',
                        ' had the lowest rate of COVID-19 ',
                        tolower(input$metric),
                        ' in the United States, adjusted for population.'
                      )}
                    else{
                        paste0(beginning,
                               max_metric,
                               ' (Population: ',
                               max_pop,
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States. ',
                               min_metric,
                               ' (Population: ',
                               min_pop,
                               ') ',
                               ' had the lowest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States.') 
                    }
                } else {
                    if(input$population_adjust){
                        paste0(beginning,
                               summary_covidmap_subset()$name[which.max(
                                   summary_covidmap_subset()$n)],
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.max(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_selector,
                               ', adjusted for population. ',
                               summary_covidmap_subset()$name[which.min(
                                   summary_covidmap_subset()$n)], 
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.min(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the lowest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_selector,
                               ', adjusted for population.')}
                    else{
                        paste0(beginning,
                               summary_covidmap_subset()$name[which.max(
                                   summary_covidmap_subset()$n)],
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.max(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_selector,
                               '. ',
                               summary_covidmap_subset()$name[which.min(
                                   summary_covidmap_subset()$n)], 
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.min(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the lowest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_selector,'.')
                    }
                }
            })
        
        #Render the text in the UI
        output$covidmap_text <-
            renderText(
                covidmap_text()
            )
        
        
        # COVID Map Output ----------------------------------------------------
        
        #Create and render an interactive map using the data processed earlier
        output$covid_map <-
            renderTmap(
                #Join the shapefile and the summary data
                shapefile_covid() %>%
                    left_join(summary_data(),
                              by = 'fips') %>%
                    tm_shape() +
                    #Fill with the COVID-19 indicator
                    tm_polygons(col = 'n',
                                #Tooltip = county name and state abbreviation
                                id = 'place',
                                #Legend title = currently active metric
                                title = str_to_title(input$metric)) +
                    #Use filtered shapefile to set bbox (sorry Alaska)
                    tm_view(bbox = st_bbox(shapefile_covid())))
        
        # Interactive Data Table Code -----------------------------------------
        
        #Create and render an interactive data table
        output$summary_table <-
            renderDataTable(
              if(input$state_selector == 'Show All'){
                #Use the summary data as a base
                summary_data() %>%
                    #Drop unwanted columns
                    select(!c(fips,
                              place,
                              socmed)) %>%
                    #Rename columns to look neater/more professional
                    select(State = state_full,
                           County = name,
                           '2020 Population' = population,
                           !!str_to_title(input$metric) := n,
                           Facebook,
                           Twitter
                    ) %>%
                    #Drop rows w/ bad data
                    filter(!is.na(State)) %>%
                    #replace zeros and ones with nos and yeses 
                    #(and NAs, where applicable)
                    mutate(Facebook = 
                               ifelse(is.na(Facebook),
                                      'NA',
                                      Facebook)) %>%
                    mutate(Twitter = 
                               case_when(Twitter == 1 ~ 'Yes',
                                         Twitter == 0 ~ 'No'),
                           Facebook =
                               case_when(Facebook == 1 ~ 'Yes',
                                         Facebook == 0 ~ 'No',
                                         Facebook == 'NA' ~ 'NA'))
                }else{
                  summary_data() %>%
                    filter(state_full == input$state_selector) %>%
                    #Drop unwanted columns
                    select(!c(fips,
                              place,
                              socmed)) %>%
                    #Rename columns to look neater/more professional
                    select(State = state_full,
                           County = name,
                           '2020 Population' = population,
                           !!str_to_title(input$metric) := n,
                           Facebook,
                           Twitter
                    ) %>%
                    #Drop rows w/ bad data
                    filter(!is.na(State)) %>%
                    #replace zeros and ones with nos and yeses 
                    #(and NAs, where applicable)
                    mutate(Facebook = 
                             ifelse(is.na(Facebook),
                                    'NA',
                                    Facebook)) %>%
                    mutate(Twitter = 
                             case_when(Twitter == 1 ~ 'Yes',
                                       Twitter == 0 ~ 'No'),
                           Facebook =
                             case_when(Facebook == 1 ~ 'Yes',
                                       Facebook == 0 ~ 'No',
                                       Facebook == 'NA' ~ 'NA'))
                })
        
        output$county_selector_table <- renderUI(
          selectizeInput('county_selector_table',
                         'County',
                         choices = c(sort(county_names_trend()$name))))
        
        similarity_table <-
          reactive({
          if(input$instate){
            summary_labels <-
              summary_data() %>%
              drop_na() %>%
              filter(state_full == input$state_selector)
            
            knn_data <-
              summary_data() %>%
              filter(state_full == input$state_selector) %>%
              select(!c(
                fips,
                name,
                state_full,
                place,
                socmed
              )) %>%
              drop_na() %>%
              mutate(
                population = 
                  range01(population),
                 n = 
                   range01(n),
                `Fully Vaccinated` = 
                  range01(`Fully Vaccinated`),
                `First Dose` = 
                  range01(`First Dose`),
                hispanic_prop = 
                  range01(hispanic_prop),
                white_prop = 
                  range01(white_prop),
                black_prop = 
                  range01(black_prop),
                asian_prop = 
                  range01(asian_prop)
              )
          }else{
            
            summary_labels <-
              summary_data() %>%
              drop_na()
            
            knn_data <-
              summary_data() %>%
              select(!c(
                fips,
                name,
                state_full,
                place,
                socmed
              )) %>%
              drop_na()}
            
            knn_model <-
              RANN::nn2(knn_data)
            knn_output <-
              as.data.frame(knn_model$nn.idx)
            knn_output <- 
              cbind.data.frame(summary_labels,
                               knn_output)
            knn_output = 
              knn_output %>%
              mutate(ID = row_number())
            
            target_row <-
              knn_output %>%
              filter(
                name == input$county_selector_table,
                state_full == input$state_selector
              )
            if(input$instate & input$state_selector == 'Delaware'){
                output_table <-
                  bind_rows(knn_output[knn_output$ID == target_row$V1,],
                            knn_output[knn_output$ID == target_row$V2,],
                            knn_output[knn_output$ID == target_row$V3,])
                output_table <-
                  output_table %>%
                  mutate('Similarity Ranking' = 
                           c('Selected County',
                             '1st',
                             '2nd'))
              }else{
                if(input$instate & input$state_selector == 'Rhode Island'){
                output_table <-
                  bind_rows(knn_output[knn_output$ID == target_row$V1,],
                            knn_output[knn_output$ID == target_row$V2,],
                            knn_output[knn_output$ID == target_row$V3,],
                            knn_output[knn_output$ID == target_row$V4,],
                            knn_output[knn_output$ID == target_row$V5,])
                output_table <-
                  output_table %>%
                  mutate('Similarity Ranking' = 
                           c('Selected County',
                             '1st',
                             '2nd',
                             '3rd',
                             '4th'))
              }else{
            
            output_table <-
              bind_rows(knn_output[knn_output$ID == target_row$V1,],
                        knn_output[knn_output$ID == target_row$V2,],
                        knn_output[knn_output$ID == target_row$V3,],
                        knn_output[knn_output$ID == target_row$V4,],
                        knn_output[knn_output$ID == target_row$V5,],
                        knn_output[knn_output$ID == target_row$V6,])
            output_table <-
              output_table %>%
              mutate('Similarity Ranking' = 
                       c('Selected County',
                         '1st',
                         '2nd',
                         '3rd',
                         '4th',
                         '5th'))
              }}
            output_table <-
              output_table %>%
              select(!c(fips,
                        state_full,
                        name,
                        Twitter,
                        Facebook
                        )) %>%
              select(
                County = place,
                `Similarity Ranking`,
                `Social Media` = socmed,
                !!str_to_title(input$metric) := n,
                Population = population,
                `Fully Vaccinated`,
                `First Dose`,
                `% Hispanic` = hispanic_prop,
                `% White` = white_prop,
                `% Black` = black_prop,
                `% Asian` = asian_prop
              )
          })
        
        output$similarity_table <-
          renderDataTable(
            options = list(scrollX = TRUE),
            similarity_table()
          )
        # Scatterplot Code ----------------------------------------------------
        
        #Create a reactive data set of county names based on chosen state
        county_names_trend <-
            reactive({
                summary_data() %>%
                    filter(state_full == input$state_selector) %>%
                drop_na()})
        
        #County selector for line plot, to render after state is selected
        output$county_selector_trend <- renderUI(
            selectizeInput('county_selector',
                           'County',
                           choices = c(sort(county_names_trend()$name)),
                           #Allow for multiple selections
                           multiple = TRUE,
                           #Cap the number of possible selections at 5
                           options = list(maxItems = 5)))
        
        #Select just the rows based on the desired counties
        scatterplot_data <-
            reactive({
                covid_adjusted() %>%
                    left_join(tw_fb_data,
                              by = 'fips') %>%
                    filter(state_full == input$state_selector &
                               name %in% input$county_selector)})
        
        #Create some reactive text explaining what's in the plot
        plot_text <-
            reactive({
                if(input$population_adjust){
                    paste0('Right now, the line plot below is showing COVID-19 ',
                           str_to_title(input$metric),
                           ', adjusted for population, from ',
                           input$date_range[1],
                           ' to ',
                           input$date_range[2],
                           ' in the selected counties in ',
                           input$state_selector,
                           ".")}
                else{
                    paste0('Right now, the line plot below is showing COVID-19 ',
                           str_to_title(input$metric),
                           ' from ',
                           input$date_range[1],
                           ' to ',
                           input$date_range[2],
                           ' in the selected counties in ',
                           input$state_selector,
                           ".")}})
        
        #Render the text
        output$plot_text <-
            renderText(
                plot_text())
        
        #Create a line plot based on the counties selected
        output$plot_output <-
            renderPlot({
                scatterplot_data() %>%
                    ggplot() +
                    geom_line(aes(x = date,
                                  y = n,
                                  group = name,
                                  color = name)) +
                    xlab('Date') +
                    scale_color_discrete(name = 'County') +
                    ylab('Incidence of COVID-19') +
                    ggtitle('County-Level Rates of COVID-19 Prevalence')+
                    theme_minimal()})
        
        
        
        # Social Media Map Data Processing ------------------------------------
        
        #Select a shapefile based on the input of the state selector on the
        #social media tab
        shapefile_socmed <-
            reactive({
                if(input$state_selector == 'Show All'){
                    us_counties
                }else{
                    us_counties %>%
                        filter(STNAME == input$state_selector)
                }
            })
        
        
        # Social Media Map Output ---------------------------------------------
        
        #Create and render an interactive map of the selected state
        output$socialmedia_map <-
            renderTmap(
                shapefile_socmed() %>%
                    left_join(summary_data(),
                              by = 'fips') %>%
                    tm_shape() +
                    #Polygon fill based on which social media the county public
                    #health department has
                    tm_polygons(col = 'socmed',
                                #Tooltip = county name and state abbreviation
                                id = 'place',
                                #Set legend title
                                title = 'Social Media') +
                    #Use shapefile slice to set bounding box
                    tm_view(bbox = st_bbox(shapefile_socmed())))    
        
        
        # Social Media Map Dynamic Text ---------------------------------------
        
        #Create some reactive text to go with the social media map
        #2 verions: U.S. vs single state
        #Text displays number of county public health departments w/ FB or Twitter
        #for chosen jurisdiction.
        socialmedia_text <-
            reactive({
                if(input$state_selector == 'Show All'){
                    paste0(nrow(tw_fb_data %>%
                                    filter(facebookYN == 1)), 
                           ' of the ',
                           nrow(tw_fb_data), 
                           ' county public health departments in the United States
                 have Facebook accounts. ',
                           nrow(tw_fb_data %>%
                                    filter(twitterYN == 1)), 
                           ' of the ',
                           nrow(tw_fb_data), 
                           ' county public health departments in the United States
                 have Twitter accounts.')
                } else{
                    paste0(nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_selector,
                                           facebookYN == 1)),
                           ' of the ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_selector)), 
                           ' county public health departments in ', 
                           input$state_selector,
                           ' have Facebook accounts. ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_selector,
                                           twitterYN == 1)),
                           ' of the ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_selector)), 
                           ' county public health departments in ',
                           input$state_selector,
                           ' have Twitter accounts.')}})
        
        #Render the above text
        output$socialmedia_text <-
            renderText(
                socialmedia_text())
        
        
    }

# knit and run app --------------------------------------------------------

shinyApp(ui, server)