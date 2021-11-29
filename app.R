
# setup -------------------------------------------------------------------
library(leaflet)
library(lubridate)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
options(scipen = 999)

# data --------------------------------------------------------------------

# My final project for this class is also centered on COVID-19, so I ended up
# using some similar elements to our practice in class in building this app.
# I did incorporate the data I've been collecting for my own project, and I 
# tried to make this as distinct from the in-class practice as I could,
# including a small change to the CSS.

# Read in covid data from New York Times:

# For this app, I'm using county-level data on cases and deaths.
# The master file is really large, so I'm using the 'recent' version,
# which just contains data for the past 30 days.
covid <-
    readr::read_csv(
        url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv')
    ) %>%
    # Pivot and rename the data
    pivot_longer('cases':'deaths',
                 names_to = 'metric',
                 values_to = 'n')

# Read in my project data from my project GitHub
tw_fb_data <-
    readr::read_csv(
        url('https://raw.githubusercontent.com/aadams149/ppol683_fall2021_project/main/data/counties_with_tweets.csv')
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

#Read in county population data
#Yes, this is the same file we used in class, I'm just excluding the 
#state totals instead of filtering to only them
population_counties <-
    readr::read_csv('data/raw/co-est2020.csv') %>%
    #Exclude state total populations
    filter(COUNTY != '000') %>%
    #Rename population for convenience
    rename('population' = POPESTIMATE2020) %>%
    #Create a fips column for easy merging
    mutate('fips' = paste0(STATE,COUNTY)) %>%
    select(fips, STNAME, population)


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

#Set tmap mode
tmap_mode('view')

# user interface ----------------------------------------------------------

#I kept the code which instantiates the dashboard and sidebar,
#but I added new tabs and changed the content of the existing tabs.

ui <- 
    dashboardPage(
        
        dashboardHeader(title = 'Social Media and Public Health'),
        
        dashboardSidebar(
            
            dateRangeInput(
                inputId = 'date_range',
                label = 'Select a range of dates:',
                start = min(covid$date),
                end = max(covid$date)),
            
            radioButtons(
                inputId = 'metric',
                label = 'View:',
                choiceNames = c('Cases', 'Deaths'),
                choiceValues = c('cases', 'deaths')),
            
            checkboxInput(
                inputId = 'population_adjust',
                label = 'Adjust for population?'),
            
            sidebarMenu(
                menuItem('Map',
                         icon = icon('map'),
                         tabName = 'maps'),
                
                menuItem('Table',
                         icon = icon('table'),
                         tabName = 'tables'),
                
                menuItem('Trend',
                         icon = icon('chart-line'),
                         tabName = 'charts'),
                
                # Add two new tabs:
                #     'Social Media': For displaying a map of county health department
                #                     social media accounts
                #     'Help':         Basically an FAQ
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
                    tabName = 'maps',
                    #Create a header
                    h2('Map of COVID-19 Outcomes'),
                    #Create a drop down input for selecting which state to show
                    selectInput(
                        inputId = 'state_map',
                        label = 'State',
                        choices = c('Show all',
                                    sort(population_counties$STNAME))),
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
                    dataTableOutput(outputId = 'summary_table')),
                
                # Scatterplot Tab -----------------------------------------------------
                
                
                tabItem(
                    tabName = 'charts',
                    #Create appropriate header
                    h2('Trends in COVID-19 Incidence'),
                    #Create drop down selector for state
                    selectInput(
                        inputId = 'state_select',
                        label = 'State',
                        choices = c(sort(population_counties$STNAME))),
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
                    #Create selector for state
                    selectInput(
                        inputId = 'state_socmed',
                        label = 'State',
                        choices = c('Show all',
                                    sort(population_counties$STNAME))),
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
               "Show all" in the drop-down menu will display a map of the 
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
                         county name. Select "Show all" to see a map of the
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
        
        
        
        # Code for COVID Map Output -------------------------------------------
        
        #This section contains all the code necessary to filter and instantiate
        #reactive data for the map on the 'Map' tab.
        
        #First, filter the COVID-19 data by metric and date range
        covid_filtered <-
            reactive({
                covid %>%
                    filter(metric == input$metric,
                           date >= input$date_range[1],
                           date <= input$date_range[2]) %>%
                    select(!metric)
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
                covid_adjusted() %>%
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
                              by = 'fips') %>%
                    #Remove cases where data is recorded for units other than county
                    filter(!is.na(fips))
            })
        
        #Filter the summary data if the option selected is anything other than
        #"Show all" (this is only used for the dynamic text which accompanies
        #the map)
        summary_covidmap_subset <-
            reactive({
                if(input$state_map != 'Show all'){
                    summary_data() %>%
                        filter(state_full == input$state_map)
                }
            })
        
        #Filter the shapefile based on the selected state
        shapefile_covid <-
            reactive({
                if(input$state_map == 'Show all'){
                    us_counties
                }else{
                    us_counties %>%
                        filter(STNAME == input$state_map)
                }
            })
        
        
        
        # COVID Map Text ------------------------------------------------------
        
        #Create some reactive text to specify what the map is showing.
        #4 versions: U.S. vs specific state, Pop_adjust vs not
        covidmap_text <-
            reactive({
                if(input$state_map == 'Show all'){
                    if(input$population_adjust){
                        paste0('Between ',
                               input$date_range[1],
                               ' and ',
                               input$date_range[2],
                               ", ",
                               summary_data()$place[which.max(summary_data()$n)],
                               ' (Population: ',
                               summary_data()$population[which.max(summary_data()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States, adjusted for population. ',
                               summary_data()$place[which.min(summary_data()$n)],
                               ' (Population: ',
                               summary_data()$population[which.min(summary_data()$n)],
                               ') ',
                               ' had the lowest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States, adjusted for population.')}
                    else{
                        paste0('Between ',
                               input$date_range[1],
                               ' and ',
                               input$date_range[2],
                               ", ",
                               summary_data()$place[which.max(summary_data()$n)],
                               ' (Population: ',
                               summary_data()$population[which.max(summary_data()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States. ',
                               summary_data()$place[which.min(summary_data()$n)],
                               ' (Population: ',
                               summary_data()$population[which.min(summary_data()$n)],
                               ') ',
                               ' had the lowest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in the United States.') 
                    }
                } else {
                    if(input$population_adjust){
                        paste0('Between ',
                               input$date_range[1],
                               ' and ',
                               input$date_range[2],
                               ", ",
                               summary_covidmap_subset()$name[which.max(
                                   summary_covidmap_subset()$n)],
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.max(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_map,
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
                               input$state_map,
                               ', adjusted for population.')}
                    else{
                        paste0('Between ',
                               input$date_range[1],
                               ' and ',
                               input$date_range[2],
                               ", ",
                               summary_covidmap_subset()$name[which.max(
                                   summary_covidmap_subset()$n)],
                               ' (Population: ',
                               summary_covidmap_subset()$population[which.max(
                                   summary_covidmap_subset()$n)],
                               ') ',
                               ' had the highest rate of COVID-19 ',
                               tolower(input$metric),
                               ' in ',
                               input$state_map,
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
                               input$state_map,'.')
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
                #Use the summary data as a base
                summary_data() %>%
                    #Drop unwanted columns
                    select(!c(fips,
                              place,
                              socmed,
                              STNAME)) %>%
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
                                         Facebook == 'NA' ~ 'NA')))
        
        
        # Scatterplot Code ----------------------------------------------------
        
        #Create a reactive data set of county names based on chosen state
        county_names_trend <-
            reactive({
                tw_fb_data %>%
                    filter(state_full == input$state_select)})
        
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
                    filter(state_full == input$state_select &
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
                           input$state_select,
                           ".")}
                else{
                    paste0('Right now, the line plot below is showing COVID-19 ',
                           str_to_title(input$metric),
                           ' from ',
                           input$date_range[1],
                           ' to ',
                           input$date_range[2],
                           ' in the selected counties in ',
                           input$state_select,
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
                if(input$state_socmed == 'Show all'){
                    us_counties
                }else{
                    us_counties %>%
                        filter(STNAME == input$state_socmed)
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
                if(input$state_socmed == 'Show all'){
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
                                    filter(state_full == input$state_socmed,
                                           facebookYN == 1)),
                           ' of the ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_socmed)), 
                           ' county public health departments in ', 
                           input$state_socmed,
                           ' have Facebook accounts. ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_socmed,
                                           twitterYN == 1)),
                           ' of the ',
                           nrow(tw_fb_data %>% 
                                    filter(state_full == input$state_socmed)), 
                           ' county public health departments in ',
                           input$state_socmed,
                           ' have Twitter accounts. ')}})
        
        #Render the above text
        output$socialmedia_text <-
            renderText(
                socialmedia_text())
        
        
    }

# knit and run app --------------------------------------------------------

shinyApp(ui, server)