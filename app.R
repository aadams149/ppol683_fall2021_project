
# setup -------------------------------------------------------------------
library(leaflet)
library(lubridate)
library(plotly)
library(RANN)
library(sf)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tmap)
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

#I'm limiting the date range to end at Dec 14 2021 so I can incorporate
#vaccination data. Retrieving it from the web through the API is 
#painfully slow (see section on reading in vaccine data).
covid <-
    vroom(
      'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
    ) %>%
  filter(date <= '2021-12-14')

vax_data <-
  vroom('data/raw/vax_data.csv') %>%
  mutate(date = 
           lubridate::mdy(date))

#Read in data for multicounty districts
covid_district_data <-
  vroom('data/raw/covid_district_data.csv') %>%
  mutate(
    fips =
      as.character(fips)
  )

#Left join into covid data, treat as metric
covid <-
  covid %>%
  left_join(
    vax_data,
    by = c('fips' = 'fips',
           'date' = 'date')
  ) %>%
  bind_rows(
    covid_district_data
  ) %>%
  select(
    date:deaths,
    Series_Complete_Yes,
    Administered_Dose1_Recip
  ) %>%
  # Pivot and rename the data
  pivot_longer('cases':'Administered_Dose1_Recip',
               names_to = 'metric',
               values_to = 'n') %>%
  #Change metric names to make reactive text work better
  mutate(metric = 
           case_when(metric == 'Series_Complete_Yes' ~ 'completed vaccinations',
                     metric == 'Administered_Dose1_Recip' ~ 'first doses received',
                     metric %in% c('cases','deaths') ~ metric))

covid$n[is.na(covid$n)] <- 0

rm(vax_data)
rm(covid_district_data)

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

tw_fb_data_districts <-
  vroom('data/raw/mc_district_socmed_data.csv') %>%
  mutate(fips = 
           as.character(fips))

tw_fb_data <-
  tw_fb_data %>%
  bind_rows(
    tw_fb_data_districts
  ) %>%
  mutate(`Most Recent Tweet` =
           case_when(!is.na(tweet) ~ paste0("<a href = ",
                                            link,
                                            "/>",
                                            name,
                                            " (",
                                            tweet_date,
                                            ")",
                                            "</a>"),
                     is.na(tweet) ~ 'No Available Tweets'))


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
    select(fips, 
           STNAME,
           population)

district_pops <-
  tw_fb_data_districts %>%
  select(
    fips,
    STNAME = state_full,
    population
  )

population_counties <-
  bind_rows(
    population_counties,
    district_pops
  )
#Read in vaccination data from CDC API
#(I wanted to have the app retrieve data from the API, but
#it takes so long to read in that I just manually downloaded the file
#and subset to relevant columns, and I've decided to have the app only
#use data from Jan 2020 to Dec 14 2021 to make things easier.)
#This data contains all relevant columns for this app.

# Read in shapefiles:

#Side note: thank you for providing the U.S. county shapefile!
#I was going to go try and track one down, so thank you for saving me
#the trouble. This is the version I modified 
us_counties <-
    st_read('data/spatial/counties_with_mc_districts_small.shp') %>%
    #Rename GEOID to fips for clarity
    rename(fips = GEOID) %>%
    #Transform to 4326 so it plays nice w/ tmap
    st_transform(crs = 4326)

#Define rescaling function
range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#Read in Moran's I table
moran_I <-
  readr::read_csv(
    'data/raw/moran_i.csv'
  )

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
            label = 'Display:',
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
              value = max(covid$date),
              min = min(covid$date),
              max = max(covid$date))
          ),
          conditionalPanel(
            condition = "input.date_or_daterange == 'daterange'",
             dateRangeInput(
             inputId = 'date_range',
               label = 'Select a range of dates:',
               start = min(covid$date),
               min = min(covid$date),
               end = max(covid$date),
               max = max(covid$date))
          ),
          radioButtons(
            inputId = 'counties_districts',
            label = 'Geography:',
            choiceNames = c('Counties Only',
                            'Counties and Multi-County Districts'),
            choiceValues = c('counties',
                             'both')
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
                                 'completed vaccinations',
                                 'first doses received')),
            
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
                           visualizations.'),
                  helpText('You can adjust the input values
                           using the control widgets on the sidebar and in
                           the tabs. Some visualizations require certain inputs
                           to work properly. For more information about any of the
                           tabs or visualizations included here, click on
                           the "Help" tab.'),
                  hr(),
                  helpText("The central premise of this project is to investigate and
                           consider a possible relationship between county health departments
                           in the United States having social media accounts on major
                           platforms and those counties' outcomes during the COVID-19
                           pandemic. This premise is derived from the convergence of
                           multiple ideas in academic literature, namely:"),
                  hr(),
                  helpText("-As of 2020, there is a 20-point gap between Americans'
                           trust in their local government and trust in the federal
                           government."),
                  hr(),         
                  helpText("-With each year, a larger share of Americans (particularly
                           those under 30) state that they receive some, most, or all
                           of their news through social media rather than traditional
                           media platforms."),
                  hr(),         
                  helpText("-People are more likely to respond to public health messaging
                           on social media when that messaging comes from an organization,
                           rather than an individual."),
                  hr(),
                  helpText("Local health departments (often administered at the county level)
                           present the ideal venue for this investigation. Since they are local
                           agencies, they are more immediately present in their communities than
                           federal or state-level departments, and are likely trusted more on 
                           average than those departments. Their social media communications
                           are also typically sent from organization-level accounts, rather 
                           than by individual messengers. I examined four different metrics
                           related to COVID-19: cases, deaths, completed vaccinations (meaning
                           people who received a full multi-course sequence of the Pfizer or Moderna
                           vaccines, or one dose of the Johnson and Johnson vaccine), and first
                           vaccine doses administered. After adjusting for population, I find that
                           counties whose health departments have Twitter accounts have slightly
                           lower rates of COVID-19 cases and deaths and slightly higher rates of
                           completed vaccinations and first doses administered than counties
                           whose health departments do not have Twitter accounts. The effect of
                           a health department having a Facebook account is not statistically 
                           significant and the effect size is almost zero. County-level COVID-19
                           outcomes are spatially autocorrelated; the p-values of the Moran's I
                           statistic for all four metrics used in this project are extremely
                           close to zero:"),
                  tableOutput('moran_I'),
                  helpText('The intent of this analysis is not to suggest a causal link
                           between local health departments having social media 
                           presences and their constituents experiencing better health
                           outcomes, but rather to identify if there is an association
                           between the two. Future exploration could center on factors
                           such as age, since the current literature suggests that younger
                           people are more likely to use social media for news than
                           older people. The presence of consistent spatial autocorrelation
                           also indicates that state-level COVID-19 policies may be 
                           associated with differential outcomes. Additionally,
                           a county health department having a social media account 
                           may be a proxy or instrumental variable for a different factor.
                           However, such considerations are beyond the scope of
                           this project at this stage.'),
                  hr(),
                  helpText('Enjoy using this app! To get started, I recommend
                           selecting a date using the date widget and viewing 
                           one of the maps.')
                ),
                
                #COVID-19 metrics map tab----
                tabItem(
                    tabName = 'maps',
                    #Create a header
                    h2('Map of COVID-19 Outcomes'),
                    #Display some dynamic text about which county in the selected
                    #jurisdiction has the highest/lowest incidence of the selected
                    #metric for the specified interval
                    textOutput('covidmap_text'),
                    #Display a map of the desired COVID indicator, state, and interval
                    tmapOutput(outputId = 'covid_map',
                                 height = '800px')
                    ),
                
                #Summary Table Tab ---------------------------------------------------
                
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
                                   "Similarity Table" section on the
                                   "Visualizations" page in the "Help" tab.'
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
                
                #County-level lineplot Tab --------------------------------------------
                
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
                    plotOutput(outputId = 'plot_output',
                               height = '800px')),
                
                # Social Media Map tab ------------------------------------------------
                
                
                tabItem(
                    tabName = 'socialmedia',
                    h2('County Public Health Department Social Media Accounts'),
                    #Display text indicating number of counties w/ FB and/or Twitter
                    tabsetPanel(
                      tabPanel('Map',
                               helpText(
                                 textOutput(
                                   'socialmedia_map_text'
                                 )
                               ),
                               tmapOutput(outputId = 'socialmedia_map',
                                          height = '800px')
                               
                               ),
                      tabPanel('Line Plot',
                               helpText(
                                 textOutput(
                                   'socmed_lineplot_text'
                                 )
                               ),
                               plotlyOutput(
                                 'socmed_lineplot',
                                 height = '800px'
                               ))
                    )
                ),
                
                # Help tab ------------------------------------------------------------
                #This tab contains a tab subset which provides information for each of 
                #the other tabs, as well as some general information for the app
                #and some relevant links.
                tabItem(
                    tabName = 'help',
                    h2('Help'),
                    
                    tabsetPanel(
                      #Widget inputs-----
                        tabPanel('Sidebar Inputs',
                                 helpText(
                                     h5(
                                         'This web app is intended to allow users to 
                         visualize data related to COVID-19 and public
                         health departments across the United States.'),
                                     hr(),
                                     h3('Display'),
                                     h5('The "Display" radio buttons allow
                                        for two types of views. Selecting
                                        the "Cumulative" option will cause
                                        the map on the "Maps" tab to display
                                        cumulative data as of the selected 
                                        date. Selecting "Total Over Selected
                                        Date Range" will switch the date
                                        widget to a date range input, and
                                        will cause the map on the "Maps"
                                        tab to show the change between 
                                        the selected dates.'),
                                     h5('The "Display" radio buttons also affect
                                        the line plots on the "Trends" page and the
                                        "Lineplot" tab of the "Social Media" page. 
                                        Selecting a date range will cause the plot to 
                                        update to that specific date range. If the
                                        "Cumulative" option is selected, the line
                                        plots will show the full range of data,
                                        from January 21, 2020 to December 14, 2021.'),
                                     hr(),
                                     h3('Geograpahy'),
                                     h5('The "Geography" widget allows for
                                        two different types of views. Selecting
                                        "Counties Only" will cause the maps
                                        on the "Maps" and "Social Media" tabs
                                        to show maps of U.S. counties. Selecting
                                        "Counties and Multi-County Districts" will
                                        switch the view so that counties which
                                        are part of multi-county public health
                                        districts will be replaced by those districts. 
                                        This widget does not affect any other visualizations
                                        in the app.'),
                                     hr(),
                                     h3('Select a state'),
                                     h5("The 'Select a state' drop-down menu
                                        lets users choose which U.S. state to display
                                        in the visualizations. This widget affects
                                        every visualization and virtually all of the reactive
                                        text in this app. Selecting a state will cause the maps
                                        to display maps of that specific state, filter the 
                                        table on the 'Master Table' tab of the 'Table' page
                                        to just show counties in that state, populate the 
                                        county selector drop down on the trends tab with the
                                        names of that state's counties, and filter the 
                                        counties used to calculate the data for the line plot
                                        in the social media page to just those within the selected
                                        state."),
                                     hr(),
                                     h3('View'),
                                     h5('The "View" radio buttons let users switch
                                        between different COVID-19 metrics. "Cases" will
                                        cause maps and plots to show COVID-19 cases, "Deaths"
                                        will show deaths, "Fully Vaccinated" will show rates
                                        of completed vaccination sequences (two doses of either the 
                                        Pfizer or Moderna vaccines, or one dose of the Johnson and Johnson
                                        vaccine), and "At Least 1 Vaccine Dose" will show rates of first
                                        doses administered.'),
                                     hr(),
                                     h3('Adjust for population?'),
                                     h5('When the "Adjust for population?" checkbox is checked, 
                                        the data for the selected COVID-19 metric (chosen by the
                                        "View" radio buttons) is divided by the population of 
                                        that county or multi-county district.'))),
                                 #data sources ----
                                 tabPanel(
                                   'Data Sources',
                                   h4('The data on COVID-19 cases and deaths
                                      used in this project was retrieved from the
                                      New York Times COVID-19 Data GitHub. The
                                      data on vaccination rates was retrieved
                                      from the Centers for Disease Control and 
                                      Prevention Socrata API. The data on 
                                      county-level social media accounts was
                                      collected by Alexander Adams, and is
                                      available at his GitHub. The data on
                                      race used in the similarity tables was
                                      retrieved from the 2020 U.S. Census
                                      through the U.S. Census Bureau. The data on
                                      county population was also retrieved from the
                                      2020 U.S. Census through the U.S. Census Bureau.
                                      The shapefile used to create this app was originally
                                      retrieved using the `tigris` package in R, 
                                      and was then modified and reduced in size by
                                      Alexander Adams. Links to these data sources
                                      are available on the "Links" tab.')
                                   
                                 ),
                        #explain each tab----
                        tabPanel('Visualizations',
                                 h2('This tab contains explanations of
                                    each of the visualizations available 
                                    through the pages on the sidebar.'),
                                 hr(),
                                 h3('Map'),
                                 h5('The "Map" tab displays a map of 
                                    COVID-19 outcomes at the county level.
                                    The specific view can be customized using
                                    the Display, date input, Geography,
                                    state selector, View, and Adjust for 
                                    Population widgets. Moving your cursor
                                    over an area on the map will show the
                                    name of that area, and clicking on it
                                    will show the value of the selected
                                    COVID-19 metric, based on the input
                                    from the sidebar widgets. If the
                                    map is showing multi-county health
                                    districts, then clicking on a district
                                    will show which counties or county-equivalent
                                    jurisdictions make up that district.'),
                                 hr(),
                                 h3('Table'),
                                 h4('Master Table'),
                                 h5("The table on the 'Master Table' tab
                                    shows data for each county, including
                                    the state, county name, population,
                                    if that county's health department has a
                                    Facebook account or Twitter account,
                                    and the metric selected using the 'View'
                                    and 'Adjust for population' inputs. If
                                    a state has been selected, the table will
                                    only show counties in that state. The table
                                    does not show multi-county health departments.
                                    The table is both searchable and sortable,
                                    and the number of counties displayed on 
                                    each page can be adjusted using the drop-down
                                    at the top."),
                                 h4('Similarity Table'),
                                 h5("The table on the 'Similarity Table' page is
                                 only visible if a state has been selected using
                                 the state selector drop-down. The county selector
                                    drop-down will populate with the list of counties
                                    in that particular state, and default to the
                                    first one alphabetically. The table will
                                    show which 5 counties are most similar to the 
                                    selected county. This similarity is calculated 
                                    using the `k-nearest neighbors` algorithm,
                                    implemented in the `RANN` package. While
                                    k-nearest-neighbors usually classifies observations
                                    based on similarity to other data, this 
                                    version of the algorithm identifies which
                                    other points (or in this case, counties) are most
                                    similar to the selected county. The variables
                                    used to compute this similarity are population,
                                    the COVID-19 metric selected on the sidebar,
                                    and the racial makeup of the county. All variables
                                    are rescaled to be between zero and one prior to 
                                    calculation. Checking the 'Within State' checkbox
                                    will limit the app to finding the most similar counties
                                    within the selected state (i.e. if the selected county is
                                    Brevard County, Florida, checking the 'Within State' box
                                    will cause the table to display the Florida counties most similar
                                    to Brevard, rather than the counties from across the entire
                                    United States). Like the master table, the similarity table
                                    is searchable and sortable."),
                                 hr(),
                                 h3('Trend'),
                                 h5('This tab shows a line plot of the selected COVID-19 metric.
                                    The plot will only appear if a state has been selected on
                                    the state selector on the sidebar. The county selector
                                    will then populate with the names of counties in that state.
                                    Users can select up to five counties. Data for those counties will
                                    be plotted on the line graph. Users can delete and add counties to the
                                    plot at will, and the plot will update after each addition or removal.'),
                                 h5('Warning: This visualization may take time to load, and will likely be slower
                                    than other elements of the app.'),
                                 hr(),
                                 h3('Social Media'),
                                 h4('Map'),
                                 h5('This tab shows a map of the selected state (or the entire
                                    country, if the "Show All" option is chosen in the state selector).
                                    If the selected geography is "Counties Only", then the map will show
                                    health department social media presence for counties in the jurisdiction.
                                    Moving your cursor over a county will show the county name, and clicking
                                    on it will show which social media accounts exist for the county health
                                    department. If the department has a Twitter account, and tweets were able
                                    to be scraped, the pop-up will also include a link to the most recent 
                                    tweet from the account. If the selected geography includes multi-county
                                    health districts, the map will show those districts instead of their
                                    constituent counties. Clicking on a district will show its name 
                                    and the counties it encompasses, as well as the social media accounts
                                    for the health district and a link to the most recent tweet, if available.
                                    For examples of multi-county public health districts, I recommend selecting
                                    Idaho, Kentucky, Nebraska, Utah, or Virginia as the state, though there
                                    are many other states with such districts.'),
                                 h4('Line Plot'),
                                 h5('This tab shows an interactive line plot of the selected COVID-19 metric
                                    for counties in the selected state, grouped by health department social
                                    media. Each line represents a category (e.g. counties whose health departments have 
                                    a Facebook page but not a Twitter account, or counties whose health departments
                                    have neither), and the y-values are the mean of the selected metric for all
                                    counties in that category in the selected state during the selected date interval.
                                    The slider at the bottom can be used to view different ranges of the data. Clicking
                                    on a legend element will hide that line on the plot.')
                                 ),             
                        #links tab----
                        tabPanel('Links',
                                 tags$a(href = 'https://github.com/nytimes/covid-19-data',
                                        'New York Times COVID-19 Data GitHub'),
                                 hr(),
                                 tags$a(href = 'https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh',
                                        'County-Level Vaccination Data, Centers for Disease Control and Prevention'),
                                 hr(),
                                 tags$a(href = 'https://data.census.gov/cedsci/table?q=2020%20redistricting%20data&g=0100000US%240500000&tid=DECENNIALPL2020.P1',
                                        '2020 County-Level Redistricting Data, U.S. Census Bureau'),
                                 hr(),
                                 tags$a(href = 'https://github.com/aadams149',
                                        "Alex's GitHub"),
                                 hr(),
                                 tags$a(href = 'https://fonts.googleapis.com/css2?family=Open+Sans&family=Source+Sans+3:wght@300&display=swap',
                                        'Get the font used in this app'))))
                
            )
        )
    )

# server ------------------------------------------------------------------

server <- 
    function(input, output) { 
      

        # Render Moran's I summary table --------------------------------------

        output$moran_I <-
          renderTable({
            moran_I
          })
        
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
                                   n / population) %>%
                    #Some counties have weird, likely erroneous data
                        mutate(n = 
                                 case_when(n >= 1 ~ 1,
                                           n < 1 ~ n))
                } else {
                    covid_filtered()
                }
            })
        
        #Create a summary table showing cumulative change over the selected
        #interval -----
        summary_data <-
            reactive({
              #Cumulative vs date range
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
                           n = n,
                           `Most Recent Tweet`
                    ) %>%
                    #Add in population data
                    left_join(population_counties,
                              by = c('fips' = 'fips',
                                     'state_full' = 'STNAME')) %>%
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
        
        # Data table text -----
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
        
        #COVID map table text ----
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
              if(input$state_selector == 'Show All') {
                us_counties1 <-
                  us_counties
              } else{
                us_counties1 <-
                  us_counties %>%
                  filter(STNAME == input$state_selector)
              }
              if (input$counties_districts == 'counties') {
                us_counties2 <-
                  us_counties1 %>%
                  filter(is_dstr != 1)
              }
              if (input$counties_districts == 'both') {
                us_counties2 <-
                  us_counties1 %>%
                  filter(in_dstr != 1)
                
              }
              
              us_counties2
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
                                id = 'name',
                                popup.vars = c('Jurisdiction: ' = 'place',
                                               'Rate: ' = 'n'),
                                #Legend title = currently active metric
                                title = str_to_title(input$metric),
                                alpha = 0.4) +
                    #Use filtered shapefile to set bbox (sorry Alaska)
                    tm_view(bbox = st_bbox(shapefile_covid())))
        
        # Interactive Data Table Code -----------------------------------------
        #Summary table master----
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
        #County selector UI----
        output$county_selector_table <- renderUI(
          selectizeInput('county_selector_table',
                         'County',
                         choices = c(sort(county_names_trend()$name))))
        #Similarity table----
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
                socmed,
                `Most Recent Tweet`
              )) %>%
              drop_na() %>%
              mutate(
                population = 
                  range01(population),
                 n = 
                   range01(n),
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
                socmed,
                `Most Recent Tweet`
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
        # Lineplot Code ----------------------------------------------------
        
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
        lineplot_data <-
          reactive({
            if (input$date_or_daterange == 'daterange') {
              if ((!is.na(input$date_range[1]) & (!is.na(input$date_range[2])))) {
                if (input$state_selector != 'Show All') {
                  covid %>%
                    left_join(tw_fb_data,
                              by = 'fips') %>%
                    filter(
                      state_full == input$state_selector,
                      metric == input$metric,
                      date >= input$date_range[1],
                      date <= input$date_range[2]
                    )
                } else{
                  covid %>%
                    left_join(tw_fb_data,
                              by = 'fips') %>%
                    filter(metric == input$metric,
                           date >= input$date_range[1],
                           date <= input$date_range[2])
                }
              }
            }
            else{
              if (input$state_selector != 'Show All') {
                covid %>%
                  left_join(tw_fb_data,
                            by = 'fips') %>%
                  filter(
                    state_full == input$state_selector,
                    metric == input$metric,
                    date <= input$single_date
                  )
              } else{
                covid %>%
                  left_join(tw_fb_data,
                            by = 'fips') %>%
                  filter(metric == input$metric,
                         date <= input$single_date)
              }
            }
          })
        trend_plot_data <-
          reactive({
            lineplot_data() %>%
              filter(name %in% input$county_selector)
          })
        #Create some reactive text explaining what's in the plot----
        plot_text <-
            reactive({
                if(input$population_adjust){
                  if((!is.na(input$date_range[1]) & (!is.na(input$date_range[2])))){
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
                           ', adjusted for population,
                           from January 21, 2020 to 
                           December 14, 2021, 
                           in the selected counties in ',
                           input$state_selector,
                           ".")
                  }}
                else{
                  if((!is.na(input$date_range[1]) & (!is.na(input$date_range[2])))){
                    paste0('Right now, the line plot below is showing COVID-19 ',
                           str_to_title(input$metric),
                           ' from ',
                           input$date_range[1],
                           ' to ',
                           input$date_range[2],
                           ' in the selected counties in ',
                           input$state_selector,
                           ".")}
                  else{
                    paste0('Right now, the line plot below is showing COVID-19 ',
                           str_to_title(input$metric),
                           'from January 21, 2020 to 
                           December 14, 2021, 
                           in the selected counties in ',
                           input$state_selector,
                           ".")
                  }}})
        
        #Render the text
        output$plot_text <-
            renderText(
                plot_text())
        
        #Create a line plot based on the counties selected
        output$plot_output <-
            renderPlot({
                trend_plot_data() %>%
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
        
        # Social Media Map Output ---------------------------------------------
        
        #Create and render an interactive map of the selected state
        output$socialmedia_map <-
            renderTmap(
                shapefile_covid() %>%
                    left_join(tw_fb_data,
                              by = 'fips') %>%
                    tm_shape() +
                    #Polygon fill based on which social media the county public
                    #health department has
                    tm_polygons(col = 'socmed',
                                #Tooltip = county name and state abbreviation
                                id = 'name',
                                popup.vars = c('Jurisdiction: ' = 'place',
                                               'Social Media: ' = 'socmed',
                                               'Most Recent Tweet: ' = 'Most Recent Tweet'),
                                popup.format = list(html.escape = F),
                                #Set legend title
                                title = 'Social Media',
                                alpha = 0.4) +
                    #Use shapefile slice to set bounding box
                    tm_view(bbox = st_bbox(shapefile_covid()))) 
        
        # Social Media Lineplot Data and Plot----------------------------------
        socmed_lineplot_data <-
          reactive({
         
             if(input$population_adjust){
               lineplot_df <-
                 lineplot_data() %>%
                 mutate(n =
                          n/population)
             }else{
               lineplot_df <-
                 lineplot_data()
             }
            
            lineplot_df %>%
              group_by(date,
                       socmed) %>%
              summarise(metric =
                          mean(n, na.rm = TRUE)) %>%
              ungroup() %>%
              drop_na()
              
          })
        #social media lineplot----
        output$socmed_lineplot <-
          renderPlotly(
            socmed_lineplot_data() %>% 
              plot_ly(
              type = 'scatter',
              mode = 'lines',
              x = ~date, 
              y = ~metric, 
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
                  title = paste('Cumulative Avg. ',
                                str_to_title(input$metric),
                                ' Rate'
                                )
                ))
            
          )
        
        # Social Media Map Dynamic Text ---------------------------------------
        
        #Create some reactive text to go with the social media map
        #2 verions: U.S. vs single state
        #Text displays number of county public health departments w/ FB or Twitter
        #for chosen jurisdiction.
        socialmedia_text <-
            reactive({
                if(input$state_selector == 'Show All'){
                    paste0(nrow(tw_fb_data %>%
                                    filter(socmed == 'Both Facebook and Twitter',
                                           fips < 99000)), 
                           ' of the ',
                           nrow(tw_fb_data %>%
                                  filter(fips < 99000)), 
                           ' county public health departments in the United States
                 have both Facebook and Twitter accounts. ',
                           nrow(tw_fb_data %>%
                                  filter(socmed == 'Facebook but no Twitter',
                                         fips < 99000)), 
                           ' of the ',
                           nrow(tw_fb_data %>%
                                  filter(fips < 99000)), 
                           ' county public health departments in the United States
                 have Facebook accounts only. ',
                           nrow(tw_fb_data %>%
                                  filter(socmed == 'Twitter but no Facebook',
                                         fips < 99000)),
                          ' of the ',
                          nrow(tw_fb_data %>%
                                 filter(fips < 99000)),
                          ' county public health departments in the United States
                 have Twitter accounts only. ',
                          nrow(tw_fb_data %>%
                                 filter(socmed == 'No Facebook of Twitter',
                                        fips < 99000)),
                          ' of the ',
                          nrow(tw_fb_data %>%
                                 filter(fips < 99000)),
                          ' county public health departments in the United States
                 do not have a Facebook or Twitter account.'
                          )
                } else{
                  paste0(nrow(tw_fb_data %>%
                                filter(socmed == 'Both Facebook and Twitter',
                                       fips < 99000,
                                       state_full == input$state_selector)), 
                         ' of the ',
                         nrow(tw_fb_data %>%
                                filter(fips < 99000,
                                       state_full == input$state_selector)), 
                         ' county public health departments in the United States
                 have both Facebook and Twitter accounts. ',
                         nrow(tw_fb_data %>%
                                filter(socmed == 'Facebook but no Twitter',
                                       fips < 99000,
                                       state_full == input$state_selector)), 
                         ' of the ',
                         nrow(tw_fb_data %>%
                                filter(fips < 99000,
                                       state_full == input$state_selector)), 
                         ' county public health departments in the United States
                 have Facebook accounts only. ',
                         nrow(tw_fb_data %>%
                                filter(socmed == 'Twitter but no Facebook',
                                       fips < 99000,
                                       state_full == input$state_selector)),
                         ' of the ',
                         nrow(tw_fb_data %>%
                                filter(fips < 99000,
                                       state_full == input$state_selector)),
                         ' county public health departments in the United States
                 have Twitter accounts only. ',
                         nrow(tw_fb_data %>%
                                filter(socmed == 'No Facebook of Twitter',
                                       fips < 99000,
                                       state_full == input$state_selector)),
                         ' of the ',
                         nrow(tw_fb_data %>%
                                filter(fips < 99000,
                                       state_full == input$state_selector)),
                         ' county public health departments in the United States
                 do not have a Facebook or Twitter account.'
                  )
                  }})
        
        #Render the above text
        output$socialmedia_map_text <-
            renderText(
                socialmedia_text())
        
        #Social Media lineplot dynamic text
        socmed_lineplot_text <-
          reactive({
            if(input$state_selector == 'Show All'){
              if((!is.na(input$date_range[1]) & (!is.na(input$date_range[2])))){
              paste0(
                'The plot below shows rates of COVID-19 ',
                input$metric,
                " across the United States between ",
                input$date_range[1],
                ' and ',
                input$date_range[2],
                ". Each line represents a category of 
                county, based on which social media accounts that county's
                health department has. The y-axis values are the mean of 
                the selected metric for all counties in that category across
                the United States. You can click and drag the slider at the bottom
                to explore the data. Clicking on one of the items in the legend
                will hide that line on the plot."
              )}else{
                paste0(
                  'The plot below shows rates of COVID-19 ',
                  input$metric,
                  " across the United States between January 21, 2020 and 
                  December 14, 2021. Each line represents a category of 
                county, based on which social media accounts that county's
                health department has. The y-axis values are the mean of 
                the selected metric for all counties in that category across
                the United States. You can click and drag the slider at the bottom
                to explore the data. Clicking on one of the items in the legend
                will hide that line on the plot."
                )
              }
            }else{
              if((!is.na(input$date_range[1]) & (!is.na(input$date_range[2])))){
                paste0(
                  'The plot below shows rates of COVID-19 ',
                  input$metric,
                  " across the United States between ",
                  input$date_range[1],
                  ' and ',
                  input$date_range[2],
                  ". Each line represents a category of 
                county, based on which social media accounts that county's
                health department has. The y-axis values are the mean of 
                the selected metric for all counties in that category in ",
                input$state_selector,
                '. You can click and drag the slider at the bottom
                to explore the data. Clicking on one of the items in the legend
                will hide that line on the plot.'
                )}else{
                  paste0(
                    'The plot below shows rates of COVID-19 ',
                    input$metric,
                    " across the United States between January 21, 2020 and 
                  December 14, 2021. Each line represents a category of 
                county, based on which social media accounts that county's
                health department has. The y-axis values are the mean of 
                the selected metric for all counties in that category in ",
                    input$state_selector,
                    '. You can click and drag the slider at the bottom
                to explore the data. Clicking on one of the items in the legend
                will hide that line on the plot.'
                  )
                }
            }
          })
        output$socmed_lineplot_text <-
          renderText(
            socmed_lineplot_text()
          )
        
        
    }

# knit and run app --------------------------------------------------------

shinyApp(ui, server)