library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

library(tidyverse)

# 1: Functions

# 1.1: Function to plot the charts
plot_chart <- function() {
    ''
}

# 1.2: Function to generate the map
plot_map <- function() {
    ''
}

# 1.3 Function to load data
load_daily_data <- function() {
    daily_data <- read_csv("data/raw/full_grouped.csv")
    
    daily_data <- daily_data %>%
        rename(country_region = "Country/Region",
               new_cases = "New cases",
               new_deaths = "New deaths",
               new_recovered = "New recovered",
               who_region = "WHO Region",
               date = "Date",
               confirmed = "Confirmed",
               deaths = "Deaths",
               recovered = "Recovered",
               active = "Active")
    
    daily_data$country_region <- daily_data$country_region %>%
        as.factor()
    
    daily_data$who_region <- daily_data$who_region %>%
        as.factor()
    
    daily_data
}

load_population_data <- function() {
    population_data <- read_csv("data/processed/worldometer_data.csv") %>%
        rename(country_region = "Country/Region",
               population = "Population")
    
    population_data$country_region <- population_data$country_region %>%
        as.factor()
    
    population_data
}

load_country_code <- function() {
    country_code_data <- read_csv("data/country_location.csv")
    
    country_code_data <- country_code_data %>%
        rename(country_region = "country") %>%
        select(-country_code)
    
    country_code_data$country_region <- country_code_data$country_region %>%
        as.factor()
    
    country_code_data
}

# 1.6: Other supporting functions

# 2: Load from the file

daily_data <- load_daily_data()
# print(head(daily_data))
population_data <- load_population_data()
# print(head(population_data))
country_code_data <- load_country_code()
# print(head(country_code_data))

# 3: Data wrangling

# 3.1: Join
country_daywise_df <- left_join(daily_data, population_data)
country_daywise_df <- left_join(country_daywise_df, country_code_data)
print(head(country_daywise_df))

# 3.2: Aggregate into world / regions 
region_daywise_df <- country_daywise_df %>%
    group_by(date, who_region) %>%
    summarize(confirmed = mean(confirmed),
              deaths = mean(deaths),
              recovered = mean(recovered),
              active = mean(active),
              new_cases = mean(new_cases),
              new_deaths = mean(new_deaths),
              new_recovered = mean(new_recovered)) %>%
    ungroup() %>%
    mutate(country_region = who_region)

# print(head(region_daywise_df))

world_daywise_df <- country_daywise_df %>%
    group_by(date) %>%
    summarize(confirmed = mean(confirmed),
              deaths = mean(deaths),
              recovered = mean(recovered),
              active = mean(active),
              new_cases = mean(new_cases),
              new_deaths = mean(new_deaths),
              new_recovered = mean(new_recovered)) %>%
    ungroup() %>%
    mutate(country_region = "World")

print(head(world_daywise_df))

countries <- country_daywise_df %>%
    select(country_region) %>%
    unique()

print(countries)

regions <- region_daywise_df %>%
    select(who_region) %>%
    unique()

print(regions)

# 4: Declare objects

# 4.1: Declare options for Selection Mode / Data Mode as factors

# 4.2: Selection mode (World, Regions, Countries)
selection_mode <- htmlH3(
    list(
    htmlLabel('Selection Mode'),
    dccRadioItems(
        id = 'selection_mode',
        options=list(list('label' = 'World', 'value' = 'World'),
                     list('label' = 'Regions', 'value' = 'Regions'),
                     list('label' = 'Countries', 'value' = 'Countries')),
        value='World',
        labelStyle=list('margin-right' = '25px'),
        inputStyle=list('margin-right'= '5px'))  
    )
)

# 4.2.1: Empty Div for World
blank_div <- htmlDiv(
    'Blank Div',
    id = 'blank_div',
    style = list(
        'color' = 'white',
        'background-color' = 'red'
        )
)

# 4.2.2: Dropdown list for Regions
region_selection <- htmlDiv(
    list(
        htmlLabel('Region Selection'),
        dccDropdown(
            id = 'region_selection',
            options = regions$who_region %>% purrr::map(function(col) list(label = col, value = col)),
            placeholder="Africa"
        )  
    )
)

# 4.2.3: Drop down list for Countries
country_selection <- htmlDiv(
    list(
        htmlLabel('Country Selection'),
        dccDropdown(
            id = 'country_selection',
            options = countries$country_region %>% purrr::map(function(col) list(label = col, value = col)),
            placeholder="Afghanistan"
        )  
    )
)

# 4.3: Date Range Picker
date_range_selection <- htmlDiv(
    list(
        htmlLabel('Date Range Selection'),
        dccDatePickerRange(
            id='date_range_selection',
            min_date_allowed=as.Date('2020-01-01'),
            max_date_allowed=as.Date('2020-07-31'),
            initial_visible_month=as.Date('2020-01-01'),
            end_date = as.Date('2020-07-31')
        ),
    htmlDiv(id='output-container-date-picker-range'),
    )
)

# 4.4: Line charts

# 4.4.1: Confirmed Cases
total_cases_linechart <- htmlDiv(
    'Total Confirmed Cases line chart',
    id = 'line_totalcases',
    style = list(
        'color' = 'white',
        'background-color' = 'green'
        )
)

# 4.4.2: Deaths
total_death_linechart <- htmlDiv(
    'Total deaths line chart',
    id = 'line_totaldeaths',
    style = list(
        'color' = 'white',
        'background-color' = 'brown'
        )
)

# 4.4.3: Recoveries
total_recovered_linechart <- htmlDiv(
    'Total recovered line chart',
    id = 'line_totalrecovered',
    style=list(
        'color' = 'white',
        'background-color' = 'blue'
        )
)
# 4.5: Map
world_map <- htmlDiv(
    'World Map',
    id = 'world_map',
    style = list(
        'color' = 'white',
        'background-color' = 'purple'
        )
)


# 4.6: Absolute Number / Per 1M
data_mode_selection <- htmlDiv(
    list(
        htmlLabel('Display Data'),
        dccRadioItems(
            id = 'data_mode_selection',
            options=list(list('label' = 'Absolute', 'value' = 'Absolute'),
                list('label' = 'Per Capita', 'value' = 'Per Capita')),
            value='Absolute',
            labelStyle=list('margin-right' = '25px'),
            inputStyle=list('margin-right'= '5px')
        )  
        )
)


# 5: Skeleton of the server

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list (
            dbcRow(
                list(
                    dbcCol(
                        list(
                            selection_mode,
                            blank_div,
                            region_selection,
                            country_selection,
                            date_range_selection,
                            data_mode_selection
                        )
                    ),
                    dbcCol(
                        world_map
                    )
                ),
            ),
            dbcRow(
                list(
                    dbcCol(
                            total_cases_linechart
                    ),
                    dbcCol(
                        total_death_linechart
                    ),
                    dbcCol(
                        total_recovered_linechart
                    )
                )
            )
        )
    )
)

# Output('line_totalcases', 'srcDoc'),
# Output('line_totaldeaths', 'srcDoc'),
# Output('line_totalrecovered', 'srcDoc'),
# Output('world_map', 'figure'),
Input('region_selection', 'value'),
Input('country_filter', 'value'),
Input('continent_filter', 'value'),
Input('date_selection_range', 'start_date'),
Input('date_selection_range', 'end_date'),
Input('select_options', 'value')

app$callback(
    list(
        output('output-container-date-picker-range', 'children'),
        output('line_totalcases', 'children'),
        output('line_totaldeaths', 'children'),
        output('line_totalrecovered', 'children'),
        output('world_map', 'children')
        # output('line_totalcases', 'srcDoc'),
        # output('line_totaldeaths', 'srcDoc'),
        # output('line_totalrecovered', 'srcDoc'),
        # output('world_map', 'figure')
    ),
    list(
        input(id = 'date_range_selection', property = 'start_date'),
        input(id = 'date_range_selection', property = 'end_date'),
        input('selection_mode', 'children'),
        input('region_selection', 'children'),
        input('country_selection', 'children'),
        input('date_range_selection', 'children'),
        input('data_mode_selection', 'children'),
        input('selection_mode', 'value'),
        input('region_selection', 'value'),
        input('country_selection', 'value'),
        input('date_range_selection', 'value'),
        input('data_mode_selection', 'value')
        ),
    function(selection_mode, region, country, start_date, end_date, data_mode) {
        # Start filtering data
        # temporarily fake data. When implement please remove the fake data
        SELECTION_WORLD = 1
        SELECTION_REGION = 2
        SELECTION_COUNTRY = 3
        selection_mode = SELECTION_WORLD
        
        DATA_ABSOLUTE = 1
        DATA_PER1M = 2
        data_mode = DATA_ABSOLUTE
        
        start_date = '2020-01-27'
        end_date = '2020-07-27'
        
        chart_data <- world_daywise_df
        map_data <- country_daywise_df
        
        if (selection_mode == SELECTION_REGION) {
            chart_data <- region_daywise_df %>%
                filter(who_region %in% region)
            
            map_data <- map_data %>%
                filter(who_region %in% region)
        } else if (selection_mode == SELECTION_COUNTRY) {
            chart_data <- country_daywise_df %>%
                filter(country_region %in% country)
            map_data <- chart_data
        }
        
        chart_data <- chart_data %>%
            filter(date >= start_date, date <= end_date)
        
        map_data <- map_data %>%
            filter(date >= start_date, date <= end_date)        

        if (data_mode == DATA_PER1M) {
            # TODO: divide by Population. Then multiply by 1M
        }
        # End filtering data
        
        # Start Plot 3 charts
        # TODO: Write a function to load 3 charts
        line_totalcases <- plot_chart('Total Confirmed Cases line chart')
        line_totaldeaths <- plot_chart('Total deaths line chart')
        line_totalrecovered <- plot_chart('Total recovered line chart')
        
        # End Plot 3 charts
        
        # Start world map
        # TODO: Write a function to load the world map
        world_map <- 'World map'
        
        
        # End world map
        
        list(line_totalcases, line_totaldeaths, line_totalrecovered)
    }
)

# Function to hide / show selection mode
# Will do after finishing above functions

# Function for loading screen
# Will do after finishing above functions


app$run_server(debug = T)