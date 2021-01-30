library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

library(tidyverse)
library(stringr)
library(plotly)

# 1: Functions


# 1.1: Function to plot the charts

plot_chart <- function(chart_data, col) {
    chart <- ggplot(chart_data) +
        aes(x = date,
            y = {{col}},
            color = country_region) +
        geom_line() 
        
    ggplotly(chart, width = 600)
}



# 1.2: Function to generate the map
plot_map <- function(map_data) {
    map <- plot_ly(map_data, 
                   type='choropleth', 
                   locations=~as.character(code), 
                   # locationmode='country names',
                   colorscale = 'Portland',
                   # zmin = 0,
                   # zmax = 1000000,
                   colorbar = list(title = 'Confirmed Cases', x = 1.0, y = 0.9),
                   z=~confirmed,
                   unselected = list(marker= list(opacity = 0.1)),
                   marker=list(line=list(color = 'black', width=0.2)
                   ))
    map %>% layout(geo = list(projection = list(type = "natural earth"), showframe = FALSE),
                   clickmode = 'event+select', autosize = FALSE, width = 650, height = 450)
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
    country_code_data <- read_csv("data/raw/2014_world_gdp_with_codes.csv")
    
    # country_code_data <- country_code_data %>%
    #     rename(country_region = "country")# %>%
        # select(-country_code)
    
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

country_daywise_df <- country_daywise_df %>%
    mutate(country_region = str_replace(country_region, 'Timor-Leste', 'East Timor')) %>%
    mutate(country_region = str_replace(country_region, 'Congo (Kinshasa)', 'Republic of the Congo')) %>% 
    mutate(country_region = str_replace(country_region, 'Cote d\'Ivoire', 'Ivory Coast')) %>%
    mutate(country_region = str_replace(country_region, 'North Macedonia', 'Macedonia')) %>%
    mutate(country_region = str_replace(country_region, 'Burma','Myanmar')) %>%
    mutate(country_region = str_replace(country_region, 'Serbia','Republic of Serbia')) %>%
    mutate(country_region = str_replace(country_region, '\\*', '')) %>%
    mutate(country_region = str_replace(country_region, 'Bahamas', 'The Bahamas')) %>%
    mutate(country_region = str_replace(country_region, 'Tanzania','United Republic of Tanzania')) %>%
    mutate(country_region = str_replace(country_region, 'US','United States of America')) %>%
    drop_na()

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
              population = sum(population)) %>%
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
              new_recovered = mean(new_recovered),
              population = sum(population)) %>%
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
selection_mode <- htmlDiv(
    list(
    htmlLabel('Selection Mode'),
    dccRadioItems(
        id = 'selection_mode',
        options=list(list('label' = 'World', 'value' = 1),
                     list('label' = 'Regions', 'value' = 2),
                     list('label' = 'Countries', 'value' = 3)),
        value=1,
        labelStyle=list('margin-right' = '15px'),
        inputStyle=list('margin-right'= '5px'))  
    )
)

# 4.2.1: Empty Div for World
blank_div <- htmlDiv(
    # 'Blank Div',
    id = 'blank_div',
    style = list(
        'color' = 'white',
        'background-color' = 'red'
        )
)

# 4.2.2: Dropdown list for Regions
region_selection <- htmlDiv(
    list(
        # htmlLabel('Region Selection'),
        dccDropdown(
            id = 'region_selection',
            options = regions$who_region %>% purrr::map(function(col) list(label = col, value = col)),
            placeholder="Africa",
            multi = TRUE
        )  
    )
)

# 4.2.3: Drop down list for Countries
country_selection <- htmlDiv(
    list(
        # htmlLabel('Country Selection'),
        dccDropdown(
            id = 'country_selection',
            options = countries$country_region %>% purrr::map(function(col) list(label = col, value = col)),
            placeholder="Afghanistan",
            multi = TRUE
        )  
    )
)

# 4.3: Date Range Picker
date_range_selection <- htmlDiv(
    list(
        htmlLabel('Date Range Selection'),
        dccDatePickerRange(
            id='date_range_selection',
            min_date_allowed=as.Date('2020-01-22'),
            max_date_allowed=as.Date('2020-07-27'),
            initial_visible_month=as.Date('2020-01-01'),
            start_date = as.Date('2020-01-22'),
            end_date = as.Date('2020-07-27')
        ),
    htmlDiv(id='output-container-date-picker-range')
    )
)

# 4.4: Line charts

# 4.4.1: Confirmed Cases
total_cases_linechart <- list(dccGraph(id = 'line_totalcases'))


# 4.4.2: Deaths
total_death_linechart <- list(dccGraph(id = 'line_totaldeaths'))

# 4.4.3: Recoveries
total_recovered_linechart <- list(dccGraph(id = 'line_totalrecovered'))

# 4.5: Map
world_map <- htmlDiv(
    list(
        dccGraph(figure = plot_map(country_daywise_df),
                 id = 'world_map')
    )
)


# 4.6: Absolute Number / Per 1M
data_mode_selection <- htmlDiv(
    list(
        htmlLabel('Display Data'),
        dccRadioItems(
            id = 'data_mode_selection',
            options=list(list('label' = 'Absolute', 'value' = 1),
                list('label' = 'Per Capita', 'value' = 2)),
            value=1,
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
                        ),
                        width = 4
                    ),
                    dbcCol(
                        world_map,
                        width = 8
                    )
                ),
            ),
            dbcRow(
                list(
                    dbcCol(
                        total_cases_linechart, width = 4
                    ),
                    dbcCol(
                        total_death_linechart, width = 4
                    ),
                    dbcCol(
                        total_recovered_linechart, width = 4
                    )
                )
            )
        )
    )
)

app$callback(
    list(
        # output('output-container-date-picker-range', 'children'),
        output('line_totalcases', 'figure'),
        output('line_totaldeaths', 'figure'),
        output('line_totalrecovered', 'figure'),
        output('world_map', 'figure')
    ),
    list(
        input('selection_mode', 'value'),
        input('region_selection', 'value'),
        input('country_selection', 'value'),
        input('date_range_selection', 'start_date'),
        input('date_range_selection', 'end_date'),
        input('data_mode_selection', 'value')
    ),
    function(selection_mode, region, country, start_date, end_date, data_mode) {
        print("callback function")
        # Start filtering data
        # temporarily fake data. When implement please remove the fake data
        SELECTION_WORLD = 1L
        SELECTION_REGION = 2L
        SELECTION_COUNTRY = 3L
        # selection_mode = SELECTION_WORLD
        
        DATA_ABSOLUTE = 1
        DATA_PER1M = 2
        data_mode = DATA_ABSOLUTE
        
        start_date = '2020-01-27'
        end_date = '2020-07-27'
        
        chart_data <- world_daywise_df
        map_data <- country_daywise_df
        
        if (selection_mode == SELECTION_REGION) {
            print("Select region")
            print(region)
            print(typeof(region))
            chart_data <- region_daywise_df %>%
                filter(who_region %in% region)
            
            map_data <- map_data %>%
                filter(who_region %in% region)
        } else if (selection_mode == SELECTION_COUNTRY) {
            print("Select country")
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
            chart_data <- chart_data %>%
                mutate(confirmed = (confirmed/population)*1000000) %>%
                mutate(deaths = (deaths/population)*1000000) %>%
                mutate(recovered = (recovered/population)*1000000)
            map_data <- chart_data
                
        }
        
        # End filtering data
        
        # Start Plot 3 charts
        line_totalcases <- plot_chart(chart_data, confirmed)
        line_totaldeaths <- plot_chart(chart_data, deaths)
        line_totalrecovered <- plot_chart(chart_data, recovered)
        
        # End Plot 3 charts
        
        # Start world map
        # TODO: Write a function to load the world map
        map_data <- map_data %>%
            group_by(country_region, code) %>%
            summarize(confirmed = mean(confirmed),
                      deaths = mean(deaths),
                      recovered = mean(recovered),
                      active = mean(active),
                      new_cases = mean(new_cases),
                      new_deaths = mean(new_deaths),
                      new_recovered = mean(new_recovered),
                      population = mean(population)) %>%
            ungroup()
        
        world_map <- plot_map(map_data)
        
        print(map_data)
        # End world map
        
        list(line_totalcases, line_totaldeaths, line_totalrecovered, world_map)
    }
)

app$callback(
    list(
        output('blank_div', 'style'),
        output('region_selection', 'style'),
        output('country_selection', 'style')
    ),
    list(
        input('selection_mode', 'value')
    ),
    function(selection_mode) {
        print("Hide/Show selection,")
        print(selection_mode)
        print(typeof(selection_mode))
        SELECTION_WORLD = 1L
        SELECTION_REGION = 2L
        SELECTION_COUNTRY = 3L
        print(typeof(SELECTION_REGION))
        
        world_style = list('height' = '35px')
        region_style = list('display' = 'none')
        country_style = list('display'= 'none')

        print("before")
        if (selection_mode == SELECTION_REGION){
            print('Region mode')
            world_style = list('display' = 'none')
            region_style = list('display' = 'block')
        }
        else if (selection_mode == SELECTION_COUNTRY){
            print('Country mode')
            world_style = list('display' = 'none')
            country_style = list('display' = 'block')
        }
        
        list(world_style, region_style, country_style)
    }
)

# Function for loading screen
# Will do after finishing above functions


app$run_server(host = '0.0.0.0')
