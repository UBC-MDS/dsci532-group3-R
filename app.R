library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

library(tidyverse)
library(stringr)
library(plotly)
library(ggplot2)
library(scales)

# 1: Functions


# 1.1: Function to plot the charts

#' Plots line chart for Confirmed, Deaths and Recovered data
#'
#' @param chart_data: df that contains target feature versus time
#' @param col: column for target feature
#' @param title: title for charts
#'
#' @return line charts for Confirmed, Deaths and Recovered cases versus time
#'   
#' @export
#'
#' @examples
#' plot_chart(chart_data, confirmed, "Confirmed Cases")
plot_chart <- function(chart_data, col, title, show_legend=FALSE) {
    chart <- ggplot(chart_data) +
        aes(x = date,
            y = {{col}},
            color = country_region) +
        geom_line() +
        scale_x_date(labels = date_format("%b"),
                     breaks = date_breaks("month")) +
        scale_y_continuous(labels = scales::label_number_si()) +
        theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
        labs(y = title) +
        ggtitle(title)
    
    if (show_legend) {
        print('show legend for ')
        print(title)
        chart <- chart + theme(legend.position = "bottom")
    } else {
        print('hide legend for ')
        print(title)
        chart <- chart + theme(legend.position = "none")
    }
        
    result <- ggplotly(chart, height = 300)
    
    if (show_legend) {
        result <- result %>%
            layout(legend = list(orientation = "h", x = 0, y = 0))
    }
    
    result
}



# 1.2: Function to generate the map

#' Generates interactive world map with Confirmed cases around the world
#'
#' @param map_data: df that contains confirmed cases data
#' @param title: title for the map
#'
#' @return world map with Confirmed cases
#'   
#' @export
#'
#' @examples
#' plot_map(map_data, "World Map")
plot_map <- function(map_data, title) {
    map <- plot_ly(map_data, 
                   type='choropleth', 
                   locations=~as.character(code), 
                   # locationmode='country names,
                   colorscale = 'Portland',
                   # zmin = 0,
                   # zmax = 1000000,
                   colorbar = list(title = title, x = 1.0, y = 0.9),
                   z=~confirmed,
                   unselected = list(marker= list(opacity = 0.1)),
                   marker=list(line=list(color = 'black', width=0.2)
                   ))
    map %>% layout(geo = list(projection = list(type = "natural earth"), showframe = FALSE),
                   clickmode = 'event+select', autosize = FALSE, width = 800, height = 500,
                   margin = list('r' = 0, 't' = 0, 'l' = 0, 'b' = 0))
}

# 1.3 Function to load data

#' Reads, modifies and loads daily_data from csv file
#'
#' @param 
#'
#' @return df for daily_data
#'   
#' @export
#'
#' @examples
#' load_daily_data()
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

#' Reads, modifies and loads population_data from csv file
#'
#' @param 
#'
#' @return df for population_data
#'   
#' @export
#'
#' @examples
#' load_population_data()
load_population_data <- function() {
    population_data <- read_csv("data/processed/worldometer_data.csv") %>%
        rename(country_region = "Country/Region",
               population = "Population")
    
    population_data$country_region <- population_data$country_region %>%
        as.factor()
    
    population_data
}

#' Reads, modifies and loads country_code_data from csv file
#'
#' @param 
#'
#' @return df for country_code_data
#'   
#' @export
#'
#' @examples
#' load_country_code()
load_country_code <- function() {
    country_code_data <- read_csv("data/raw/2014_world_gdp_with_codes.csv")
    
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

# 4.1: Learn More Button
collapse = htmlDiv(
    list(
        htmlButton("Learn More", id="collapse-button")
    )
)

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
            placeholder = "Select region",
            value = "Africa",
            multi = TRUE,
            style = list('display' = 'none')
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
            placeholder = "Select country",
            value="Afghanistan",
            multi = TRUE,
            style = list('display' = 'none')
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
    htmlDiv(id='output-container-date-picker-range'),
    htmlBr()
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
        dccGraph(figure = plot_map(country_daywise_df, 'Confirmed'),
                 id = 'world_map',
                 style=list(height='200px'))
    )
)


# 4.6: Absolute Number / Per 1M
data_mode_selection <- htmlDiv(
    list(
        htmlLabel('Display Data'),
        dccRadioItems(
            id = 'data_mode_selection',
            options=list(list('label' = 'Absolute', 'value' = 1),
                list('label' = 'Per 1M', 'value' = 2)),
            value=1,
            labelStyle=list('margin-right' = '25px'),
            inputStyle=list('margin-right'= '5px')
        )  
    )
)

loading <- htmlDiv(
    dccLoading(
        id = 'loading',
        type = 'circle'
    ),
    style = list('height' = '1px', 'width' = '1920px')
)

# 5: Skeleton of the server

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
            dbcRow(
                list(
                    dbcCol(
                        list(
                            htmlH1('WHO Coronavirus Disease (COVID-19) Dashboard',
                                   style=list(
                                       'backgroundColor'= '#e6e6e6',
                                       'padding'= 30,
                                       'color'= 'black',
                                       'margin-top'= 20,
                                       'margin-bottom'= 20,
                                       'text-align'= 'left',
                                       'font-size'= '25px',
                                       'border-radius'= 3,
                                       'width' = '1500px')),
                            dbcCollapse(list(
                                htmlP(paste("This dashboard allows you to explore COVID-19 data! Get started by selecting the options you are interested in on the menu to the left.")),
                                style=list('color'= 'black', 'width'= '50%'),
                                id="collapse")),
                            dbcCol(list(collapse))
                    ))
            )),
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
                        style=list(
                            'background-color'= '#e6e6e6',
                            'padding' = 15,
                            'border-radius'= 3,
                            'height' = '400px',
                            'position' ='relative',
                            'left' = '15px')
                    ),
                    dbcCol(
                        dbcCard(list(
                            dbcCardHeader('World Map'),
                            dbcCardBody(
                        world_map,
                        style=list('width' = '15', 'height'= '350px')
                            ))
                    )
                    )
                ),
            ),
            dbcRow(
                loading    
            ),
            dbcRow(style=list(height='200px')),
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
                ),
                style = list('margin-top' = '-100px')
            ),
            htmlHr(),
            htmlP(paste0("This dashboard was made by Sukhdeep Kaur, Arash Shamseddini, Tran Doan Khanh Vu, and Heidi Ye. You can find the source code original data below:")), 
            htmlA(
                children = list("GitHub"),
                href = 'https://github.com/UBC-MDS/dsci532-group3-R',
                style = list(color = 'blue', padding = '30px')
            ),
            htmlA(
                children = list("COVID-19 Data"),
                href = 'https://www.kaggle.com/imdevskp/corona-virus-report?select=covid_19_clean_complete.csv',
                style = list(color = 'blue')
            ),
            htmlBr(),
            htmlP(paste0("The server was last updated at: ", Sys.Date()))
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
    #' Updates data based on selection criteria and outputs charts and map
    #'
    #' @param selection_mode indicates if user is filtering for world, region or country
    #' @param region region being filtered
    #' @param country country being filtered
    #' @param start_date start date being filtered
    #' @param end_date end date being filtered
    #' @param data_mode indicates absolute or per 1M data being filtered
    #'
    #' @return three plots and world map
    #'   
    #' @export
    function(selection_mode, region, country, start_date, end_date, data_mode) {
        print("callback function")
        # Start filtering data
        SELECTION_WORLD = 1L
        SELECTION_REGION = 2L
        SELECTION_COUNTRY = 3L

        DATA_ABSOLUTE = 1L
        DATA_PER1M = 2L

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
            print(country)
            chart_data <- country_daywise_df %>%
                filter(country_region %in% country)
            map_data <- chart_data
        }
        
        # print(start_date, end_date)
        chart_data <- chart_data %>%
            filter(date >= start_date & date <= end_date)
        
        map_data <- map_data %>%
            filter(date >= start_date & date <= end_date)        

        map_title <- 'Confirmed Cases'
        suffix <- ''
        if (data_mode == DATA_PER1M) {
            print("Switching to Per 1M")
            suffix <- ' per 1M'
            map_title <- paste0(map_title, '\n', suffix)
            
            chart_data <- chart_data %>%
                mutate(confirmed = (confirmed/population)*1000000) %>%
                mutate(deaths = (deaths/population)*1000000) %>%
                mutate(recovered = (recovered/population)*1000000)
            map_data <- map_data %>%
                mutate(confirmed = (confirmed/population)*1000000) %>%
                mutate(deaths = (deaths/population)*1000000) %>%
                mutate(recovered = (recovered/population)*1000000)
        }
        
        
        chart_data <- chart_data %>%
            mutate(across(where(is.numeric), round, 2))
        map_data <- map_data %>%
            mutate(across(where(is.numeric), round, 0))
        # End filtering data
        
        # Start Plot 3 charts
        line_totalcases <- plot_chart(chart_data, confirmed, paste0('Confirmed', suffix))
        line_totaldeaths <- plot_chart(chart_data, deaths, paste0('Deaths', suffix), TRUE)
        line_totalrecovered <- plot_chart(chart_data, recovered, paste0('Recovered', suffix))
        
        # End Plot 3 charts
        
        # Start world map
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
        
        world_map <- plot_map(map_data, map_title)
        
        # print(map_data)
        print(chart_data)
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
    #' Hides and shows selection field based on options selected
    #'
    #' @param selection_mode indicates if user is filtering for world, region or country
    #'
    #' @return hidden or selected fields
    #'   
    #' @export
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
            region_style = list('display' = 'table', 'width' = '100%')
        }
        else if (selection_mode == SELECTION_COUNTRY){
            print('Country mode')
            world_style = list('display' = 'none')
            country_style = list('display' = 'table', 'width' = '100%')
        }
        
        list(world_style, region_style, country_style)
    }
)

# Function for loading screen
app$callback(
    list(
        output('loading', 'children')
    ),
    list(
        input('selection_mode', 'value'),
        input('region_selection', 'value'),
        input('country_selection', 'value'),
        input('date_range_selection', 'start_date'),
        input('date_range_selection', 'end_date'),
        input('data_mode_selection', 'value')
    ),
    #' Displays loading screen
    #'
    #' @param selection_mode indicates if user is filtering for world, region or country
    #' @param region region being filtered
    #' @param country country being filtered
    #' @param start_date start date being filtered
    #' @param end_date end date being filtered
    #' @param data_mode indicates absolute or per 1M data being filtered
    #'
    #' @return loading screen
    #'   
    #' @export
    function(selection_mode, region, country, start_date, end_date, data_mode) {
        time_to_sleep <- 1
        
        Sys.sleep(time_to_sleep)
        
        ''
    }
)

# Function for learn more button
app$callback(
    list(
        output('collapse', 'is_open')
    ),
    list(
        input('collapse-button', 'n_clicks'), state('collapse', 'is_open')
    ),
    #' Determines if toggle button is selected
    #'
    #' @param n number of clicks
    #' @param is_open is the toggle in open mode
    #'
    #' @return collapsed content
    #'   
    #' @export
    function(n, is_open) {
        if (n) {
            !is_open
        }
        else {
            is_open
        }
    }
)


app$run_server(host = '0.0.0.0')
