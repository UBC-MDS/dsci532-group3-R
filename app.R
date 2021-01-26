library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

library(tidyverse)

# 1: Functions

# 1.1: Function to do the filtering

# 1.2: Function to plot the charts

# 1.3: Function to generate the map

# 1.4: Function to hide / show selection mode

# 1.5: Load country code data
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


# 2: Read data

# 2.1: Load from the file
daily_data <- read_csv("data/raw/full_grouped.csv")
# head(daily_data)

population_data <- read_csv("data/processed/worldometer_data.csv")
# print(head(population_data))

country_code_data <- load_country_code()
# print(head(country_code_data))

# 2.2: Rename columns
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

population_data <- population_data %>%
    rename(country_region = "Country/Region",
           population = "Population")

# 2.3 Convert `Country/Region` and `WHO Region` to factors
daily_data$country_region <- daily_data$country_region %>%
    as.factor()

daily_data$who_region <- daily_data$who_region %>%
    as.factor()

population_data$country_region <- population_data$country_region %>%
    as.factor()

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

# 4: Declare objects

# 4.1: Declare options for Selection Mode / Data Mode as factors

# 4.2: Selection mode (World, Regions, Countries)

# 4.2.1: Empty Div for World

# 4.2.2: Dropdown list for Regions

# 4.2.3: Drop down list for Countries

# 4.3: Date Range Picker

# 4.4: Line charts

# 4.4.1: Confirmed Cases

# 4.4.2: Deaths

# 4.4.3: Recoveries

# 4.5: Map

# 4.6: Absolute Number / Per 1M




# 5: Skeleton of the server

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(
    htmlDiv(
        list(
            dccDropdown(
                options = list(list(label = "New York City", value = "NYC"),
                               list(label = "Montreal", value = "MTL"),
                               list(label = "San Francisco", value = "SF")),
                value = 'MTL'
            )
        )
    )
)

app$run_server(debug = T)