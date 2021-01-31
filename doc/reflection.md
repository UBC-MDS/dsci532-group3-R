# Reflection

## Milestone 3 Implementation

The objective of this week’s milestone was to recreate and improve upon the dashboard from [Milestone 2](https://github.com/UBC-MDS/dsci532-group3) using R instead of Python. Generally speaking, the [Final Dashboard](https://covid-19-mds-532-group3-r.herokuapp.com/) largely resembles the main features and functionality seen in the last iteration. There is an assortment of radio buttons and dropdowns that allow the user to specify the time period, part of the world and metrics they are interested in. The outputs of the dashboard include three interactive plots, as well as a heatmap of the world. 

Since this dashboard was an iteration of a previous version, less time was spent on conceptualizing the layout and cleaning the data. The syntactic similarities between how Dash is implemented in Python versus R meant that there was a relatively small learning curve in transitioning to R. For this particular dashboard, there were no features or functionality limitations from using R instead of Python. Interestingly, the R implementation of the dashboard runs much more quickly than the Python version on Heroku, resolving one of the primary issues from the last milestone. This meant that even though the loading screen was implemented in the R version, it wasn't particularly necessary.

The secondary focus on this milestone was to improve upon the feedback that was provided. Some of the changes that were implemented include:

- Showing per capita data in millions so that scale is easier to understand
- Automatic resizing of dropdown list as the number of fields increase


## Future Improvements & Additions

One of the known issues specific to the R implementation is that as more countries or regions are added to the dropdown field, the legend expands into the center plot. This causes the center plot to shrink and appear disproportionate in sizing relative to the other plots. 

As milestone four approaches, some general features for future implementation include:

- Improving the general aesthetics and design 
- Introducing more interactivity with clicking on the map
