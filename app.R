library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

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