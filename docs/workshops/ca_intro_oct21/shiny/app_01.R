library(shiny, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(caladaptr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(units, quietly = TRUE)
library(ggplot2, quietly = TRUE)  

ui <- fluidPage(

  titlePanel("Time Series Plot of Annual Climate Data", windowTitle = "Time Series"),
  
  fluidRow(
    column(3,
           sliderInput("year", "Year range:", min = 2006, max = 2099, value = c(2040,2070), sep = "", step = 1),
           selectInput("cvar", label = "Climate variable:", choices = cvars[1:3], selected = cvars[1], multiple = FALSE),
           selectInput("gcm", label = "GCMs:", choices = gcms, selected = gcms[1:4], multiple = TRUE),
           selectInput("scenario", label = "Emissions scenario:", choices = scenarios[1:2], selected = scenarios[1], multiple = FALSE),
           textInput("lon", "Longitude"),
           textInput("lat", "Latitude")
    ),
    column(9,
           leafletOutput("mymap"),
    )
  ),
  
  fluidRow(column(9,
                  offset = 3,
                  hr(),
                  actionButton("cmd_plot", "Fetch Data and Plot"),
                  p(),
                  plotOutput("timeseriesPlot"),
                  p())
  )
)

server <- function(input, output, session) {

  
}

shinyApp(ui, server)
