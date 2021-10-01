library(shiny, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(shinybusy, quietly = TRUE)
library(caladaptr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(units, quietly = TRUE)
library(ggplot2, quietly = TRUE)  

## TODO Mark the priority GCMs with an asterik
## TODO Add info icons next to the form controls
## TODO Add a DT table so the user can download the output values

## Create a reactiveVal object which is where we'll 
## store  the coordinates when someone clicks the map
pt_coords <- reactiveVal()

ylab_lst <- list(tasmin = "temp (F)",
                 tasmax = "temp (F)",
                 pr = "precip (kg/m2/s)")

ui <- fluidPage(
  #if (file.exists("gtag_timeseries.js")) tags$head(includeHTML("gtag_timeseries.js")),
  
  tags$head(tags$style(type="text/css", ".leaflet-container {cursor: pointer !important;}")),
  
  use_busy_spinner(spin = "atom", position = "bottom-left"),
  
  titlePanel(title = div(img(src="https://ucanr-igis.github.io/caladaptr/reference/figures/logo.gif", height = "75px"), "Demo App: Plot a Time Series of Annual Climate Data"), 
             windowTitle = "caladaptR Demo"),
  
  fluidRow(
    column(12,
           HTML("<p>This <a href='https://shiny.rstudio.com/'>R Shiny</a> app demonstrates how to use <a href='https://ucanr-igis.github.io/caladaptr/'>caladaptR</a> to fetch data from Cal-Adapt. To use, select a point in California or Nevada on the map, and set the options for the climate data you want. Then click the Plot button. [<a href='https://github.com/ucanr-igis/caladaptr.apps/blob/master/inst/shiny/timeseries/app.R' target='_blank'>Source code</a> | <a href='https://raw.githubusercontent.com/ucanr-igis/caladaptr.apps/master/inst/shiny/timeseries/timeseries_reactivity.png' target='_blank'>Reactivity map</a>].</p>"))
  ),
  
  fluidRow(
    column(3,
           sliderInput("year", "Year range:", min = 2006, max = 2099, value = c(2040,2070), sep = "", step = 1),
           selectInput("cvar", label = "Climate variable:", choices = cvars[1:3], selected = cvars[1], multiple = FALSE),
           selectInput("gcm", label = "GCMs:", choices = gcms, selected = gcms[1:4], multiple = TRUE),
           selectInput("scenario", label = "Emissions scenario:", choices = scenarios[1:2], selected = scenarios[1], multiple = FALSE)
    ),
    column(9,
           p(strong("Click on the map to select a location:")),
           leafletOutput("mymap"),
           textOutput("txtCoords")
    )
  ),
  fluidRow(column(9,
                  offset = 3,
                  hr(),
                  actionButton("cmd_plot", "Fetch Data and Plot"),
                  p(),
                  textOutput("txtCapMsg"),
                  textOutput("txtTblMsg"),
                  plotOutput("timeseriesPlot"),
                  p())
  )
)

server <- function(input, output, session) {
  
  ## This will set the initial map extent.
  ## It will never be called after that (is this the best way to do it?)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(-120.2, 36.4, zoom=6)
  })
  
  ## This will run every time cmd_plot is clicked
  pt_cap <- eventReactive(input$cmd_plot, {
    
    if (is.null(pt_coords())) {
      output$txtCapMsg <- renderText("Please first click on the map to select a point, then try again.")
      
      ## Stop here - return NULL
      NULL
      
    } else {
      output$txtCapMsg <- renderText(NULL)
      
      ## Return a cal-adapt API request object
      ca_loc_pt(coords = c(pt_coords()[1], pt_coords()[2])) %>% 
        ca_gcm(input$gcm) %>% 
        ca_scenario(input$scenario) %>% 
        ca_period("year") %>% 
        ca_cvar(input$cvar) %>% 
        ca_years(start = input$year[1], end = input$year[2])
      
    }
    
  })
  
  ## The following will run whenever input$mymap_click changes
  ## (i.e., someone clicks on the map)
  observe({
    req(!is.null(input$mymap_click))
    
    myclick_lst <- input$mymap_click
    
    ## Update the value of pt_coords with the coordinates 
    pt_coords(c(myclick_lst$lng, myclick_lst$lat))
    
    # ## Clear existing markers and add a marker at the new location
    leafletProxy('mymap') %>% 
      clearMarkers() %>%
      addMarkers(lng = pt_coords()[1], lat = pt_coords()[2])
    
    ## Clear the message in txtCapMsg
    output$txtCapMsg <- renderText(NULL)
    
  })
  
  output$txtCoords <- renderText({
    ## Stop if pt_coords() is NULL (there's no map coordinate saved)
    req(pt_coords())
    
    ## Return text to go into txtCoords
    paste0("Selected point: (", 
           round(pt_coords()[1], 4), 
           ", ", 
           round(pt_coords()[2], 4), ")")
  })
  
  ## The following will update whenever pt_cap is updated
  ## (which is turn is updated when the plot button is clicked)
  ## It will *not* run however when the app first opens (because pt_cap() will be NULL)
  pt_tbl <- eventReactive(pt_cap(), {
    show_spinner() 
    my_vals <- ca_getvals_tbl(pt_cap(), quiet = TRUE) 
    hide_spinner() 
    my_vals
  }, ignoreNULL = TRUE)
  
  ## This code will run every time pt_tbl() is updated (i.e., new 
  ## data is fetched). It will update the plot.
  output$timeseriesPlot <- renderPlot({
    
    req(pt_tbl())
    
    cvar_in <- isolate(input$cvar)
    
    if (cvar_in == "tasmin" || cvar_in == "tasmax") {
      data_tbl <- pt_tbl() %>% mutate(val2 = set_units(val, degF))
    } else {
      data_tbl <- pt_tbl() %>% mutate(val2 = val)
    }
    
    ggplot(data = data_tbl, 
           aes(x = as.Date(dt), y = as.numeric(val2))) +
      geom_line(aes(color=gcm)) +
      labs(title = toupper(cvar_in), 
           subtitle = paste0("Location: ", 
                             round(isolate(pt_coords())[1], 4), 
                             ", ", 
                             round(isolate(pt_coords())[2], 4),
                             ". Emissions scenario: ",
                             isolate(input$scenario)),
           x = "year", 
           y = ylab_lst[[cvar_in]],
           caption = "Data source: Cal-Adapt.org")
  })
  
}

shinyApp(ui, server)