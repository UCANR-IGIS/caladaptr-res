## cat("\nTotal memory used at start is: ", round(as.numeric(pryr::mem_used()) / 1000000), "MB\n")

## Attach packages
## MEOMORY HIT - THESE 8 PACKAGES TAKE 92 MB
library(shiny)
library(leaflet)
library(caladaptr)
library(units)
library(ggplot2)
library(tidyr)
library(stringr)
library(shinyhelper)
library(dplyr)

## Check for required namespaces

## I am only using one or two functions from the following 
## packages, so rather than attaching them with library() and 
## risk conflicts, we simply verify the namespace is available 
## (i.e., package is installed) and then will call those 
## functions explicitly (e.g., scales::percent).

## In the case of DT, I choose to explicitly call those functions
## because there are identical functions in shiny which I explicitly don't want.

## MEMORY HIT: THESE 4 PACKAGES CHEW 100 MB
# requireNamespace("chillR")
requireNamespace("DT")
requireNamespace("lubridate")
requireNamespace("scales")

## The following three packages are *not* needed, but are dependencies of
## RMAWGEN. For some reason however the script which deploys this app
## to ShinyApps.io doesn't automatically install these dependencies,
## so to get the app to deploy I have to pretend like this script needs them.

# MEMORY HIT: THESE THREE NAMESPACES CHEW 10 MB
# UPDATE: NO LONGER NEEDED B/C I NO LONGER LOAD CHILLR DIRECTLY
# requireNamespace("date")
# requireNamespace("vars")
# requireNamespace("chron")

## Set up conflict resolution
requireNamespace("conflicted")
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)

## Utility function to add an additional class to the outer-most div of a shiny-tag
shinytag_add_class <- function(x, additional_class) {
  if (!inherits(x, "shiny.tag")) stop("x should be of class shiny.tag")
  x$attribs$class <- paste(x$attribs$class, additional_class)
  x
}

## Utility function to modify a Shiny tag with 2 children, wrapping the 2nd child in a DIV with a style attribute.
## If style = "display:inline-block;", this will have the effect of making the label inline.
shinytag_wrapchild2div <- function(x, style) {
  if (length(x$children) != 2) stop("This function is designed to modify a shiny.tag with two children")
  x$children[[2]] <- div(x$children[[2]], style = style)
  x
}

## Utility function to print memory usage to the console
# requireNamespace("pryr")
# report_memory <- function(x) {
#   cat("\n", x, "\n", sep = "")
#   cat(" - total memory used: ", round(as.numeric(pryr::mem_used()) / 1000000), " MB\n", sep = "")
#   cat(" - objects in memory: ", paste(ls(), collapse = ", "), "\n", sep = "")
#   cat(" - memory used by objects is: ", as.numeric(pryr::object_size(ls())) / 1000000, " MB\n", sep = "")
# }

## Load a custom version of chillr::make_hourly_temps
source('ca_make_hourly_temps.R')
source('chillr_daylength.R')
source('chillr_dynamic_model.R')

## report_memory("Memory usages after packages are loaded")

ui <- function(request) {

  fluidPage(
    tags$head(includeHTML("gtag_chill.js")),
    tags$head(tags$style(type="text/css", 
        "p.step {color:black; font-weight:bold; font-size:120%; padding-top:5px;}
        p.topborder {border-top:3px solid lightgray;}
        h1.report {font-weight:bold; font-size:16px; padding-top:10px; border-top:1px solid lightgray;}
        h2 {font-weight:bold;}
        h3 {font-weight:bold;}
        .leaflet-container {cursor: pointer !important;}
        .iblock {display: inline-block;}   /* inline block for side-by-side UI elements  */
        div.error-msg {color:red; margin:1em 0;}
        div.dt-buttons {margin-top:5px;}
        div.space_above_below {margin:0.5em 0;}
        div#txtout_rpt_coords {color:#333; font-size:90%; font-style:italic;}
        div#txtout_rpt_locname {color:black; font-size:130%; font-weight:bold;}
        p.report_lead {color:#555; font-size:110%; font-weight:bold; font-style:italic; border-top:6px solid dodgerblue; padding-top:0.5em;}")
    ),
    
    ## We put this tag in the body of the HTML document, rather than the head, so it doesn't
    ## get over-written by shinyhelper.css. This is needed to move the shinyhelp info buttons
    ## closer to the label rather than the far right edge of the column
    tags$style(type="text/css",
               ".shinyhelper-container {display: inline-block; position: relative; margin-left: 1em;"),
    
    titlePanel(title = "Projected Chill Portions Under Climate Change Calculator",
      windowTitle = "Chill Portions Calculator"),
  
    fluidRow(
      column(12,
             tags$p(),
             tags$div(tags$a(
               href = "https://cran.r-project.org/package=chillR", 
               tags$img(src="chillr_hex_75x65.png", height = "75px"),
               target = "_blank",
               rel = "noopener"), 
               style = "margin-top:10px; float:right; padding-left:10px; width:65px;"),
             
             tags$div(
               tags$a(href = "https://ucanr-igis.github.io/caladaptr/",
                 tags$img(src="https://ucanr-igis.github.io/caladaptr/reference/figures/logo.gif", 
                     height = "75px"),
                 target = "_blank",
                 rel = "noopener"), 
               style = "margin-top:10px; float:right; width:65px;"),
             
             tags$p("Introduction", class = "step topborder"),
             HTML("<p>This calculator can be used to compute end-of-season <a href='http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/' target='_blank' rel='noopener'>chill portions</a> under projected climate scenarios. The calculator draws upon downscale projected climate data from the California 4th Climate Change assessment hosted on <a href='https://cal-adapt.org/' target='_blank' rel='noopener'>Cal-Adapt.org</a>.</p>"),
             HTML("<details>
                  <summary style='color:blue; cursor:pointer; outline:none;'>Notes</summary>
                  <ul>
                  <li>This is a pilot app for demonstration purposes only.</li>
                  <li>This calculator uses downscaled climate data from Cal-Adapt, which is available for California, Nevada, and a <a href='https://ucanr-igis.github.io/caladaptr-res/workshops/caladaptr_intro_dec20/slides_files/figure-slidy/unnamed-chunk-6-1.png' target='_blank'>little bit of neighboring states</a> in the western USA.</li>
                  <li>This calculator uses chill <i>portions</i> rather than chill <i>hours</i>, because chill portions do a better job at predicting tree phenology. <a href='http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/' target='_blank' rel='noopener'>More info</a>.</li>
                  <li>RStudio users can run this and other demo Shiny apps directly from RStudio. See this <a href='https://github.com/ucanr-igis/caladaptr-res/blob/main/shiny/caladaptr_shinyapps_setup.R' target='_blank' rel='noopener'>setup script</a>. For additional details on the R code, see this <a href='https://ucanr-igis.github.io/caladaptr-res/notebooks/chill.nb.html' target='_blank' rel='noopener'>R Notebook</a>.</li>
                  <li>If the calculator unexpectedly disconnects while processing, the most likely reason is an out-of-memory error on the server. Refresh the page, reduce the number of GCMs or years, and try again. Or you can run the app locally from RStudio (recommended).</li>
                  <li>Questions or suggestions? Please email for <a href='mailto:caladaptr@gmail.com?subject=Chill Portions Shiny App'>feedback and support</a>.</li>
                  </ul>
                  </details>")
      )
    ),
    
    fluidRow(
      column(12,
             tags$br(),
             tags$p("1. Select location", class = "step topborder"),
             tags$p("To select a location, click on the map or enter coordinates below. "),
             div(leafletOutput("mymap"),
                 style = "max-width:600px; margin-bottom:1em;"),
             div(
               textInput("txtin_coords", label = "Coordinates: ", placeholder = "-120.425, 37.365") %>% 
                 shinytag_wrapchild2div("display:inline-block;") %>% 
                 shinytag_add_class("space_above_below") %>% 
                 shinytag_add_class("iblock"),
               actionButton("cmd_showonmap", "Show on Map", style = "display:inline-block;"),
               style = "display:inline-block;"
               ) %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "enter_coords",
                      buttonLabel = "OK",
                      size = "m"),
             htmlOutput("htmlout_coords_msg") %>% shinytag_add_class("error-msg"))
    ),
    
    fluidRow(
      column(12, 
             tags$p("2. Give this point a name for the report (optional)", class = "step topborder"),
             textInput("txtin_ptname", "Location name: ", width = "680px", placeholder = "My Farm") %>% 
               shinytag_wrapchild2div("display:inline-block;") %>% 
               shinytag_add_class("space_above_below"),
             tags$p())
    ),
    
    fluidRow(
      column(12,
             tags$br(),
             tags$p("3. Select the climate data to use as the ", tags$u("historic baseline"), class = "step topborder"),
             sliderInput("base_year", "Year range:", min = 1950, max = 2005, value = c(1975, 2005), sep = "", step = 1) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "years_select",
                      buttonLabel = "OK",
                      size = "m"),
             htmlOutput("htmlout_basetime_msg") %>% shinytag_add_class("error-msg"),
             selectInput("base_gcm", label = "GCMs:", choices = gcms[1:10], selected = gcms[1:4], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "base_gcm",
                      buttonLabel = "OK",
                      size = "l"),
             selectInput("base_scenario", label = "Emissions scenario:", choices = scenarios[3], selected = scenarios[3], multiple = FALSE)
      )
    ),
    fluidRow(
      column(12,
             tags$br(),
             tags$p("4. Select the climate data to use for the ", tags$u("projected future"), class = "step topborder"),
             sliderInput("prj_year", "Year range:", min = 2006, max = 2099, value = c(2040, 2060), sep = "", step = 1) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "year_select",
                      buttonLabel = "OK",
                      size = "m"),
             htmlOutput("htmlout_prjtime_msg") %>% shinytag_add_class("error-msg"),
             selectInput("prj_gcm", label = "GCMs:", choices = gcms[1:10], selected = gcms[1:4], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "prj_gcm",
                      buttonLabel = "OK",
                      size = "m"),
             selectInput("prj_scenario", label = "Emissions scenarios:", choices = scenarios[1:2], selected = scenarios[1:2], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "prj_scenario",
                      buttonLabel = "OK",
                      size = "m")
             
      )
    ),
    
    fluidRow(
      column(12,
             tags$br(),
             tags$p("5. Chill analysis options", class = "step topborder"),
             tags$p(tags$strong("Start counting chill portions on the first day of:")),
             div(
               div(selectInput("chill_month", label = NULL, choices = month.abb, selected = "Sep", 
                               multiple = FALSE, width = "90px"),
                   style = "display:inline-block; width:100px; vertical-align:top;")
               ),
             tags$head(tags$style(type="text/css", "#safe_chill, #req_chill {width: 60px}")),
             textInput("safe_chill", label = "Safe chill certainty level (%):", value = 90) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "safe_chill",
                      buttonLabel = "OK",
                      size = "m"),
             textInput("req_chill", label = "Required chill portions:", value = 48) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "req_chill",
                      buttonLabel = "OK",
                      size = "m")
      )
    ),
    
    fluidRow(column(12,
                    tags$p("6. Compute chill", class = "step topborder"),
                    actionButton("cmd_fetch", "Fetch Data and Analyze"),
                    tags$p(),
                    htmlOutput("htmlout_fetch_msg") %>% shinytag_add_class("error-msg"),
                    textOutput("txt_status"),
                    tags$p())
    ),
  
    conditionalPanel(
      condition = "output.safe_chill_computed",
      
      fluidRow(column(12,
                      tags$p(),
                      tags$p("Projected Chill Analysis", class = "report_lead"),
                      textOutput("txtout_rpt_locname"),
                      textOutput("txtout_rpt_coords"))
      ),
  
      fluidRow(column(12,
                      tags$h1("End-of-Season Chill Distribution", class = "report"),
                      htmlOutput("html_chill_histograms_lbl"))
      ),
      
      
      fluidRow(column(4,
                      plotOutput("hist_eos_chill_base"),
                      tags$p()),
               column(4,
                      plotOutput("hist_eos_chill_rcp45"),
                      tags$p()),
               column(4,
                      plotOutput("hist_eos_chill_rcp85"),
                      tags$p())
      ),
      
      fluidRow(column(12,
                      tags$h1("Safe Chill", class = "report"),
                      htmlOutput("html_safe_chill_lbl"))
                      
      ),
      
      fluidRow(column(4,
                      DT::dataTableOutput("dt_safe_chill"),
                      tags$br()),
               column(8)
      ),
      
      
      fluidRow(column(12,
                      tags$h1("Chill Portions by Month", class = "report"),
                      htmlOutput("html_chill_ports_ts_lbl"))
      ),
      
      
      fluidRow(column(4,
                      plotOutput("plot_chillts_hist"),
                      tags$p()),
               column(4,
                      plotOutput("plot_chillts_rcp45"),
                      tags$p()),
               column(4,
                      plotOutput("plot_chillts_rcp85"),
                      tags$p())
      ),
  
      fluidRow(column(12,
                tags$h1("Likelihood of Getting the Required Chill", class = "report"),
                htmlOutput("html_req_chill_lbl"))
      ),
      
      fluidRow(column(4,
                      DT::dataTableOutput("dt_req_chill_pct"),
                      tags$p()),
               column(8)
      ),
  
      fluidRow(column(12,
                      tags$p(),
                      bookmarkButton(label = "Bookmark link..."))
      )
      
      
    ),

    includeHTML("igis_footer.html")

  )
}

server <- function(input, output, session) {
  
  ## Create a reactiveVal object which is where we'll store  the coordinates 
  ## when someone clicks the map *or* enters coordinates into the text box
  pt_coords <- reactiveVal()
  
  ## Create reactiveVal objects to store the API requests.
  ## The data fetching reactiveVal objects will update when these
  ## API requests are modified
  pt_prj_cap <- reactiveVal()
  pt_base_cap <- reactiveVal()

  ## Create reactiveVal objects for the number of points clicked on the map
  ## (Used to generate a default location name that is different than
  ## earlier ones)
  pt_num <- reactiveVal(0)
  
  ## Add an observer to watch out for clicks on the shinyhelper buttons.
  ## The md files will be located in the default 'helpfiles' subdir
  observe_helpers()
  
  ## Set the initial map extent (called once but never called again after that)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(-120.2, 36.4, zoom=6)
  })
  
  ## The following will run whenever input$mymap_click changes
  observeEvent(input$mymap_click, {
    
    ## Get the coordinates
    myclick <- input$mymap_click
    
    ## Update the value of pt_coords with the coordinates 
    pt_coords(c(myclick$lng, myclick$lat))
    
    ## Clear existing markers and add a marker at the new location
    leafletProxy('mymap') %>% 
      clearMarkers() %>%
      addMarkers(lng = pt_coords()[1], lat = pt_coords()[2])
    
    ## Update the coordinates input box
    updateTextInput(session, 
                    inputId = "txtin_coords",
                    value = paste0(round(pt_coords()[1], 5), ", ", round(pt_coords()[2], 5))
    )
    
    ## Increment the point number
    pt_num(pt_num() + 1)
    
    ## Update the default location name text input
    updateTextInput(session, 
                    inputId = "txtin_ptname",
                    value = paste0("Pt", pt_num(), " (", round(pt_coords()[1], 3), ", ", round(pt_coords()[2], 3), ")")
                    )

    ## Clear error messages 
    output$htmlout_coords_msg <- renderUI(NULL)
    output$htmlout_fetch_msg <- renderUI(NULL)
    
    ## Nullify API requests (which will cascade and disable any results being shown)
    pt_prj_cap(NULL)
    pt_base_cap(NULL)
    
  })

  ## Update the default value of the location name and nullify any current results when 
  ## the user modifies input$txtin_coords (i.e., a new character is typed)
  observe({
    
    ## Update the default location name text input
    if (input$txtin_coords != "") {
      updateTextInput(session, 
                      inputId = "txtin_ptname",
                      value = paste0("Pt", isolate(pt_num()), " (", input$txtin_coords, ")"))
    }
    
    ## Nullify API requests (which will cascade and disable any results being shown)
    pt_prj_cap(NULL)
    pt_base_cap(NULL)
    
  })
  
  ## Show the manually typed in coordinates in the leaflet map
  ## This is run when the 'Show on Map' action button is clicked.
  observeEvent(input$cmd_showonmap, {
    
    ## Parse out the coordinates
    mycoords <- str_split(input$txtin_coords, ",")[[1]] %>%
      trimws() %>%
      as.numeric()
       
    if (NA %in% mycoords) {
      output$htmlout_coords_msg <- renderUI(HTML("Error: Please enter coordinates in decmial degrees separated by a comma.<br/>Example:  -120.226, 36.450"))
    } else {
      output$htmlout_coords_msg <- renderUI(NULL)

      ## See if these coordinates are actually new or not      
      ## If they're not new, no action is needed (the user is just clicking the button for no reason)
      if (!isTRUE(all.equal(pt_coords(), c(mycoords[1], mycoords[2]), tolerance = 0.0001))) {

        ## Update pt_coords() - this will trigger other stuff
        pt_coords(c(mycoords[1], mycoords[2]))

        # ## Clear existing markers, add a marker at the new location, and pan
        leafletProxy('mymap') %>%
          clearMarkers() %>%
          addMarkers(lng = pt_coords()[1], lat = pt_coords()[2]) %>% 
          flyTo(pt_coords()[1], pt_coords()[2], zoom = input$mymap_zoom)
        
      } else {
        #cat("Coordinates have not changed since the last time this button was clicked. Nothing to do. \n")
      }

    }
    
  })
  
  ## Render an error messages for time periods that are too short - historic period
  output$htmlout_basetime_msg <- renderUI({
    req(input$base_year)
    if (diff(input$base_year) < 10) {
      HTML("Caution: this time period may be too short to be representative of climate. See info button for details.")
    } else {
      NULL
    }
  })
  
  ## Render an error messages for time periods that are too short - projected future
  output$htmlout_prjtime_msg <- renderUI({
    req(input$prj_year)
    if (diff(input$prj_year) < 10) {
      HTML("Caution: this time period may be too short to be representative of climate. See info button for details.")
    } else {
      NULL
    }
  })
  
  ## Render the coordinates in the report section
  output$txtout_rpt_coords <- renderText({
    req(pt_coords())
    paste0("Coordinates: ", round(pt_coords()[1], 4),
           ", ", round(pt_coords()[2], 4))
  })
  
  ## Fetch data and start the analysis
  observeEvent(input$cmd_fetch, {

    ## report_memory("Memory usage as we start a new fetch")

    ## First thing is to check whether pt_coords() is up to date.
    ## If someone manually entered coordinates but did not click the
    ## show-on-map button, then the value in input$txtin_coords may
    ## be different than pt_coords(). (This is because I made a decision to *not*
    ## update pt_coords() whenever a character is typed in txtin_coords
    ## which caused an endless stream of validatation errors). If the 
    ## text in input$txtin_coords is different than pt_coords(), txtin_coords
    ## wins (because they only way they could be different is if someone typed
    ## in coordinates after clicking on the map.)
    
    if (input$txtin_coords != "") {

      ## Parse out the coordinates
      mycoords <- str_split(input$txtin_coords, ",")[[1]] %>%
        trimws() %>%
        as.numeric()
      
      if (NA %in% mycoords) {
        output$htmlout_fetch_msg <- renderUI(HTML("Error: Can not read coordinates. Please enter coordinates in decmial degrees separated by a comma.<br/>Example:  -120.226, 36.450"))
        req(FALSE)
        
      } else {
        ## If they're different, update pt_coords()
        if (!isTRUE(all.equal(pt_coords(), c(mycoords[1], mycoords[2]), tolerance = 0.0001))) {
          pt_coords(c(mycoords[1], mycoords[2]))
          output$htmlout_fetch_msg <- renderUI(NULL)
        }
        
      }  
       
    }

    ## Check if a location has been selected
    if (is.null(pt_coords())) {
      output$htmlout_fetch_msg <- renderUI(p("Please select a location by clicking on the map or entering coordinates, then try again."))
      
      ## Return NULL
      NULL
      
    } else {
      ## Erase any message that might be in htmlout_fetch_msg
      output$htmlout_fetch_msg <- renderUI(NULL)
      
      ## Create an API request object for projected climate
      cur_prj_cap <- ca_loc_pt(coords = c(pt_coords()[1], pt_coords()[2])) %>% 
        ca_gcm(input$prj_gcm) %>% 
        ca_scenario(input$prj_scenario) %>% 
        ca_period("day") %>% 
        ca_cvar(c("tasmax", "tasmin")) %>% 
        ca_dates(start = paste(input$prj_year[1] - 1, which(input$chill_month == month.abb), "01", sep = "-"), 
                 end = paste(input$prj_year[2], which(input$chill_month == month.abb) - 1, "30", sep = "-"))
        
      ## Only update pt_prj_cap() if it is different than the current settings.
      ## (otherwise it'll trigger fetching data all over again)
      ## The following works even if pt_prj_cap() is NULL (not yet initialized)
      
      if (!identical(cur_prj_cap, pt_prj_cap())) {
        pt_prj_cap(cur_prj_cap)  ## (this will trigger pt_prj_hourly_chill_tbl to update)
      }

      ## Create an API request object for baseline climate
      cur_base_cap <- ca_loc_pt(coords = c(pt_coords()[1], pt_coords()[2])) %>% 
        ca_gcm(input$base_gcm) %>% 
        ca_scenario(input$base_scenario) %>% 
        ca_period("day") %>% 
        ca_cvar(c("tasmax", "tasmin")) %>% 
        ca_dates(start = paste(input$base_year[1] - 1, which(input$chill_month == month.abb), "01", sep = "-"), 
               end = paste(input$base_year[2], which(input$chill_month == month.abb) - 1, "30", sep = "-"))
      
      ## Only update pt_base_cap() if it is different than the current settings.
      ## The following works even if pt_base_cap() is NULL (not yet initialized)
      if (!identical(cur_base_cap, pt_base_cap())) {
        pt_base_cap(cur_base_cap)   ## (this will trigger pt_base_hourly_chill_tbl)
      }
          
    }

  })
  
  ## The following will update whenever pt_prj_cap is updated
  ## (which is turn is updated when the plot button is clicked)
  ## It will *not* run however when the app first opens (because pt_prj_cap() will be NULL)
  pt_prj_hourly_chill_tbl <- eventReactive(pt_prj_cap(), {
    
    ## report_memory("Memory usage as we're about to update pt_prj_hourly_chill_tbl")
    
    ## Create a progress object that we can pass to ca_getvals_tbl()
    ## Make sure it closes when we exit this reactive, even if there's an error
    ## Create the message
    progress <- Progress$new()
    on.exit(progress$close())
    
    progress$set(value = 0, message = "Fetching data from Cal-Adapt: ", detail = "Projected daily min and max temp")
    
    ## Get values
    pt_prj_dtemp_tbl <- ca_getvals_tbl(pt_prj_cap(), quiet = TRUE, shiny_progress = progress) 

    ## Add Year, Month and Day, temp_c, growing season; pivot tasmin
    progress$set(message = "Processing Projected Data:", detail = "Formatting columns")
    
    pt_prj_ymd_gs_wide_tbl <- pt_prj_dtemp_tbl %>%
      mutate(Year = as.integer(substr(dt, 1, 4)), 
             Month = as.integer(substr(dt, 6, 7)),
             Day = as.integer(substr(dt, 9, 10)),
             temp_c = as.numeric(set_units(val, degC))) %>% 
      mutate(gs = case_when(Month < which(input$chill_month == month.abb) ~ Year,
                            Month >= which(input$chill_month == month.abb) ~ as.integer(Year + 1))) %>% 
      filter(!is.na(gs)) %>% 
      select(cvar, temp_c, Year, Month, Day, gcm, scenario, gs) %>%
      pivot_wider(names_from = cvar, values_from = temp_c) %>%
      rename(Tmax = tasmax, Tmin = tasmin)

    ## recover some memory
    rm(pt_prj_dtemp_tbl)
    
    ## Interpolate Hourly Temperatures in 'long' format
    ## This uses 'ca_make_hourly_temps', a modified version of chillr::make_hourly_temps
    progress$set(message = "Processing Projected Data:", detail = "Interpolating hourly temps")
    pt_prj_hrtmp_long <- pt_prj_ymd_gs_wide_tbl %>% 
      ca_make_hourly_temps(latitude = pt_coords()[2]) %>% 
      mutate(date_hour = lubridate::make_datetime(Year, Month, Day, Hour, tz = "America/Los_Angeles")) %>% 
      arrange(date_hour)

    ## recover some memory
    rm(pt_prj_ymd_gs_wide_tbl) 

    ## Compute hourly chill portions
    progress$set(message = "Processing Projected Data:", detail = "Computing hourly chill portions")
    
    ## Return a tibble with chill portions
    pt_prj_hrtmp_long %>% 
      group_by(gs, gcm, scenario) %>% 
      mutate(accum_chill_prtn = Dynamic_Model(Temp))
    
    ## report_memory("Memory when we're done creating chill portions by hour (projected)")
    
  }, ignoreNULL = TRUE)

  ## The following will update whenever pt_base_cap is updated
  ## (which is turn is updated when the plot button is clicked)
  pt_base_hourly_chill_tbl <- eventReactive(pt_base_cap(), {
    
    ## report_memory("Memory usage as we're about to update pt_base_hourly_chill_tbl")
    
    ## Create a progress object that we can pass to ca_getvals_tbl()
    ## Make sure it closes when we exit this reactive, even if there's an error
    ## Create the message
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Fetching data from Cal-Adapt: ", detail = "Historic daily min and max temp", value = 0)
    
    pt_base_dtemp_tbl <- ca_getvals_tbl(pt_base_cap(), quiet = TRUE, shiny_progress = progress) 
    
    progress$set(message = "Processing Historic Data:", detail = "Formatting columns")

    ## Add Year, Month, Day growing season column and temp_c
    pt_base_ymd_gs_wide_tbl <- pt_base_dtemp_tbl %>%
      mutate(Year = as.integer(substr(dt, 1, 4)), 
             Month = as.integer(substr(dt, 6, 7)),
             Day = as.integer(substr(dt, 9, 10)),
             temp_c = as.numeric(set_units(val, degC))) %>% 
      mutate(gs = case_when(Month < which(input$chill_month == month.abb) ~ Year,
                            Month >= which(input$chill_month == month.abb) ~ as.integer(Year + 1))) %>% 
      filter(!is.na(gs)) %>% 
      select(cvar, temp_c, Year, Month, Day, gcm, scenario, gs) %>%
      pivot_wider(names_from = cvar, values_from = temp_c) %>%
      rename(Tmax = tasmax, Tmin = tasmin)

    ## recover some memory
    rm(pt_base_dtemp_tbl)
    
    ## report_memory("Memory usage right before we call ca_make_hourly_temps for baseline")
    
    ## Compute hourly temperatures
    progress$set(message = "Processing Historic Data:", detail = "Interpolating hourly temps")
    
    pt_base_hrtmp_long <- pt_base_ymd_gs_wide_tbl %>% 
      ca_make_hourly_temps(latitude = pt_coords()[2]) %>% 
      mutate(date_hour = lubridate::make_datetime(Year, Month, Day, Hour, tz = "America/Los_Angeles")) %>% 
      arrange(date_hour)
    
    ## Recover some memory
    rm(pt_base_ymd_gs_wide_tbl)
    
    ## Compute hourly chill portions
    progress$set(message = "Processing Historic Data:", detail = "Computing hourly chill portions")
    
    ## Return the tibble with hourly chill portions column
    pt_base_hrtmp_long %>% 
      group_by(gs, gcm, scenario) %>% 
      mutate(accum_chill_prtn = Dynamic_Model(Temp))
    
    ## report_memory("Memory usage when we're done creating pt_base_hourly_chill_tbl")
    
  }, ignoreNULL = TRUE)

  ## Output status text (for testing)
  output$txt_status <- renderText({
    req(pt_prj_hourly_chill_tbl())
    NULL
  })

  ## Compute the end-of-season chill for PROJECTED climate for each 
  ## growing season, emissions scenario, and GCM, 
  ## This uses pt_prj_hourly_chill_tbl (cumulative chill by hour)
  prj_eos_chill_tbl <- reactive({
    req(pt_prj_hourly_chill_tbl())
    pt_prj_hourly_chill_tbl() %>% 
      group_by(scenario, gs, gcm) %>% 
      summarise(max_chill = max(accum_chill_prtn), .groups = 'drop') %>% 
      mutate(time_period = paste0(isolate(input$prj_year[1]), "-", isolate(input$prj_year[2])))
  })
  
  ## Compute the end-of-season chill for BASELINE climate for each 
  ## growing season, emissions scenario, and GCM, 
  base_eos_chill_tbl <- reactive({
    req(pt_base_hourly_chill_tbl())
    pt_base_hourly_chill_tbl() %>% 
      group_by(scenario, gs, gcm) %>% 
      summarise(max_chill = max(accum_chill_prtn), .groups = 'drop') %>% 
      mutate(time_period = paste0(isolate(input$base_year[1]), "-", isolate(input$base_year[2])))
  })
  
  ## Compute the safe chill table (i.e., 10% quantile)
  comb_safe_chill_tbl <- reactive({
    req(!is.null(prj_eos_chill_tbl()) || !is.null(base_eos_chill_tbl())  )
    
    ## Compute safe winter chill for each RCP
    base_eos_chill_tbl() %>% 
      bind_rows(prj_eos_chill_tbl()) %>%
      group_by(time_period, scenario) %>% 
      summarize(safe_chill = quantile(max_chill, probs = 1 - as.numeric(input$safe_chill) / 100), .groups = "drop")
  })

  ## Compute the min and max EOS chill to use as the xrange on the histograms
  
  ## Compute the safe chill table (i.e., 10% quantile)
  comb_eos_chill_range <- reactive({
    req(!is.null(prj_eos_chill_tbl()) && !is.null(base_eos_chill_tbl())  )
    range(range(prj_eos_chill_tbl() %>% pull(max_chill)),
          range(base_eos_chill_tbl() %>% pull(max_chill)))
  })
  
  ## Compute the table of the percentage of runs that reach required chill - PROJECTED
  prj_req_chill_pct_tbl <- reactive({
    req(prj_eos_chill_tbl())

    ## For each scenario, compute the percent of runs that hit the required chill
    prj_eos_chill_tbl() %>%
      group_by(gs, gcm, scenario) %>%
      summarise(reached_thresh = max(max_chill) >= as.numeric(input$req_chill), .groups = 'drop') %>%
      group_by(scenario) %>%
      summarise(percent_reached_thresh = sum(reached_thresh) / n(), .groups = 'drop') %>%
      mutate(time_period = paste0(isolate(input$prj_year[1]), "-", isolate(input$prj_year[2])), percent_reached_thresh = scales::percent(percent_reached_thresh)) %>%
      relocate(time_period, scenario, percent_reached_thresh)

  })

  ## Compute the table of the percentage of runs that reach required chill - BASELINE
  base_req_chill_pct_tbl <- reactive({
    req(base_eos_chill_tbl())
    
    ## For each scenario, compute the percent of runs that hit the required chill
    base_eos_chill_tbl() %>%
      group_by(gs, gcm, scenario) %>%
      summarise(reached_thresh = max(max_chill) >= as.numeric(input$req_chill), .groups = 'drop') %>%
      group_by(scenario) %>%
      summarise(percent_reached_thresh = sum(reached_thresh) / n(), .groups = 'drop') %>%
      mutate(time_period = paste0(isolate(input$base_year[1]), "-", isolate(input$base_year[2])), percent_reached_thresh = scales::percent(percent_reached_thresh)) %>%
      relocate(time_period, scenario, percent_reached_thresh)
    
  })
  
  ## Combine the tables of proportion of runs that reach required chill
  comb_req_chill_pct_tbl <- reactive({
    req(!is.null(prj_req_chill_pct_tbl()) || !is.null(base_req_chill_pct_tbl())  )
    
    ## Combine the percentages of runs that reach the threshhold
    base_req_chill_pct_tbl() %>% 
      bind_rows(prj_req_chill_pct_tbl())
  })
  
  ## Render the safechill table
  output$dt_safe_chill <- DT::renderDataTable({
    req(comb_safe_chill_tbl())
    
    DT::datatable(comb_safe_chill_tbl() %>% 
                mutate(safe_chill = round(safe_chill, 2)), 
              rownames= FALSE, 
              extensions = 'Buttons',
              options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                info = FALSE,
                dom = 'tB',
                buttons = c('copy', 'csv')
              ),
              class = "display")
  })

  ## Render the tagline for the safe chill section
  output$html_safe_chill_lbl <- renderUI({
    req(comb_safe_chill_tbl())
    HTML(paste0("<p>The table below shows the minimum chill portions that can be expected <strong>", input$safe_chill, "% of the time</strong> for each time period and emissions scenario.</p>"))
  })
  
  ## Render the tagline for the histograms
  output$html_chill_histograms_lbl <- renderUI({
    req(comb_safe_chill_tbl())
    HTML(paste0("<p>The histograms below show the distribution of end-of-season chill portions. The vertical red line represents the <em>safe chill</em> level (see next table).</p>"))
  })

  ## Populate the required chill table
  output$dt_req_chill_pct <- DT::renderDataTable({
    req(comb_req_chill_pct_tbl())
    
    DT::datatable(comb_req_chill_pct_tbl(),
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                info = FALSE,
                dom = 'tB',
                buttons = c('copy', 'csv')
              ),
              class = "display")
    
  })

  ## Render the tagline for the required chill section
  output$html_req_chill_lbl <- renderUI({
    req(comb_req_chill_pct_tbl())
    HTML(paste0("<p>The table below shows the probability of getting at least <strong>", input$req_chill, " chill portions</strong> by the end of the growing season for each time period and emissions scenario.</p>"))
  })
  
  ## Render the tagline for the required chill section
  output$html_chill_ports_ts_lbl <- renderUI({
    req(comb_req_chill_pct_tbl())
    HTML(paste0("<p>The plots below show the accumulation of chill portions during the growing season for each time period and emissions scenario. The horizonal black line indicates <strong>", input$req_chill, " chill portions</strong>.</p>"))
  })

  ## Render the text location name in the results section
  output$txtout_rpt_locname <- renderText({input$txtin_ptname})

  ## Output the baseline histogram
  output$hist_eos_chill_base <- renderPlot({
    req(!is.null(base_eos_chill_tbl()))
    
    ## report_memory("Memory usage right before rendering the histogram for baseline")
    
    # eos_chill_base_tbl <- base_eos_chill_tbl() %>% 
    #   filter(scenario == "historical")
    
    if (nrow(base_eos_chill_tbl()) == 0) {
      NULL
    } else {
      ggplot(data = base_eos_chill_tbl(), aes(x=max_chill)) + 
        geom_histogram() +
        xlim(comb_eos_chill_range()) +
        labs(title = "Historical: End-of-Season Chill Portion", 
             subtitle = paste0("Modelled historic data. ", isolate(input$base_year[1]), " - ", isolate(input$base_year[2])), 
             caption = paste0(
               "Data source: Cal-Adapt.org\n",
               str_wrap(paste0("GCMs: ", paste(isolate(input$base_gcm), collapse = ", ")),
                                 width = 150)),
             x = "end-of-season chill portion", 
             y = "count") +
        geom_vline(data = comb_safe_chill_tbl() %>% 
                     filter(scenario == "historical"), 
                   color = "red", aes(xintercept = safe_chill), size = 1) +
        theme(plot.caption = element_text(hjust = 0),
              plot.margin = margin(20, 20, 20, 20),
              plot.background = element_rect(colour = "gray50", size = 3))
    }
  })
  
  ## Output the RCP45 histogram
  output$hist_eos_chill_rcp45 <- renderPlot({
    req(prj_eos_chill_tbl())
    ## report_memory("Memory usage right before rendering the histogram for rcp45")
    eos_chill_45_tbl <- prj_eos_chill_tbl() %>% 
      filter(scenario == "rcp45")
    if (nrow(eos_chill_45_tbl) == 0) {
      NULL
    } else {
      ggplot(data = eos_chill_45_tbl, aes(x=max_chill)) + 
        geom_histogram() +
        xlim(comb_eos_chill_range()) +
        labs(title = "RCP 4.5: End-of-Season Chill Portion", 
             subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
             caption = paste0(
               "Data source: Cal-Adapt.org\n",
               str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                        width = 120)),
             x = "end-of-season chill portion", 
             y = "count") +
        geom_vline(data = comb_safe_chill_tbl() %>% 
                     filter(scenario == "rcp45"), 
                   color = "red", aes(xintercept = safe_chill), size = 1) +
        theme(plot.caption = element_text(hjust = 0),
              plot.margin = margin(20, 20, 20, 20),
              plot.background = element_rect(colour = "gray50", size = 3))
    }
  })
  
  ## Output the RCP85 histogram
  output$hist_eos_chill_rcp85 <- renderPlot({
    req(!is.null(prj_eos_chill_tbl()))
    ## report_memory("Memory usage right before rendering the histogram for rcp85")
    eos_chill_85_tbl <- prj_eos_chill_tbl() %>% 
      filter(scenario == "rcp85")
    if (nrow(eos_chill_85_tbl) == 0) {
      NULL
    } else {
      ggplot(data = eos_chill_85_tbl, aes(x=max_chill)) + 
        geom_histogram() +
        xlim(comb_eos_chill_range()) +
        labs(title = "RCP 8.5: End-of-Season Chill Portion", 
             subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
             caption = paste0(
               "Data source: Cal-Adapt.org\n",
               str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                                 width = 150)),
             x = "end-of-season chill portion", 
             y = "count") +
        geom_vline(data = comb_safe_chill_tbl() %>% 
                     filter(scenario == "rcp85"), 
                   color = "red", aes(xintercept = safe_chill), size = 1) +
        theme(plot.caption = element_text(hjust = 0),
              plot.margin = margin(20, 20, 20, 20),
              plot.background = element_rect(colour = "gray50", size = 3))
    }
  })
  
  ## Render the chill portion time series for the historical period
  output$plot_chillts_hist <- renderPlot({
    req(pt_base_hourly_chill_tbl())
    ## report_memory("Memory usage right before rendering time series plot for historical")
    
    ## Convert the month to an integer
    chill_month_int <- which(month.abb == isolate(input$chill_month))

    ## Generate the plot
    ggplot(data = pt_base_hourly_chill_tbl() %>%
             filter(Hour == 0) %>% 
             mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
             mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                    gs_gcm = paste(gs, gcm, sep = "_")) %>%
             select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
           aes(x = date_plot, y = accum_chill_prtn)) +
      geom_line(aes(color=gs_gcm), show.legend = FALSE) +
      geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Historical: Chill Portion Accumulation",
           subtitle = paste0(isolate(input$base_year[1]), " - ", isolate(input$base_year[2])), 
           caption = paste0(
             "Data source: Cal-Adapt.org\n",
             str_wrap(paste0("GCMs: ", paste(isolate(input$base_gcm), collapse = ", ")),
                      width = 80)),
          x = NULL, y = "Chill Portion") +
      theme(plot.caption = element_text(hjust = 0),
            plot.margin = margin(20, 20, 20, 20),
            plot.background = element_rect(colour = "gray50", size = 3))

  })
  
  
  ## Render the chill portion time series for RCP45
  output$plot_chillts_rcp45 <- renderPlot({
    req(pt_prj_hourly_chill_tbl())
    ## report_memory("Memory usage right before rendering time series plot for rcp45")
    ## Convert the month to an integer
    chill_month_int <- which(month.abb == isolate(input$chill_month))
    
    ## Create a tibble containing accumulated seasonal chill portions by day
    # prj_daily_chill_tbl <- pt_prj_hourly_chill_tbl() %>%
    #   filter(Hour == 0, scenario == "rcp45") %>% 
    #   mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
    #   mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
    #          gs_gcm = paste(gs, gcm, sep = "_")) %>%
    #   select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn)
    
    ## Generate the plot
    ggplot(data = pt_prj_hourly_chill_tbl() %>%
             filter(Hour == 0, scenario == "rcp45") %>% 
             mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
             mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                    gs_gcm = paste(gs, gcm, sep = "_")) %>%
             select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
           aes(x = date_plot, y = accum_chill_prtn)) +
      geom_line(aes(color=gs_gcm), show.legend = FALSE) +
      geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "RCP 4.5: Chill Portion Accumulation",
           subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
           caption = paste0(
             "Data source: Cal-Adapt.org\n",
             str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                      width = 80)),
           x = NULL, y = "Chill Portion") +
      theme(plot.caption = element_text(hjust = 0),
            plot.margin = margin(20, 20, 20, 20),
            plot.background = element_rect(colour = "gray50", size = 3))
    
  })
  
  ## Render the chill portion time series for RCP85
  output$plot_chillts_rcp85 <- renderPlot({
    req(pt_prj_hourly_chill_tbl())
    ## report_memory("Memory usage right before rendering time series plot for rcp85")
    
    ## Convert the month to an integer
    chill_month_int <- which(month.abb == isolate(input$chill_month))
    
    ## Create a tibble containing accumulated seasonal chill portions by day
    # prj_daily_chill_tbl <- pt_prj_hourly_chill_tbl() %>%
    #   filter(Hour == 0, scenario == "rcp85") %>% 
    #   mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
    #   mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
    #          gs_gcm = paste(gs, gcm, sep = "_")) %>%
    #   select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn)
    
    ## Generate the plot
    ggplot(data = pt_prj_hourly_chill_tbl() %>%
             filter(Hour == 0, scenario == "rcp85") %>% 
             mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
             mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                    gs_gcm = paste(gs, gcm, sep = "_")) %>%
             select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
           aes(x = date_plot, y = accum_chill_prtn)) +
      geom_line(aes(color=gs_gcm), show.legend = FALSE) +
      geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "RCP 8.5: Chill Portion Accumulation",
           subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
           caption = paste0(
             "Data source: Cal-Adapt.org\n",
             str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                      width = 80)),
           x = NULL, y = "Chill Portion") +
      theme(plot.caption = element_text(hjust = 0),
            plot.margin = margin(20, 20, 20, 20),
            plot.background = element_rect(colour = "gray50", size = 3))
    
  })
  
  #############################################################
  ## CONDITIONAL PANEL EXPRESSIONS
  #############################################################
  
  ## Show the safechill conditional panel (label)
  output$safe_chill_computed <- reactive({
    !is.null(comb_safe_chill_tbl())
  })
  outputOptions(output, "safe_chill_computed", suspendWhenHidden = FALSE)
  
  # ## Show the req_chill conditional panel (label)
  # output$req_chill_computed <- reactive({
  #   !is.null(comb_safe_chill_tbl()) ################################################################################# <<<<<<<
  # })
  # outputOptions(output, "req_chill_computed", suspendWhenHidden = FALSE)
  
  ## Populate the end-of-season chill table - removed, TMI
  # output$dt_eos_chill <- renderDataTable({
  #   req(!is.null(prj_eos_chill_tbl()))
  #   prj_eos_chill_tbl() %>% head()
  # })

}

shinyApp(ui, server, enableBookmarking = "url")

