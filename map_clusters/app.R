##==================================================================================##
# GEO OF JOBS: SHINY APP FOR JOB CLUSTER VIZ
#  Creates interactive map with dropdown 
#
# Cecile Murray
# Created April 2018; last updated May 2018
##==================================================================================##

library(shiny)
library(tidyverse)
library(magrittr)
library(sp)
library(sf)
library(rgdal)
library(stringr)
library(janitor)
library(tigris)
library(foreign)
library(leaflet)

# use this ONLY if running locally and with here(); comment out for Shiny server
# library(here)
# setwd(paste0(here(), "/map_clusters"))

# loading necessary data and functions
source("prepare_data.R")

# Define UI for map application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Job density in U.S. metros"),
   
   # header for cluster maps
   h3("Job clusters"),
   
   # explanation of job hub definition
   p("This dashboard explores the geography of job clusters in the top 100 U.S.
      metro areas in 2010 and 2015. We define job clusters as Census tracts that
      meet two critera:"),
   
   tags$ol(
     tags$li(tags$b("High job density:"), "they rank in the top part of the
             distribution of job density (jobs per square mile) within their metro."),
     tags$li(tags$b("High job count:"), "they contain a minimally significant
             percentage of their metro's jobs.")
     ),
   
   br(),
   
   # select box for metro names
   selectInput("cbsa_name",
               label = "Choose a metro area",
               choices = sort(unique(cbsa_xwalk$cbsa_name[cbsa_xwalk$top100==1])),
               selected = "Baltimore-Columbia-Towson, MD"),
   br(),
   
   # slider for years (currently set up for only 2010 and 2015)
   sliderInput("year_slider", label = "Year",
               min = 2010, max = 2015, value = 2015, step = 5, sep = ""),
   br(),
   
   # split layout for radio buttons and sliders
   splitLayout(
   
   # radio buttons for density
   radioButtons("radio", label = "Density threshold",
                choices = list("Top 5 percent" = 1, 
                               "Top 10 percent" = 2,
                               "Top 20 percent" = 3),
                selected = 2),
   
   # slider for minimum job threshold
   sliderInput("job_min", label = "Minimum job threshold (% of metro jobs)", 
               min = 0, max = 1, step = 0.25, value = 0),
   
   br()
   
   ),
    # leaflet map of the job clusters
   leafletOutput("cbsa"),
   
   # Source line for the map
   textOutput("source"),
   br(),
   
   # Descriptive stats
   h4("Descriptive stats for this metro area:"),
   textOutput("descriptive_text"),
   br(),
   
   # overview stats that are the same from year to year
   tableOutput("overview"),
   
   # descriptive stats that show 2010 and 2015
   # tableOutput("descriptive_stats"),
   
   # histogram
   plotOutput("distribution")

)

# Define server logic required to draw map
server <- function(input, output) {
  
  # cbsa name
  output$CBSA <- renderText({unlist(str_split(input$cbsa_name, "-"))[[1]]})
  
  # reactive expression to get filtered shapefile
  filteredShp <- reactive({

    # get a CBSA ID from user input
    cbsa_id <- unique(top100_xwalk$cbsa[top100_xwalk$cbsa_name==input$cbsa_name])
    
    # get threshold from user input and recode
    thresh <- "default"
    thresh <- case_when(
      input$radio == 1 ~ "mapvar_20",
      input$radio == 2 ~ "mapvar_10",
      input$radio == 3 ~ "mapvar_5"
    )
    
    # filter by year and density threshold
    filteredData <- filter(density,
                           year==input$year_slider,
                           num_quant==thresh)
    
    # recode according to job threshold
    filteredData %<>% recode_job_min(cbsa_id = cbsa_id,
                                     yr = input$year_slider,
                                     thresh = thresh,
                                     min_pp = input$job_min,
                                     met_jobs = met_jobs)
    
    # select tracts in that CBSA and create cbsa-specific tract shapefile with data
    create_cbsa_shp(select_cbsa_tracts(filteredData,
                                                  cbsa_id,
                                                  xwalk = top100_xwalk))

  })
  
  # define function to assign colors to polygons
  colorPal <- reactive({
    cbsa.shp <- filteredShp()
    
    if(input$job_min > 0 ){
      colorFactor(c("#FF5E1A", "#FFCF1A","#E0ECFB", "#A4C7F2", "#3E83C1", "#00649F"),
                  as.factor(cbsa.shp@data$mapvar))
    } else {
      colorFactor(c("#FF5E1A", "#E0ECFB", "#A4C7F2", "#3E83C1", "#00649F"),
                  as.factor(cbsa.shp@data$mapvar))
    }
  })
  
  # sets coordinates for start point
  Coords <- reactive({
    coords <- filter(top100_coords, cbsa_name==input$cbsa_name)
    })
  
  # draw the map (this part remains static unless the CBSA changes)
  output$cbsa <- renderLeaflet({

    coords <- Coords()
    
    # make the leaflet map
    leaflet() %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 9) %>%
      addTiles() 
  })

  # Observer that updates map with new selected data
  observe({
    
    pal0 <- colorPal()
    
    # set labels
    labeldata <- filteredShp()@data
    labels <- sprintf("<strong>Tract %s</strong><br/>%g jobs in %g mi<sup>2</sup> = density of %g",
                      labeldata$tract, labeldata$job_tot, labeldata$ALAND_SQMI,
                      labeldata$density) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("cbsa", data = filteredShp()) %>%
      clearShapes() %>%
      addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.75,
                  fillColor = ~pal0(mapvar),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(list("font-weight" = "normal",
                                                   padding = "3px 8px"),
                                              textsize = "15px", direction = "auto"))
  })
  
  # observer to update legend with new data selection
  observe({
    proxy <- leafletProxy("cbsa", data = filteredShp())
    
    proxy %>% clearControls() %>%
      addLegend(pal = colorPal(), values = ~mapvar, opacity = 0.75,
                title = NULL, position = "bottomright")
  })

  # creates text for descriptive stats section
  output$descriptive_text <- renderText({
    met <- filter(met_summary, year==input$year_slider, cbsa_name==input$cbsa_name)
    
    paste0(input$cbsa_name,
           " has ",
           format(met$tr_ct[1], big.mark = ","),
           " Census tracts, which contained ",
           format(met$jobs[1], big.mark = ","), 
           " jobs in ",
           format(input$year_slider),
           ".")
  })
  
  # creates overview table
  output$overview <- renderTable ({
    
    filter(select(ungroup(met_summary), cbsa_name, year, num_quant, min, 
                  cluster_jobs, cluster_ct, job_share),
           year==input$year_slider,
           cbsa_name==input$cbsa_name) %>%
      arrange(min, num_quant) %>% 
      dplyr::rename(CBSA = cbsa_name, 
                    `Density threshold` = num_quant,
                    `Min % jobs` = min,
                    `Jobs in clusters` = cluster_jobs,
                    `Number of clusters` = cluster_ct,
                    `% jobs in clusters` = job_share) %>%
      select(-CBSA, -year)
  }, colnames = TRUE)
  
  # creates table of desriptive stats for 2010 and 2015
  output$descriptive_stats <- renderTable ({
    
    filter(select(met_summary, -cbsa), CBSA==input$cbsa_name,
           name %in% c("Total jobs")) %>%
      select(-CBSA) %>%
      spread(year, n)
  }, colnames = FALSE)
  
  # creates density plot
  output$distribution <- renderPlot({
    ggplot() +
      geom_histogram(data =  filter(density15, cbsa_name==input$cbsa_name),
                     aes(x = density), binwidth=25, color = "#00649f") +
      geom_histogram(data = filter(density10, cbsa_name==input$cbsa_name),
                     aes(x = density), binwidth=25, color = "#8AC6FF") +
    labs(x = "Jobs per square mile", y = "") +
      xlim(0, 30000)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

