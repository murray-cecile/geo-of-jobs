##==================================================================================##
# GEO OF JOBS: SHINY APP FOR JOB CLUSTER VIZ
#  Creates interactive map with dropdown 
#
# Cecile Murray
# April 2018
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

# use this if running locally ONLY; comment out for Shiny server
# setwd(paste0(here(), "/map_clusters"))

# loading necessary data and functions
source("prepare_data.R")

# Define UI for map application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Job density in U.S. metros"),
   
   # header for cluster maps
   h3("Job clusters"),
   
   # MAKE THIS ADJUST FOR EACH METRO
   p("This dashboard explores job density (jobs per square mile) in the top 100 U.S. metro areas in 2010 and 2015. 
     We identify job clusters as Census tracts with job density at the top part of the distribution 
     in their metro."),
   br(),
   
   # select box for metro names
   selectInput("cbsa_name",
               label = "Choose a metro area",
               choices = sort(unique(cbsa_xwalk$cbsa_name[cbsa_xwalk$top100==1])),
               selected = "Baltimore-Columbia-Towson, MD"),
   br(),
   
   # split layout for radio buttons and slider
   splitLayout(
   
   # radio buttons for density
   radioButtons("radio", label = "Density threshold",
                choices = list("Top 5 percent" = 1, 
                               "Top 10 percent" = 2,
                               "Top 20 percent" = 3),
                selected = 2),
   
   # slider for years
   sliderInput("slider", label = "Year",
               min = 2010, max = 2015, value = 2015, step = 5, sep = ""),
   
   br()
   
   ),
    # leaflet map of the job clusters
   leafletOutput("cbsa"),
   
   # Source line for the map
   textOutput("source"),
   br(),
   
   # Descriptive stats
   h4("Descriptive stats for this metro area:"),
   
   # overview stats that are the same from year to year
   tableOutput("overview"),
   
   # descriptive stats that show 2010 and 2015
   tableOutput("descriptive_stats"),
   
   # histogram
   plotOutput("distribution")

)

# Define server logic required to draw map
server <- function(input, output) {
  
  output$print_radio <- renderPrint({input$radio == "2"})
  
  # draw the map
  output$cbsa <- renderLeaflet({

    # get a CBSA name from user input
    cbsa_name <- input$cbsa_name
    cbsa_id <- unique(top100_xwalk$cbsa[top100_xwalk$cbsa_name==cbsa_name])

    # get year from user input
    if(input$slider==2010){
      density <- density10
    } else {
      density <- density15
    }
    
    # get the selected density threshold from user input, rename variable
    thresh <- "default"
    thresh <- case_when(
      input$radio == 1 ~ "mapvar_20",
      input$radio == 2 ~ "mapvar_10",
      input$radio == 3 ~ "mapvar_5"
    )
    density %<>% dplyr::rename(mapvar = !!thresh)
    
    # select tracts in that CBSA
    cbsa <- select_cbsa_tracts(density,
                               cbsa_id,
                               xwalk = top100_xwalk)

    # create cbsa-specific tract shapefile with data
    cbsa.shp <-create_cbsa_shp(cbsa)
    
    # gets coords for initial map location
    coords <- filter(top100_coords, cbsa==cbsa_id)

    # set labels
    labels <- sprintf("<strong>Tract %s</strong><br/>%g jobs in %g mi<sup>2</sup> = density of %g",
                      cbsa.shp@data$tract, cbsa.shp@data$job_tot, cbsa.shp@data$ALAND_SQMI,
                      cbsa.shp@data$density) %>%
      lapply(htmltools::HTML)
    
    
    # create color scheme
    # pal0 <- colorBin(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
    #                  cbsa$most_dense_10, bins = 2)
    pal0 <- colorFactor(c("#FFCF1A", "#E0ECFB", "#A4C7F2", "#3E83C1", "#00649F"),
                        as.factor(cbsa$mapvar))
    
    # make the leaflet map
    leaflet(cbsa.shp) %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 9) %>%
      addTiles() %>%
      addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.75,
                  fillColor = ~pal0(mapvar),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(list("font-weight" = "normal",
                                                   padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
      addLegend(pal = pal0, values = ~mapvar, opacity = 0.75, title = NULL,
                position = "bottomright")

  })

  output$test_text <- renderText ({
    paste("Source: LEHD LODES")
    # # get a CBSA name from user input
    # cbsa_name <- input$cbsa_name
    # coords <- filter(top100_coords, cbsa==cbsa_id)
    # paste("These are supposed to be the coordinates:", coords$lon, coords$lat)
  })
  
  # creates overview table
  output$overview <- renderTable ({
    
    filter(select(met_summary15, cbsa_name, tr_ct_sum, contains("most_dense")),
                  cbsa_name==input$cbsa_name) %>%
      dplyr::rename(CBSA = cbsa_name, 
                    `Number of tracts` = tr_ct_sum,                
                    `Tracts in top 20%` = most_dense_5_sum,
                    `Tracts in top 10%` = most_dense_10_sum,
                    `Tracts in top 5%` = most_dense_20_sum) %>%
      gather()
  }, colnames = FALSE)
  
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

