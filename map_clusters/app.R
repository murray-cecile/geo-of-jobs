##==================================================================================##
# GEO OF JOBS: SHINY APP FOR JOB CLUSTER VIZ
#  Creates interactive map with dropdown 
#
# Cecile Murray
# Created April 2018; last updated June 2018
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
library(ggridges)
library(viridis)

# # use this ONLY if running locally and with here()
# # comment out before uploading to Shiny server!
# library(here)
# setwd(paste0(here(), "/map_clusters"))

# loading necessary data and functions
source("prepare_data.R")

# Define UI: this part controls the app layout and explanatory text
ui <- fluidPage(
   
   # Application title
   titlePanel("Job density in U.S. metros"),
   
   # header for cluster maps
   h3("Job clusters"),
   
   # explanation of job hub definition
   p("This dashboard explores the geography of job clusters in 98 of the top 100 U.S.
      metro areas in 2010 and 2015 (we exclude Boston and Worcester due to data
      limitations). We define job clusters as Census tracts that meet two critera:"),
   
   # definitional criteria
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
               choices = sort(unique(top100_coords$cbsa_name)),
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
   
   # explanatory paragraph above the map
   p("Tracts identified as clusters are shown on the map in orange; tracts that meet
     the density threshold (criteria #1) but do not contain the specified minimum
     job percentage (criteria #2) are shown in yellow. All other tracts are colored
     according to where they fall in the distribution of job density in the metro area."),
   br(),
   
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

# Define server: this part handles all of the data
server <- function(input, output) {
  
  # grab the cbsa name from user input
  output$CBSA <- renderText({unlist(str_split(input$cbsa_name, "-"))[[1]]})
  
  # produce filtered shapefile using user input
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
    
    if(input$job_min > 0){
      colorFactor(c("#FF5E1A", "#FFCF1A","#E0ECFB", "#00649F", "#3E83C1", "#A4C7F2"),
                  levels = c("cluster", "high density", "low density", "p90",
                             "p85", "p80"))
    } else {
      colorFactor(c("#FF5E1A", "#E0ECFB", "#A4C7F2", "#3E83C1", "#00649F"),
                  levels = c("cluster", "low density", "p90",
                             "p85", "p80"))
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
    
    # get threshold from user input and recode
    thresh <- "default"
    thresh <- case_when(
      input$radio == 1 ~ "mapvar_20",
      input$radio == 2 ~ "mapvar_10",
      input$radio == 3 ~ "mapvar_5"
    )
    
    # filter by year and density threshold
    plot_data <- filter(density, cbsa_name == input$cbsa_name,
                        year == input$year_slider,
                        num_quant == thresh)
    
    # filter density thresholds to cbsa, identify p80 threshold
    cbsa_thresh <- filter(thresholds, cbsa_name == input$cbsa_name)
    p80 <- cbsa_thresh[cbsa_thresh$percentile == "p80",
                       paste0("n_", input$year_slider)]
    p90 <- cbsa_thresh[cbsa_thresh$percentile == "p90",
                          paste0("n_", input$year_slider)]
    p95 <- cbsa_thresh[cbsa_thresh$percentile == "p95",
                       paste0("n_", input$year_slider)]
    
    ggplot(plot_data) +
      geom_density(aes(x = density), fill = "#23A7E0", color = "gray90") +
      geom_vline(aes(xintercept = p80), show.legend = TRUE) +
      geom_text(aes(label = "p80", x = p80 + 250, y = .0006)) +
      geom_vline(aes(xintercept = p90), show.legend = TRUE) +
      geom_text(aes(label = "p90", x = p90 + 250, y = .0006)) +
      geom_vline(aes(xintercept = p95), show.legend = TRUE) +
      geom_text(aes(label = "p95", x = p95 + 250, y = .0006)) +
      labs(title = paste0("Distribution of job density in ", input$cbsa_name, 
                          ", ", input$year_slider),
           x = "Jobs per square mile", y = "") +
      xlim(0, 15000) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



