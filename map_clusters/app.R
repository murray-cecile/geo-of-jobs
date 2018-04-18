##==================================================================================##
# GEO OF JOBS: SHINY APP FOR JOB CLUSTER VIZ
#  Creates interactive map with dropdown 
#
# Cecile Murray
# April 2018
##==================================================================================##

library(shiny)

# loading necessary data and functions
library(here)
source(here("R","make_leaflet_maps.R"))


# Define UI for map application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Job density in U.S. metros"),
   
   # header for cluster maps
   h3("Job clusters"),
   
   # MAKE THIS ADJUST FOR EACH METRO
   p("This map displays job density (jobs per square mile) by census tract. 
     Census tracts with job density at the top part of the distribution in their metro are highlighted."),
   br(),
   
   # select box for metro names
   selectInput("cbsa_name",
               label = "Choose a metro area",
               choices = sort(unique(cbsa_xwalk$cbsa_name[cbsa_xwalk$top100==1])),
               selected = "Baltimore-Columbia-Towson, MD"),
   br(),
   
   # radio buttons
   radioButtons("radio", label = h3("Density threshold"),
                choices = list("Top 5 percent" = 1, 
                               "Top 10 percent" = 2,
                               "Top 20 percent" = 3),
                selected = 2),
   br(),
   
    # leaflet map of the job clusters
   leafletOutput("cbsa"),
   
   # Source line for the map
   textOutput("source")
)

# Define server logic required to draw map
server <- function(input, output) {
  
  output$print_radio <- renderPrint({input$radio == "2"})
  
  # draw the map
  output$cbsa <- renderLeaflet({

    # get a CBSA name from user input
    cbsa_name <- input$cbsa_name
    cbsa_id <- unique(top100_xwalk$cbsa[top100_xwalk$cbsa_name==cbsa_name])

    # get the selected density threshold from user input, rename variable
    thresh <- "default"
    thresh <- case_when(
      input$radio == 1 ~ "most_dense_20",
      input$radio == 2 ~ "most_dense_10",
      input$radio == 3 ~ "most_dense_5"
    )
    density %<>% dplyr::rename(most_dense = !!thresh)
    
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
    pal0 <- colorFactor(c("#E0ECFB", "#FFCF1A"), as.factor(cbsa$most_dense))
    
    # make the leaflet map
    leaflet(cbsa.shp) %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 9) %>%
      addTiles() %>%
      addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.75,
                  fillColor = ~pal0(most_dense),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(list("font-weight" = "normal",
                                                   padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
      addLegend(pal = pal0, values = ~most_dense, opacity = 0.75, title = NULL,
                position = "bottomright")

  })

  output$test_text <- renderText ({
    paste("Source: LEHD LODES")
    # # get a CBSA name from user input
    # cbsa_name <- input$cbsa_name
    # coords <- filter(top100_coords, cbsa==cbsa_id)
    # paste("These are supposed to be the coordinates:", coords$lon, coords$lat)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

