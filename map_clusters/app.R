#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# loading necessary data and functions
library(here)
source(here("R"," make_leaflet_maps.R"))


# Define UI for map application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Job clusters in U.S. metros"),
   p("This map displays tracts with job density in the top 5% of the distribution in their metro."),
   div(),
   
   # select box for metro names
   selectInput("cbsa_name",
               label = "Choose a metro area",
               choices = unique(cbsa_xwalk$cbsa_name[cbsa_xwalk$top100==1]),
               selected = "Baltimore-Columbia-Towson, MD"),
   
    # leaflet map
   leafletOutput("cbsa"),
   
   # test splotch
   textOutput("test_text")
   
)

# Define server logic required to draw map
server <- function(input, output) {
  
  # # get CBSA name from user input
  # cbsa_name <- renderPrint({ input$select})
  
  # set latitude and longitude
  
  # create color scheme
  pal0 <- colorBin(colorRamp(c("#E0ECFB", "#00649F"), interpolate = "spline"), 
                   cbsa$most_dense, bin = 2)

  # draw the map
  output$cbsa <- renderLeaflet({

    # get a CBSA name from user input
    cbsa_name <- input$cbsa_name
    cbsa_id <- unique(top100_xwalk$cbsa[top100_xwalk$cbsa_name==cbsa_name])

    # select tracts in that CBSA
    cbsa <- select_cbsa_tracts(density,
                               cbsa_id,
                               xwalk = top100_xwalk)

    # create cbsa-specific tract shapefile with data
    cbsa.shp <-create_cbsa_shp(cbsa)
    
    # gets coords for initial map location
    coords <- filter(top100_coords, cbsa==cbsa_id)

    # set labels
    labels <-sprintf("<strong>Tract %s</strong><br/>%g jobs / mi <sup> 2 </sup",
                      cbsa.shp@data$tract, cbsa.shp@data$density) %>%
      lapply(htmltools::HTML)
    
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

