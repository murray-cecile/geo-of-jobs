#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Job clusters in US metro areas"),
   
   # Drop down menu
   
   
   # leaflet map
   leafletOutput("cle"),
   p()
   
)

# Define server logic required to draw map
server <- function(input, output) {
  

  # draw the histogram with the specified number of bins
  output$cle <- renderLeaflet({
    leaflet(oh.shp) %>%
      setView(lng = -81.6944, lat = 41.4993, zoom = 10) %>%
      addTiles() %>%
      addPolygons(color = "#FFFFFF", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.75, 
                  fillColor = ~colorBin(c("#E0ECFB", "#00649F"),
                                        most_dense, bins = 2)(most_dense))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

