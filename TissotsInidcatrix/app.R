#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(rworldmap)
library(rworldxtra)
library(tibble)

projections <- tribble(~"projection", ~"code",
                          "Orthographic", "+proj=ortho +lat_0=50 +lon_0 = 0",
                          "wgs84", "4326",
                          "Mercator", "3857",
                          "Interrupted Goode Homolosine","+proj=igh",
                          "Azimuthal Equidistant","+proj=aeqd",
                          "Nicolosi Globular","+proj=nicol",
                          "van der Grinten IV","+proj=vandg4")

#Making the map
world<- getMap()%>%
  st_as_sf()%>%
  st_transform(., crs = 4326)

#Making the circles for the indicatrix
lon <- seq(-135,135, 45)
lat <- seq(-70,70,35)

latlon<- lapply(lat, data.frame, lon)%>%
  bind_rows()
names(latlon) <- c("lat", "lon")

tissots <-latlon%>%
  st_as_sf(., coords = c("lon", "lat"))%>%
  st_set_crs(4326)%>%
  st_buffer(., dist = units::set_units(500, "km"))

ggplot()+
  geom_sf(data = world)+
  geom_sf(data = tissots)+
  coord_sf(crs = "+proj=vandg4")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring map projections using Tissot's Indicatix"),

    sidebarLayout(
      sidebarPanel(
        selectInput("projection", 
                    "Select a projection",
                    choices = projections$projection,
                    selected = "Orthographic")
      ),
      
      mainPanel(
        plotOutput("mapPlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Create reactive ggplot map based on selected variable
reactive_map<- reactive({
  
  #Change to switch- should be faster when all codes added in finished app.
if(input$projection == "wgs84"){
    world_ <- world
    proj_code <- as.numeric(projections$code[projections$projection == input$projection])
}else if(input$projection == "Mercator"){
      world_<- world%>%
        filter(continent != "Antarctica")
      proj_code <- as.numeric(projections$code[projections$projection == input$projection])
}else{
    proj_code <- projections$code[projections$projection == input$projection]
    world_ <- world
}
      ggplot()+
      geom_sf(data = world_)+
      geom_sf(data = tissots)+
      coord_sf(crs = proj_code)
 })

output$mapPlot <- renderPlot({print(reactive_map())})
}


# Run the application 
shinyApp(ui = ui, server = server)
