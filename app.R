

library(shiny)
library(tidyverse)
library(tidycensus)
library(choroplethr)
library(leaflet)
library(choroplethrMaps)
library(sf)
census_api_key("1a4f022b359f067832be892ea04ace5106a335af", install = TRUE, overwrite = TRUE)
data(state)

# Define UI for application that draws a histogram

   
   ui <- fluidPage(
    
     titlePanel("American Community Survey data"),
     
     leafletOutput("map", height="600px"),
     absolutePanel(fixed = TRUE,
                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = 330, height = "auto",radioButtons("featureID", "Feature",  
                                                             choices = c("Median Household Income", "Median Gross Rent", "Ratio of Both"), selected = "Median Household Income"),
                   selectInput("stateID", "State:",
                               state.abb, selected = "NJ", multiple = FALSE), style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;")
   )
   
   server <- function(input, output) {
     output$map <- renderLeaflet({
       features_vec <- switch(input$featureID,
                              `Median Household Income` = "B19013_001",
                              `Median Gross Rent` = "B25064_001",
                              `Ratio of Both` = "B25074_001")
       state_df <- get_acs(geography = "tract", 
                           variables = features_vec, 
                           state = input$stateID , 
                           geometry = TRUE)
       
       pal <- colorQuantile("Reds", domain = state_df$estimate, n = 9)
       state_df %>% st_transform(crs = "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider = "CartoDB.Positron") %>%
         addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                     stroke = FALSE,
                     smoothFactor = 0,
                     fillOpacity = 0.95,
                     color = ~ pal(estimate)) %>%
         addLegend("bottomright", 
                   pal = pal, 
                   values = ~ estimate,
                   title = "Range of Estimates",
                   opacity = 1)
     })
   }
   
   # Run the application 
   shinyApp(ui = ui, server = server)
   
