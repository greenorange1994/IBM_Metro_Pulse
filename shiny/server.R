library(shiny)
library(leaflet)
library(WDI)
library(tidyr)
library(dplyr)
library(xts)
library(dygraphs)

source('setup.R')


shinyServer(function(input, output, session) {
  
  # Build reactive expression that takes values from the input to fetch the data
  
  nyc_shape2 <- reactive({merge_to_nyc(input$indicator, input$month)})
  
  # Attempt to draw the map 
  
  output$nyc_map <- renderLeaflet({
    
    stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"
    
    stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'
    
    map <- leaflet() %>%
      addTiles(urlTemplate = stamen_tiles,
               attribution = stamen_attribution) %>%
      setView(-73.985131, 40.758895, zoom = 10)
    map
  })
  
  # Update the map appropriately when a new input is selected
  
  observe({
    
    classes <- 3
    
    pal <- colorQuantile(input$colors, NULL, n = classes)
    
    labs <- quantile_labels(nyc_shape2()[[input$indicator]], classes)
    
    tract_popup <- paste0("<strong>Tract: </strong>",
                            nyc_shape2()$TRACT_GEOID,
                            "<br><strong>",
                            input$indicator,
                            ", ",
                            input$month,
                            ": </strong>",
                            nyc_shape2()[[input$indicator]],
                            "<br><strong>",
                            "NUM_Banks_Credit_Unions",
                            ": </strong>",
                            nyc_shape2()$NUM_Banks_Credit_Unions,
                            "<br><strong>",
                            "NUM_Colleges_Universities",
                            ": </strong>",
                            nyc_shape2()$NUM_Colleges_Universities,
                            "<br><strong>",
                            "NUM_Landmarks_Historical_Buildings",
                            ": </strong>",
                            nyc_shape2()$NUM_Landmarks_Historical_Buildings,
                            "<br><strong>",
                            "NUM_Nightlife",
                            ": </strong>",
                            nyc_shape2()$NUM_Nightlife,
                            "<br><strong>",
                            "NUM_Fitness_Instruction",
                            ": </strong>", 
                            nyc_shape2()$NUM_Fitness_Instruction,
                            "<br><strong>",
                            "NUM_Parks",
                            ": </strong>", 
                            nyc_shape2()$NUM_Parks
                          )
    
      
    map <- leafletProxy("nyc_map", session) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = nyc_shape2(), 
                fillColor = ~pal(nyc_shape2()[[input$indicator]]),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                popup = tract_popup, 
                layerId = ~GEOID) %>%
      addLegend(colors = c(RColorBrewer::brewer.pal(classes, input$colors), "#808080"),
                position = "bottomright",
                bins = classes,
                labels = labs, 
                title = paste0(input$indicator, ', ', input$month))
  })
  
  # Create a reactive event that grabs the ID of a clicked country
  
  ctry <- eventReactive(input$nyc_map_shape_click,  {
  
    x <- input$nyc_map_shape_click
  
    y <- x$id
    
    y
    
  })
  
  output$dchart <- renderDygraph({ 
    
    ind <- input$indicator
    
    df <- nyco[nyco$TRACT_GEOID == ctry(), c(input$indicator, "MP_DATE", "TRACT_GEOID")]
    
    df1 <- df %>%
      select_("TRACT_GEOID", "MP_DATE", ind) %>%
      spread_(key = "TRACT_GEOID", value = ind) %>%
      mutate(date = as.Date(MP_DATE, format = "%Y-%m-%d"))
    
    xtdata <- xts(df1, order.by = df1$date) 
    
    xtdata$date <- NULL
    
    dygraph(xtdata) %>%
      dyOptions(colors = c("black", "black"))
    
    })
})
  
