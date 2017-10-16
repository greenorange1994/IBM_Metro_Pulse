library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(dygraphs)

source('setup.R')

brewer_df <- brewer.pal.info

brewer_df$colors <- row.names(brewer_df)

sub <- dplyr::filter(brewer_df, category == "div" | category == "seq")

color_picker <- sub$colors

shinyUI(
  fluidPage(
    tags$head(includeCSS("styles.css")),
    fixedPanel(
      id = "fullscreen", 
      top = 0, left = 0, width = "100%", height = "100%", 
      leafletOutput("nyc_map", width = "100%", height = "100%")
    ), 
    absolutePanel(id = "controls", draggable = FALSE, 
               top = 10, right = 10, width = 500, height = "auto", 
               h4("Metro Pulse Data Summary"), 
    tabsetPanel(
      tabPanel("Controls", 
               selectInput('indicator', 'Tract Information Indicator', lookup_list2, 
                           selected = 'dv'), 
               textInput('month',
                         label = paste('get which month data : YYYY-MM'),
                         value = "2010-01"
               ), 
               selectInput("colors", label = "ColorBrewer palette", 
                           choices = color_picker, 
                           selected = "Blues")), 
               
      tabPanel("Chart", p("Click a tract id on the map for an interactive chart"), 
               dygraphOutput("dchart", width = "100%"))
  ))
)
)