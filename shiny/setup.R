library(readxl)
library(shiny)
library(leaflet)
library(WDI)
library(tidyr)
library(dplyr)
library(xts)
library(dygraphs)
library(RColorBrewer)

# Turn off scientific notation
options(scipen=999)

nyco <- readRDS("shiny_data")

#names(lookup) <- c("a", "b", "c", "d", "e", "f")

# Lookup variable names / descriptions
lookup_list2  <- as.list(colnames(nyco))[c(3:152, 177, 178)]

## Get map data and merge to WDI data
# Set up the data
ny_shape <- rgdal::readOGR("NYC_shapefile/cb_2015_36_tract_500k.shp")
nyc_shape <- ny_shape[ny_shape$COUNTYFP %in% c("005","047","061","081","085"),]

# Function to join data frame and spatial data
join_shapefile_to_df <- function(spatial_data, data_frame)
{
  spatial_data@data <- data.frame(spatial_data@data, data_frame[match(spatial_data@data[["GEOID"]], 
                                                                      data_frame[["TRACT_GEOID"]]), ])
  spatial_data
}

# Function to grab WDI data and merge to the spatial data frame
merge_to_nyc <- function(indicator, month) {
  # Get WDI data
  date <- paste(month, "-01", sep = "")
  dat <- nyco[nyco$MP_DATE==date, ]
  dat[[indicator]] <- round(as.numeric(dat[[indicator]]), 2)
  # Join to map data
  nyc_shape2 <- 
    join_shapefile_to_df(nyc_shape, dat[, c(indicator, "TRACT_GEOID", "NUM_Banks_Credit_Unions",
                                            "NUM_Colleges_Universities", "NUM_Landmarks_Historical_Buildings", 
                                            "NUM_Nightlife", "NUM_Fitness_Instruction", "NUM_Parks")])
  nyc_shape2
}

# Quantile Labels
# The leaflet package uses percentiles in its quantile legends; This function shows the actual values.

quantile_labels <- function (vec, n) 
{
  qs <- round(quantile(vec, seq(0, 1, 1/n), na.rm = TRUE), 
              1)
  len <- length(qs) - 1
  qlabs <- c()
  for (i in 1:len) {
    j <- i + 1
    v <- paste0(as.character(qs[i]), "-", as.character(qs[j]))
    qlabs <- c(qlabs, v)
  }
  final_labs <- c(qlabs, "Data unavailable")
  final_labs
}