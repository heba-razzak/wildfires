# Load libraries
library(sf)            # Spatial data manipulation
library(dplyr)         # Data manipulation
library(dotenv)        # Load env variables
library(leaflet)       # Interactive maps
library(ggplot2)       # Data visualization
library(lubridate)     # Time handling
library(htmlwidgets)   # HTML widgets

# LA & OC Area bounding box
bbox <- c(xmin = -119.5, ymin = 33.5, xmax = -117.5, ymax = 34.5)
bbox_sf <- st_as_sfc(st_bbox(bbox))
st_crs(bbox_sf) <- 4326

# Define the URL
url <- "https://services1.arcgis.com/jUJYIo9tSA7EHvfZ/arcgis/rest/services/California_Historic_Fire_Perimeters/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# Fetch GeoJSON data
fire_perimeters <- st_read(url)

# Inspect the data
head(fire_perimeters)


# Convert to a datetime object
fire_perimeters$ALARM_DATE <- as_datetime(fire_perimeters$ALARM_DATE/1000)

sort(unique(year(fire_perimeters$ALARM_DATE)))
as.character()
