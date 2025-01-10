# Load libraries
library(httr)
library(sf)
library(leaflet)
library(leaflegend)
library(dplyr)
library(RColorBrewer)
library(scales)  # For nice color interpolation

# Load environment variables from the .env file
dotenv::load_dot_env(".env")

# Access the API key
MAP_KEY <- Sys.getenv("MAP_KEY")

# Parameters
dataset <- "VIIRS_SNPP_NRT"
bbox <- c(xmin = -120, ymin = 33, xmax = -117, ymax = 35)  # LA region
confidence <- 1
date <- format(Sys.time(), tz = "UTC", "%Y-%m-%d")

# Construct the URL
url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
              MAP_KEY, "/", dataset, "/",
              bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"], "/",
              confidence, "/", date)

# Fetch the data
response <- GET(url, write_disk("data/FIRMS_Data.csv", overwrite = TRUE))

# Check the response and load data
if (status_code(response) == 200) {
  message("Data downloaded successfully!")
  firms_data <- read.csv("data/FIRMS_Data.csv")
} else {
  stop("Failed to download data. Check your API key or parameters.")
}

# Convert data to sf object
firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

# FRP colors
frp_pal <- colorNumeric(
  palette = c("yellow", "orange", "red"),
  domain = c(0, 300),
  na.color = "#000000"
)

# Function to get opacity based on confidence
get_confidence_opacity <- function(confidence) {
  case_when(
    confidence == "l" ~ 0.6,  # Low confidence
    confidence == "n" ~ 0.8,  # Nominal confidence
    confidence == "h" ~ 1.0,  # High confidence
    TRUE ~ 0.5                # Default
  )
}

# Create map
m <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # Add fire points
  # addCircleMarkers(
  addCircles(
      data = firms_sf,
    radius = 10,
    color = ~frp_pal(pmin(frp, 300)),
    fillColor = ~frp_pal(pmin(frp, 300)),
    fillOpacity = ~get_confidence_opacity(confidence),
    stroke = TRUE,
    weight = 1,
    popup = ~sprintf(
      "<strong>VIIRS Fire Detection</strong><br/>
      FRP: %.1f MW",
      frp
    )
  ) %>%
  # Add FRP color legend
  addLegend(
    position = "bottomright",
    pal = frp_pal,
    values = c(0, 300),
    title = "Fire Radiative Power (MW)",
    bins = 6,
    labFormat = labelFormat(
      transform = function(x) sort(c(0, 50, 100, 150, 200, 250, 300))
    )
  )
m
# Save the map
saveWidget(m, file = "maps/wildfire-map.html", selfcontained = TRUE)

# Save last updated time
currenttime_la <- format(Sys.time(), tz = "America/Los_Angeles",
                         "%d %b %Y, %I:%M %p %Z")
writeLines(currenttime_la, "maps/latest_update.txt")

