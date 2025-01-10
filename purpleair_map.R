# Load libraries
library(sf)            # Spatial data manipulation
library(here)          # Robust file paths
library(dplyr)         # Data manipulation
library(dotenv)        # Load env variables
library(leaflet)       # Interactive maps
library(ggplot2)       # Data visualization
library(lubridate)     # Time handling
library(leaflegend)    # Leaflet symbol legend
library(htmlwidgets)   # HTML widgets
library(PurpleAirAPI)  # PurpleAir Data API

# Load environment variables from the .env file
dotenv::load_dot_env(".env")

# Access the API key
api_key <- Sys.getenv("PURPLEAIR_API_KEY")

# Download sensor metadata
pa_sensors <- getPurpleairSensors(
  apiReadKey = api_key,
  fields = c("latitude", "longitude", "date_created", "last_seen", "location_type")) %>%
  na.omit()

# LA & OC Area bounding box
bbox <- c(xmin = -119.5, ymin = 33.5, xmax = -117.5, ymax = 34.5)
bbox_sf <- st_as_sfc(st_bbox(bbox))
st_crs(bbox_sf) <- 4326

# Filter to sensors within bounding box
pa_sf <- st_as_sf(pa_sensors, coords=c("longitude", "latitude"), crs = 4326)
purpleairs_sf <- st_intersection(pa_sf, bbox_sf)

# Configure measurement parameters
fields <- c(
  "pm2.5_alt", "humidity", "temperature", "pressure", "rssi",
  "uptime", "memory", "analog_input"
)

# Time configuration using PST/PDT
tz_la <- "America/Los_Angeles"
current_time_la <- with_tz(Sys.time(), tz_la)

# Start date: 24 hours ago
end_date_utc <- with_tz(Sys.time(), "UTC")
# End date: now
start_date_utc <- end_date_utc - hours(24)

# 30-minute averages for data
average <- "30"

# Filter sensors active during time period
filtered_sensors_sf <- purpleairs_sf %>%
  filter(last_seen >= start_date_utc, date_created <= end_date_utc, location_type==0) %>%
  select(sensor_index, location_type) %>%
  st_drop_geometry()

# Download hourly data
purpleair_rawdata <- getSensorHistory(
  sensorIndex = unique(filtered_sensors_sf$sensor_index),
  apiReadKey = api_key,
  startDate = start_date_utc,
  endDate = end_date_utc,
  average = average,
  fields = fields
)

# Add LA Timestamp, Time Ago, radius calc
purpleair_data <- purpleair_rawdata %>%
  mutate(
    time_stamp_la = with_tz(as.POSIXct(time_stamp, "UTC"), tz_la),
    time_ago_numeric = as.numeric(difftime(current_time_la, time_stamp_la, units = "mins")), # Time difference in minutes
    time_ago = case_when(
      time_ago_numeric < 60 ~ paste(round(time_ago_numeric), "mins ago"),
      time_ago_numeric <= 1440 ~ paste(round(time_ago_numeric / 60), "hours ago"),
      TRUE ~ format(time_stamp_la, "%Y-%m-%d %H:%M %Z")
    )
  )

# Get latest reading for each sensor
latest_readings <- purpleair_data %>%
  group_by(sensor_index) %>%
  slice_max(time_stamp_la, n = 1) %>%
  ungroup()

# Join sensor air quality with locations
map_sensors <- purpleairs_sf %>% filter(sensor_index %in% unique(latest_readings$sensor_index))
map_data <- left_join(map_sensors, latest_readings, by = "sensor_index")

# Create AQI measure
map_data <- map_data %>% mutate(aqi = case_when(
  pm2.5_alt <= 12.0 ~ "Good",
  pm2.5_alt <= 35.4 ~ "Moderate",
  pm2.5_alt <= 55.4 ~ "Unhealthy for Sensitive Groups",
  pm2.5_alt <= 150.4 ~ "Unhealthy",
  pm2.5_alt <= 250.4 ~ "Very Unhealthy",
  TRUE ~ "Hazardous"
))

# Define custom colors for AQI levels
aqi_colors <- c(
  "Good" = "#00e400",
  "Moderate" = "#ffff00",
  "Unhealthy for Sensitive Groups" = "#ff7e00",
  "Unhealthy" = "#ff0000",
  "Very Unhealthy" = "#8f3f97",
  "Hazardous" = "#7e0023"
)
# Create a categorical palette for AQI
aqi_pal <- colorFactor(
  palette = aqi_colors,
  domain = map_data$aqi  # Ensure the AQI levels are used
)

# Corresponding marker sizes (matching the order of the factor levels)
marker_sizes <- c(4, 3, 2, 1)

# Create marker radius function
get_marker_radius <- function(time_ago_numeric) {
  case_when(
    time_ago_numeric <= 60 ~ marker_sizes[1],  # Last hour
    time_ago_numeric <= 360 ~ marker_sizes[2],  # Last 6 hours
    time_ago_numeric <= 720 ~ marker_sizes[3], # Last 12 hours
    TRUE ~ marker_sizes[4]                     # Older
  )
}

# Air Quality Updated Within
time_categories <- factor(
  c("1 hour", "6 hours", "12 hours",  "24 hours"),
  levels = c("1 hour", "6 hours", "12 hours",  "24 hours")
)

# Create map
m <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = map_data,
                   fillColor = ~aqi_pal(aqi),
                   fillOpacity = 0.5,
                   weight = 1,
                   color = ~aqi_pal(aqi),
                   radius = ~1.5*get_marker_radius(time_ago_numeric),
                   popup = ~sprintf(
                     "<b>PM2.5: %.1f μg/m³</b><br>
                     Updated %s",pm2.5_alt, time_ago),
                   label = ~sprintf(
                     "PM2.5: %.1f μg/m³, %s",pm2.5_alt, time_ago)) %>%
  # Add color legend for AQI
  addLegendFactor(
    pal = aqi_pal,
    values = map_data$aqi,
    title = "Air Quality Index",
    position = "bottomleft",
    shape = "circle",
    width = 16,
    height = 16
  ) %>%
  # Add size legend for time ago
  addLegendSymbol(
    values = time_categories,
    shape = rep("circle", length(time_categories)),
    color = "#000000",
    fillColor = "#000000",
    fillOpacity = 0.5,
    width = 3*marker_sizes,
    title = "Air Quality Updated Within",
    position = "bottomright",
    orientation = 'horizontal',
    labelStyle = "font-size: 12px;"
  )

# Save html map
saveWidget(m, file = here("maps", "purpleair-map.html"), selfcontained = TRUE)

# Save last updated time
currenttime_la <- format(current_time_la, "%d %b %Y, %I:%M %p %Z")
writeLines(currenttime_la, here("maps", "latest_update.txt"))
