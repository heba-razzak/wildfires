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
library(RAQSAPI)       # EPA Air Quality API

# Load environment variables from the .env file
dotenv::load_dot_env(".env")

# Access the API key
aqs_user <- Sys.getenv("AQS_USER")
aqs_pass <- Sys.getenv("AQS_PASS")

# Set AQS credentials
aqs_credentials(username = aqs_user, key = aqs_pass)

# Define the Bay Area bounding box coordinates
bbox <- c(xmin = -120, ymin = 33, xmax = -117, ymax = 35)  # LA region
bbox_sf <- st_bbox(bbox)

# Extract min and max latitudes and longitudes for the buffered area
minlon <- bbox_sf["xmin"]
maxlon <- bbox_sf["xmax"]
minlat <- bbox_sf["ymin"]
maxlat <- bbox_sf["ymax"]

# Get PM2.5 monitors in the Bay Area for the specified date range
monitor_info <- aqs_monitors_by_box(
  parameter = "88101",
  bdate = Sys.Date() - days(1),
  edate = Sys.Date(),
  minlat = minlat, maxlat = maxlat,
  minlon = minlon, maxlon = maxlon
)

# Convert monitor data to an sf object for mapping
monitors_sf <- monitor_info %>%
  select(si_id, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ids <- unique(monitors_sf$si_id)

monitors_data <- list()
for (i in seq_along(ids)) {
  print(i)
  m <- monitor_info[i,]
  monitor_data <- aqs_sampledata_by_site(
    parameter = "88101",
    bdate = Sys.Date() - days(1),
    edate = Sys.Date(),
    stateFIPS = m$state_code,
    countycode = m$county_code,
    sitenum = m$site_number,
    duration = "1"
  )
  monitors_data[[i]] <- monitor_data
}

monitors_raw <- do.call(rbind, monitors_data)

#
map_data <- left_join(map_sensors, purpleair_data, by = "sensor_index") %>%
  mutate(
    status = case_when(
      !is.na(pm2.5_alt) ~ "active",
      TRUE ~ "offline"
    ),
    aqi = case_when(
      status == "offline" ~ "Offline",
      pm2.5_alt <= 12.0 ~ "Good",
      pm2.5_alt <= 35.4 ~ "Moderate",
      pm2.5_alt <= 55.4 ~ "Unhealthy for Sensitive Groups",
      pm2.5_alt <= 150.4 ~ "Unhealthy",
      pm2.5_alt <= 250.4 ~ "Very Unhealthy",
      TRUE ~ "Hazardous"
    )
  )

# Define AQI levels and colors (now including Offline)
aqi_levels <- c(
  "Good",
  "Moderate",
  "Unhealthy for Sensitive Groups",
  "Unhealthy",
  "Very Unhealthy",
  "Hazardous",
  "Offline")

aqi_colors <- c(
  "#00e400",  # Good
  "#ffff00",  # Moderate
  "#ff7e00",  # Unhealthy for Sensitive Groups
  "#ff0000",  # Unhealthy
  "#8f3f97",  # Very Unhealthy
  "#7e0023",   # Hazardous
  "#808080"  # Offline (grey)

)

# Convert to factor and create palette
map_data$aqi <- factor(map_data$aqi, levels = aqi_levels, ordered = TRUE)
aqi_pal <- colorFactor(palette = aqi_colors, domain = aqi_levels, ordered = TRUE)

# Create map
m <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircles(
    data = map_data,
    fillColor = ~aqi_pal(aqi),
    fillOpacity = 0.7,
    weight = 1,
    color = ~aqi_pal(aqi),
    radius = 500,
    popup = ~sprintf(
      if_else(status == "active",
              sprintf("PM2.5: <b>%.1f</b> μg/m³<br><b>%s</b>", pm2.5_alt,aqi),
              sprintf("Last Active: %s", last_seen)))
  ) %>%
  addLegendFactor(
    pal = aqi_pal,
    values = factor(aqi_levels, levels = aqi_levels),
    title = "Air Quality Index",
    position = "bottomleft",
    shape = "circle",
    width = 16,
    height = 16
  )
m
# Save outputs
saveWidget(m, file = here("maps", "purpleair-map.html"), selfcontained = TRUE)

writeLines(format(current_time_la, "%d %b %Y, %I:%M %p %Z"),
           here("maps", "latest_update.txt"))
