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
bbox <- c(xmin = -120, ymin = 33, xmax = -117, ymax = 35)  # LA region
bbox_sf <- st_as_sfc(st_bbox(bbox))
st_crs(bbox_sf) <- 4326

# Filter to sensors within bounding box
pa_sf <- st_as_sf(pa_sensors, coords=c("longitude", "latitude"), crs = 4326)
purpleairs_sf <- st_intersection(pa_sf, bbox_sf)

# Time configuration using PST/PDT
tz_la <- "America/Los_Angeles"
current_time_la <- with_tz(Sys.time(), tz_la)
start_date_reference <- as.POSIXct("2025-01-01", tz = "UTC")

# Filter sensors that have been active since Jan 1, 2025
active_sensors <- purpleairs_sf %>%
  filter(
    last_seen >= start_date_reference,
    location_type == 0
  ) %>%
  st_drop_geometry()

# Configure measurement parameters
# fields <- c("pm2.5_alt", "humidity", "temperature", "pressure", "rssi",
#             "uptime", "memory", "analog_input")
fields <- c("pm2.5_alt", "temperature", "humidity")

# End date: now
end_date_utc <- with_tz(Sys.time(), "UTC")
timestamp_la <- with_tz(as.POSIXct(end_date_utc, "UTC"), tz_la)
# Start date: 24 hours ago
start_date_utc <- end_date_utc - minutes(30)
# Real time
average <- "0"

# Split sensor list into smaller chunks for API calls
chunk_size <- 50
sensor_chunks <- split(
  unique(active_sensors$sensor_index),
  ceiling(seq_along(unique(active_sensors$sensor_index))/chunk_size)
)

# Download recent data in chunks
purpleair_rawdata_list <- list()
for (i in seq_along(sensor_chunks)) {
  message("Processing chunk ", i, " of ", length(sensor_chunks))
  chunk_data <- tryCatch({
    getSensorHistory(
      sensorIndex = sensor_chunks[[i]],
      apiReadKey = api_key,
      startDate = start_date_utc,
      endDate = end_date_utc,
      average = average,  # Real-time data
      fields = fields
    )
  }, error = function(e) {
    message("Error in chunk ", i, ": ", e$message)
    return(data.frame())
  })
  purpleair_rawdata_list[[i]] <- chunk_data
  Sys.sleep(1)  # Be nice to the API
}
purpleair_raw <- do.call(rbind, purpleair_rawdata_list)

# Get 30 min avg from latest real time 30 mins
purpleair_data <- purpleair_raw %>%
  group_by(sensor_index) %>%
  summarise(pm2.5_alt = mean(pm2.5_alt, na.rm = TRUE)) %>%
  ungroup()

# # Process the data
# purpleair_data <- purpleair_rawdata %>%
#   mutate(time_stamp_la = with_tz(as.POSIXct(time_stamp, "UTC"), tz_la))

# Join with location data and include offline sensors
map_sensors <- purpleairs_sf %>%
  filter(sensor_index %in% unique(active_sensors$sensor_index))

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
