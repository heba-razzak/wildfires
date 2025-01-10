# Load libraries
library(httr)
library(sf)
library(leaflet)


# Load environment variables from the .env file
dotenv::load_dot_env(".env")

# Access the API key
MAP_KEY <- Sys.getenv("MAP_KEY")

# netrc_path <- "~/.netrc"  # Path to your .netrc file
# cookie_path <- "~/.urs_cookies"  # Path to your cookies file
# downloaded_file_path <- "data.nc4"  # Specify where the downloaded file will be saved

# Parameters
dataset <- "VIIRS_SNPP_NRT"
bbox <- c(xmin = -119.5, ymin = 33.5, xmax = -117.5, ymax = 34.5)  # LA region
confidence <- 1  # All confidence levels
date <- "2025-01-09"  # Single date or range (e.g., "2025-01-01,2025-01-09")

# Construct the URL
url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
              MAP_KEY, "/", dataset, "/",
              bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"], "/",
              confidence, "/", date)

# Fetch the data
response <- GET(url, write_disk("data/FIRMS_Data.csv", overwrite = TRUE))

# https://www.earthdata.nasa.gov/data/instruments/viirs/viirs-i-band-375-m-active-fire-data

# Check the response
if (status_code(response) == 200) {
  message("Data downloaded successfully!")
} else {
  stop("Failed to download data. Check your API key or parameters.")
}

# Load the data
firms_data <- read.csv("data/FIRMS_Data.csv")

# Preview the data
print(head(firms_data))

# Convert data to sf object
firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)


# Create an interactive map
leaflet(data = firms_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    # radius = ~confidence / 10,  # Adjust size based on confidence
    color = ~ifelse(daynight == "D", "orange", "blue"),  # Day vs night colors
    popup = ~paste0("Brightness: ", bright_ti4, "<br>Confidence: ", confidence)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("orange", "blue"),
    labels = c("Daytime", "Nighttime"),
    title = "Detection Time"
  ) %>%
  setView(lng = mean(firms_data$longitude), lat = mean(firms_data$latitude), zoom = 9)



# Create map
m <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = firms_sf
                   # fillColor = ~aqi_pal(aqi),
                   # fillOpacity = 0.5,
                   # weight = 1,
                   # color = ~aqi_pal(aqi),
                   # radius = ~1.5*get_marker_radius(time_ago_numeric),
                   # popup = ~sprintf(
                   #   "<b>PM2.5: %.1f μg/m³</b><br>
                   #   Updated %s",pm2.5_alt, time_ago),
                   # label = ~sprintf(
                   #   "PM2.5: %.1f μg/m³, %s",pm2.5_alt, time_ago)
                   )
  # addLegendFactor(
  #   pal = aqi_pal,
  #   values = factor(aqi_levels, levels = aqi_levels),  # Ensure ordered factor
  #   title = "Air Quality Index",
  #   position = "bottomleft",
  #   shape = "circle",
  #   width = 16,
  #   height = 16
  # ) %>%
  # # Add size legend for time ago
  # addLegendSymbol(
  #   values = time_categories,
  #   shape = rep("circle", length(time_categories)),
  #   color = "#000000",
  #   fillColor = "#000000",
  #   fillOpacity = 0.5,
  #   width = 3*marker_sizes,
  #   title = "Air Quality Updated Within",
  #   position = "bottomright",
  #   orientation = 'horizontal',
  #   labelStyle = "font-size: 12px;"
  # )
m





######




# Load libraries
library(sf)            # Spatial data manipulation
library(here)          # Robust file paths
library(httr)
library(dplyr)         # Data manipulation
library(leaflet)       # Interactive maps
library(leaflegend)    # Leaflet symbol legend

# Load environment variables from the .env file
dotenv::load_dot_env(".env")

# Access the API key
MAP_KEY <- Sys.getenv("MAP_KEY")

# Parameters
dataset <- "VIIRS_SNPP_NRT"
bbox <- c(xmin = -119.5, ymin = 33.5, xmax = -117.5, ymax = 34.5)  # LA region
confidence <- 1  # All confidence levels
date <- format(Sys.time(), tz = "UTC", "%Y-%m-%d")

# Construct the URL
url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
              MAP_KEY, "/", dataset, "/",
              bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"], "/",
              confidence, "/", date)

# Fetch the data
response <- GET(url, write_disk("data/FIRMS_Data.csv", overwrite = TRUE))

# Check the response
if (status_code(response) == 200) {
  message("Data downloaded successfully!")
} else {
  stop("Failed to download data. Check your API key or parameters.")
}

# Load the data
firms_data <- read.csv("data/FIRMS_Data.csv")

# Convert data to sf object
firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

# Define color palettes based on VIIRS documentation
confidence_colors <- c(
  "low" = "#ff7f00",     # Orange for low confidence
  "nominal" = "#ff0000", # Red for nominal confidence
  "high" = "#8b0000"     # Dark red for high confidence
)

# Function to determine marker size based on FRP
get_frp_radius <- function(frp) {
  # Scale FRP to reasonable circle sizes (3-10 pixels)
  pmin(3 + (frp/50) * 7, 10)
}

# Create map
m <- leaflet() %>%
  # Add CartoDB dark matter basemap for better fire visualization
  addProviderTiles("CartoDB.DarkMatter") %>%

  # Add fire points with dynamic sizing and colors
  addCircleMarkers(
    data = firms_sf,
    radius = ~get_frp_radius(frp),
    color = ~confidence_colors[confidence],
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    popup = ~sprintf(
      "<strong>VIIRS Fire Detection</strong><br/>
      Time: %s<br/>
      Confidence: %s<br/>
      FRP: %.1f MW<br/>
      Brightness Temp: %.1f K",
      ifelse(daynight == "D", "Day", "Night"),
      confidence,
      frp,
      bright_ti4
    )
  ) %>%

  # Add confidence level legend
  addLegendFactor(
    pal = colorFactor(confidence_colors, domain = names(confidence_colors)),
    values = names(confidence_colors),
    title = "Detection Confidence",
    position = "bottomleft",
    shape = "circle",
    width = 10,
    height = 10
  ) %>%

  # Add FRP size legend
  addLegendSymbol(
    values = c("Small", "Medium", "Large"),
    shape = "circle",
    color = "#ffffff",
    width = c(6, 8, 10),
    title = "Fire Radiative Power",
    position = "bottomright"
  )

# View map
m

# Save html map
saveWidget(m, file = "maps/wildfire-map.html", selfcontained = TRUE)

# Save last updated time
currenttime_la <- format(Sys.time(), tz = "America/Los_Angeles",
                         "%d %b %Y, %I:%M %p %Z")
writeLines(currenttime_la, "maps/latest_update.txt")

###################################################################################################################

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
bbox <- c(xmin = -119.5, ymin = 33.5, xmax = -117.5, ymax = 34.5)  # LA region
confidence <- 1  # All confidence levels
date <- "2025-01-09"

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

# Create custom color palette for FRP ranges
# Colors inspired by GOES-R fire products
frp_colors <- colorRampPalette(c(
  "#000080",  # Dark blue (0 MW)
  "#0000FF",  # Blue
  "#00FF00",  # Green
  "#FFFF00",  # Yellow
  "#FFA500",  # Orange
  "#FF0000",  # Red (1000 MW)
  "#FFFF00",  # Yellow (2000 MW)
  "#FFFFFF"   # White (3000+ MW)
))(100)

# Create color palette function
frp_pal <- colorNumeric(
  palette = frp_colors,
  domain = c(0, 3000),
  na.color = "#000000"
)

# Function to get opacity based on confidence
get_confidence_opacity <- function(confidence) {
  case_when(
    confidence == "l" ~ 0.3,  # Low confidence
    confidence == "n" ~ 0.7,  # Nominal confidence
    confidence == "h" ~ 1.0,  # High confidence
    TRUE ~ 0.5                # Default
  )
}

# Create map
m <- leaflet() %>%
  # Add dark basemap for better fire visualization
  addProviderTiles("CartoDB.DarkMatter") %>%

  # Add fire points
  addCircleMarkers(
    data = firms_sf,
    radius = 3,  # Fixed size as recommended
    color = ~frp_pal(pmin(frp, 3000)),  # Cap at 3000 MW for color scale
    fillColor = ~frp_pal(pmin(frp, 3000)),
    fillOpacity = ~get_confidence_opacity(confidence),
    stroke = TRUE,
    weight = 1,
    popup = ~sprintf(
      "<strong>VIIRS Fire Detection</strong><br/>
      Time: %s<br/>
      FRP: %.1f MW<br/>
      Confidence: %s<br/>
      Brightness Temp: %.1f K",
      ifelse(daynight == "D", "Day", "Night"),
      frp,
      confidence,
      bright_ti4
    )
  ) %>%

  # Add FRP color legend
  addLegend(
    position = "bottomright",
    pal = frp_pal,
    values = c(0, 3000),
    title = "Fire Radiative Power (MW)",
    bins = 6,
    labFormat = labelFormat(
      transform = function(x) sort(c(0, 500, 1000, 1500, 2000, 2500, 3000))
    )
  ) %>%

  # Add confidence opacity legend
  addLegend(
    position = "bottomleft",
    colors = c("#FF0000", "#FF0000", "#FF0000"),
    labels = c("High", "Nominal", "Low"),
    opacity = c(1.0, 0.7, 0.3),
    title = "Detection Confidence"
  )

# Save the map
saveWidget(m, file = "maps/wildfire-map.html", selfcontained = TRUE)

# Save last updated time
currenttime_la <- format(Sys.time(), tz = "America/Los_Angeles",
                         "%d %b %Y, %I:%M %p %Z")
writeLines(currenttime_la, "maps/latest_update.txt")
