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
ndays <- 1

# Construct the URL - https://firms.modaps.eosdis.nasa.gov/api/area/
url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
              MAP_KEY, "/", dataset, "/",
              bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"], "/",
              ndays)

# Fetch the data
response <- GET(url, write_disk("/Users/heba/Documents/GitHub/wildfires/data/FIRMS_Data.csv", overwrite = TRUE))

# Check the response and load data
if (status_code(response) == 200) {
  message("Data downloaded successfully!")
  firms_data <- read.csv("/Users/heba/Documents/GitHub/wildfires/data/FIRMS_Data.csv")
} else {
  stop("Failed to download data. Check your API key or parameters.")
}

# ######## ######## ######## ######## ######## ######## ######## ######## ######
# EDA
#
# numeric_cols <- names(firms_data)[sapply(firms_data, is.numeric)]
# numeric_cols <- setdiff(numeric_cols, c("latitude", "longitude"))
# # Loop through the columns and plot histograms
# for (col in numeric_cols) {
#   x <- firms_data[[col]]
#
#   # If max is much larger than the 99th percentile, filter the data for plotting
#   p99 <- quantile(x, 0.99, na.rm = TRUE)
#   if (max(x, na.rm = TRUE) > 2 * p99) {
#     x <- x[x >= quantile(x, 0.01, na.rm = TRUE) & x <= p99]
#   }
#   hist(x, main = paste("Histogram of", col))
# }
#
# non_numeric_cols <- setdiff(names(firms_data), c(numeric_cols, "latitude", "longitude"))
#
# for (col in non_numeric_cols) {
#   # Plot the histogram using barplot for categorical data
#   barplot(table(firms_data[[col]]), main = paste("Histogram of", col), ylab = "Frequency")
# }
# ######## ######## ######## ######## ######## ######## ######## ######## ######

# Convert data to sf object
firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

# Create map
m <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircles(data = firms_sf,
             fillColor = "red",
             fillOpacity = 0.5,
             weight = 0,
             radius = 400) %>%
  addCircles(data = firms_sf,
             fillColor = "red",
             fillOpacity = 0.5,
             weight = 0,
             radius = 300) %>%
  addCircles(data = firms_sf,
             fillColor = "red",
             fillOpacity = 1,
             weight = 0,
             radius = 200)
m
# Save the map
saveWidget(m, file = "maps/wildfire-map.html", selfcontained = TRUE)

# Save last updated time
currenttime_la <- format(Sys.time(), tz = "America/Los_Angeles",
                         "%d %b %Y, %I:%M %p %Z")
writeLines(currenttime_la, "maps/latest_update.txt")

