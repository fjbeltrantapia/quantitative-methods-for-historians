setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/quantitative-methods-for-historians/data")

library(tidyverse)
library(sf)

data <- read_csv("tnp-letters-locations.csv")

data <- data |> select(from, to, date_from, place_from, x_from, y_from) |>
  rename(x = x_from,
         y = y_from) |>
  filter(!is.na(x)) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

data |>
  st_write(
  dsn = "tnp-shp/tnp-letters.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE                    # overwrite if exists
)


# check that it works

tnp <- read_sf("tnp-shp/tnp-letters.shp")
tnp

# Map it: tm_shape() + tm_dots() (or tm_bubbles...)
library(tmap)
library(rnaturalearth)

coast <- ne_coastline(scale = "medium", returnclass = "sf")
tnp |>
  tm_shape() +
  tm_dots(fill = "blue", size = 0.05) +
  tm_shape(coast, bbox = tnp) + 
  tm_lines(lwd = 0.5, col_alpha = 0.5)

tm_shape(coast) + 
  tm_lines()
