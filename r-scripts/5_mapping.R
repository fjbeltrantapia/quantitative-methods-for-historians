###### Mapping ######

# clear de "Global Environment"
rm(list=ls()) 

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# upload basic packages
library(tidyverse)
library(readxl)
library(ggplot2)

# install.packages("sf")
# install.packages("tmap")
# install.packages("geodata")
library(sf)
library(tmap)
library(geodata)


##### Shapefiles: polygons, lines, points

### Polygons

# Import files
dist_sh <- read_sf("data/mapping/educ_1860/dist_1860.shp") # Spanish districts
dist_sh

# Map it: t_shape() + tm_polygons() (or tm_borders, tm_fill...)
dist_sh |> 
  tm_shape() +
  tm_polygons(fill = "lightblue", col = "grey", lwd = 0.5)

# Map the info contained in particular fields (variables)
dist_sh |>
  tm_shape() +
    tm_polygons(fill = "literacy_m", lwd = 0.5)

?tm_polygons

dist_sh |>
  tm_shape() +
    tm_polygons(fill = "literacy_m", lwd = 0.5,
                fill.scale = tm_scale_intervals(
                  style = "fixed",
                  breaks = c(0, 15, 30, 45, 60, Inf),
                  values = "brewer.reds"), # color palette
                fill.legend = tm_legend(
                  title = "") + # Remove the legend title
    tm_scale_bar(position = c("right", "bottom"))

# Multiple maps
dist_sh |>
  tm_shape() +
  tm_polygons(fill = c("literacy_m", "literacy_f"),
              fill.scale = tm_scale_continuous(
                ticks = c(0, 10, 20, 30, 40, 50, 60, 70),
                values = "brewer.reds"),
              fill.legend = tm_legend(
                title = ""
                , orientation = "landscape"),
              fill.free = FALSE) +
  tm_layout(panel.labels = c("Men", "Women"),
            panel.label.bg.color = "white",
            panel.label.frame = FALSE)

# exporting maps
map <- dist_sh |>
  tm_shape() +
  tm_polygons(fill = "literacy_m")


tmap_save(map, "output/map_lit_1860.png", dpi = 600)
  # save the map as a .png file (with resolution = 600 pixels)

# Categorical (qualitative) variables

dist_sh |>
  tm_shape() +
    tm_polygons(fill = "province",
                fill.scale = tm_scale_categorical(
                  n.max = 48)) # when many categories (avoid recycling colors)

### Point shapefiles

letters <- read_sf("data/tnp-shp/tnp-letters.shp")
letters
  # 845 rows
  # some locations are duplicated (letters being sent from the same place)

# Map it: tm_shape() + tm_dots() (or tm_bubbles...)
letters |>
  tm_shape() +
  tm_dots(fill = "blue", size = 0.05)

# Adding contour for references
  # import your own shapefile
  # obtain it from somewhere else
# install.packages("rnaturalearth")
library(rnaturalearth)

coast <- ne_coastline(scale = "medium", returnclass = "sf")
  # world coastline (for contour)
coast

letters |>
  tm_shape() +
  tm_dots(fill = "blue", size = 0.05) +
  tm_shape(coast, bbox = letters) + # bounding box around the dots
  tm_lines(lwd = 0.5, col_alpha = 0.5)

# Adjusting the size of the dots according to a particular field
  # number of letters sent from each location
  # create that field first

letters_n <- letters |> # 696 unique locations
  count(from, place_from)
  # some actors sent letters from different places

letters_n |> as_tibble() |>
  ggplot(aes(x = n)) + geom_histogram()

letters_n |>
  tm_shape() +
  tm_symbols(fill = "blue", col = "blue", col_alpha = 0.5, 
          size = "n",
          size.scale = tm_scale_discrete(
            ticks = c(1, 2, 3, 4, 5, 6), # or "ticks = 1:6"   
            values  = c(0.03, 0.18))) + # size of the dots (range)
  tm_shape(coast, bbox = letters) + # bounding box around the dots
  tm_lines(lwd = 0.5, col_alpha = 0.5)

  # this is "discrete" but it could be:
    # tm_scale_intervals() or tm_scale_continous()
    # depending the type of field we want to map

# Adding labels to the features: tm_text()
letters_n |>
  tm_shape() +
  tm_symbols(fill = "blue", col = "blue", col_alpha = 0.5, 
             size = "n",
             size.scale = tm_scale_discrete(
               ticks = c(1, 2, 3, 4, 5, 6), # or "ticks = 1:6"   
               values  = c(0.03, 0.18))) + # size of the dots (range)
  tm_text("from", xmod = 0.5, size = 0.2) +
  tm_shape(coast, bbox = letters) + # bounding box around the dots
  tm_lines(lwd = 0.5, col_alpha = 0.5)

key_actors <- letters_n |>
  filter(n>=5)

letters_n |>
  tm_shape() +
  tm_symbols(fill = "blue", col = "blue", col_alpha = 0.5, 
             size = "n",
             size.scale = tm_scale_discrete(
               ticks = c(1, 2, 3, 4, 5, 6), # or "ticks = 1:6"   
               values  = c(0.03, 0.18))) + # size of the dots (range)
  tm_shape(key_actors) +
  tm_text("from", xmod = 0.5, size = 0.2) +
  tm_shape(coast, bbox = letters) + # bounding box around the dots
  tm_lines(lwd = 0.5, col_alpha = 0.5)


  # you can also add labels to polygons if needed

# Temporal variation: 
  
  # only two periods for simplicty

  # let's create them: elizabethan period (1558-) and before
bbox_all <- letters

letters |>
  mutate(year = date_from %/% 10000) |>
  mutate(period = if_else(year>=1558, "Elizabethan", "Pre-1558"),
         period = factor(period, 
                         levels = c("Pre-1558", "Elizabethan"))) |>
  count(period, from, place_from) |> # or group_by() and summarise()
  tm_shape() +
  tm_symbols(fill = "blue", col = "blue", col_alpha = 0.5, 
             size = "n",
             size.scale = tm_scale_discrete(
               ticks = c(1, 2, 3, 4, 5, 6), # or "ticks = 1:6"   
               values  = c(0.03, 0.18)), # size of the dots (range)
             size.legend = tm_legend(
               title = "Number of letters", 
               orientation = "landscape")) + 
  tm_facets(by = "period", nrow = 1, free.coords = FALSE) +
  tm_shape(coast, bbox = bbox_all) + # so it does not change over maps
  tm_lines(lwd = 0.5, col_alpha = 0.5) +
  tm_options(component.autoscale = FALSE) + # to disable rescaling
  tm_layout(legend.outside.position = "bottom",
            legend.text.size  = 0.5,
            legend.title.size = 0.5,
            panel.label.bg.color = "white",
            panel.label.size = 0.5)

  # %/% is integer division 
  # it divides two numbers and returns the whole number part
    # (dropping any remainder) / note that date is a number (not a proper date)


## it could be done by creating two independent maps 
  # and putting them together with patchwork
  # library(patchwork)
  # map1 + map2
letters_1 <- letters |>
  mutate(year = date_from %/% 10000) |>
  filter(year<1558) |>
  count(from, place_from)

letters_2 <- letters |>
  mutate(year = date_from %/% 10000) |>
  filter(year>=1558) |>
  count(from, place_from)

### A brief note on coordinate systems and projections

# install.packages("spData")
library(spData)

m0 <- tm_shape(world, projection = 4326) + tm_polygons() + tm_credits("WGS 84", position = c("LEFT", "BOTTOM")) + tm_layout(asp = 4)
m1 <- tm_shape(world, projection = 8857) + tm_polygons() + tm_credits("Equal Earth", position = c("LEFT", "BOTTOM")) + tm_layout(asp = 4)
m2 <- tm_shape(world, projection = "+proj=moll", ) + tm_polygons() + tm_credits("Mollweide", position = c("LEFT", "BOTTOM")) + tm_layout(asp = 4) 
m3 <- tm_shape(world, projection = "+proj=wintri", ) + tm_polygons() + tm_credits("Winkel Tripel", position = c("LEFT", "BOTTOM")) + tm_layout(asp = 4)

tmap_arrange(m0, m1, m2, m3, ncol = 2)

# distortions either in shape, area, distance or direction

# Spatial objects usually have the adequate CRS already defined
# Combining spatial objects with different CRSs is problematic

## Some CRSs (authority:code)
# WGS 84 (short for World Geodetic System 1984 (EPSG:4326)
# WGS 84 / World Mercator (EPSG:3395) -- used by Google Maps
# WGS 84 / Pseudo-Mercator (EPSG:3857)
# LAEA Europe (EPSG:3035) -- Lambert Azimuthal Equal Area
# UTM projections are especially suited for working with small areas. 
  # The earth is divided into 60 tiles (North/South the Equator). 
  # You should choose the one that covers your area of study. 
  # For Spain: ETRS 1989 UTM Zone 30N ("EPSG:25830").

# check for help when choosing CRSs: 
# https://jjimenezshaw.github.io/crs-explorer/

## Retrieving the CRS: authority:code -- summary()
spain                         # ETRS89 / UTM zone 30N
spain |> summary("geometry") # epsg:25830
spain |> st_crs()
  # provides all the information needed to properly identify the CRS.

## Changing the CRS
spain2 <- st_set_crs(spain, "EPSG:3035") # set CRS (LAEA Europe)
spain2 |> summary("geometry")
spain2 <- st_transform(spain, "EPSG:3035") # set CRS


m0 <- tm_shape(spain) + tm_polygons() + tm_credits("ETRS89 / UTM zone 30N", position = c("RIGHT", "BOTTOM"))
m1 <- tm_shape(spain2) + tm_polygons() + tm_credits("LAEA Europe", position = c("RIGHT", "BOTTOM"))
tmap_arrange(m0, m1, ncol = 2)


#### Mapping historical (or otherwise) data

## Rely on existing GIS files
# Search online for what you are looking for
# The Historical GIS Research Network
  # http://www.hgis.org.uk/resources.htm
# Geospatial Historian
  # https://geospatialhistorian.wordpress.com/finding-data/
# Historical gazetteers
  # World Historical Gazetteer: https://whgazetteer.org
# Use contemporary files (and adapt them if necessary)
  # Natural Earth: https://www.naturalearthdata.com/features/
  # GADM: https://www.gadm.org (administrative boundaries)
  # National agencies


## (1) Import them using read_sf()
  # regardless whether shapefiles are historical or contemporary
  # use filter() if necessary to extract the features you are interested in

## (2) merge them with the information you have gathered 
  # from other the archive or other sources

## Illustration using Paisley

library(readxl) 
paisley <- read_excel("data/paisley_data.xlsx") # Paisley data
paisley_born <- paisley |>
  filter(countryb=="scotland") |>
  count(born, sort = TRUE)
paisley_born

locations <- read_sf("data/mapping/Localities2020centroids/Localities2020_Centroids.shp") # import the spatial object (shapefile)
locations # shapefile with Scottish locations

scotland <- read_sf("data/mapping/scotland/scotland.shp") # import the spatial object (shapefile)

locations |> summary("geometry") # epsg:27700
scotland |> summary("geometry") # epsg:4326
scotland <- st_transform(scotland, "EPSG:27700") # set CRS
# or
scotland <- st_transform(scotland, st_crs(locations)) 
  # using the crs in the object "locations"

tm_shape(scotland, bbox = locations) + tm_borders() +
  tm_shape(locations) + tm_dots(col = "blue")

## Clean the Paisley locations
paisley <- paisley |>
  mutate(born = str_trim(born)) |>       # removes leading/trailing spaces
  mutate(born = str_to_lower(born)) |>   # all to lower letters
  mutate(born_adj = recode(born,          # homogenising categories
                           "campsey" = "campsie",                     
                           "bridge of wier" = "bridge of weir",
                           "n kilpatrick" = "new kilpatrick"))

paisley <- paisley |>
  mutate(born_adj = str_replace(born_adj, "shire", "")) # removing "shire"

paisley_born <- paisley |>
  filter(countryb=="scotland") |>
  count(born_adj)
paisley_born

# Merge both objects: shapefile - paisley places 
locations_ext <- locations |>
  mutate(name = str_to_lower(name)) |>   # converts to lower case
  full_join(paisley_born, by = join_by(name == born_adj))
locations_ext |> 
  select(code, name, n)

locations_ext |>
  filter(!is.na(n)) & is.na(code))

# map the number of prisoners 
  # assuming we are satified with the matching
tm_shape(scotland, bbox = locations_ext) + tm_borders() +
  tm_shape(locations) + tm_dots(fill = "grey", size = 0.05) +
  tm_shape(locations_ext) +
  tm_bubbles(fill = "red",
             size = "n",
             size.scale = tm_scale_continuous(
               ticks = c(1, 5, 10, 25, 50, 100, 200)),
             size.legend = tm_legend(
               title = "Number of prisoners, by origin"))
  # most of our Scottish prisoners were born relatively near the prison

## extracting features from larger spatial objects
prov_shp <- read_sf("data/mapping/ESP_adm2/ESP_adm2.shp")
prov_shp |>
  tm_shape() +
  tm_borders()

zgz_shp <- prov_shp |>
  filter(NAME_1=="Zaragoza")
zgz_shp |>
  tm_shape() +
  tm_borders()

## Adding XY coordinates: st_as_sf()

library(readxl) 
zgz_mun <- read_excel("data/mapping/mun_zgz_1860.xlsx")
zgz_mun 

zgz_mun_shp <- st_as_sf(zgz_mun, coords = c("lat", "lon"), crs = 3042)
zgz_mun_shp

zgz_mun_shp |>
  tm_shape() + tm_dots() +
  tm_shape(zgz_shp) + tm_borders()


## Geocoding

paisley_born |> arrange(-n)

# install.packages("tidygeocoder")
library(tidygeocoder)

places_geo <- paisley_born |>
  geocode(born_adj, method = "osm", 
          full_results = TRUE)
places_geo
view(places_geo)

  # the method refers to the geocoding service you are requesting
    # `osm` refers to the *Open Street Map Nominatim API
    # others: arcgis, census, google maps, etc.; 
    # see the package documentation:
      # https://cran.r-project.org/web/packages/tidygeocoder/tidygeocoder.pdf
      # the Google Maps Geocoding API requires an API key, so it might not be free

  # some locations are not found
  # others are found in other countries: US, Canada, Australia

# improve the geocoding by adding more info (country)

paisley_born <- paisley_born |>
  mutate(born_adj = str_to_title(born_adj)) |>            # capitalise the first letter
  mutate(born_adj = paste(born_adj, ", Scotland", sep = ""))  # add string
paisley_born

places_geo <- paisley_born |>
  geocode(born_adj, method = "osm", full_results = TRUE)
places_geo

places_geo |>
  filter(is.na(lat))
  # correct typos
  # finding the coordinates (lat, long) manually and add them using mutate()

# transform it into a spatial object (including CRSs)
places_geo_sf <- places_geo |>
  filter(!is.na(lat)) |>
  st_as_sf(coords = c("lat", "long"), crs = 4326) # WGS 84 4326
  
# map it
places_geo_sf |>
  tm_shape() +
  tm_bubbles(fill = "red",
             size = "n",
             size.scale = tm_scale_continuous(
               ticks = c(1, 5, 10, 25, 50, 100, 200))) +
  tm_shape(scotland) + tm_borders()


## Digitise your own maps -- ArcGIS / QGIS


