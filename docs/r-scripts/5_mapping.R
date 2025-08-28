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

mun_sh <- read_sf("data/mapping/mun_1860_1930/mun_1860_1930.shp")
mun_sh

# Map it: tm_shape() + tm_dots() (or tm_bubbles...)
mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
    tm_dots(fill = "blue", size = 0.01)

# Adding contour for references
spain <- read_sf("data/mapping/ESP_adm0/ESP_adm0_pr_peninsula.shp") # import Spanish boundaries
  # import shapefile with the contour first

mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
    tm_dots(col = "blue", size = 0.01) + 
  tm_shape(spain) +
    tm_borders(col = "grey70", lwd = 0.5)

# Adjusting the size of the dots according to a particular field
  # population in 1860 here (pop1860)
mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
  tm_dots(fill = "blue", fill_alpha = 0.5, 
          col = "blue", col_alpha = 0.5, 
          size = "pop1860",
          size.scale = tm_scale_continous(
            ticks = c(500, 1000, 5000, 10000, 20000, 50000, 100000, 200000))) + 
  tm_shape(spain) +
    tm_borders(lwd = 0.05, col = "grey70")

# Adding labels to the features: tm_text()
mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
  tm_dots(
    fill = "blue", fill_alpha = 0.4,
    col = "blue", col_alpha = 0.4,
    size = "pop1860") +
  tm_text("municipio", just = "left", xmod = 0.5, size = 0.5) +
  tm_shape(spain) +
  tm_borders(lwd = 0.5, col = "grey70")

cities <- mun_sh |>
  filter(pop1860>=50000)

mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
  tm_dots(
    fill = "blue", fill_alpha = 0.4,
    col = "blue", col_alpha = 0.4,
    size = "pop1860") +
  tm_shape(spain) +
  tm_borders(lwd = 0.5, col = "grey70") +
  tm_shape(cities) +
  tm_text("municipio", just = "left", xmod = 0.5, size = 0.5)

  # you can also add labels to polygons if needed

# Temporal variation

mun_sh |>
  filter(ccau!=5) |>
  tm_shape() +
  tm_dots(
    fill = "blue", fill_alpha = 0.4,
    col = "blue", col_alpha = 0.4,
    size = c("pop1860", "pop1900", "pop1930"),
    size.legend = tm_legend(
      title = "", 
      orientation = "landscape"),
    size.free = FALSE) +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = c("1860", "1900", "1930"),
            panel.label.bg.color = "white") +
  tm_shape(spain) +
  tm_borders(lwd = 0.5, col = "grey70")


# or using facets but we need to structure the data differently
mun_sh_long <- mun_sh |>
  pivot_longer(cols = starts_with("pop"), 
               names_to = "year", 
               names_prefix = "pop", 
               values_to = "pop") |>
  filter(year=="1860" | year=="1900" | year=="1930") |> # select only the years I am interested in
  filter(ccau!=5) # excluding Canarias
mun_sh_long

mun_sh_long |>
  tm_shape() +
  tm_dots(fill = "blue", fill_alpha = 0.5,
          size = "pop",
          size.legend = tm_legend(
            title = ""
            , orientation = "landscape")) +
  tm_facets(by = "year", nrow = 1) +
  tm_layout(legend.outside.position = "bottom") +
  tm_shape(spain) + tm_borders()




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


