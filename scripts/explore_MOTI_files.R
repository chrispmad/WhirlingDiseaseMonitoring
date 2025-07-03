library(sf)
library(leaflet)
library(tidyverse)

d = sf::read_sf("data/MOTI_files/RFI_ALL_BC_2017.kml")
# The above looks like some order of highways. No metadata.
ggplot() +
  geom_sf(data = d)

unzip(zipfile = "data/MOTI_files/MoT MC BoundariesService Areas.kmz",
      exdir = "data/MOTI_files/")

file.rename("data/MOTI_files/doc.kml",
            "data/MOTI_files/MoT_MC_boundaries_services_areas.kml")

t = sf::read_sf("data/MOTI_files/MoT_MC_boundaries_services_areas.kml")

t = sf::st_as_sf(t)

t = sf::st_zm(t)

ggplot() +
  geom_sf(data = t)

leaflet() |>
  addTiles() |>
  addPolygons(
    data = t,
    label = ~Name
  )
