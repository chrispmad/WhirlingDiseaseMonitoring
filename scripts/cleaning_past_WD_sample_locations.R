library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(bcdata)

wd_past_sampling_fish = read_excel("data/WD Sampling Data 2017_2023.xlsx", sheet = "Fish Data")
wd_past_sampling_worms = read_excel("data/WD Sampling Data 2017_2023.xlsx", sheet = "Worm Data")

wd_past_sampling_fish_sf = wd_past_sampling_fish |>
  sf::st_as_sf(coords = c("Easting","Northing"), crs = 32611) |>
  sf::st_transform(4326) |>
  dplyr::mutate(sample_type = "Fish Samples")

wd_past_sampling_worms_sf = wd_past_sampling_worms |>
  sf::st_as_sf(coords = c("Easting","Northing"), crs = 32611) |>
  sf::st_transform(4326) |>
  dplyr::mutate(sample_type = "Tubifex")

wd_past_sampling = dplyr::bind_rows(
  wd_past_sampling_fish_sf,
  wd_past_sampling_worms_sf
) |>
  sf::st_transform(4326) |>
  dplyr::mutate(the_date = lubridate::ymd(Date)) |>
  dplyr::mutate(orig_lat = sf::st_coordinates(geometry)[,2],
                orig_lng = sf::st_coordinates(geometry)[,1])

i = 153
d_i = wd_past_sampling[i,] |> sf::st_transform(3005)
d_i$Site
d_i_b = sf::st_buffer(d_i, 20)

river_overlap = bcdc_query_geodata('freshwater-atlas-rivers') |>
  filter(INTERSECTS(d_i)) |>
  collect()

nrow(river_overlap)
river_overlap$GNIS_NAME_1

lake_overlap = bcdc_query_geodata('freshwater-atlas-lakes') |>
  filter(INTERSECTS(d_i)) |>
  collect()

nrow(lake_overlap)

leaflet(options = leafletOptions(minZoom = 1)) |>
  addTiles() |>
  addCircleMarkers(
    data = d_i |> sf::st_transform(4326),
    label = ~paste0(Reach, ", ",Site,", ",the_date)
  ) |>
  leafem::addMouseCoordinates() |>
  addPolygons(data = bcdc_query_geodata('freshwater-atlas-rivers') |>
                filter(INTERSECTS(d_i_b)) |>
                collect() |>
                sf::st_transform(4326)) |>
  addPolygons(data = lake_overlap |>
                st_transform(4326)) |>
  addPolygons(
    data = bcdc_query_geodata('freshwater-atlas-stream-network') |>
      filter(GNIS_NAME == "Kikomun Creek") |>
      collect() |>
      sf::st_zm() |>
      st_buffer(10) |>
      st_transform(4326)
  )

paste0(round(d_i$orig_lat,6),
       ", ",
       round(d_i$orig_lng,6))

# Streams
bcdc_query_geodata('freshwater-atlas-stream-network') |>
  filter(INTERSECTS(d_i_b)) |>
  collect() |>
  dplyr::pull(GNIS_NAME)
