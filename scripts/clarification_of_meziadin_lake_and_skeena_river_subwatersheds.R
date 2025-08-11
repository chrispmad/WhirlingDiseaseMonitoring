# Waterbody names are: Babine Lake, Meziadin Lake, Skeena River.
library(bcdata)
library(tidyverse)

blake = bcdc_query_geodata('freshwater-atlas-lakes') |>
  filter(GNIS_NAME_1 == 'Babine Lake') |>
  collect()

mlake = bcdc_query_geodata('freshwater-atlas-lakes') |>
  filter(GNIS_NAME_1 == 'Meziadin Lake') |>
  collect()

sriver = bcdc_query_geodata('freshwater-atlas-rivers') |>
  filter(GNIS_NAME_1 == 'Skeena River') |>
  collect() |>
  sf::st_zm() |>
  dplyr::group_by(GNIS_NAME_1) |>
  dplyr::summarise()

# Now check each of these waterbodies to see if they are contained
# by the suspiciously small sounding watershed names.
kinskuch = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  filter(WATERSHED_GROUP_NAME == 'Kinskuch River') |>
  collect()

kalum = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  filter(WATERSHED_GROUP_NAME == 'Kalum River') |>
  collect()

# Test 1!
test_1 = dplyr::bind_rows(
  kinskuch |>
    dplyr::select(the_name = WATERSHED_GROUP_NAME) |>
    dplyr::mutate(the_name = paste0(the_name, " Subwatershed")) |>
    dplyr::mutate(type = 'subwatershed'),
  mlake |>
    dplyr::select(the_name = GNIS_NAME_1) |>
    dplyr::mutate(type = 'lake')
)

ggplot() +
  geom_sf(data = test_1, aes(fill = the_name)) +
  labs(fill = "Name")

# Test 2!
test_2 = dplyr::bind_rows(
  kalum |>
    dplyr::select(the_name = WATERSHED_GROUP_NAME) |>
    dplyr::mutate(the_name = paste0(the_name, " Subwatershed")) |>
    dplyr::mutate(type = 'subwatershed'),
  sriver |>
    dplyr::select(the_name = GNIS_NAME_1) |>
    dplyr::mutate(type = 'lake')
)

ggplot() +
  geom_sf(data = test_2, aes(fill = the_name)) +
  labs(fill = "Name")


