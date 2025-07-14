if(!file.exists("data/named_watersheds_of_bc.rds")){
  # small_watersheds = bcdc_query_geodata('freshwater-atlas-named-watersheds') |>
  small_watersheds = bcdc_query_geodata('wsa-third-order-and-greater-watersheds-50-000') |>
    collect()
  saveRDS(small_watersheds, "data/named_watersheds_of_bc.rds")
} else {
  small_watersheds = readRDS("data/named_watersheds_of_bc.rds")
  # test = small_watersheds |>
  #   # filter for 2nd order watersheds
  #   dplyr::filter(str_detect(FWA_WATERSHED_CODE,"^[0-9]{3}-[^0]{6}"))
}

pos_fish_edna_sf_albers = pos_fish_edna_sf |>
  sf::st_transform(3005)

small_watersheds_infected = small_watersheds |>
  sf::st_filter(pos_fish_edna_sf_albers)

watershed_codes_infected = unique(str_extract(small_watersheds_infected$WSD_ID,'^.{4}'))

# watershed groups?
wsg = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  filter(WATERSHED_GROUP_CODE %in% watershed_codes_infected) |>
  collect()

infected_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |>
  filter(INTERSECTS(small_watersheds_infected)) |>
  collect() |>
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME_1,FWA_WATERSHED_CODE) |>
  dplyr::summarise()
