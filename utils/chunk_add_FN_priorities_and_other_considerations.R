wb_list = wb_list |>
  dplyr::mutate(other_priority_species = NA, opportunistic_sampling = FALSE)

list_nations <- c("Dutch Creek", "Windermere Creek", "Luxor Creek", "Galena Creek",
                  "Slocan Lake", "Lower Kootenay River", "Lower Columbia River",
                  "Columbia River", "Kootenay River",
                  "Okanagan Lake", "Skaha Lake", "Vaseux Lake", "Osoyoos Lake",
                  "Okanagan River", "Upper Arrow Lake", "Lower Arrow Lake",
                  "Slocan River", "Duncan Lake", "Moyie Lake")

# first, see if the the wbs in list_nations is already present in wb_list
wb_list<-wb_list |>
  dplyr::mutate(Indigenous_priority = ifelse(GNIS_NA %in% list_nations, TRUE, FALSE))

#for the ones that have not been found
list_nations <- list_nations[!list_nations %in% wb_list$GNIS_NA]

new_wbs<-bcdc_query_geodata("freshwater-atlas-rivers") |>
  filter(GNIS_NAME_1 %in% list_nations) |>
  collect() |>
  st_transform(4326)

if (nrow(new_wbs) > 0) {
  new_wbs <- new_wbs |>
    st_transform(4326) |>
    group_by(GNIS_NAME_1, WATERSHED_GROUP_ID) |>
    summarise()
} else {
  new_wbs <- st_sf(
    GNIS_NAME_1 = character(),
    WATERSHED_GROUP_ID = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}

new_streams<-bcdc_query_geodata("freshwater-atlas-stream-network") |>
  filter(GNIS_NAME %in% list_nations) |>
  collect() |>
  st_transform(4326)

if(nrow(new_streams) > 0) {
  new_streams <- new_streams |>
    st_transform(4326) |>
    group_by(GNIS_NAME, WATERSHED_GROUP_ID, BLUE_LINE_KEY) |>
    summarise() |>
    dplyr::mutate(WB_POLY_ID = 0) |>
    dplyr::ungroup()
} else {
  new_streams <- st_sf(
    GNIS_NAME = character(),
    WATERSHED_GROUP_ID = numeric(),
    BLK = numeric(),
    WB_POLY_IS = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}

new_streams <- new_streams |>
  st_buffer(10)

new_lakes<-bcdc_query_geodata("freshwater-atlas-lakes") |>
  filter(GNIS_NAME_1 %in% list_nations) |>
  collect() |>
  st_transform(4326)

if (nrow(new_lakes) > 0) {
  new_lakes <- new_lakes |>
    st_transform(4326) |>
    group_by(GNIS_NAME_1, WATERSHED_GROUP_ID, BLK, WB_POLY_ID) |>
    summarise()
} else {
  new_lakes <- st_sf(
    GNIS_NAME_1 = character(),
    WATERSHED_GROUP_ID = numeric(),
    BLK = numeric(),
    WB_POLY_ID = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}


if(nrow(new_streams) > 1){
  new_streams <- new_streams |>
    dplyr::rename(GNIS_NAME_1 = GNIS_NAME,
                  BLK = BLUE_LINE_KEY)
}

nations_wb<-bind_rows(new_wbs, new_streams, new_lakes) |>
  select(GNIS_NAME_1, WATERSHED_GROUP_ID, WB_POLY_ID, BLK, geometry)

nations_priorities <- nations_wb |>
  group_by(GNIS_NA = GNIS_NAME_1,
           WATERSH = WATERSHED_GROUP_ID,
           WB_POLY_ID,
           BLK
           ) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

#lets add the nations_priorities to the wb_list and change the Indigenous_priority to TRUE for the new entries
nations_priorities <- st_transform(nations_priorities, st_crs(wb_list))

wb_list <- wb_list |>
  dplyr::bind_rows(
    nations_priorities |>
      dplyr::mutate(Indigenous_priority = TRUE)
  )

still_missing <- list_nations[!list_nations %in% wb_list$GNIS_NA]
