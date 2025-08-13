ws = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/subwatersheds_BC.shp")

edna_results = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx")) |>
  purrr::set_names(snakecase::to_snake_case)

pos_fish_plus_parasite_edna = edna_results |>
  dplyr::filter(stringr::str_detect(fish_sampling_results_q_pcr_mc_detected, 'Positive') | e_dna_results_mc == 'Weak Detection')

pos_fish_edna_sf = sf::st_as_sf(pos_fish_plus_parasite_edna,coords = c('long','lat'), crs = 4326)

# Also pull in emerald lake, find its centroid.
emerald_lake = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
  bcdata::filter(GNIS_NAME_1 == 'Emerald Lake', WATERBODY_POLY_ID == 705013945) |>
  bcdata::collect()

emerald_lake_centroid = emerald_lake |>
  sf::st_centroid()

ws_w_edna_fish = ws |>
  sf::st_filter(
    pos_fish_edna_sf |>
      sf::st_transform(3005) |>
      dplyr::bind_rows(emerald_lake_centroid)
  )

ws_bbox = sf::st_bbox(ws_w_edna_fish |> sf::st_buffer(dist = 50000))

if(!interactive()){
  print(
    ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = ws_w_edna_fish, aes(fill = "Positive Fish \neDNA Results")) +
    geom_sf(data = pos_fish_edna_sf, col = 'lightgreen') +
    geom_sf(data = emerald_lake, fill = 'lightgreen', col = 'lightgreen') +
    geom_sf_label(data = emerald_lake, aes(label = 'Emerald Lake'), nudge_x = 80000) +
    # geom_sf_label(data = pos_fish_edna_sf, aes(label = sample_site_name), nudge_x = 80000, nudge_y = -100000) +
    ggsflabel::geom_sf_label_repel(data = pos_fish_edna_sf,
                                   aes(label = sample_site_name),
                                   force = 10, seed = 10) +
    labs(fill = 'eDNA') +
    coord_sf(xlim = ws_bbox[c(1,3)],
             ylim = ws_bbox[c(2,4)]) +
    ggthemes::theme_map() +
    ggspatial::annotation_scale()
  )
}

wb_list = wb_list |>
  dplyr::mutate(fish_edna_positive_in_watershed = WATERSH %in% ws_w_edna_fish$WATERSHED_)
