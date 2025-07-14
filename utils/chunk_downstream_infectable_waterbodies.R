downstream_infectable_waterbodies = rivers_in_infectable_adj_watersheds |>
  dplyr::bind_rows(
    lakes_in_infectable_adj_watersheds
  ) |>
  dplyr::ungroup() |>
  sf::st_join(adj_small_watersheds |>
                dplyr::select(watershed_name = GAZETTED_NAME,
                              stream_magnitude = STREAM_MAGNITUDE))

downstream_infectable_waterbodies_no_geom = downstream_infectable_waterbodies |>
  sf::st_drop_geometry() |>
  dplyr::select(-FWA_WATERSHED_CODE) |>
  dplyr::rename(waterbody_name = GNIS_NAME_1) |>
  dplyr::arrange(watershed_name)

openxlsx::write.xlsx(downstream_infectable_waterbodies_no_geom, file = "output/list_of_infectable_downstream_waterbodies.xlsx")

DT::datatable(downstream_infectable_waterbodies_no_geom)

wb_list_infectables = wb_list |>
  sf::st_filter(
    downstream_infectable_waterbodies
  ) |>
  dplyr::mutate(infectable = TRUE) |>
  dplyr::select(infectable, GNIS_NA, WATERSH) |>
  sf::st_drop_geometry()

wb_list = wb_list |>
  dplyr::left_join(wb_list_infectables) |>
  dplyr::mutate(infectable = tidyr::replace_na(infectable, FALSE))
