# Angler survey data
source("utils/bring_in_angler_survey_data.R")

ang$waterbody = NULL
ang$watershed = NULL

binned_ang<-bin_to_kmeans(ang, "fishing_days" , k_breaks)
bin_vals<-getKMeansBreaks(binned_ang$fishing_days, k = k_breaks)

if(!interactive()){
  print(
    ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = binned_ang, aes(col = fishing_days_kmeans_bin, fill = fishing_days_kmeans_bin)) +
    scale_fill_brewer(palette = 'Spectral', direction = -1) +
    scale_color_brewer(palette = 'Spectral', direction = -1) +
    ggthemes::theme_map() +
    bin_info_labs(bin_vals,'Fishing Days')
  )
}

binned_ang<-st_transform(binned_ang, st_crs(wb_list))

binned_ang<-binned_ang |>
  dplyr::rename(days_fished = fishing_days,
                days_fished_kmeans_bin = fishing_days_kmeans_bin)

# Identify any water bodies that aren't already in the list.
new_wb_for_list = binned_ang |>
  dplyr::anti_join(
    wb_list |>
      sf::st_drop_geometry() |>
      dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID)
  ) |>
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID,
                days_fished, days_fished_kmeans_bin)

wb_already_in_list = binned_ang |>
  dplyr::filter(paste0(GNIS_NA, WATERSH, BLK, WB_POLY_ID) %in% paste0(wb_list$GNIS_NA,wb_list$WATERSH,wb_list$BLK, wb_list$WB_POLY_ID)) |>
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID,
                days_fished, days_fished_kmeans_bin) |>
  sf::st_drop_geometry()

# Not new waterbodies per list, per se, but new columns.
wb_list = wb_list |>
  dplyr::left_join(
    wb_already_in_list
  )

# Are there any new waterbodies to be added to the wb_list?
if(nrow(new_wb_for_list) > 0){
  wb_list = wb_list |>
    dplyr::bind_rows(
      new_wb_for_list
    )
}

