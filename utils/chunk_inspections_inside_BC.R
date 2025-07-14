insp_from_wd_bc_wb_b = bin_to_kmeans(insp_from_wd_bc_wb, 'TotalInspections', k_breaks)

# Strip out pacific ocean and 'dry storage' destinations for watercraft.
insp_from_wd_bc_wb_b = insp_from_wd_bc_wb_b |>
  dplyr::filter(!GNIS_NA %in% c("Pacific Ocean","Dry Storage"))

bins_title_bc = getKMeansBreaks(insp_from_wd_bc_wb_b$TotalInspections, k = k_breaks)

# Plot showing binned numbers of watercraft headed to waterbodies in BC from infected states / provinces as well as infected waterbodies in BC.
if(!interactive()){
  ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = insp_from_wd_bc_wb_b, aes(fill = TotalInspections_kmeans_bin ,
                                             col = TotalInspections_kmeans_bin )) +
    ggthemes::theme_map() +
    scale_fill_brewer(palette = 'Spectral', direction = -1) +
    scale_color_brewer(palette = 'Spectral', direction = -1) +
    bin_info_labs(bins_title_bc)
}
# Identify any water bodies that aren't already in the list.
new_wb_for_list = insp_from_wd_bc_wb_b |>
  dplyr::anti_join(
    wb_list |>
      sf::st_drop_geometry() |>
      dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID)
  ) |>
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID,
                insp_from_wd_bc_wb = TotalInspections,
                insp_from_wd_bc_wb_kmeans_bin = TotalInspections_kmeans_bin )

wb_already_in_list = insp_from_wd_bc_wb_b |>
  dplyr::filter(paste0(GNIS_NA, WATERSH, BLK, WB_POLY_ID) %in% paste0(wb_list$GNIS_NA,wb_list$WATERSH,wb_list$BLK,wb_list$WB_POLY_ID)) |>
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID,
                insp_from_wd_bc_wb = TotalInspections,
                insp_from_wd_bc_wb_kmeans_bin = TotalInspections_kmeans_bin ) |>
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
