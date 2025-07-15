
insp_from_wd_inf_to_wb_b = bin_to_kmeans(insp_from_wd_inf_to_wb, 'TotalInspections', k_breaks)

# Strip out pacific ocean and 'dry storage' destinations for watercraft.
insp_from_wd_inf_to_wb_b = insp_from_wd_inf_to_wb_b |>
  dplyr::filter(!GNIS_NA %in% c("Pacific Ocean","Dry Storage"))

bins_title = getKMeansBreaks(insp_from_wd_inf_to_wb_b$TotalInspections, k = k_breaks)

# Plot showing binned numbers of watercraft headed to waterbodies in BC from infected states / provinces as well as infected waterbodies in BC.
if(!interactive()){
  print(
    ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = insp_from_wd_inf_to_wb_b, aes(
      fill = TotalInspections_kmeans_bin,
      col = TotalInspections_kmeans_bin)) +
    ggthemes::theme_map() +
    scale_fill_brewer(palette = 'Spectral', direction = -1) +
    scale_color_brewer(palette = 'Spectral', direction = -1) +
    bin_info_labs(bins_title)
  )
}
wb_list = insp_from_wd_inf_to_wb_b |>
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID, TotalInspections, TotalInspections_kmeans_bin )
