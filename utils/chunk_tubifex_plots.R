
tub_files = list.files(path = 'output/', pattern = 'habitat_maxent_tubifex.*', full.names = T)

if(length(tub_files) > 0){

  tub_file_options = data.frame(tub_files)
  tub_file_options$file_cdate = NA

  for(i in 1:nrow(tub_file_options)){
    tub_file_options$file_cdate = file.info(tub_file_options$tub_files)$ctime
  }

  tub_file = tub_file_options |>
    dplyr::filter(!str_detect(tub_files,"binary")) |>
    dplyr::slice_max(file_cdate)

  tub_hab_not_hab_file = tub_file_options |>
    dplyr::filter(stringr::str_detect(tub_files,"_binary")) |>
    dplyr::slice_max(file_cdate)

  tubifex_maxent <- terra::rast(tub_file$tub_files)
  hab_not_hab_tub_maxent = terra::rast(tub_hab_not_hab_file$tub_files)
  # tubifex_maxent <- terra::as.factor(tubifex_maxent)

  #create factors for the values in the raster
  # For the wbs_list, find the overlap with the raster abd add a column for tubifex habitat suitability.
  bc_crs <- "EPSG:3005"
  hab_not_hab_tub_maxent <- project(hab_not_hab_tub_maxent, bc_crs, method = "near")
  wb_vect <- vect(wb_list)
  # factor_levels <- levels(hab_not_hab_tub_maxent)[[1]]  # Only one layer
  extracted <- terra::extract(hab_not_hab_tub_maxent, wb_vect)
  # extracted <- dplyr::left_join(extracted, factor_levels, by = "fc.LQ_rm.4")
  extracted = tidyr::as_tibble(extracted)
  names(extracted)[2] = 'pred_var'
  suitability_summary <- extracted |>
    group_by(ID) |>
    summarize(
      max_val = if (all(is.na(pred_var))) NA_real_ else max(as.numeric(pred_var), na.rm = TRUE)
    ) |>
    mutate(
      tubifex_habitat_suitability = case_when(
        is.na(max_val) ~ NA_character_,
        max_val >= 1 ~ "suitable",
        TRUE ~ "unsuitable"
      )
    )

  wb_list$tubifex_habitat_suitability <- suitability_summary$tubifex_habitat_suitability


  names(tubifex_maxent) = 'rel.suitability'
  p_1 = ggplot() +
    tidyterra::geom_spatraster(data = tubifex_maxent, aes(fill = rel.suitability))+
    geom_sf(data = bcmaps::bc_bound(), fill = NA, color = "black") +
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(9, "YlOrBr"),
      na.value = "transparent",
      name = "Habitat suitability"
    ) +
    ggthemes::theme_map()

  print(p_1)

  names(hab_not_hab_tub_maxent) = 'hab.not.hab'
  # hab_not_hab_tub_maxent$suitable = 'NA'

  p_2 = ggplot() +
    tidyterra::geom_spatraster(data = hab_not_hab_tub_maxent, aes(fill = hab.not.hab)) +  # Raster layer
    geom_sf(data = bcmaps::bc_bound(), fill = NA, color = "black") +  # BC boundary
    # geom_sf(data = wb_list, color = "blue", alpha = 0.5) +  # Waterbodies
    # scale_fill_manual(
    #   values = c("0" = "lightgreen", "1" = "purple"),
    #   na.value = "transparent"  # Handle NA values
    # ) +
    labs(fill = 'Suitable\nHabitat') +
    ggthemes::theme_map()+
    theme(legend.position = "right",
          plot.background = element_rect(fill = 'lightblue'))

  p_2
}
