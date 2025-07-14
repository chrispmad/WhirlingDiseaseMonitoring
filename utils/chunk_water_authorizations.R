if(!file.exists('data/water_authorization_points.rds')){
  # water_apps = bcdc_query_geodata('water-approval-points') |>
  #   # filter(INTERSECTS(wsg)) |>
  #   collect()
  water_apps = sf::read_sf("W:/CMadsen/shared_data_sets/water_sustainability_act_approvals_section_10_and_11.shp")
  water_apps = sf::st_filter(water_apps, bcmaps::bc_bound())
  water_apps = water_apps |> dplyr::filter(stringr::str_detect(WSD_TECHNI, "(Lyndsey|Duane|Mai-Linh|Kristin)"))
  saveRDS(water_apps,'data/water_authorization_points.rds')
} else {
  water_apps = readRDS('data/water_authorization_points.rds')
}

water_apps = water_apps |>
  dplyr::mutate(approval_status = dplyr::case_when(
    APPROVAL_S %in% c("Abandoned","Aborted","Cancelled") ~ "Abandoned/Cancelled",
    T ~ APPROVAL_S
  ))

library(leaflet)
uniq_app_stat = unique(water_apps$approval_status)
uniq_app_stat = na.omit(uniq_app_stat)

app_stat_leg = colorFactor(palette = 'Spectral', domain = uniq_app_stat)

l = leaflet() |>
  addTiles() |>
  addLayersControl(position = 'bottomleft',
                   overlayGroups = uniq_app_stat,
                   options = layersControlOptions(collapsed = F))

zIndex_base = 300
for(app_stat in uniq_app_stat){
  zIndex_base = zIndex_base + 100
  l = l |>
    addMapPane(name = app_stat, zIndex = zIndex_base) |>
    addCircleMarkers(
      data = sf::st_transform(water_apps[water_apps$approval_status == app_stat,], 4326),
      label = app_stat,
      color = 'black',
      weight = 1,
      # color = ~app_stat_leg(approval_status),
      fillColor = ~app_stat_leg(approval_status),
      fillOpacity = 0.8,
      group = app_stat,
      options = pathOptions(pane = app_stat)
    )
}

l = l |>
  addLegend(pal = app_stat_leg, values = uniq_app_stat)

if(!interactive()){
  # ggplot() +
  #   geom_sf(data = bcmaps::bc_bound()) +
  #   geom_sf(data = water_apps, aes(col = approval_status))
  l
}

water_apps_w_wb_info = water_apps |>
  dplyr::filter(approval_status %in% c('Current')) |>
  sf::st_join(wb_list |> dplyr::select(GNIS_NA,WATERSH))

water_apps_w_wb_cumulative = water_apps |>
  dplyr::filter(approval_status %in% c("Current","Superseded")) |>
  sf::st_join(wb_list |> dplyr::select(GNIS_NA,WATERSH)) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(GNIS_NA)) |>
  dplyr::count(GNIS_NA,WATERSH, name = "cumulative_authorizations")


number_water_apps_per_wb = water_apps_w_wb_info |>
  dplyr::filter(!is.na(GNIS_NA)) |>
  sf::st_drop_geometry() |>
  dplyr::count(GNIS_NA,WATERSH, name = "active_water_authorizations")


# Bin!
number_water_apps_per_wb = bin_to_kmeans(number_water_apps_per_wb, 'active_water_authorizations', k_breaks)

water_auths_bins = getKMeansBreaks(number_water_apps_per_wb$active_water_authorizations, k = k_breaks)

wbs_with_water_apps = wb_list |>
  dplyr::inner_join(
    number_water_apps_per_wb
  )

if(!interactive()){
  ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = wbs_with_water_apps, aes(col = active_water_authorizations_kmeans_bin, fill = active_water_authorizations_kmeans_bin)) +
    scale_fill_brewer(palette = 'Spectral', direction = -1) +
    scale_color_brewer(palette = 'Spectral', direction = -1) +
    ggthemes::theme_map() +
    bin_info_labs(water_auths_bins,"Water Auths")
}


wb_list = wb_list |>
  dplyr::left_join(number_water_apps_per_wb) |>
  dplyr::mutate(active_water_authorizations = tidyr::replace_na(active_water_authorizations, 0))

water_apps |>
  sf::st_drop_geometry() |>
  dplyr::count(WSD_TECHNI, sort = T, name = 'number of rows') |>
  knitr::kable()
