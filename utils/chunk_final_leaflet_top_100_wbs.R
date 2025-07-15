# Prep leaflet map
years_sampled = c(unique(as.character(lubridate::year(wd_past_sampling$Date))),'2024',"not sampled 2024")

my_pal = leaflet::colorFactor(palette = 'Spectral',
                              domain = years_sampled)

l = leaflet() |>
  addTiles() |>
  addLayersControl(
    position = 'bottomleft',
    overlayGroups = c(years_sampled, "not sampled 2024", "SARA present", "SARA absent", "First Nations priority"),
    options = layersControlOptions(collapsed = FALSE)
  )

for(i in 1:length(years_sampled)){
  the_year = years_sampled[i]
  if(the_year == "not sampled 2024") next
  if(the_year == '2024'){
    the_dat = wd_results_2024_sf
  } else {
    the_dat = wd_past_sampling |>
      dplyr::mutate(year = lubridate::year(Date)) |>
      dplyr::filter(year == the_year) |>
      sf::st_transform(4326)
  }

  l = l |>
    addMapPane(name = as.character(the_year), zIndex = (230 + i*30)) |>
    addCircleMarkers(
      data = the_dat,
      group = as.character(the_year),
      options = pathOptions(pane = as.character(the_year)),
      # fillColor = the_pal[i],
      fillColor = my_pal(as.character(the_year)),
      fillOpacity = 0.8,
      color = 'black',
      weight = 1,
      radius = 3,
      label = ~paste0(Site, " (",Reach,")"),
      popup = lapply(
        leafpop::popupTable(the_dat |> sf::st_drop_geometry()),
        htmltools::HTML
      )
    )
}

# Identify waterbodies that are within Fraser / Columbia watersheds and
# at or above priority 5.


# wb_no_sara_over_5_in_frascol = wb_list_NoSARA[wb_list_NoSARA$priority >= 5,] |>
#   sf::st_filter(frascol) |>
#   dplyr::filter(!duplicated(GNIS_NA))
#
# wb_sara_over_5_in_frascol = wb_list_SARA[wb_list_SARA$priority >= 5,] |>
#   sf::st_filter(frascol) |>
#   dplyr::filter(!duplicated(GNIS_NA))


l = l |>
  addMapPane(name = 'waterbodies', zIndex = 230) |>
  addPolygons(
    data = wb_no_sara_over_5_in_frascol,
    fillColor = 'purple',
    color = 'purple',
    label = ~paste0(GNIS_NA, "; priority rank ", priority),
    group = "SARA absent"
  ) |>
  addMapPane(name = 'waterbodies', zIndex = 260) |>
  addPolygons(
    data = wb_sara_over_5_in_frascol,
    fillColor = 'blue',
    color = 'blue',
    label = ~paste0(GNIS_NA, "; priority rank ", priority),
    group = "SARA present",
    popup = leafpop::popupTable(
      wb_sara_over_5_in_frascol,
      zcol = c("SARA")
    )
  )|>
  addMapPane(name = 'waterbodies', zIndex = 300) |>
  addPolygons(
    data = wb_sara_in_frascol_idigenous,
    fillColor = 'gold',
    color = 'gold',
    label = ~paste0(GNIS_NA, "; priority rank ", priority),
    group = "First Nations priority",
    popup = leafpop::popupTable(
      wb_sara_over_5_in_frascol,
      zcol = c("SARA")
    )
  )|>
  addMapPane(name = 'frascol', zIndex = 200) |>
  addPolygons(
    data = frascol,
    label = ~paste0(watershed," Watershed"),
    fillColor = 'grey',
    fillOpacity = 0.25,
    weight = 1,
    color = 'black',
    options = pathOptions(pane = 'frascol')
  ) |>
  addMapPane(name = 'frascol', zIndex = 200) |>
  addPolygons(
    data = frascol,
    label = ~paste0(watershed," Watershed"),
    fillColor = 'grey',
    fillOpacity = 0.25,
    weight = 1,
    color = 'black',
    options = pathOptions(pane = 'frascol')
  ) |>
  addMapPane(name = "not sampled 2024", zIndex = (600)) |>
  addCircleMarkers(
    data = wd_not_sampled_2024_sf,
    label = ~paste0(Site, " (",Reach,")"),
    options = pathOptions(pane = "not sampled 2024"),
    group = "not sampled 2024",
    fillColor = my_pal("not sampled 2024"),
    fillOpacity = 0.8,
    radius = 3,
    color = 'black',
    weight = 1
  ) |>
  addLegend(position = 'topright', title = "WD Sampling History", pal = my_pal, values = years_sampled) |>
  addLegend(position = "bottomright", title = "Waterbodies",
            colors = c("blue", "purple","gold"),
            labels = c("SARA present", "SARA absent", "FN Priority"),
            opacity = 1)

l_top_100 = l
