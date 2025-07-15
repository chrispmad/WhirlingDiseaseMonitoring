### Leaflet Map - Provincial Risk
library(leaflet)

priority_pal = colorNumeric("viridis", domain = wb_list$priority)
wb_list$priority = as.numeric(wb_list$priority)

wb_list_over_3 <- wb_list |> filter(priority > 3)

priority_vals <- sort(unique(wb_list_over_3$priority))

m <- leaflet() |>
  addTiles(group = 'openStreetMap') |>
  addProviderTiles(provider = providers$CartoDB, group = 'cartoDB')

# Add a polygon layer for each priority value
for (p in priority_vals) {
  wb_subset <- wb_list_over_3 |> filter(priority == p)

  m <- m |> addPolygons(
    data = wb_subset,
    label = ~paste0(GNIS_NA, " (priority ", priority, ")"),
    fillColor = ~priority_pal(priority),
    fillOpacity = 0.8,
    color = "#333333",
    weight = 1,
    group = paste0("Priority ", p),
    popup = leafpop::popupTable(
      wb_subset |> st_drop_geometry() |>
        select(GNIS_NA, susceptible_spp, SARA, stocked_species, known_fish_occs, boats_inside_BC_bin, boats_entering_BC_bin, days_fished_bin, priority)
    )
  )
}


# Add layer controls to toggle each priority level
m <- m |> addLayersControl(
  position = 'bottomleft',
  overlayGroups = paste0("Priority ", priority_vals),
  options = layersControlOptions(collapsed = FALSE)
)

# Optionally add legend
m <- m |> addLegend(
  pal = priority_pal,
  values = wb_list$priority,
  title = "Priority",
  position = "topright"
)

# Add reset map button
m = m |>
  leaflet.extras::addResetMapButton() |>
  leaflet.extras::addSearchFeatures(
    targetGroups = c(paste0("Priority ",priority_vals)),
    options = leaflet.extras::searchFeaturesOptions(
      zoom=8, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = F, hideMarkerOnCollapse = TRUE,
    ))

# m
