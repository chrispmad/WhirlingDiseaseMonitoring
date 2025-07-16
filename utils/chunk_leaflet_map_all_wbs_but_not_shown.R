### Leaflet Map - Provincial Risk
library(leaflet)

priority_pal = colorNumeric("viridis", domain = wb_nice_names$Priority)

wb_nice_names$Priority = as.numeric(wb_nice_names$Priority)

wb_list_over_3 <- wb_nice_names |> filter(Priority > 3)

priority_vals <- sort(unique(wb_list_over_3$Priority))

m <- leaflet() |>
  addTiles(group = 'openStreetMap') |>
  addProviderTiles(provider = providers$CartoDB, group = 'cartoDB')

# Add a polygon layer for each priority value
for (p in priority_vals) {
  wb_subset <- wb_list_over_3 |> filter(Priority == p)

  m <- m |> addPolygons(
    data = wb_subset,
    label = ~paste0(`Waterbody Name`, " (priority ", Priority, ")"),
    fillColor = ~priority_pal(Priority),
    fillOpacity = 0.8,
    color = "#333333",
    weight = 1,
    group = paste0("Priority ", p),
    popup = leafpop::popupTable(
      wb_subset |> st_drop_geometry() |>
        select(`Waterbody Name`,
               WD_susceptible_spp, SARA,
               stocked_species,
               `Boats inside BC (bins 1 - 5)`,
               `Boats entering BC (bins 1 - 5)`,
               `Days fished (bins 1 - 5)`,
               Priority)
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
  values = wb_nice_names$Priority,
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
