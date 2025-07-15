priority_pal = colorNumeric("viridis", domain = wb_list_SARA$priority)

priority_vals <- sort(unique(wb_list_SARA$priority))

priority_vals = priority_vals[priority_vals > 4]

m <- leaflet() |>
  addTiles(group = 'openStreetMap') |>
  addProviderTiles(provider = providers$CartoDB, group = 'cartoDB')

# Add a polygon layer for each priority value
for (p in priority_vals) {
  if(p < 5) next
  wb_subset <- wb_list_SARA |> filter(priority == p)

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
        select(GNIS_NA, susceptible_spp, SARA, stocked_species, boats_inside_BC_bin, boats_entering_BC_bin, days_fished_bin, priority)
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
  values = wb_list_SARA$priority,
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

m_sara_overlap = m
