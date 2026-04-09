edna_results = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx")) |>
  purrr::set_names(snakecase::to_snake_case)

pos_fish_plus_parasite_edna = edna_results |>
  dplyr::filter(
    stringr::str_detect(fish_sampling_results_q_pcr_mc_detected, 'Positive') |
      e_dna_results_mc == 'Weak Detection'
  ) |>
  dplyr::mutate(
    result_type = dplyr::case_when(
      stringr::str_detect(fish_sampling_results_q_pcr_mc_detected, 'Positive') &
        e_dna_results_mc == 'Weak Detection' ~ 'Both (Fish & eDNA)',
      stringr::str_detect(fish_sampling_results_q_pcr_mc_detected, 'Positive') ~ 'Fish Positive',
      e_dna_results_mc == 'Weak Detection'                                      ~ 'eDNA Detection'
    )
  )

pos_fish_edna_sf = sf::st_as_sf(
  pos_fish_plus_parasite_edna,
  coords = c('long', 'lat'),
  crs = 4326
)

# Also pull in emerald lake, find its centroid.
emerald_lake = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
  bcdata::filter(GNIS_NAME_1 == 'Emerald Lake', WATERBODY_POLY_ID == 705013945) |>
  bcdata::collect()

emerald_lake_centroid = emerald_lake |>
  sf::st_centroid()

ws_w_edna_fish = ws |>
  sf::st_filter(
    pos_fish_edna_sf |>
      sf::st_transform(3005) |>
      dplyr::bind_rows(emerald_lake_centroid)
  )

ws_bbox = sf::st_bbox(ws_w_edna_fish |> sf::st_buffer(dist = 50000))

result_colours = c(
  'Fish Positive'      = '#E05C2A',
  'eDNA Detection'     = '#4DA6FF',
  'Both (Fish & eDNA)' = '#9B59B6'
)

result_shapes = c(
  'Fish Positive'      = 21,
  'eDNA Detection'     = 24,
  'Both (Fish & eDNA)' = 23
)
p1 <- ggplot() +
  geom_sf(data = bcmaps::bc_bound()) +
  geom_sf(
    data = ws_w_edna_fish,
    fill = "steelblue", alpha = 0.3, colour = "grey40"  # hardcoded, not aes()
  ) +
  geom_sf(
    data = pos_fish_edna_sf,
    aes(colour = result_type, shape = result_type, fill = result_type),
    size = 3, stroke = 0.8
  ) +
  geom_sf(data = emerald_lake, fill = 'lightgreen', col = 'darkgreen') +
  geom_sf_label(
    data = emerald_lake,
    aes(label = 'Emerald Lake'),
    nudge_x = 80000
  ) +
  ggsflabel::geom_sf_label_repel(
    data = pos_fish_edna_sf,
    aes(label = sample_site_name),
    force = 10, seed = 10, size = 3
  ) +
  scale_colour_manual(values = result_colours, name = "Result Type") +
  scale_fill_manual(values = result_colours, name = "Result Type") +
  scale_shape_manual(values = result_shapes, name = "Result Type") +
  coord_sf(
    xlim = ws_bbox[c(1, 3)],
    ylim = ws_bbox[c(2, 4)]
  ) +
  ggthemes::theme_map() +
  ggspatial::annotation_scale() +
  theme(legend.position = "right")
p1
ggsave("./output/positive_tests_and_watersheds.png", p1, width = 10, height = 8, dpi = 400)
