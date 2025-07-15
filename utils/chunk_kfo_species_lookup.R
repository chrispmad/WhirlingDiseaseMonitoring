# save it to onedrive
if(!file.exists(paste0(onedrive_wd,"kfo.rds"))){
  kfo <- bcdc_query_geodata("known-bc-fish-observations-and-bc-fish-distributions") |>
    collect() |>
    st_as_sf()
  saveRDS(kfo, paste0(onedrive_wd,"kfo.rds"))
} else {
  kfo = readRDS(paste0(onedrive_wd,"kfo.rds"))
}

# Filter KFO to fish within waterbody bounds (fast!)
kfo_spp <- kfo |>
  st_filter(wb_list, .predicate = st_intersects) |>
  select(SPECIES_NAME, geometry)

# Join from fish to waterbodies (faster)
joined <- st_join(kfo_spp, wb_list, join = st_intersects, left = FALSE)

# Summarize known fish by waterbody
fish_summary <- joined |>
  group_by(GNIS_NA, WATERSH, BLK, WB_POLY_ID) |>
  summarize(
    known_fish = paste(sort(unique(SPECIES_NAME)), collapse = ", "),
    .groups = "drop"
  )

# new_wb_fish <- fish_summary |>
#   anti_join(
#     wb_list |> st_drop_geometry() |> select(GNIS_NA, WATERSH),
#     by = c("GNIS_NA", "WATERSH")
#   )

# wb_fish_existing <- fish_summary |>
#   filter(paste0(WATERSH, GNIS_NA) %in% paste0(wb_list$WATERSH, wb_list$GNIS_NA))

wb_list <- wb_list |>
  left_join(fish_summary |> st_drop_geometry(),
            by = c("GNIS_NA", "WATERSH","WB_POLY_ID","BLK"))

rm(joined); rm(kfo); rm(kfo_spp); rm(new_wb_for_list)
