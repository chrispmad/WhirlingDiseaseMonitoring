# Read in feedback compilation
fn_feedback = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/2025/First_Nations_Feedback_compilation.xlsx"))

# Pull out waterbody names and watershed codes for wbs that have been identified by FN partners.
identified_by_FN_partners = fn_feedback |>
  dplyr::filter(original_email_lists | !is.na(Ktunaxa_ID) | !is.na(Shuswap_ID) | !is.na(ONA_ID) | `Waterbody Name` == "Fraling Creek") |>
  dplyr::select(GNIS_NA = `Waterbody Name`,
                WATERSH = `Watershed Code`) |>
  dplyr::mutate(Flagged_by_FN_partners = TRUE) |>
  dplyr::distinct()

# Join on to wb_list
wb_list = wb_list |>
  dplyr::left_join(identified_by_FN_partners)

# Pull out the same for opportunisitic sampling.
opportunistic_sampling = fn_feedback |>
  dplyr::select(GNIS_NA = `Waterbody Name`,
                WATERSH = `Watershed Code`,
                Ktunaxa_OS, ONA_OS, Shuswap_OS) |>
  dplyr::mutate(Ktunaxa_OS = ifelse(!str_detect(Ktunaxa_OS,"^KTUNAXA_"),paste0("KTUNAXA_",Ktunaxa_OS),Ktunaxa_OS),
                Shuswap_OS = ifelse(!str_detect(Shuswap_OS,"^SHUSWAP_"),paste0("SHUSWAP_",Shuswap_OS),Shuswap_OS),
                ONA_OS = ifelse(!str_detect(ONA_OS,"^ONA_"),paste0("ONA_",ONA_OS),ONA_OS)) |>
  dplyr::distinct() |>
  dplyr::rowwise() |>
  #from chatgpt
  mutate(opportunistic_sampling = pmap_chr(
    list(Ktunaxa_OS, Shuswap_OS, ONA_OS),
    ~ paste(na.omit(c(...)), collapse = ", ")
  )) |> 
  dplyr::select(GNIS_NA,
                WATERSH,
                opportunistic_sampling) |>
  dplyr::distinct() |>
  dplyr::filter(opportunistic_sampling != "")

wb_list = wb_list |>
  dplyr::left_join(opportunistic_sampling)

# opportunistic_sampling$GNIS_NA[!opportunistic_sampling$GNIS_NA %in% wb_list$GNIS_NA]

wb_list = wb_list |>
  dplyr::mutate(other_priority_species = NA)

list_nations <- c("Dutch Creek", "Windermere Creek", "Luxor Creek", "Fraling Creek",
                  "Slocan Lake", "Lower Kootenay River", "Lower Columbia River",
                  "Columbia River", "Kootenay River",
                  "Okanagan Lake", "Skaha Lake", "Vaseux Lake", "Osoyoos Lake",
                  "Okanagan River", "Upper Arrow Lake", "Lower Arrow Lake",
                  "Slocan River", "Duncan Lake", "Moyie Lake", "Hill Creek")

# # first, see if the the wbs in list_nations is already present in wb_list
# wb_list<-wb_list |>
#   dplyr::mutate(Flagged_by_FN_partners = ifelse(GNIS_NA %in% list_nations, TRUE, FALSE))

#for the ones that have not been found
list_nations <- list_nations[!list_nations %in% wb_list$GNIS_NA]

new_wbs<-bcdc_query_geodata("freshwater-atlas-rivers") |>
  filter(GNIS_NAME_1 %in% list_nations) |>
  collect() |>
  st_transform(4326)

if (nrow(new_wbs) > 0) {
  new_wbs <- new_wbs |>
    st_transform(4326) |>
    group_by(GNIS_NAME_1, WATERSHED_GROUP_ID) |>
    summarise()
} else {
  new_wbs <- st_sf(
    GNIS_NAME_1 = character(),
    WATERSHED_GROUP_ID = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}

new_streams<-bcdc_query_geodata("freshwater-atlas-stream-network") |>
  filter(GNIS_NAME %in% list_nations) |>
  collect() |>
  st_transform(4326)

if(nrow(new_streams) > 0) {
  new_streams <- new_streams |>
    st_transform(4326) |>
    group_by(GNIS_NAME, WATERSHED_GROUP_ID, BLUE_LINE_KEY) |>
    summarise() |>
    dplyr::mutate(WB_POLY_ID = 0) |>
    dplyr::ungroup()
} else {
  new_streams <- st_sf(
    GNIS_NAME = character(),
    WATERSHED_GROUP_ID = numeric(),
    BLK = numeric(),
    WB_POLY_IS = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}

new_streams <- new_streams |>
  st_buffer(10)

new_lakes<-bcdc_query_geodata("freshwater-atlas-lakes") |>
  filter(GNIS_NAME_1 %in% list_nations) |>
  collect() |>
  st_transform(4326)

if (nrow(new_lakes) > 0) {
  new_lakes <- new_lakes |>
    st_transform(4326) |>
    group_by(GNIS_NAME_1, WATERSHED_GROUP_ID, BLK, WB_POLY_ID) |>
    summarise()
} else {
  new_lakes <- st_sf(
    GNIS_NAME_1 = character(),
    WATERSHED_GROUP_ID = numeric(),
    BLK = numeric(),
    WB_POLY_ID = numeric(),
    geometry = st_sfc(crs = 4326)
  )
}


if(nrow(new_streams) > 0){
  new_streams <- new_streams |>
    dplyr::rename(GNIS_NAME_1 = GNIS_NAME,
                  BLK = BLUE_LINE_KEY)
}

nations_wb<-bind_rows(new_wbs, new_streams, new_lakes) |>
  select(GNIS_NAME_1, WATERSHED_GROUP_ID, WB_POLY_ID, BLK, geometry)

# Filter output by the GNIS_NA and WATERSHED_GROUP_ID from feedback document.
nations_wb = nations_wb |>
  dplyr::left_join(fn_feedback |>
                     dplyr::rename(GNIS_NAME_1 = `Waterbody Name`,
                                   WATERSHED_GROUP_ID = `Watershed Code`
                     ) |>
                     dplyr::mutate(keep_me = TRUE) |>
                     dplyr::select(GNIS_NAME_1, WATERSHED_GROUP_ID,keep_me)) |>
  dplyr::filter(keep_me | GNIS_NAME_1 %in% c("Fraling Creek")) |>
  dplyr::select(-keep_me)

nations_priorities <- nations_wb |>
  group_by(GNIS_NA = GNIS_NAME_1,
           WATERSH = WATERSHED_GROUP_ID,
           WB_POLY_ID,
           BLK
           ) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

#lets add the nations_priorities to the wb_list and change the Flagged_by_FN_partners to TRUE for the new entries
nations_priorities <- st_transform(nations_priorities, st_crs(wb_list))

additional_nations_priorities = nations_priorities |>
  filter(GNIS_NA %in% identified_by_FN_partners$GNIS_NA)

wb_list <- wb_list |>
  dplyr::bind_rows(
    additional_nations_priorities |>
      dplyr::mutate(Flagged_by_FN_partners = TRUE) |>
      dplyr::left_join(opportunistic_sampling)
  )

additional_op_sample_rows = nations_priorities |>
  filter(GNIS_NA %in% opportunistic_sampling$GNIS_NA) |>
  dplyr::filter(!GNIS_NA %in% wb_list$GNIS_NA)

wb_list <- wb_list |>
  dplyr::bind_rows(
    additional_op_sample_rows |>
      dplyr::left_join(opportunistic_sampling)
  )

# wb_list |>
#   dplyr::filter(GNIS_NA %in% c("Fraling Creek","Windermere Lake"))
#   dplyr::mutate(Flagged_by_FN_partners = ifelse(GNIS_NA %in% c("Fraling Creek","Windermere Lake")))
# still_missing <- list_nations[!list_nations %in% wb_list$GNIS_NA]
