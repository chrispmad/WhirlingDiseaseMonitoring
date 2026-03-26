library(tidyverse)
library(sf)
library(ggplot2)
library(bcdata)
library(basemaps)

k_breaks = 5 # number of breaks for k-means clustering
requery_occs = F


base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

#k-means clustering functions
source("utils/bin_to_kmeans_func.R")
source("utils/getKMeansBreaks_func.R")

source("utils/bring_in_entering_BC_inspection_data.R", local = T)
# Inspections for watercraft within BC
source("utils/bring_in_within_BC_inspection_data.R", local = T)

# The above datasets are key in establishing which of BC's many, many waterbodies are in scope for the rest of the analysis (i.e., they define wb_list)

# Which waterbodies were sampled last year?
wbs_sampled_2024 = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx")) |> 
  purrr::set_names(snakecase::to_snake_case) |> 
  dplyr::filter(sampled_in_2024_y_n == 'Y')


source("utils/chunk_inspections_entering_BC.R", local = T)
source("utils/chunk_inspections_inside_BC.R", local = T)
source("utils/chunk_dfo_angler_survey_data.R", local = T)
source('utils/chunk_water_authorizations.R', local = T)

source("utils/chunk_kfo_species_lookup.R", local = T)
source("utils/chunk_watersheds_with_WD_in.R", local = T)
source("utils/chunk_susceptible_species_overlay.R", local = T)


wb_sampled_2024 = wb_list |> 
  sf::st_filter(
    sf::st_as_sf(
      wbs_sampled_2024,
      coords = c("long","lat"),
      crs = 4326
    ) |> sf::st_transform(3005) |> 
      sf::st_buffer(dist = 25)
  ) |> 
  dplyr::select(GNIS_NA, WATERSH, BLK, WB_POLY_ID) |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(sampled_2024 = TRUE)

wb_list = wb_list |> 
  dplyr::left_join(wb_sampled_2024) |> 
  dplyr::mutate(sampled_2024 = tidyr::replace_na(sampled_2024, FALSE))

wb_list = wb_list |> 
  dplyr::mutate(tib = as.numeric(TotalInspections_kmeans_bin),
                iwbc = as.numeric(insp_from_wd_bc_wb_kmeans_bin),
                dfb = as.numeric(days_fished_kmeans_bin),
                wab = as.numeric(active_water_authorizations_kmeans_bin)) |> 
  dplyr::mutate(priority = ifelse(is.na(tib),0,tib) + ifelse(is.na(iwbc),0,iwbc) + ifelse(is.na(dfb),0,dfb) + ifelse(is.na(wab),0,wab)) |> 
  dplyr::select(-c(tib,iwbc,dfb,wab))


source("utils/chunk_add_FN_priorities_and_other_considerations.R", local = T)

# Snag the watershed codes based on names
subw = sf::read_sf(paste0(lan_root,"2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/WatershedGroups_lowres.shp"))
wb_list = wb_list |> 
  dplyr::left_join(
    subw |> sf::st_drop_geometry() |> dplyr::select(WATERSH = WATERSHED_,
                                                    WATERSH_NAME = WATERSHE_1)
  ) |> 
  dplyr::select(GNIS_NA,WATERSH,WATERSH_NAME,dplyr::everything())

wb_list = wb_list |> 
  dplyr::rename(boats_entering_BC = TotalInspections,
                boats_entering_BC_bin = TotalInspections_kmeans_bin,
                boats_inside_BC = insp_from_wd_bc_wb,
                boats_inside_BC_bin = insp_from_wd_bc_wb_kmeans_bin,
                days_fished_bin = days_fished_kmeans_bin,
                active_water_auths = active_water_authorizations,
                active_water_auths_bin = active_water_authorizations_kmeans_bin,
                WD_susceptible_spp = sus_spp,
                SARA = sara,
                known_fish_occs = known_fish
  ) 

wb_list<-st_transform(wb_list, 4326)

wb_list_SARA = wb_list |> 
  dplyr::filter(!is.na(SARA) | Flagged_by_FN_partners)

wb_list_NoSARA = wb_list |> 
  dplyr::filter(is.na(SARA))

# wb_list |> 
#   filter(duplicated(paste0(GNIS_NA,WATERSH,WATERSH_NAME)))
# 
# wb_list |> 
#   dplyr::filter(GNIS_NA == "Babine River",WATERSH == 6) |> View()

if(!file.exists("data/parks_prot_areas_bcdata_layer.rds")){
  parks = bcdc_query_geodata('terrestrial-protected-areas-representation-by-ecosection-parc-') |>
    collect()
  # Note: variable PROTECTED_AREA_TYPE has levels PROVINCIAL PARK (1038), NGO CONSERVANCY (603), etc.
  
  saveRDS(parks, file = "data/parks_prot_areas_bcdata_layer.rds")
} else {
  parks = readRDS("data/parks_prot_areas_bcdata_layer.rds")
}

# Read in Stephen Ban's feedback here:
park_priorities = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/2025/Whirling_Disease_Park_Prioritization.xlsx"))

park_priorities_s = park_priorities |> 
  dplyr::select(nearby_wb,priority) |> 
  dplyr::filter(!is.na(priority)) |> 
  tidyr::separate_wider_delim(cols = nearby_wb, delim = ', ', names = c("GNIS_NA","WATERSHED_NAME")) |> 
  dplyr::mutate(priority = stringr::str_remove(priority, " \\(.*")) |> 
  dplyr::mutate(priority = ifelse(priority == "L-M", "M", priority)) |> 
  dplyr::mutate(priority = dplyr::case_when(
    priority == "L" ~ "Low",
    priority == "M" ~ "Medium",
    priority == "H" ~ "High",
    priority == "VH" ~ "Very High",
  )) |> 
  dplyr::mutate(priority = factor(priority, levels = c("Low","Medium","High","Very High"))) |> 
  group_by(GNIS_NA, WATERSHED_NAME) |> 
  dplyr::slice_max(priority) |> 
  dplyr::ungroup() |> 
  dplyr::rename(parks_priority = priority) |> 
  dplyr::distinct()

park_priorities_s = park_priorities_s |> 
  dplyr::left_join(
    subw |> sf::st_drop_geometry() |> dplyr::select(WATERSH = WATERSHED_,
                                                    WATERSHED_NAME = WATERSHE_1)
  )

wb_list = wb_list |> 
  dplyr::left_join(park_priorities_s)

wb_list_reduced <- wb_list |> 
  arrange(desc(priority)) |> 
  dplyr::select(
    GNIS_NA, WATERSH, WATERSH_NAME, priority, Flagged_by_FN_partners,
    boats_entering_BC_bin, boats_inside_BC_bin, days_fished_bin,
    active_water_auths_bin, WD_susceptible_spp, SARA, opportunistic_sampling,sampled_2024,parks_priority
  ) #|> 
# dplyr::distinct() |> 
# dplyr::distinct(GNIS_NA, .keep_all = TRUE)

indig_wbs <- wb_list_reduced |> 
  filter(Flagged_by_FN_partners == TRUE)

opp_sample_wbs = wb_list_reduced |> 
  filter(!is.na(opportunistic_sampling))

# Where is there overlap between indig_wbs and opp_sample_wbs
# join them based on gnis_na and watershed
opp_sample_wbs_nogeo <- opp_sample_wbs |> st_drop_geometry()
indig_wbs_nogeo <- indig_wbs |> st_drop_geometry()
# Identify only those opp_sample_wbs rows that aren't already in indig_wbs by GNIS_NA + WATERSH
opp_diff <- opp_sample_wbs |> 
  semi_join(opp_sample_wbs_nogeo |> 
              anti_join(indig_wbs_nogeo, by = c("GNIS_NA", "WATERSH")),
            by = colnames(opp_sample_wbs_nogeo))

# Combine indig_wbs with non-overlapping opp_sample_wbs rows
nation_wbs <- bind_rows(indig_wbs, opp_diff)

remaining_wbs <- wb_list_reduced |> 
  filter(is.na(Flagged_by_FN_partners)) |>
  filter(is.na(opportunistic_sampling)) |> 
  arrange(desc(priority)) |> 
  slice_head(n = 100 - (nrow(nation_wbs)))

wb_list_final <- bind_rows(nation_wbs, 
                           remaining_wbs) |> 
  arrange(desc(Flagged_by_FN_partners),desc(opportunistic_sampling), desc(priority))

# # For the leaflet map
# wb_no_sara_over_5_in_frascol = wb_list_final |> 
#   dplyr::filter(is.na(SARA)) |> 
#   sf::st_filter(frascol) |> 
#   dplyr::filter(!duplicated(GNIS_NA))
# 
# wb_sara_over_5_in_frascol = wb_list_final |>
#   dplyr::filter(!is.na(SARA)) |> 
#   sf::st_filter(frascol) |> 
#   dplyr::filter(!duplicated(GNIS_NA))
# 
# wb_sara_in_frascol_idigenous = wb_list_final |> 
#   dplyr::filter(Flagged_by_FN_partners == TRUE)
# wb_sara_in_frascol_nonindigenous = wb_list_final |>
#   dplyr::filter(Flagged_by_FN_partners == FALSE)
# 
# wb_list_final_w_FN = wb_list_final |> 
#   dplyr::filter(Flagged_by_FN_partners | !is.na(opportunistic_sampling))

### now we ggplot the locations
# Custom BC bounding box
bc_station_view <- st_bbox(
  tibble(lon = c(-135, -112),
         lat = c(48, 58)) |> 
    st_as_sf(coords = c("lon","lat"), crs = 4326)
)

# Minimal basemap (white/light)
bc_stations_basemap_white <- basemap_terra(
  ext = bc_station_view, 
  map_service = 'carto', 
  map_type = 'light'
)

## factor the priority in wb_list_final for colour in ggplot
wb_list_final_plot = wb_list_final |> 
  #filter(priority > 3) |> 
  filter(priority >=1) |> 
  dplyr::mutate(
    priority_factor = factor(
      priority,
      levels = 0:14,
      ordered = TRUE
    )
  )


# Plot with ggplot2
p1 = ggplot() +
  # Basemap
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  
  # Stations
  geom_sf(data = wb_list_final_plot,
          aes(geometry = geometry, fill = priority_factor, color = priority_factor))+
  
  # Minimal theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )+
   scale_fill_viridis_d(
    option = "mako",
    name = "Ranking",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_colour_viridis_d(
    option = "mako",
    name = "Ranking",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  )+
  ggtitle("Final Ranking of Waterbodies")

p1
ggsave("./images/final_rank_waterbodies.png", p1, width = 7.6, height = 6.4, dpi = 300)


## factor the priority in wb_list_final for colour in ggplot
wb_list_final_plot = wb_list_final |> 
  mutate(
    boats_entering_BC_bin = str_trim(boats_entering_BC_bin),
    boats_entering_BC_bin = str_remove(boats_entering_BC_bin, "\\s*\\(.*")
  ) |> 
  filter(boats_entering_BC_bin >= 1) |> 
  mutate(
    boats_entering_BC_bin_factor = factor(
      boats_entering_BC_bin,
      levels = 0:14,
      ordered = TRUE
    )
  )


# Plot with ggplot2
p2 = ggplot() +
  # Basemap
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  
  # Stations
  geom_sf(data = wb_list_final_plot,
          aes(geometry = geometry, fill = boats_entering_BC_bin_factor, color = boats_entering_BC_bin_factor))+
  
  # Minimal theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )+
  scale_fill_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_colour_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  )+
  ggtitle("Binned Values - Boats Entering BC")

p2
ggsave("./images/rank_boats_entering_BC.png", p2, width = 7.6, height = 6.4, dpi = 300)


## factor the priority in wb_list_final for colour in ggplot
wb_list_final_plot = wb_list_final |> 
  mutate(
    boats_inside_BC_bin = str_trim(boats_inside_BC_bin),
    boats_inside_BC_bin = str_remove(boats_inside_BC_bin, "\\s*\\(.*")
  ) |> 
  filter(boats_inside_BC_bin >= 1) |> 
  mutate(
    boats_inside_BC_bin_factor = factor(
      boats_inside_BC_bin,
      levels = 0:14,
      ordered = TRUE
    )
  )


# Plot with ggplot2
p3 = ggplot() +
  # Basemap
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  
  # Stations
  geom_sf(data = wb_list_final_plot,
          aes(geometry = geometry, fill = boats_inside_BC_bin_factor, color = boats_inside_BC_bin_factor))+
  
  # Minimal theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )+
  scale_fill_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_colour_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  )+
  ggtitle("Binned Values - Boats Inside BC")

p3
ggsave("./images/rank_boats_inside_BC.png", p3, width = 7.6, height = 6.4, dpi = 300)




## factor the priority in wb_list_final for colour in ggplot
wb_list_final_plot = wb_list_final |> 
  mutate(
    days_fished_bin = str_trim(days_fished_bin),
    days_fished_bin = str_remove(days_fished_bin, "\\s*\\(.*")
  ) |> 
  filter(days_fished_bin >= 1) |> 
  mutate(
    days_fished_bin_factor = factor(
      days_fished_bin,
      levels = 0:14,
      ordered = TRUE
    )
  )


# Plot with ggplot2
p4 = ggplot() +
  # Basemap
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  
  # Stations
  geom_sf(data = wb_list_final_plot,
          aes(geometry = geometry, fill = days_fished_bin_factor, color = days_fished_bin_factor))+
  
  # Minimal theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )+
  scale_fill_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_colour_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  )+
  ggtitle("Binned Values - Days Fished")

p4
ggsave("./images/rank_days_fished_BC.png", p4, width = 7.6, height = 6.4, dpi = 300)






## factor the priority in wb_list_final for colour in ggplot
wb_list_final_plot = wb_list_final |> 
  mutate(
    active_water_auths_bin = str_trim(active_water_auths_bin),
    active_water_auths_bin = str_remove(active_water_auths_bin, "\\s*\\(.*")
  ) |> 
  filter(active_water_auths_bin >= 1) |> 
  mutate(
    active_water_auths_bin_factor = factor(
      active_water_auths_bin,
      levels = 0:14,
      ordered = TRUE
    )
  )


# Plot with ggplot2
p5 = ggplot() +
  # Basemap
  tidyterra::geom_spatraster_rgb(data = bc_stations_basemap_white) +
  
  # Stations
  geom_sf(data = wb_list_final_plot,
          aes(geometry = geometry, fill = active_water_auths_bin_factor, color = active_water_auths_bin_factor))+
  
  # Minimal theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )+
  scale_fill_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_colour_viridis_d(
    option = "cividis",
    name = "Binned Values",
    direction = -1,
    guide = guide_legend(reverse = TRUE)
  )+
  ggtitle("Binned Values - Water Authorizations")

p5
ggsave("./images/rank_water_auths_BC.png", p5, width = 7.6, height = 6.4, dpi = 300)
