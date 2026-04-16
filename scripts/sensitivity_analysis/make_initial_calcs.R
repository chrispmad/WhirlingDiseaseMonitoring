library(tidyverse)
library(readxl)
library(BAMMtools)
library(terra)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(tidyterra)
library(sf)
library(bcdata)

requery_occs = F
k_breaks = 5 # number of breaks for k-means clustering

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

#k-means clustering functions
source("utils/bin_to_kmeans_func.R")
source("utils/getKMeansBreaks_func.R")

## GGplot label convenience function
source("utils/bin_info_labs_ggplot_preset.R")

# Inspections for watercraft entering BC
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

source("utils/chunk_kfo_species_lookup.R", local = T)

source("utils/chunk_watersheds_with_WD_in.R", local = T)

source("utils/chunk_connectivity_of_connected_watersheds_outset.R", local = T)

source('utils/chunk_water_authorizations.R', local = T)

### Downstream infection section


adj_small_watersheds = small_watersheds |> 
  sf::st_filter(
    small_watersheds_infected |> 
      sf::st_buffer(dist = 50)
  )

rivers_in_infectable_adj_watersheds = bcdc_query_geodata('freshwater-atlas-rivers') |> 
  filter(INTERSECTS(adj_small_watersheds)) |> 
  collect() |> 
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME_1,FWA_WATERSHED_CODE) |> 
  dplyr::summarise()

lakes_in_infectable_adj_watersheds = bcdc_query_geodata('freshwater-atlas-lakes') |> 
  filter(INTERSECTS(adj_small_watersheds)) |> 
  collect() |> 
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME_1,FWA_WATERSHED_CODE) |> 
  dplyr::summarise()

rivers_in_infectable_adj_watersheds = rivers_in_infectable_adj_watersheds |> 
  dplyr::ungroup() |> 
  sf::st_filter(adj_small_watersheds)

lakes_in_infectable_adj_watersheds = lakes_in_infectable_adj_watersheds |> 
  dplyr::ungroup() |> 
  sf::st_filter(adj_small_watersheds)

pscis_in_area = bcdc_query_geodata('pscis-assessments') |> 
  filter(INTERSECTS(adj_small_watersheds)) |> 
  collect()

# Filter potential fish passage barriers to within 50m of rivers.
pscis_near_rivers = pscis_in_area |> 
  sf::st_filter(
    sf::st_buffer(infected_rivers, dist = 50)
  )

dams = bcdc_query_geodata('bc-dams') |> 
  filter(INTERSECTS(adj_small_watersheds)) |> 
  collect()

# Clip rivers with PSCIS , buffered by like 100 meters?
pscis_near_rivers_buff = sf::st_buffer(pscis_near_rivers, dist = 100)

pscis_in_area = pscis_in_area |> sf::st_filter(adj_small_watersheds)
pscis_near_rivers = pscis_near_rivers |> sf::st_filter(adj_small_watersheds)
dams = dams |> sf::st_filter(adj_small_watersheds)


source('utils/chunk_downstream_infectable_waterbodies.R', local = T)
###################################################



tubifex_results = edna_results |>
  dplyr::filter(stringr::str_detect(e_dna_results_tubifex, 'Detected'))

tubifex_results_sf = sf::st_as_sf(tubifex_results, coords = c('long','lat'), crs = 4326)

ws_w_edna_tubifex = ws |>
  sf::st_filter(
    tubifex_results_sf |>
      sf::st_transform(3005) |>
      dplyr::bind_rows(emerald_lake_centroid)
  )

wb_list = wb_list |>
  dplyr::mutate(tubifex_edna_positive_in_watershed = WATERSH %in% ws_w_edna_tubifex$WATERSHED_)

tub_contributions<-readRDS("output/contributions_tubifex_maxent.rds")


## mines
if(!file.exists("data/mining_bcdata_layer.rds")){
  mining = bcl[str_detect(bcl,"mine-")]
  
  mines = bcdc_query_geodata(mining[1]) |>
    collect()
  
  saveRDS(mines, file = "data/mining_bcdata_layer.rds")
} else {
  mines = readRDS("data/mining_bcdata_layer.rds")
}
mines_nearby = mines |> 
  sf::st_buffer(dist = 5000) |> 
  sf::st_join(wb_list |> dplyr::select(GNIS_NA, WATERSH)) |> 
  sf::st_drop_geometry() |> 
  dplyr::count(GNIS_NA, WATERSH, name = "mines_nearby")

wb_list = wb_list |> 
  dplyr::left_join(mines_nearby)

## rec sites
if(!file.exists("data/rec_sites_bcdata_layer.rds")){
  recsites = bcl[str_detect(bcl,'recreation-site')]
  
  reccies = bcdc_query_geodata(recsites[12]) |>
    collect() |>
    dplyr::filter(PROJECT_TYPE == 'SIT - Recreation Site')
  
  saveRDS(reccies, file = "data/rec_sites_bcdata_layer.rds")
} else {
  reccies = readRDS("data/rec_sites_bcdata_layer.rds")
}


reccies_nearby = reccies |> 
  sf::st_buffer(dist = 500) |> 
  sf::st_join(wb_list |> dplyr::select(GNIS_NA, WATERSH)) |> 
  sf::st_drop_geometry() |> 
  dplyr::count(GNIS_NA, WATERSH, name = "rec_sites_nearby")

wb_list = wb_list |> 
  dplyr::left_join(reccies_nearby)



## parks
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

# Snag the watershed codes based on names
subw = sf::read_sf(paste0(lan_root,"2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/WatershedGroups_lowres.shp"))

park_priorities_s = park_priorities_s |> 
  dplyr::left_join(
    subw |> sf::st_drop_geometry() |> dplyr::select(WATERSH = WATERSHED_,
                                                    WATERSHED_NAME = WATERSHE_1)
  )

parks_nearby = parks |> 
  sf::st_buffer(dist = 500) |> 
  sf::st_join(wb_list |> dplyr::select(GNIS_NA, WATERSH)) |> 
  sf::st_drop_geometry() |> 
  dplyr::count(GNIS_NA, WATERSH, name = "parks_prot_areas_nearby")

wb_list = wb_list |> 
  dplyr::left_join(parks_nearby)

# Join on Stephen Ban's feedback regarding park priority levels
wb_list = wb_list |> 
  dplyr::left_join(park_priorities_s)

source("utils/chunk_water_quality_plots.R", local = TRUE)

source("utils/chunk_susceptible_species_overlay.R", local = T)

source("utils/chunk_stocked_species.R", local = T)

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


##------------------------------------------------------------------------------
#
## This is the point where some of the variables are being added
#
##------------------------------------------------------------------------------

# things have already been binned, but we also have raw values



wb_list = wb_list |> 
  dplyr::mutate(tib = as.numeric(TotalInspections_kmeans_bin),
                iwbc = as.numeric(insp_from_wd_bc_wb_kmeans_bin),
                dfb = as.numeric(days_fished_kmeans_bin),
                wab = as.numeric(active_water_authorizations_kmeans_bin)) |> 
  dplyr::mutate(priority = ifelse(is.na(tib),0,tib) + ifelse(is.na(iwbc),0,iwbc) + ifelse(is.na(dfb),0,dfb) + ifelse(is.na(wab),0,wab)) |> 
  dplyr::select(-c(tib,iwbc,dfb,wab))



#### First Nations inclusions
source("utils/chunk_add_FN_priorities_and_other_considerations.R", local = T)

source("utils/chunk_previously_sampled_and_unsampled_sites.R", local = T)

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
  arrange(desc(priority)) 
### Comment out the clipped version of the file. We wan't all waterbodies for testing
#|#> 
#slice_head(n = 100 - (nrow(nation_wbs)))

wb_list_final <- bind_rows(nation_wbs, 
                           remaining_wbs) |> 
  arrange(desc(Flagged_by_FN_partners),desc(opportunistic_sampling), desc(priority))

# For the leaflet map
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

make_final_column_names = function(dat){
  dat = dat |> 
    sf::st_sf() |> 
    dplyr::rename(`Waterbody Name` = GNIS_NA, 
                  `Watershed Code` = WATERSH,
                  `Watershed Name` = WATERSH_NAME,
                  `Identified by FN partners` = Flagged_by_FN_partners,
                  `Potential WD Sampling Overlap with Existing Projects` = opportunistic_sampling,
                  `Model Ranking` = priority
    )
  if("BLK" %in% names(dat)){
    dat = dat |> dplyr::select(-c(BLK,WB_POLY_ID,watershed))
  }
  dat |> 
    dplyr::mutate(dplyr::across(dplyr::ends_with("_bin"), \(x) stringr::str_remove_all(x," \\(.*"))) |> 
    dplyr::rename(`Boats entering BC (bins 1 - 5)` = boats_entering_BC_bin,
                  `Boats inside BC (bins 1 - 5)` = boats_inside_BC_bin,
                  `Days fished (bins 1 - 5)` = days_fished_bin,
                  `Water authorizations (bins 1 - 5)` = active_water_auths_bin,
    ) |>
    dplyr::mutate(`Identified by FN partners` = ifelse(!`Identified by FN partners`,NA,`Identified by FN partners`)) |> 
    dplyr::select(`Waterbody Name`, `Watershed Code`, `Watershed Name`,
                  `Model Ranking`, `Identified by FN partners`, dplyr::everything())
}

# To each cut of the data, apply the function that renames columns
wb_nice_names = make_final_column_names(wb_list)
wb_list_SARA_nice_names = make_final_column_names(wb_list_SARA)
wb_list_final_nice_names = make_final_column_names(wb_list_final)
wb_list_final_w_FN_nice_names = make_final_column_names(wb_list_final_w_FN)

# Final re-ordering of columns for final tabs.
wb_list_final_nice_names = wb_list_final_nice_names |> 
  dplyr::select(`Waterbody Name`, `Watershed Code`, `Watershed Name`,
                `Model Ranking`,sampled_2024, `Identified by FN partners`,
                `Potential WD Sampling Overlap with Existing Projects`,SARA,
                dplyr::everything()) |> 
  dplyr::rename(DRAFT_parks_priority = parks_priority)

# wb_list_final_w_FN_nice_names = wb_list_final_w_FN_nice_names |> 
#   dplyr::mutate(`Sample Site` = NA,
#                 `Site Latitude` = NA,
#                 `Site Longitude` = NA) |> 
#   dplyr::select(`Waterbody Name`, `Watershed Name`, `Sample Site`,
#                 `Site Latitude`, `Site Longitude`, `Watershed Code`, 
#                 `Model Ranking`, sampled_2024, `Identified by FN partners`,
#                 `Potential WD Sampling Overlap with Existing Projects`,SARA,
#                 dplyr::everything()) |> 
#   dplyr::rename(DRAFT_parks_priority = parks_priority)


write_rds(wb_list_final_nice_names, "./output/sara_model_full_output.RDS")
