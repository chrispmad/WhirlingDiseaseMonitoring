
### Previous WD Sampling and 2024 Identified Unsampled Sites

# WD sampling 2017 - 2023
wd_past_sampling_fish = read_excel("data/WD Sampling Data 2017_2023.xlsx", sheet = "Fish Data")
wd_past_sampling_worms = read_excel("data/WD Sampling Data 2017_2023.xlsx", sheet = "Worm Data")

wd_past_sampling_fish_sf = wd_past_sampling_fish |>
  sf::st_as_sf(coords = c("Easting","Northing"), crs = 32611) |>
  sf::st_transform(4326) |>
  dplyr::mutate(sample_type = "Fish Samples")

wd_past_sampling_worms_sf = wd_past_sampling_worms |>
  sf::st_as_sf(coords = c("Easting","Northing"), crs = 32611) |>
  sf::st_transform(4326) |>
  dplyr::mutate(sample_type = "Tubifex")

wd_past_sampling = dplyr::bind_rows(
  wd_past_sampling_fish_sf,
  wd_past_sampling_worms_sf
) |>
  sf::st_transform(3005)

# # Code from ChatGPT that works but doesn't fix the problem
# st_endpoint <- function(line) {
#   coords <- st_coordinates(line)
#   st_point(coords[nrow(coords), ])
# }
#
# nearest_wb_index = sf::st_nearest_feature(wd_past_sampling, wb_list)
# nearest_wb_geom = st_nearest_points(wd_past_sampling, wb_list[nearest_wb_index, ], pairwise = TRUE)
# wd_past_sampling_snapped <- wd_past_sampling
# wd_past_sampling_snapped$geometry <- st_geometry(nearest_wb_geom) %>% st_cast("LINESTRING") %>% lapply(function(line) st_endpoint(line)) %>% st_sfc(crs = st_crs(wd_past_sampling))

# ggplot() + geom_sf(data = wd_past_sampling_snapped, col = 'red') +
#   geom_sf(data =wd_past_sampling, col = 'black')

# wd_past_sampling = wd_past_sampling_snapped

# # Summarise past WD sampling by wb.
# wd_past_sampling_by_wb_list = wd_past_sampling |>
#   sf::st_join(wb_list |> dplyr::select(GNIS_NA, WATERSH))
#
# # Which ones have doubled up?
# wd_past_sampling_by_wb_list |>
#   dplyr::filter(duplicated(paste0(Site,Date))) |>
#   View()
# wb_past_sampling_by_wb_list_summarised = wb_past_sampling_by_wb_list |>
#   dplyr::select(GNIS_NA, WATERSH, previous_sampling_result = Results) |>
#   sf::st_drop_geometry() |>
#   dplyr::mutate(previous_WD_sampling = TRUE) |>
#   dplyr::filter(!is.na(previous_sampling_result)) |>
#   dplyr::distinct()
#
# wb_list = wb_list |>
#   dplyr::left_join(wb_past_sampling_by_wb_list_summarised) |>
#   dplyr::mutate(previous_WD_sampling = ifelse(is.na(previous_WD_sampling),FALSE,previous_WD_sampling))

# Also pull in 2024 WD sampling results
lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

new_potential_dat_file = read_excel(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"), sheet = "Fish and eDNA")
local_data_file = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx')

if(identical(names(new_potential_dat_file),names(local_data_file)) & nrow(new_potential_dat_file) == nrow(local_data_file)){
  # print("New data file has identical column names and number of rows. Copying locally...")
  file.copy(from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"),
            to = paste0('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx'),
            overwrite = T)
}

wd_results = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx', sheet = "Fish and eDNA") |>
  purrr::set_names(snakecase::to_snake_case)

# Split 2024 results into sites with results and sites that were INTENDED to be sampled
# but weren't in the end.
wd_results_2024_sf = wd_results |>
  dplyr::filter(!is.na(lat)) |>
  sf::st_as_sf(coords = c("long","lat"),
               crs = 4326) |>
  dplyr::select(Reach = reach, Site = sample_site_name,
                fish_sampling = fish_sampling_results_q_pcr_mc_detected,
                e_dna_results_mc, e_dna_results_tubifex) |>
  dplyr::filter(!is.na(fish_sampling) |
                  !is.na(e_dna_results_mc) |
                  !is.na(e_dna_results_tubifex))

wd_not_sampled_2024_sf = wd_results |>
  dplyr::filter(!is.na(lat)) |>
  sf::st_as_sf(coords = c("long","lat"),
               crs = 4326) |>
  #dplyr::filter(include_in_map_y_n == "Y") |>
  dplyr::mutate(on_list_but_not_sampled_2024 = sampled_in_2024_y_n == "N") |>
  dplyr::filter(on_list_but_not_sampled_2024) |>
  dplyr::select(Reach = reach, Site = sample_site_name, on_list_but_not_sampled_2024)

# Also, read in fras/col and do overlay.
fras = sf::read_sf("W:/CMadsen/shared_data_sets/Fraser_River_Big_Watershed.shp")
col = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp")

fras_b = fras |> dplyr::summarise() |> dplyr::mutate(watershed = 'Fraser River')
col_b = col |> dplyr::summarise() |> dplyr::mutate(watershed = 'Columbia River')

frascol = dplyr::bind_rows(fras_b, col_b) |>
  sf::st_transform(4326)

# i. 2017-2024 past WD sampling results.
past_wd_monitoring_tbl = wd_past_sampling |>
  dplyr::mutate(WD_sampled_year = lubridate::year(Date)) |>
  dplyr::select(WD_sampled_year) |>
  sf::st_zm() |>
  sf::st_transform(4326)

past_wd_monitoring_2024_tbl = wd_results_2024_sf |>
  dplyr::mutate(WD_sampled_year = 2024) |>
  dplyr::select(WD_sampled_year)

past_wd_monitoring_tbl = dplyr::bind_rows(
  past_wd_monitoring_tbl,
  past_wd_monitoring_2024_tbl
)

# past_wd_monitoring_tbl_by_wb_list = past_wd_monitoring_tbl |>
#   sf::st_join(wb_list |> dplyr::select(GNIS_NA, WATERSH)) |>
#   dplyr::filter(!is.na(GNIS_NA)) |>
#   sf::st_drop_geometry() |>
#   dplyr::group_by(GNIS_NA, WATERSH) |>
#   dplyr::reframe(WD_sampled_year = paste0(unique(WD_sampled_year), collapse = ', '))
#
# wb_list = wb_list |>
#   dplyr::left_join(past_wd_monitoring_tbl_by_wb_list)

# ii. Is waterbody within Fraser or Columbia rivershed?
wb_list = sf::st_transform(wb_list, 4326)

wb_list = wb_list |>
  sf::st_join(frascol) |>
  dplyr::mutate(watershed = ifelse(is.na(watershed),"Other Watershed",watershed))
