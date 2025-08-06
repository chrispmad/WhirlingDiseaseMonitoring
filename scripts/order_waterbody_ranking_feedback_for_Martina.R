library(readxl)
library(tidyverse)

# This data file comes from a Microsoft Teams channel (specifically, the Whirling Disease one)
# I have manually downloaded that excel file and placed it in this R project's data folder.

onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

the_filepath = list.files(path = 'data', pattern = "Whirling_Disease_top 100.*",
                          full.names = T)

d = read_excel(path = the_filepath, sheet = "Top 100 priority list")

# Drop the 'BC Parks Priority Ranking' column.
d = d |>
  dplyr::select(-`BC Parks Priority Ranking`)

# Just keep waterbodies that have been flagged as a priority in columns F - L,
# or identified by FN partners in columns N and O.
d_f = d |>
  dplyr::filter(`Identified by FN partners` |
                  !is.na(`WLRS AEB Provincial Fisheries Priority Ranking (1-20)`) |
                  !is.na(`WLRS KBR Regional Fisheries Priority Ranking (1-20)`) |
                  !is.na(`WLRS Provincial Water Authorizations Priority Ranking (all regions, incl KBR) (1-20)`) |
                  !is.na(`WLRS KBR Water Authorizations Priority Ranking (1-20)`) |
                  !is.na(`MOTT Priority Ranking (1-20)`) |
                  !is.na(`BC Parks`)) |>
  # Deal with duplicate rows stemming from different BLKs earlier in the model.
  dplyr::group_by(`Waterbody Name`,`Watershed Name`) |>
  dplyr::arrange(dplyr::desc(`Model Priority Ranking`)) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

# Add in waterbodies from last year (currently: 2024) that were not sampled.
final_list_wbs = readRDS(paste0(onedrive_wd,"output_of_WD_monitoring_analysis_top_100_waterbodies.rds"))

# Get geometries for the waterbodies in this excel file list.
wd_results = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx', sheet = "Fish and eDNA") |>
  purrr::set_names(snakecase::to_snake_case) |>
  dplyr::filter(sampled_in_2024_y_n == "N") |>
  sf::st_as_sf(coords = c("long","lat"), crs = 4326)

# Do spatial match between 2024 WD sampling results and final list of wbs for 2025 sampling.
final_list_wbs = final_list_wbs |>
  sf::st_join(wd_results |> dplyr::select(sample_site_name,sampled_in_2024_y_n))

final_list_wbs = final_list_wbs |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(sampled_in_2024_y_n)) |>
  dplyr::select(`Waterbody Name`,`Watershed Name`,sampled_in_2024_y_n)

d_f = d_f |>
  dplyr::left_join(final_list_wbs)

# Add comments to flag
d_f = d_f |>
  dplyr::mutate(Comments = ifelse(sampled_in_2024_y_n == "N","2024 Earmarked for sampling but not sampled",Comments))

# Find which of the earmarked-for-sampling-2024-but-not-sampled waterbodies
# do NOT overlap with the final 100 wb list.
wd_results_new_for_list = wd_results |>
  dplyr::anti_join(
    final_list_wbs |>
      sf::st_drop_geometry() |>
      dplyr::filter(sampled_in_2024_y_n == "N") |>
      dplyr::select(sample_site_name = `Waterbody Name`)
  )

wd_results_new_for_list = wd_results_new_for_list |>
  sf::st_drop_geometry() |>
  dplyr::reframe(`Waterbody Name` = paste0(sample_site_name, ", (",region,")")) |>
  dplyr::mutate(Comments = "2024 Earmarked for sampling but not sampled")

d_f = d_f |>
  dplyr::bind_rows(wd_results_new_for_list)

# Double check - if 'sampled_2024' is true, set the comment to null. Also,
# drop the column sampled_in_2024_y_n to not be confusing.
d_f = d_f |>
  dplyr::mutate(sampled_2024 = tidyr::replace_na(sampled_2024, FALSE)) |>
  dplyr::mutate(Comments = ifelse(sampled_2024 == TRUE, NA, Comments)) |>
  dplyr::select(-sampled_in_2024_y_n)

openxlsx::write.xlsx(x = d_f, file = "output/Whirling_Disease_top_flagged_waterbody_list.xlsx")
