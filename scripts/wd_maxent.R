
library(terra)
library(dbplyr)
library(DBI)
library(tidyverse)
library(bcinvadeR)
library(bcdata)
library(RColorBrewer)
library(leaflet)
library(leafpop)
library(readxl)
library(sf)
library(ENMeval)


base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"


lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"

# Data
# Copy excel results from LAN folder IF it has all the right columns. This is
# intended to save us from overwriting the local data with some garbage
# LAN file that has replaced our goldenboy data file.
new_potential_dat_file = read_excel(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"), sheet = "Fish and eDNA")

local_data_file = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx')

if(identical(names(new_potential_dat_file),names(local_data_file)) & nrow(new_potential_dat_file) == nrow(local_data_file)){
  print("New data file has identical column names and number of rows. Copying locally...")
  file.copy(from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/Whirling Disease/Monitoring/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx"),
            to = paste0('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx'),
            overwrite = T)
}

dat = read_excel('data/WD_sampling_results_fish_eDNA_used_for_making_maps_CMADSEN.xlsx', sheet = "Fish and eDNA")

dat = purrr::set_names(dat, snakecase::to_snake_case)

dat = dat |> filter(!is.na(lat) & !is.na(long))

# Split rows that have both eDNA and fish sampling into two rows each.
dat = dat |>
  tidyr::separate_longer_delim(cols = sampling_method, delim = " + ")

dat = sf::st_as_sf(dat, coords = c("long","lat"), crs = 4326)

# Typo corrections etc.
dat = dat |>
  dplyr::mutate(fish_sampling_results_q_pcr_mc_detected = ifelse(str_detect(fish_sampling_results_q_pcr_mc_detected,"Positive"),"Positive",fish_sampling_results_q_pcr_mc_detected)) |>
  dplyr::mutate(comments = ifelse(comments == '', NA, comments))

dat_max<-dat |>
  select(sample_site_name,e_dna_results_mc, fish_sampling_results_q_pcr_mc_detected, geometry) |>
  plyr::mutate(e_dna_results_mc = ifelse(str_detect(e_dna_results_mc,"Weak Detection"),"Positive","Negative")) |>
  filter(e_dna_results_mc == "Positive" | fish_sampling_results_q_pcr_mc_detected == "Positive") |>
  distinct()

anglerUse<-terra::rast(paste0(onedrive_wd,"CNF/DFO_angling_survey_days_fished_raster.tif"))
watercraft2<-sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries_all_years.gpkg")
road_distances<-terra::rast(paste0(onedrive_wd,"CNF/distance_to_numbered_highway_raster.tif"))

watercraft_rast<-
  watercraft2 |>
  sf::st_transform(crs = terra::crs(road_distances)) |>
  filter(!GNIS_NA %in% c("Pacific Ocean", "Dry Storage")) |>
  terra::vect() |>
  terra::rasterize(anglerUse, field = "TotalInspections", fun = "sum")
#repalce NA with 0
watercraft_rast[is.na(watercraft_rast)] <- 0


rast_brick<-c(
  anglerUse,
  watercraft_rast,
  road_distances)

bc<-bcmaps::bc_bound()
extentvect<- project(vect(bc),"EPSG:4326")
watercourses = terra::rast(paste0(onedrive_wd,"fwa_streams/stream_order_three_plus_2km_res.tif"))
watercourses<-terra::crop(watercourses, extentvect)
watercourses<-terra::mask(watercourses, extentvect)

pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(dat), n = 10000, extf = 0.9) |>
  as.data.frame()




    dat_max = dat_max |>
      dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                    y = sf::st_coordinates(geometry)[,2])

    for(raster_var in unique(names(rast_brick))){
      dat_max[[raster_var]] <- terra::extract(rast_brick[[raster_var]],
                                          dat_max[,c("x","y")], ID = FALSE)[[raster_var]]
    }




pres_xy<- dat_max |>
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2]) |>
  dplyr::select(x,y) |>
  sf::st_drop_geometry()

pseudo<- pseudoabsences |>
  dplyr::select(x,y)

me = ENMevaluate(occs = pres_xy,
                 envs = rast_brick,
                 bg = pseudo,
                 algorithm = 'maxent.jar',
                 partitions = 'block',
                 tune.args = list(fc = c("L", "Q", "LQ"),
                                  rm = c(1:5)))

top_model1 = me@results |>
  dplyr::mutate(auccbi = (cbi.train + auc.train) / 2) |>
  dplyr::arrange(dplyr::desc(auccbi)) |>
  dplyr::slice(1)

top_model = me@predictions[[top_model1$tune.args]]

maxent_html = me@models[[top_model1$tune.args]]@html



#jpeg(paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/response_curve_",snakecase::to_snake_case(the_group),".jpg"), width = 1200, height = 800)
response(me@models[[top_model1$tune.args]])
dev.off()
