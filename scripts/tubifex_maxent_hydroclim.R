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
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
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
  select(e_dna_results_tubifex, geometry) |> 
  dplyr::filter(e_dna_results_tubifex == "Detected") |> 
  distinct()

# carbon<-terra::rast(paste0(onedrive_wd,"/raster/Carbon_Dissolved_Organic_All_masked_krig.tif"))
# conductivitity<-terra::rast(paste0(onedrive_wd,"/raster/Conductivity_All_masked_krig.tif"))
# nitrates<-terra::rast(paste0(onedrive_wd,"/raster/Nitrogen_Total_All_masked_krig.tif"))
# oxygen<-terra::rast(paste0(onedrive_wd,"/raster/Oxygen_Dissolved_All_masked_krig.tif"))
# turbidity<-terra::rast(paste0(onedrive_wd,"/raster/Turbidity_All_masked_krig.tif"))
# slope<-terra::rast(paste0(onedrive_wd,"/raster/slope_BC.tif"))
# 
# rast_brick<-c(carbon, conductivitity, nitrates, oxygen, turbidity, slope)
# names(rast_brick) <- c("carbon", "conductivity", "nitrates", "oxygen", "turbidity", "slope")

#read all raster files in the directory
raster_files <- list.files(path = paste0(onedrive_wd,"/raster/hydroclim"), pattern = "\\.tif$", full.names = TRUE)
#read in all raster files
rast_brick <- terra::rast(raster_files)
layer_names <- tools::file_path_sans_ext(basename(raster_files))
names(rast_brick) <- layer_names
average_indices <- grep("Average", layer_names, ignore.case = TRUE)

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


## add in the column fras - clip the raster down - just the perimiter?
colum<-read_sf(paste0(onedrive_wd, "/CNF/columbia_watershed_priority_area.gpkg")) |> 
  sf::st_transform(4326)
fras<-read_sf(paste0(onedrive_wd, "/CNF/fraser_watershed_priority_area.gpkg")) |> 
  sf::st_transform(4326)

columfras<-bind_rows(colum, fras)
terra::plot(top_model)
terra::writeRaster(top_model, paste0("data/tubifex_maxent.tif"), overwrite = T)
rast<-terra::rast(paste0("data/tubifex_maxent.tif"))




ggplot()+
  tidyterra::geom_spatraster(data=rast, aes(fill = tubifex_maxent)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "YlOrRd"), name = "Tubifex Maxent") +
  geom_sf(data = dat_max, fill = NA, color = "black") +
  geom_sf(data = summarise(columfras), fill = NA, color = "black")





eval.results(me) |> head()

evalplot.stats(e = me, stats = "or.10p", color = "fc", x.var = "rm")


browseURL(me@models[[top_model1$tune.args]]@html)


## switch to presence absence