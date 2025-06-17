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
library(rJava)

rJava::.jinit()

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()

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

carbon<-terra::rast(paste0(onedrive_wd,"/raster/Carbon_Dissolved_Organic_All_masked_krig.tif"))
conductivitity<-terra::rast(paste0(onedrive_wd,"/raster/Conductivity_All_masked_krig.tif"))
nitrates<-terra::rast(paste0(onedrive_wd,"/raster/Nitrogen_Total_All_masked_krig.tif"))
oxygen<-terra::rast(paste0(onedrive_wd,"/raster/Oxygen_Dissolved_All_masked_krig.tif"))
turbidity<-terra::rast(paste0(onedrive_wd,"/raster/Turbidity_All_masked_krig.tif"))
slope<-terra::rast(paste0(onedrive_wd,"/raster/slope_BC.tif"))

rast_brick<-c(carbon, conductivitity, nitrates, oxygen, turbidity, slope)
names(rast_brick) <- c("carbon", "conductivity", "nitrates", "oxygen", "turbidity", "slope")
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



## add in the negative
ggplot()+
  tidyterra::geom_spatraster(data=rast, aes(fill = tubifex_maxent)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "YlOrRd"), name = "Tubifex Maxent") +
  geom_sf(data = dat_max, fill = NA, color = "black") +
  geom_sf(data = summarise(columfras), fill = NA, color = "black")





eval.results(me) |> head()

evalplot.stats(e = me, stats = "or.10p", color = "fc", x.var = "rm")


browseURL(me@models[[top_model1$tune.args]]@html)


top5 <- me@results |>
  filter(!is.na(AICc)) |>
  mutate(auc_cbi_mix = (as.numeric(auc.train) + as.numeric(cbi.train)) / 2) |>
  arrange(desc(auc_cbi_mix)) |>
  slice_head(n = 5)

#write.table(top5, paste0(output_fn, "top_5_models.txt"), row.names = F)
topfc<-as.character(top5[1,]$fc)
toprm<-as.character(top5[1,]$rm)

opt.aicc<- eval.results(me) |>
  dplyr::filter(fc == topfc & rm == toprm)

# Find which model had the lowest AIC; we'll use this for now.
# opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)

var_importance = me@variable.importance[[opt.aicc$tune.args]]

predictions = terra::rast(eval.predictions(me)[[opt.aicc$tune.args]])

# eval_model<- eval.models(me)[[opt.aicc$tune.args]]

# eval_plot<-eval_model
# Pull out maxent's predictions for occurrence locations.

# Check out results - this dataframe could be simplified to just hone in
# on the particular metrics we are curious about!
maxent_results = me@results |>
  dplyr::filter(tune.args == opt.aicc$tune.args) |>
  tidyr::as_tibble() |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |>
  tidyr::pivot_longer(cols = dplyr::everything())

maxent_results.partitions = me@results.partitions |>
  dplyr::filter(tune.args == opt.aicc$tune.args) |>
  tidyr::as_tibble()

maxent_html = me@models[[opt.aicc$tune.args]]@html

single_model_metrics = me@models[[opt.aicc$tune.args]]@results[,1] |>
  as.matrix() |>
  as.data.frame()

single_model_metrics = single_model_metrics |>
  dplyr::mutate(metric = snakecase::to_snake_case(rownames(single_model_metrics))) |>
  dplyr::rename(value = V1) |>
  tidyr::as_tibble() |>
  dplyr::select(metric, value)



# Convert RasterStack to SpatRaster
preds_terra <- rast(me@predictions)  # terra::rast converts raster to SpatRaster

# Extract the right layer using the tune.args name
layer_name <- as.character(opt.aicc$tune.args)
suitability_raster <- preds_terra[[layer_name]]

# Set threshold
threshold_value <- opt.aicc$or.10p.avg

# Create binary raster (1 = habitat, 0 = not habitat)
habitat_binary <- suitability_raster >= threshold_value

# Now you can safely classify using terra
# (This step converts logical to 0/1, though not strictly necessary if >= already gave 0/1)
habitat_binary <- classify(habitat_binary, rcl = matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))

# Plot or export
plot(habitat_binary, main = paste("Habitat / Not Habitat -", layer_name))
writeRaster(habitat_binary, paste0("output/habitat_maxent_tubifex_binary_", layer_name, ".tif"), overwrite = TRUE)
