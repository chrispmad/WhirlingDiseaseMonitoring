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
#set.seed(912)

rJava::.jinit()

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
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
nitrates<-terra::rast(paste0(onedrive_wd,"/raster/Nitrate_(NO3)_Dissolved_All_masked_krig.tif"))
oxygen<-terra::rast(paste0(onedrive_wd,"/raster/Dissolved_Oxygen-Field_All_masked_krig.tif"))
turbidity<-terra::rast(paste0(onedrive_wd,"/raster/Turbidity_All_masked_krig.tif"))
slope<-terra::rast(paste0(onedrive_wd,"/raster/slope_BC.tif"))
phosphorus<-terra::rast(paste0(onedrive_wd,"/raster/Phosphorus_All_masked_krig.tif"))
water_temp<-terra::rast(paste0(onedrive_wd,"/raster/Temperature_All_masked_krig.tif"))



rast_brick<-c(carbon, conductivitity, nitrates, oxygen, turbidity, slope, phosphorus, water_temp)
names(rast_brick) <- c("carbon", "conductivity", "nitrates", "oxygen", "turbidity", "slope", "phosphorus", "water_temperature")
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

contributions = single_model_metrics |>
  dplyr::filter(str_detect(metric, "contribution")) |>
  dplyr::mutate(variable = str_remove(metric, "contribution_")) |>
  dplyr::select(variable, value) |>
  dplyr::mutate(value = as.numeric(value)) |>
  dplyr::arrange(dplyr::desc(value))

saveRDS(contributions, "output/contributions_tubifex_maxent.rds")

# Convert RasterStack to SpatRaster
preds_terra <- rast(me@predictions)  # terra::rast converts raster to SpatRaster

# Extract the right layer using the tune.args name
layer_name <- as.character(opt.aicc$tune.args)
suitability_raster <- preds_terra[[layer_name]]

crs(suitability_raster) <- "EPSG:4326"

# plot each of the parameters and their contributions to the maxent model
best_model <- me@models[[opt.aicc$tune.args]]

# Get the min, max, and mean of each raster layer
env_stats <- list()
for (var_name in names(rast_brick)) {
  env_stats[[var_name]]$min <- terra::minmax(rast_brick[[var_name]])[1]
  env_stats[[var_name]]$max <- terra::minmax(rast_brick[[var_name]])[2]
  # For mean, it's safer to sample or calculate from non-NA cells
  # For simplicity, we'll use a sample of cells to get means
  sampled_values <- terra::values(rast_brick[[var_name]])
  env_stats[[var_name]]$mean <- mean(sampled_values[!is.na(sampled_values)])
}

# 2. Create dummy data for plotting response curves
# This will be a single data frame that we will then split or filter for plotting

n_points <- 100 # Number of points to sample across the variable range

# Create a template data frame with all variables at their mean
template_df <- data.frame(
  carbon = env_stats$carbon$mean,
  conductivity = env_stats$conductivity$mean,
  nitrates = env_stats$nitrates$mean,
  oxygen = env_stats$oxygen$mean,
  turbidity = env_stats$turbidity$mean,
  slope = env_stats$slope$mean,
  phosphorus = env_stats$phosphorus$mean,
  water_temperature = env_stats$water_temperature$mean
)

# Initialize an empty list to store data for each variable's response curve
plot_data_list <- list()

# Loop through each variable to create its specific response data
for (var_to_vary in names(rast_brick)) {
  # Generate a sequence of values for the current variable
  vary_sequence <- seq(env_stats[[var_to_vary]]$min, env_stats[[var_to_vary]]$max, length.out = n_points)

  # Create a data frame for prediction, with the current variable varying
  # and all other variables held at their mean
  temp_pred_df <- template_df[rep(1, n_points), ] # Replicate the template row
  temp_pred_df[[var_to_vary]] <- vary_sequence

  # Predict suitability using the best_model
  # Ensure the column order matches the training data (names(rast_brick))
  pred_result <- predict(best_model, temp_pred_df[, names(rast_brick)], type = "cloglog")

  # Store the data for plotting
  plot_data_list[[var_to_vary]] <- data.frame(
    variable = var_to_vary,
    x_value = vary_sequence,
    suitability = pred_result
  )
}

# Combine all the individual variable data frames into one for easier plotting
all_plot_data <- bind_rows(plot_data_list)


response_curves_plot <- ggplot(all_plot_data, aes(x = x_value, y = suitability)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) + # scales="free_x" allows x-axis to vary per plot
  labs(
    title = "Maxent Model Response Curves",
    x = "Variable Value",
    y = "Habitat Suitability (cloglog)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12), # Variable names in facets
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

print(response_curves_plot)

ggsave("output/response_curves_tubifex_maxent.png", response_curves_plot, width = 12, height = 8, dpi = 300)


##############################################################################

browseURL(maxent_html)

writeRaster(suitability_raster, paste0("output/habitat_maxent_tubifex_", layer_name, ".tif"), overwrite = TRUE)


# Set threshold
threshold_value <- opt.aicc$or.10p.avg

# Create binary raster (1 = habitat, 0 = not habitat)
habitat_binary <- suitability_raster >= threshold_value

# Now you can safely classify using terra
# (This step converts logical to 0/1, though not strictly necessary if >= already gave 0/1)
habitat_binary <- classify(habitat_binary, rcl = matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))

# Plot or export
plot(habitat_binary, main = paste("Habitat / Not Habitat -", layer_name))
crs(habitat_binary) <- "EPSG:4326"
writeRaster(habitat_binary, paste0("output/habitat_maxent_tubifex_binary_", layer_name, ".tif"), overwrite = TRUE)
