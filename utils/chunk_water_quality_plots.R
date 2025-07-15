
do_r = terra::rast(paste0(onedrive_wd,"raster/Oxygen_Dissolved_All_masked_krig.tif"))
doc_r = terra::rast(paste0(onedrive_wd,"raster/Carbon_Dissolved_Organic_All_masked_krig.tif"))
phos_r = terra::rast(paste0(onedrive_wd,"raster/Phosphorus_Total_Dissolved_All_masked_krig.tif"))
nitr_r = terra::rast(paste0(onedrive_wd,"raster/Nitrogen_Total_All_masked_krig.tif"))
temp_r = terra::rast(paste0(onedrive_wd,"raster/Temperature_All_masked_krig.tif"))
turb_r = terra::rast(paste0(onedrive_wd,"raster/Turbidity_All_masked_krig.tif"))

# Concatenate rasts into a spatraster with many layers
all_r = c(do_r, doc_r, phos_r, nitr_r, temp_r, turb_r)

# Name the layers
names(all_r) = c("DO","D_Carbon","Phos","Nitr","Temp","Turb")

# plot, splitting by layer
plot_list = 1:length(names(all_r)) |>
  purrr::map(~ {
    ggplot() +
      tidyterra::geom_spatraster(data = all_r[[.x]]) +
      scale_fill_viridis_c() +
      labs(fill = unique(names(all_r[[.x]]))) +
      ggthemes::theme_map()
  })

print(patchwork::wrap_plots(plot_list))

bc_crs <- "EPSG:3005"
all_r <- project(all_r, bc_crs, method = "near")
wb_vect <- vect(wb_list)
# factor_levels <- levels(tubifex_maxent)[[1]]  # Only one layer
all_r_extracted <- terra::extract(all_r, wb_vect)

all_r_summarised <- all_r_extracted |>
  group_by(ID) |>
  reframe(across(everything(), \(x) round(median(x,na.rm=T),3)))

wb_list = wb_list |>
  cbind(all_r_summarised)
