library(tidyverse)
library(sf)
library(terra)
library(tidyterra)


landbased<-read.csv("data/aquaculture licensing/licence-permis-landbased-terre-rpt-pac-dfo-mpo-aquaculture-eng.csv")

# marine<-read.csv("data/aquaculture licensing/license-permis-mer-mar-rpt-pac-dfo-mpo-aquaculture-eng.csv")

enchancements<-read.csv("data/aquaculture licensing/licence-permis-enhancement-renforcement-rpt-pac-dfo-mpo-aquaculture-eng.csv")

landbased_salmonids<-landbased |> 
  filter(grepl("Salmon|trout|whitefish", Licensed.Species, ignore.case = TRUE))

# marine_salmonids<-marine |>
#   filter(grepl("Salmon|trout|whitefish", Licensed.Species, ignore.case = TRUE))

enchancements_salmonids<-enchancements |>
  filter(grepl("Salmon|trout|whitefish", Licensed.Species, ignore.case = TRUE))

# Convert to sf objects
landbased_sf <- st_as_sf(landbased_salmonids, coords = c("Longitude", "Latitude"), crs = 4326)
landbased_sf <- landbased_sf |> 
  select(Facility.Reference.Number, Licence.Number, Expiry.Date, 
         Licence.Holder, Site.Common.Name,Facility.Type, Licensed.Species, geometry) |> 
  rename(Licence.Type = Facility.Type)
# marine_sf <- st_as_sf(marine_salmonids, coords = c("Longitude", "Latitude"), crs = 4326)
enchancements_sf <- st_as_sf(enchancements_salmonids, coords = c("Longitude", "Latitude"), crs = 4326)
enchancements_sf <- enchancements_sf |> 
  select(Facility.Reference.Number, Licence.Number, Expiry.Date, 
         Licence.Holder, Site.Common.Name, Licence.Type, Licensed.Species, geometry) 
# Combine all sf objects into one
all_aquaculture_sf <- rbind(landbased_sf, enchancements_sf)
# distance to roads is the template raster
road_distances<-terra::rast(paste0(onedrive_wd,"CNF/distance_to_numbered_highway_raster.tif"))

# Convert sf to SpatVector
all_aquaculture_spv <- vect(all_aquaculture_sf)
# Rasterize the points
aquaculture_raster <- terra::rasterize(all_aquaculture_spv, road_distances, fun = "count", background = 0)
#convert raster to presence absence
presence_absence_raster <- classify(aquaculture_raster_bc, rcl = matrix(c(0, Inf, 1), ncol = 3, byrow = TRUE))
presence_absence_raster[aquaculture_raster_bc == 0] <- 0
# Save the rasters
terra::writeRaster(aquaculture_raster, paste0(onedrive_wd,"raster/aquaculture_raster.tif"), overwrite = TRUE)
terra::writeRaster(presence_absence_raster, paste0(onedrive_wd,"raster/aquaculture_presence_absence_raster.tif"), overwrite = TRUE)
#save the sf objects that were used to make the raster
st_write(all_aquaculture_sf, paste0(onedrive_wd,"raster/aquaculture_facilities.shp"), delete_dsn = TRUE)
# Plot the raster
plot(aquaculture_raster, main = "Aquaculture Facilities in BC")

bc<-bcmaps::bc_bound()
crs = st_crs(all_aquaculture_sf)
bc <- st_transform(bc, crs = crs)
#crop and clip the raster by bc
aquaculture_raster_bc <- terra::crop(aquaculture_raster, bc)
aquaculture_raster_bc <- terra::mask(aquaculture_raster_bc, bc)
#plot and set zero to transparent
#set zero to be transparent
terra::values(aquaculture_raster_bc)[terra::values(aquaculture_raster_bc) == 0] <- NA

ggplot()+
  geom_sf(data = bc, fill = "lightgrey", color = "black", alpha = 0.7)+
  geom_spatraster(data = aquaculture_raster_bc, aes(fill = count), alpha = 0.9) +
  scale_fill_viridis_c(option = "inferno", na.value = "transparent")+
  labs(title = "Aquaculture Facilities in BC") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(crs = st_crs(bc))
