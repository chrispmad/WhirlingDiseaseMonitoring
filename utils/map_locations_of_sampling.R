library(tidyverse)
library(sf)
library(ggplot2)
library(ggplot2)
library(openxlsx)
library(bcdata)


onedrive<-"C:/Users/JPHELAN/OneDrive - Government of BC/data/"
locations<-read.xlsx("data/gov_bc_sites_August.xlsx")

#create an sf object of it
locations_sf <- st_as_sf(locations, coords = c("Easting", "Northing"), crs = 32610)
locations_sf <- locations_sf |> st_transform(3005)


bc<-bcmaps::bc_bound()

named_wbs<-readRDS(paste0(onedrive,"named_lakes_and_rivers_merged.rds"))

# Ensure both layers are in the same CRS
named_wbs <- st_transform(named_wbs, st_crs(locations_sf))

# Filter named_wbs to only those within a buffer around locations_sf
buffered_locations <- st_buffer(locations_sf, dist = 5000)  # 5 km buffer
named_wbs_filtered <- named_wbs[st_intersects(named_wbs, st_union(buffered_locations), sparse = FALSE), ]

#now we map, based on extent of hte locations_sf. Add bc, rivers and then the points
p1<-ggplot()+
  geom_sf(data=bc, fill="lightgrey")+
  geom_sf(data = named_wbs_filtered, color = "blue", size = 0.1, alpha = 0.5)+
  geom_sf(data=locations_sf, aes(color=Site.Description), size=3)+
  coord_sf(xlim = c(st_bbox(locations_sf)$xmin - 25000, st_bbox(locations_sf)$xmax + 25000),
           ylim = c(st_bbox(locations_sf)$ymin - 25000, st_bbox(locations_sf)$ymax + 25000))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x="Easting (m)", y="Northing (m)")

ggsave("images/map_gov_bc_sampling_locations_August2023.png",p1, width=12, height=10)
