# Stephen Ban asked about which parks overlap with the list
# of top 100 waterbodies.

library(sf)
library(tidyverse)
library(bcdata)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

top_100 = readRDS(paste0(onedrive_wd,"output_of_WD_monitoring_analysis_top_100_waterbodies.rds"))

top_100_bc_alb = top_100 |> sf::st_transform(3005)

nearby_parks_l = list()

for(i in 1:nrow(top_100_bc_alb)){
  print(i)
  nearby_parks = bcdc_query_geodata('terrestrial-protected-areas-representation-by-ecosection-parc-') |>
    filter(INTERSECTS(local(top_100_bc_alb[i,]))) |>
    collect()
  if(nrow(nearby_parks) > 0){
    nearby_parks = nearby_parks |> sf::st_filter(top_100_bc_alb[i,])
  }
  nearby_parks$nearby_wb = paste0(top_100_bc_alb$`Waterbody Name`[i],", ",top_100_bc_alb$`Watershed Name`[i])
  nearby_parks_l[[i]] = nearby_parks |> sf::st_transform(4326)
}

nearby_parks_b = dplyr::bind_rows(nearby_parks_l)

openxlsx::write.xlsx(nearby_parks_b |> sf::st_drop_geometry(),
                     "C:/Users/CMADSEN/Downloads/Parks_overlap_with_top_100_WD_priority_waterbodies.xlsx")

sf::write_sf(nearby_parks_b, "C:/Users/CMADSEN/Downloads/Parks_overlap_with_top_100_WD_priority_waterbodies.gpkg")

sf::write_sf(top_100_bc_alb, "C:/Users/CMADSEN/Downloads/Top_100_WD_priority_waterbodies.gpkg")

ggplot() +
  geom_sf(data = bcmaps::bc_bound()) +
  geom_sf(data = top_100_bc_alb, fill = 'purple', color = 'purple') +
  geom_sf(data = nearby_parks_b, fill = 'darkgreen', color = 'darkgreen')
