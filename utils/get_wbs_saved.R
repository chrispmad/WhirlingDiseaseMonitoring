library(tidyverse)
library(sf)


file_name = read_rds("C:/Users/JPHELAN/OneDrive - Government of BC/data/named_lakes_and_rivers.rds")


sf::write_sf(file_name, "C:/Users/JPHELAN/OneDrive - Government of BC/data/named_lakes_and_rivers_sf.gpkg")
