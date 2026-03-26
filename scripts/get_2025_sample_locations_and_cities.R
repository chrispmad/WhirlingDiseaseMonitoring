library(tidyverse)
library(openxlsx)

library(sf)
library(dplyr)
library(maps)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

sample <- openxlsx::read.xlsx("./data/sample_locations_2025.xlsx")

waterbodies_sf <- st_as_sf(sample, coords = c("Longitude", "Latitude"), crs = 4326)

sample

data(world.cities)
canada_cities <- world.cities %>% filter(country.etc == "Canada")
cities_sf <- st_as_sf(canada_cities, coords = c("long", "lat"), crs = 4326)

nearest_city <- st_nearest_feature(waterbodies_sf, cities_sf)
waterbodies_sf$Closest.City <- cities_sf$name[nearest_city]

write.xlsx(waterbodies_sf |> st_drop_geometry(), "./data/sample_locations_and_cities_2025.xlsx")
