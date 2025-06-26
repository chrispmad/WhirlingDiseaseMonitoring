library(tidyverse)
library(dplyr)
library(sf)
library(readxl)
library(bcdata)
library(tidygeocoder)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

dat2024<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2024.csv"))
dat2023<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2023.csv"))
dat2022<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2022.csv"))

dat2022 <- dat2022 |>  mutate(Year = 2022)
dat2023 <- dat2023 |>  mutate(Year = 2023)
dat2024 <- dat2024 |>  mutate(Year = 2024)

all_years <- bind_rows(dat2022, dat2023, dat2024)

named_lakes<-bcdc_query_geodata('freshwater-atlas-lakes') |>
  dplyr::filter(!is.na(GNIS_NAME_1)) |>
  collect() |>
  dplyr::group_by(BLUE_LINE_KEY,GNIS_NAME_1,FWA_WATERSHED_CODE) |>
  dplyr::summarise()

towns<- all_years |>
  dplyr::select(Nearest.Town) |>
  distinct() |>
  mutate(Nearest.Town = str_trim(Nearest.Town)) |>
  mutate(clean_town = str_trim(str_remove(Nearest.Town, ".*\\bOF\\b")))

towns <- towns |>
  mutate(geocode_input = paste0(clean_town, ", British Columbia"))

geocoded <- towns |>
  geocode(address = geocode_input, method = "osm", lat = latitude, long = longitude)


geocoded_sf <- st_as_sf(geocoded[!is.na(geocoded$latitude),], coords = c("longitude", "latitude"), crs = 4326)

# bc<-bcmaps::bc_bound() |> st_transform(4326)
# ggplot()+
#   geom_sf(data = bc) +
#   geom_sf(data = geocoded_sf)

# buffer
dat2024_geo <- all_years |>
  left_join(geocoded, by = "Nearest.Town")

data2024_sp<-st_as_sf(dat2024_geo[!is.na(dat2024_geo$latitude),], coords = c("longitude", "latitude"), crs = 4326)
data2024_buff <- data2024_sp |>
  st_transform(3005) |>
  st_buffer(100000)

named_lakes <- named_lakes |>
  mutate(
    lake_name_clean = str_to_lower(str_remove(GNIS_NAME_1, "\\s+lakes?$"))  # removes ' lake' or ' lakes'
  )
data2024_buff <- data2024_buff |>
  mutate(
    lake_name_clean = str_to_lower(str_remove(Waterbody.Name, "\\s+lakes?$"))
  )

named_lakes <- st_transform(named_lakes, crs = st_crs(data2024_buff))
joined <- st_join(data2024_buff, named_lakes, join = st_intersects)
matched <- joined %>%
  filter(lake_name_clean.x == lake_name_clean.y)

# - - - - - - - - - - - - -

bc_locs = data.frame(location = unique(stringr::str_to_sentence(all_years$Nearest.Town)))

#Create new variables that we will fill with the loop below.
bc_locs$lon = 0
bc_locs$lat = 0

#This loop uses the BC geocoder to find the most likely coordinates
# for each of the unique place names.
for(i in 1:nrow(bc_locs)){
  print(i)
  # Pull out place name.
  my.name = bc_locs[i,]$location
  # Replace any accented names, starting with just é.
  my.name = stringr::str_replace_all(my.name, "(é|Ã©)", "e")
  #Clean up names. Remove anything in brackets.
  my.name = stringr::str_remove_all(my.name, " \\(.*\\)")
  #Add spaces to names.
  my.name = stringr::str_replace_all(my.name, " ", "%20")

  url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
               my.name,'&maxResults=1&outputSRS=4326')

  my.coords = jsonlite::fromJSON(url)$features$geometry |>
    dplyr::summarise(lon = stringr::str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
                     lat = stringr::str_extract(coordinates, "(?<=\\,).*(?=\\))"))

  bc_locs[i,]$lon = my.coords$lon
  bc_locs[i,]$lat = my.coords$lat
}

# how many were successfully matched to locations?
bc_locs |>
  dplyr::filter(!is.na(lon))
# All of them?! Wahoo!

bc_locs = bc_locs |>
  dplyr::filter(!is.na(lon))

