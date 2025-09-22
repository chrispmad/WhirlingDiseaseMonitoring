library(tidyverse)
library(sf)
library(bcmaps)
library(bcdata)
library(dplyr)
library(leaflet)

# Get onedrive path
base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

wshds = bcdc_query_geodata("freshwater-atlas-watershed-groups") |>
  collect()

############################################
# Looking for the waterbodies to test - will be commented out after use one time
test = bcdc_query_geodata('freshwater-atlas-rivers') |>
  filter(GNIS_NAME_1 == "St. Mary River") |>
  collect()
if(nrow(test) > 0){
  test = test |>
    sf::st_zm() |>
    dplyr::group_by(BLUE_LINE_KEY) |>
    dplyr::summarise() |>
    dplyr::mutate(type = 'river')
}

st_test = bcdc_query_geodata('freshwater-atlas-stream-network') |>
  filter(GNIS_NAME == "St. Mary River") |>
  collect() |>
  sf::st_zm() |>
  dplyr::group_by(BLUE_LINE_KEY) |>
  dplyr::summarise() |>
  sf::st_buffer(dist = 10) |>
  dplyr::mutate(type = 'stream')

if(nrow(test) > 0){
  test = dplyr::bind_rows(test, st_test)
} else {
  test = st_test
}

overlapping_watersheds = wshds |>
  sf::st_filter(test)

leaflet() |>
  addTiles() |>
  addPolygons(data = sf::st_transform(overlapping_watersheds,4326),
              label = ~WATERSHED_GROUP_NAME) |>
  addPolygons(data = sf::st_transform(test, 4326),
              label = ~BLUE_LINE_KEY)
##########################################################


# two waterbodies to be tested - comment out as appropriate
joseph_creek<-bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(GNIS_NAME == "Joseph Creek") |> 
  filter(BLUE_LINE_KEY == 356565201) |> 
  collect() |> 
  dplyr::group_by(BLUE_LINE_KEY) |> 
  dplyr::summarise() |> 
  sf::st_buffer(dist = 10) |> 
  dplyr::mutate(`Waterbody Name` = "Joseph Creek",
                `Watershed Name` = "St Mary River")  |> 
  sf::st_transform(4326)

perry_creek<-bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(GNIS_NAME == "Perry Creek") |> 
  filter(BLUE_LINE_KEY == 356567797) |> 
  collect() |> 
  dplyr::group_by(BLUE_LINE_KEY) |> 
  dplyr::summarise() |> 
  sf::st_buffer(dist = 10) |> 
  dplyr::mutate(`Waterbody Name` = "Perry Creek",
                `Watershed Name` = "St Mary River")  |> 
  sf::st_transform(4326)

st_mary_river<-bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(GNIS_NAME == "St. Mary River") |> 
  #filter(BLUE_LINE_KEY == 356567797) |> 
  collect() |> 
  dplyr::group_by(BLUE_LINE_KEY) |> 
  dplyr::summarise() |> 
  sf::st_buffer(dist = 10) |> 
  dplyr::mutate(`Waterbody Name` = "St. Mary River",
                `Watershed Name` = "St Mary River")  |> 
  sf::st_transform(4326)

wbs_test<-dplyr::bind_rows(joseph_creek, perry_creek, st_mary_river)

# load in the SAR locations
dfo_sara = sf::read_sf(paste0(onedrive_wd,"DFO_SARA/dfo_sara_occurrences_in_BC_no_marine.gpkg"))

# Filter for just CNF agreement species.
dfo_sara_f = dfo_sara |> 
  dplyr::filter(Common_Name_EN  == "Bull Trout" & Population_EN == "South Coast" |
                  Common_Name_EN == "Westslope Cutthroat Trout" & Population_EN == "Pacific" |
                  Common_Name_EN == "Sockeye Salmon" & Population_EN == "Cultus")

new_geom_w_SAR = wbs_test |> 
  sf::st_transform(sf::st_crs(dfo_sara_f)) |> 
  sf::st_join(dfo_sara_f |> dplyr::select(Common_Name_EN,Population_EN))

new_geom_w_SAR_sum_tbl = new_geom_w_SAR |> 
  dplyr::filter(!is.na(Common_Name_EN)) |> 
  sf::st_drop_geometry() |> 
  dplyr::count(`Waterbody Name`,`Watershed Name`,Common_Name_EN, Population_EN) |> 
  dplyr::group_by(`Waterbody Name`,`Watershed Name`) |> 
  dplyr::summarise(`SARA Species` = paste0(paste0(Common_Name_EN, " (",Population_EN,")"),collapse = ', ')) |> 
  dplyr::ungroup()

#now make a leaflet map of these, colouring the polygons by Common_Name_EN
leaflet() |> 
  addTiles() |> 
  addPolygons(data = sf::st_transform(wbs_test,4326),
              label = ~`Waterbody Name`,
              color = 'blue',
              weight = 2,
              fillOpacity = 0.1) |>
  addPolygons(data = sf::st_transform(new_geom_w_SAR,4326),
              label = ~paste0(`Waterbody Name`, ": ", Common_Name_EN, " (",Population_EN,")"),
              color = 'red',
              weight = 2,
              fillOpacity = 0.5) |>
  addPolygons(data = sf::st_transform(dfo_sara_f,4326),
              label = ~paste0(Common_Name_EN, " (",Population_EN,")"),
              color = "black",
              weight = 2,
              fillOpacity = 0.6)
