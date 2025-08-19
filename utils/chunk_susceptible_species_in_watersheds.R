# Fish occurrence records queried from typical data sources.
library(bcinvadeR)
library(tidyverse)
library(dplyr)
library(sf)
library(bcdata)
library(leaflet)

requery_occs= F

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

bc<-bcmaps::bc_bound() |>
  sf::st_transform(4326)

species_names <- c(
  "Bull Trout", "Sockeye Salmon", "Cutthroat Trout", "Coho Salmon",
  "Rainbow Trout", "Chinook Salmon", "Mountain Whitefish",
  "Atlantic Salmon", "Brown Trout", "Brook Trout", "Westslope Cutthroat Trout",
  "Steelhead","Lake Whitefish","Round Whitefish","Pygmy Whitefish"
)

if(requery_occs){
  species_occs <- lapply(species_names, function(sp) {
    ocs<-try(suppressMessages(suppressWarnings(grab_aq_occ_data(sp, quiet = TRUE, sources = c("FDIS","Old Aquatic","iNaturalist")))))
    if (!is.null(ocs)) {
      outfile <- paste0("data/", gsub(" ", "_", sp), "_occurrences.gpkg")
      sf::write_sf(ocs, outfile)
    }
    return(ocs)
  })

}else{
  species_occs <- lapply(species_names, function(sp) {
    sf::read_sf(paste0("data/", gsub(" ", "_", sp), "_occurrences.gpkg"))
  })
}

names(species_occs) <- gsub(" ", "_", species_names)

fish_occs <- dplyr::bind_rows(species_occs, .id = "species")



if(!interactive()){
  print(
    ggplot() +
    geom_sf(data = bc, color = "lightgrey") +
    geom_sf(data = fish_occs, aes(color = as.factor(Species)))+
    facet_wrap( ~ Species)+
    scale_color_viridis_d() +
    labs(color = "Species") +
    ggthemes::theme_map()+
    theme(legend.position = "bottom")
  )
}

watersheds<-bcdc_query_geodata("freshwater-atlas-watershed-groups") |> 
  collect()

fish_occs <- st_transform(fish_occs, crs = st_crs(watersheds))
#for each distinct watershed, check for overlap with fish-occs, and record each
#unique occurence in the watershed spatially
# grouped_fish = watersheds |> 
#   group_by(WATERSHED_GROUP_NAME) %>%
#   # get unique spp occs in each waterhsed
#   summarise(sus_spp = list(unique(fish_occs$species[fish_occs$species %in% species_names & 
#                                                      sf::st_intersects(fish_occs, ., sparse = FALSE)])),
#             .groups = "drop")


another = fish_occs %>%
  st_join(watersheds)

test <- another |>
  group_by(WATERSHED_GROUP_NAME) |>
  summarise(
    sus_spp = paste(unique(species), collapse = ", "),
    .groups = "drop"
  )


  

final_test = watersheds |> 
  left_join(test |> st_drop_geometry() |> select(c("WATERSHED_GROUP_NAME", "sus_spp")), by = "WATERSHED_GROUP_NAME") |> 
  st_transform(4326) |> 
  st_simplify()

final_test <- final_test |> 
  filter(!is.na(sus_spp))

# l = leaflet() |> 
#   addTiles() |> 
#   addPolygons(data = final_test,
#               color = "black",
#               popup = leafpop::popupTable(
#                 final_test,
#                 zcol = c("WATERSHED_GROUP_NAME", "sus_spp")))
#   
# 
# l

allwbs <- read_rds(paste0(onedrive_wd, "named_lakes_and_rivers.rds")) |> 
  st_transform(3005)

wbs_spp<- allwbs |> 
  st_join(fish_occs)

unique_wbs<- wbs_spp |> 
  group_by(waterbody) |> 
  summarise(
    sus_spp = paste(unique(species), collapse = ", "),
    .groups = "drop"
  )

unique_wbs = unique_wbs |> 
  filter(!is.na(sus_spp)) |> 
  filter(sus_spp != "NA")


l = leaflet() |> 
  addTiles() |> 
  addPolygons(data = unique_wbs |> st_transform(4326),
              color = "black",
              popup = leafpop::popupTable(
                unique_wbs,
                zcol = c("waterbody", "sus_spp")))


l


sf::write_sf(unique_wbs, paste0(onedrive_wd, "all_wbs_whirling_sus_spp.gpkg"))
