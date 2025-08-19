# Fish occurrence records queried from typical data sources.
library(bcinvadeR)

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

# Quick double check of mountain whitefish and rainbow trout.

# ggplot() + geom_sf(data = fish_occs[fish_occs$species == 'Mountain_Whitefish',])
# ggplot() + geom_sf(data = fish_occs[fish_occs$species == 'Rainbow_Trout',])

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

# now is there an overlap of susceptible fish with the wb_list, for each species, new column with TRUE/FALSE for the species

fish_occs<-sf::st_transform(fish_occs, 3005)

# Initialize columns for each species in wb_list
#species_names <-gsub(" ", "_", species_names)  # Ensure species name matches the column name

wb_list$sus_spp <- ""
#wb_list<- sf::st_transform(wb_list, 3005)

for (sp in species_names) {
  # Filter points for the species
  spoi<-gsub(" ", "_", sp)
  sp_points <- fish_occs[fish_occs$species == spoi, ]

  # Check which waterbodies intersect with any points of this species
  intersects <- st_intersects(wb_list, sp_points, sparse = FALSE)

  # Append species name to Fish_present where there's at least one intersecting point
  for (i in seq_len(nrow(wb_list))) {
    if (any(intersects[i, ])) {
      if (wb_list$sus_spp[i] == "") {
        wb_list$sus_spp[i] <- sp
      } else {
        wb_list$sus_spp[i] <- paste(gsub("_", " ",wb_list$sus_spp[i]), sp, sep = ", ")
      }
    }
  }
}

# ggplot() +
#   geom_sf(data = wb_list[str_detect(wb_list$Fish_present,"Rainbow"),]) +
#   geom_sf(data = fish_occs[fish_occs$species == 'Rainbow_Trout',], col = 'yellow')

# # An alternative to the above!
# fish_occs_by_wb = fish_occs |>
#   st_join(wb_list |> dplyr::select(GNIS_NA,WATERSH))
#
# fish_occs_by_wb = fish_occs_by_wb |>
#   sf::st_drop_geometry() |>
#   dplyr::group_by(GNIS_NA, WATERSH) |>
#   dplyr::reframe(Species = paste0(unique(Species), collapse = ', '))
#
# wb_list = wb_list |>
#   dplyr::left_join(fish_occs_by_wb)

# wb_list |> dplyr::filter(str_detect(Fish_present,"[rR]ainbow"))

# The above looks perfect!

# get the sara listed species in each of the listed waterbodies

sara_all<-sf::read_sf(paste0(onedrive_wd,"CNF/DFO_SARA_occ_data_QGIS_simplified_SouthCoastBullTrout.gpkg"))

sara_sp <- sara_all |>
  filter(str_detect(Common_Name_EN,
                    regex("Cultus Pygmy Sculpin|Westslope Cutthroat Trout|Bull Trout|Rocky Mountain Ridged Mussel|Sockeye Salmon",
                          ignore_case = TRUE)))

# add the sockeye from Cultus lake
#get geom - then drop all column, makes new columns from sara layer and then add on sockeye salmon

cultus_shape <- bcdata::bcdc_query_geodata("freshwater-atlas-lakes") |>
  filter(GNIS_NAME_1 == "Cultus Lake") |>
  collect() |>
  slice(2) |>
  sf::st_transform(4326)

clake <- data.frame(
  Common_Name_EN = "Sockeye Salmon",
  Population_EN = "Cultus Lake"
) |>
  dplyr::mutate(geom = sf::st_geometry(cultus_shape)) |>
  sf::st_as_sf(sf_column_name = "geom", crs = sf::st_crs(cultus_shape))


sara_sp<-bind_rows(sara_sp, clake)

sara_sp <- sf::st_transform(sara_sp, st_crs(wb_list))

# Perform spatial join: match SARA species polygons to waterbodies
wb_with_sara <- st_join(wb_list, sara_sp |> select(Common_Name_EN), left = TRUE)

# Create or update a `sara` column: list of overlapping species names
wb_with_sara_grouped <- wb_with_sara |>
  group_by(across(all_of(names(wb_list)))) |>  # Group by original wb_list columns
  summarise(sara = paste(unique(Common_Name_EN[!is.na(Common_Name_EN)]), collapse = "; "),
            .groups = "drop")

# Replace empty strings with NA (no SARA species overlap)
wb_with_sara_grouped <- wb_with_sara_grouped |>
  mutate(sara = na_if(trimws(sara), ""))

wb_list<-wb_with_sara_grouped
rm(wb_with_sara_grouped, wb_with_sara,sara_all)
