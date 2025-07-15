stonks = read_excel("data/hand-picked stocking locations.xlsx")

stonks_sf = stonks |>
  dplyr::mutate(across(c(hand_lng,hand_lat), \(x) as.numeric(x))) |>
  dplyr::filter(!is.na(hand_lng)) |>
  dplyr::filter(!is.na(hand_lat)) |>
  dplyr::filter(is.numeric(hand_lng)) |>
  sf::st_as_sf(coords = c('hand_lng','hand_lat'), crs = 4326)

# grab the species stocked in each.
dat2024<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2024.csv"))
dat2023<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2023.csv"))
dat2022<-read.csv(paste0(onedrive_wd,"FFSBC_fish_stocking_2022.csv"))

dat2022 <- dat2022 |>  mutate(Year = 2022)
dat2023 <- dat2023 |>  mutate(Year = 2023)
dat2024 <- dat2024 |>  mutate(Year = 2024)

all_years <- bind_rows(dat2022, dat2023, dat2024)

stonks_sf_w_species = stonks_sf |>
  inner_join(all_years) |>
  dplyr::select(-c(lng,lat)) |>
  dplyr::group_by(Waterbody.Name, Nearest.Town) |>
  dplyr::summarise(stocked_species = paste0(unique(Species), collapse = ', '),
                   geometry = st_union(geometry)) |>
  dplyr::ungroup() |>
  sf::st_transform(crs = st_crs(wb_list))

# Do the little reverse spatial join thingy.
stonks_sf_w_species_w_wblist = stonks_sf_w_species |>
  st_join(
    wb_list |> dplyr::select(GNIS_NA,WATERSH)
  )

stocking_summarised_by_wb_list = stonks_sf_w_species_w_wblist |>
  sf::st_drop_geometry() |>
  dplyr::group_by(GNIS_NA, WATERSH) |>
  dplyr::reframe(stocked_species = paste0(unique(stocked_species), collapse = ', '))

wb_list = wb_list |>
  dplyr::left_join(
    stocking_summarised_by_wb_list
  )

stocked_species_dt = DT::datatable(wb_list |> sf::st_drop_geometry())
