library(bcdata)

bcl = bcdc_list()

roads = bcl[str_detect(bcl,"road-atlas")]

my_shape = t[t$Name == 'Kootenay Boundary',] |> sf::st_transform(3005)

rds = bcdc_query_geodata(roads[3]) |>
  filter(INTERSECTS(my_shape)) |>
  collect()

bcmaps::bc_cities()

mining = bcl[str_detect(bcl,"mine-")]

mines = bcdc_query_geodata(mining[1]) |>
  collect()

ggplot() + geom_sf(data = mines)

# Recreation sites / beaches?
recsites = bcl[str_detect(bcl,'recreation-site')]

reccies = bcdc_query_geodata(recsites[12]) |>
  collect() |>
  dplyr::filter(PROJECT_TYPE == 'SIT - Recreation Site')

ggplot() + geom_sf(data = reccies)

# Provincial parks
parkies = bcl[str_detect(bcl,'parks-and-protected')]

park_layer = bcdc_query_geodata('terrestrial-protected-areas-representation-by-ecosection-parc-') |>
  collect()
# Note: variable PROTECTED_AREA_TYPE has levels PROVINCIAL PARK (1038), NGO CONSERVANCY (603), etc.
park_layer_s = sf::st_simplify(park_layer)

# We could pull these apart and put PROVINCIAL PARKS in one layer,
# and
ggplot() + geom_sf(data = park_layer_s)

park_layer_s |> sf::st_drop_geometry() |> View()

library(tidyhydat)
# tidyhydat::download_hydat()
bc_stats = tidyhydat::hy_stations(prov_terr_state_loc = "BC")

some_flow = tidyhydat::hy_daily_flows(station_number = bc_stats$STATION_NUMBER[1]) |>
  dplyr::filter(!is.na(Value))

active_stats = bc_stats[bc_stats$HYD_STATUS == 'ACTIVE',]

ggplot() + geom_sf(data = st_as_sf(bc_stats,coords=c("LONGITUDE","LATITUDE"),crs = 4326))
ggplot() + geom_sf(data = st_as_sf(active_stats |> dplyr::filter(!is.na(LONGITUDE)),coords=c("LONGITUDE","LATITUDE"),crs = 4326))

some_flow_s = some_flow |>
  dplyr::mutate(the_month = lubridate::month(Date),
                the_day = lubridate::day(Date)) |>
  dplyr::group_by(the_month, the_day) |>
  dplyr::reframe(Value = mean(Value, na.rm=T)) |>
  dplyr::mutate(the_year = '2010') |>
  dplyr::mutate(the_month = str_pad(the_month, side = 'left', pad = '0', width = 2),
                the_day = str_pad(the_day, side = 'left', pad = '0', width = 2)) |>
  dplyr::mutate(the_date = paste0(the_year, '-', the_month, '-', the_day)) |>
  dplyr::mutate(the_date = lubridate::ymd(the_date))

some_flow_s |>
  ggplot() +
  geom_point(aes(x = the_date, y = Value))

