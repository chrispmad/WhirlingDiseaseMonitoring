library(tidyverse)
library(sf)
library(ggplot2)


ems_wt = DBI::dbGetQuery(ems_db, "select * from results where PARAMETER like 'Temperature' or PARAMETER like 'Temperature-Field'")

# Convert the date field from character to datetime.
ems_wt = ems_wt |> 
  dplyr::mutate(the_date = lubridate::ymd_hms(COLLECTION_START)) |> 
  dplyr::mutate(the_year = lubridate::year(the_date))

#### Testing aquarius

aquarius_list<-read.csv("data/aquarius_wt_list.csv", skip = 1)

ems_wt_clean <- sf::st_drop_geometry(ems_wt)

match_logical <- sapply(ems_wt_clean$MONITORING_LOCATION, function(loc) {
  any(grepl(loc, aquarius_list$Location, ignore.case = TRUE, fixed = TRUE) |
      grepl(aquarius_list$Location, loc, ignore.case = TRUE, fixed = TRUE))
})
ems_wt_matched <- ems_wt_clean[match_logical, ]
