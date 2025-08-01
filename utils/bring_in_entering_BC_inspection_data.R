if(!file.exists('data/inspections_WD_outside_BC.rds')){
  insp_from_wd_inf_to_wb = sf::read_sf("W:/CMadsen/Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries_all_years_WD_Infected_Areas.gpkg")

  insp_from_wd_inf_to_wb$WB_POLY_ID = NA
  insp_from_wd_inf_to_wb$BLK = NA

  for(i in 1:nrow(insp_from_wd_inf_to_wb)){
    print(i)
    the_wb = insp_from_wd_inf_to_wb[i,]
    if(is.na(the_wb$BLK)){
      if(str_detect(the_wb$GNIS_NA,"Lake")){
        extra_info = bcdc_query_geodata('freshwater-atlas-lakes') |>
          filter(GNIS_NAME_1 == local(the_wb$GNIS_NA)) |>
          filter(INTERSECTS(the_wb)) |>
          collect()

        if(nrow(extra_info) > 0){
          extra_info = extra_info |>
            dplyr::select(WATERSH = WATERSHED_GROUP_ID,
                          GNIS_NA = GNIS_NAME_1,
                          WATERBODY_POLY_ID, BLUE_LINE_KEY) |>
            dplyr::slice(1)
        }
        if(nrow(extra_info) > 0){
          insp_from_wd_inf_to_wb[i,]$WB_POLY_ID = extra_info$WATERBODY_POLY_ID
          insp_from_wd_inf_to_wb[i,]$BLK = extra_info$BLUE_LINE_KEY
        }
      }
      if(str_detect(the_wb$GNIS_NA,"(River|Creek)")){
        extra_info = bcdc_query_geodata('freshwater-atlas-rivers') |>
          filter(GNIS_NAME_1 == local(the_wb$GNIS_NA)) |>
          filter(INTERSECTS(the_wb)) |>
          collect()

        if(nrow(extra_info) > 0){
          extra_info = extra_info |>
            dplyr::select(WATERSH = WATERSHED_GROUP_ID,
                          GNIS_NA = GNIS_NAME_1,
                          WATERBODY_POLY_ID, BLUE_LINE_KEY) |>
            dplyr::slice(1)
        }
        if(nrow(extra_info) > 0){
          insp_from_wd_inf_to_wb[i,]$WB_POLY_ID = extra_info$WATERBODY_POLY_ID
          insp_from_wd_inf_to_wb[i,]$BLK = extra_info$BLUE_LINE_KEY
        }
      }
    }
  }
  saveRDS(insp_from_wd_inf_to_wb, file = 'data/inspections_WD_outside_BC.rds')
} else {
  if(!exists("insp_from_wd_inf_to_wb")){
    insp_from_wd_inf_to_wb = readRDS('data/inspections_WD_outside_BC.rds')
  }
}
