if(!file.exists('data/fishing_days_by_waterbody_and_watershed_w_BLK.rds')){

  ang = readRDS(paste0(onedrive_wd,"fishing_days_by_waterbody_and_watershed.rds"))

  # Add BLK and WB_POLY_ID
  ang$WB_POLY_ID = NA
  ang$BLK = NA

  ang$GNIS_NA = ang$waterbody
  ang$WATERSH = ang$watershed

  for(i in 1:nrow(ang)){
    print(i)
    the_wb = ang[i,] |> sf::st_transform(3005)
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
          ang[i,]$WB_POLY_ID = extra_info$WATERBODY_POLY_ID
          ang[i,]$BLK = extra_info$BLUE_LINE_KEY
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
          ang[i,]$WB_POLY_ID = extra_info$WATERBODY_POLY_ID
          ang[i,]$BLK = extra_info$BLUE_LINE_KEY
        }
      }
    }
  }

  saveRDS(ang, file = 'data/fishing_days_by_waterbody_and_watershed_w_BLK.rds')
} else {
  ang = readRDS('data/fishing_days_by_waterbody_and_watershed_w_BLK.rds')
}
