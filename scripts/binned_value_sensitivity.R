library(tidyverse)
library(readxl)
library(BAMMtools)
library(terra)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(tidyterra)
library(sf)

options(scipen = 999)



base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
# proj_wd = getwd()
# if(stringr::str_detect(proj_wd,"scripts")){
#   proj_wd = stringr::str_remove(proj_wd, "scripts/markdown")
# }

insp_from_wd_inf_to_wb = sf::read_sf("W:/CMadsen/Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries_all_years_WD_Infected_Areas.gpkg")
insp_from_wd_bc_wb = sf::read_sf("W:/CMadsen/Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries_all_years_BC_WD_Infected_Waterbodies.gpkg")
ang = sf::read_sf("W:/CMadsen/shared_data_sets/freshwater_fisheries_society_angler_survey_2022_2023.gpkg")

# This function finds the natural breaks (or, "Jenks") for a numeric column
bin_to_natural_breaks = function(dat,variable){
  # find natural breaks ("jenks")
  nbreaks = BAMMtools::getJenksBreaks(var = dat[[variable]], k = 4)
  dat |>
    dplyr::rename(var_to_bin = !!rlang::sym(variable)) |>
    dplyr::mutate(var_bin = as.numeric(cut(var_to_bin,nbreaks))) |>
    dplyr::mutate(var_bin = tidyr::replace_na(var_bin, 1)) |>
    dplyr::mutate(var_bin = as.character(var_bin)) |>
    dplyr::mutate(var_bin = factor(var_bin, levels = c(1:3))) |>
    dplyr::rename(!!rlang::sym(paste0(variable,"_natural_bin")) := var_bin,
                  !!rlang::sym(variable) := var_to_bin)
}

#k-means clustering function
bin_to_kmeans <- function(dat, variable, centers = k_breaks) {
  has_geometry <- inherits(dat, "sf")
  if (has_geometry) {
    dat_df <- sf::st_drop_geometry(dat)
  } else {
    dat_df <- dat
  }

  valid_rows <- is.finite(dat_df[[variable]])
  dat_df_valid <- dat_df[valid_rows, ]

  set.seed(123)
  km <- stats::kmeans(dat_df_valid[[variable]], centers = centers, iter.max = 100)

  cluster_means <- tapply(dat_df_valid[[variable]], km$cluster, mean)
  ordered_clusters <- order(cluster_means)
  relabeled_clusters <- match(km$cluster, ordered_clusters)

  bin_col <- rep(NA, nrow(dat_df))
  bin_col[valid_rows] <- factor(relabeled_clusters, levels = as.character(1:centers))

  return(bin_col)
}



## GGplot label convenience function
bin_info_labs = function(bins_title){
  labs(color = 'Inspections (bin)',
       fill = 'Inspections (bin)',
       subtitle = paste0("Inspection Bins: 1 ('low', ",
                         bins_title[1]," to ",bins_title[2],
                         "), 2 ('medium', ",
                         bins_title[2]," to ",bins_title[3],
                         "), 3 ('high', ",
                         bins_title[3]," to ",bins_title[4],
                         ")"))
}

getKMeansBreaks <- function(var, k = k_breaks) {
  var_clean <- var[!is.na(var)]  # remove NAs
  set.seed(123)  # for reproducibility
  km <- stats::kmeans(var_clean, centers = k)
  centers <- sort(km$centers)
  # Compute midpoints between sorted centers
  breaks <- c(min(var_clean),
              zoo::rollmean(centers, 2),
              max(var_clean))
  return(breaks)
}

test_k<-function(data,var, k){

  col_name<-paste0("k_break_",k)
  data[[col_name]]<-bin_to_kmeans(data,var,k)

  return(data)
}


for (i in 3:7){
  k_breaks <- i
  insp_from_wd_inf_to_wb<-test_k(test, "TotalInspections", k_breaks)
  insp_from_wd_inf_to_wb<-test_k(insp_from_wd_inf_to_wb, "TotalInspections", k_breaks)
  ang<-test_k(ang, "days_fished", k_breaks)
}





########################################################
# Create a list to store results
# Create a list to store per-k priority tables
priority_tables_by_k <- list()

for (k in 3:7) {
  # Apply k-means binning
  insp_from_wd_inf_to_wb$TotalInspections_kmeans_bin <- bin_to_kmeans(insp_from_wd_inf_to_wb, "TotalInspections", k)
  insp_from_wd_bc_wb$TotalInspections_kmeans_bin <- bin_to_kmeans(insp_from_wd_bc_wb, "TotalInspections", k)
  ang$days_fished_kmeans_bin <- bin_to_kmeans(ang, "days_fished", k)

  # Prepare bin tables
  inf_to_bc_bins <- insp_from_wd_inf_to_wb |>
    sf::st_drop_geometry() |>
    dplyr::select(WATERSH, GNIS_NA,
                  inf_to_bc_kmns_bin = TotalInspections_kmeans_bin)

  bc_wd_bins <- insp_from_wd_bc_wb |>
    sf::st_drop_geometry() |>
    dplyr::select(WATERSH, GNIS_NA,
                  bc_wd_kmns_bin = TotalInspections_kmeans_bin)

  ang_bins <- ang |>
    sf::st_drop_geometry() |>
    dplyr::rename(WATERSH = WATERSHED_GROUP_ID,
                  GNIS_NA = Waterbody) |>
    dplyr::select(WATERSH, GNIS_NA,
                  ang_kmns_bin = days_fished_kmeans_bin)

  # Join and calculate total priority per waterbody
  priority_table <- inf_to_bc_bins |>
    dplyr::full_join(bc_wd_bins, by = c("WATERSH", "GNIS_NA")) |>
    dplyr::full_join(ang_bins, by = c("WATERSH", "GNIS_NA")) |>
    dplyr::mutate(across(c(inf_to_bc_kmns_bin, bc_wd_kmns_bin, ang_kmns_bin), ~as.numeric(as.character(.)))) |>
    dplyr::mutate(across(c(inf_to_bc_kmns_bin, bc_wd_kmns_bin, ang_kmns_bin), ~tidyr::replace_na(., 0))) |>
    dplyr::mutate(total_priority_kmeans = inf_to_bc_kmns_bin + bc_wd_kmns_bin + ang_kmns_bin,
                  k = k)  # Add k value for tracking

  # Store the full table for this k
  priority_tables_by_k[[paste0("k_", k)]] <- priority_table
}


library(dplyr)
library(ggplot2)
library(purrr)

# Combine all priority tables into one long data frame
priority_long <- bind_rows(priority_tables_by_k, .id = "k_label") |>
  mutate(k = as.integer(gsub("k_", "", k_label))) |>
  mutate(Waterbody = paste(WATERSH, GNIS_NA, sep = " - "))

library(reshape2)

heatmap_data <- priority_long |>
  select(Waterbody, k, total_priority_kmeans) |>
  pivot_wider(names_from = k, values_from = total_priority_kmeans)

# Convert to matrix and plot
heatmap_matrix <- as.matrix(heatmap_data[,-1])
rownames(heatmap_matrix) <- heatmap_data$Waterbody

heatmap(heatmap_matrix, Rowv = NA, Colv = NA,
        scale = "none", col = heat.colors(256),
        main = "Heatmap of Model Ranking Scores by k")

