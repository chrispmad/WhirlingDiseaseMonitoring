bin_to_kmeans <- function(dat, variable, centers = k_breaks) {
  has_geometry <- inherits(dat, "sf")
  # Store and drop geometry if present
  if (has_geometry) {
    geometry <- sf::st_geometry(dat)
    dat_df <- sf::st_drop_geometry(dat)
  } else {
    dat_df <- dat
  }

  # Identify valid rows
  valid_rows <- is.finite(dat_df[[variable]])
  dat_df_valid <- dat_df[valid_rows, ]

  # Run k-means clustering
  set.seed(123)
  km <- stats::kmeans(dat_df_valid[[variable]], centers = centers, iter.max = 100)

  # Reorder cluster labels by mean value
  cluster_means <- tapply(dat_df_valid[[variable]], km$cluster, mean)
  ordered_clusters <- order(cluster_means)
  relabeled_clusters <- match(km$cluster, ordered_clusters)

  # Assign bins back to full dataset
  dat_df$var_bin <- NA
  dat_df$var_bin[valid_rows] <- relabeled_clusters

  # Final formatting
  dat_df <- dat_df |>
    dplyr::rename(var_to_bin = !!rlang::sym(variable)) |>
    dplyr::mutate(var_bin = as.character(var_bin)) |>
    dplyr::mutate(var_bin = factor(var_bin, levels = as.character(1:centers))) |>
    dplyr::rename(
      !!rlang::sym(paste0(variable, "_kmeans_bin")) := var_bin,
      !!rlang::sym(variable) := var_to_bin
    )

  # Reattach geometry if present
  # Reattach geometry if present
  if (has_geometry) {
    sf::st_geometry(dat_df) <- geometry
    dat_df <- sf::st_as_sf(dat_df)
  }


  return(dat_df)
}
