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
