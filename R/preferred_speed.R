#' Calculate preferred speed
#'
#' @param M data.frame: Coordinates for markers
#'
#' @return numeric: Mean forward velocity
#'
#' @export
#'
preferred_speed <- function(M) {
  d_time <- max(diff(M$time))

  # Extract sacrum points
  sacrum <- M %>% select(starts_with("sacrum")) %>%
    drop_na()

  # Only calculate if there are 2 sacrum points
  if (nrow(sacrum) > 1) {
    d <- numeric(length = (nrow(sacrum) - 1))
    for (jj in 2:nrow(sacrum)) {
      d[jj - 1] <- dist_3d(sacrum[jj, ], sacrum[jj - 1, ])
    }

    # This seems fast (~0.486 m/s; 0.135 km/h -- Is that reasonable?)
    speed <- d / d_time
    mean_speed <- mean(speed)
  } else {
    message("No sacrum points found.")
    mean_speed <- NA
  }
  return(mean_speed)
}
