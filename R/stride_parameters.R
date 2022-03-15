#' Calculate stride parameters
#'
#' @param ii integer: Row index of Trials_Info to process
#' @param M data.frame: Coordinates for markers
#'
#' @return list
#' \enumerate{
#'   \item \code{stride_frequency} numeric: mean stride frequency
#'   \item \code{mean_stride_length} numeric: mean stride length
#' }
#'
#' @export
#'
stride_parameters <- function(ii, M) {
  ti <- Trial_Info %>% dplyr::slice(ii)

  # Find rows corresponding to footfalls
  # Extract footfall frames in original timing
  # Remove NAs
  # Subtract Start_time to get row number
  ff <- ti %>% select(starts_with("Footfall")) %>% as.numeric()
  ff <- ff[!is.na(ff)]
  ff <- ff - ti$Start_frame

  # Only analyze if 2 or more footfalls
  if (length(ff) > 1) {
    ###
    # Stride frequency
    # Convert frames to time after Start_frame
    ff_times <- ff * 1 / 120
    total_time <- diff(range(ff_times))
    total_strides <- length(ff) - 1
    stride_frequency <- total_strides / total_time

    ###
    # Stride length
    # xyz coordinates at footfalls
    ff_xyz <- M %>% dplyr::slice(ff) %>%
      dplyr::select(starts_with("foot"))

    # Calculate distance between sequential foot points
    d <- numeric(length = (nrow(ff_xyz) - 1))
    for (jj in 2:nrow(ff_xyz)) {
      d[jj - 1] <- dist_3d(ff_xyz[jj, ], ff_xyz[jj - 1, ])
    }
    mean_stride_length <- mean(d)
  } else {
    stride_frequency <- NA
    mean_stride_length <- NA
  }
  return(list(stride_frequency = stride_frequency,
              mean_stride_length = mean_stride_length))
}
