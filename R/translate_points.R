#' Translate points
#'
#' @param d Data.frame with points as columns (e.g., V1_x, V1_y, V1_z, V2_x,
#' V2_y, V2_z...)
#' @param origin Numeric vector with coordinates to translate points `d` to.
#'
#' @return Data.frame with points as columns translated.
#' @export
#'
translate_points <- function(d, origin) {
  n <- ncol(d)
  x_cols <- seq(1, n, by = 3)
  y_cols <- seq(2, n, by = 3)
  z_cols <- seq(3, n, by = 3)

  d[, x_cols] <- d[, x_cols] - origin[1]
  d[, y_cols] <- d[, y_cols] - origin[2]
  d[, z_cols] <- d[, z_cols] - origin[3]
  return((d))
}
