#' Dot product
#'
#' @param u Numeric vector
#' @param v Numeric vector
#'
#' @return Numeric dot product of `u` and `v`.
#'
dot <- function(u, v) {
  return(sum(u * v))
}


#' Radians to degrees
#'
#' @param r Numeric vector of radians
#'
#' @return Vector with r converted to degrees
#'
rad2deg <- function(r) {
  return(r * 180 / pi)
}


#' Convert wide data to (x, y, z) rows
#'
#' @param d Data.frame with points in wide format
#'
#' @return Data.frame with (x, y, z) points as rows 1, 2, and 3
#'
make_xyz_matrix <- function(d) {
  n_rows <- 3
  n_col <- ncol(d) / 3
  return(matrix(as.numeric(d), nrow = n_rows, ncol = n_col, byrow = FALSE))
}


