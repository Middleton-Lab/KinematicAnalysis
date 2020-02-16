#' Dot product
#'
#' @param u
#' @param v
#'
#' @return
#'
dot <- function(u, v) {
  return(sum(u * v))
}


#' Radians to degrees
#'
#' @param r numeric vector of radians
#'
#' @return vector with r converted to degrees
#'
rad2deg <- function(r) {
  return(r * 180 / pi)
}


#' Convert wide data to (x, y, z) rows
#'
#' @param d
#'
#' @return
#'
make_xyz_matrix <- function(d) {
  n_rows <- 3
  n_col <- ncol(d) / 3
  return(matrix(as.numeric(d), nrow = n_rows, ncol = n_col, byrow = FALSE))
}


