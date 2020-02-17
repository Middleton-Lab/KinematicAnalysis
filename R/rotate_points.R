#' Rotate a wide set of points
#'
#' @param x Data.frame or matrix of points in wide form
#' @param Rx Rotation matrix for the x-axis
#' @param Ry Rotation matrix for the y-axis
#'
#' @return Data.frame with rotated points
#' @export
#'
rotate_points <- function(x, Rx, Ry) {
  # Convert wide points to (x, y, z) rows and multiply by x-axis rotation
  pts_rotx <- Rx %*% matrix(x, nrow = 3)

  # Rotate about y-axis
  pts_rot <- Ry %*% pts_rotx
  pts_rot <- matrix(pts_rot, nrow = 1)
  return(as.data.frame(pts_rot))
}
