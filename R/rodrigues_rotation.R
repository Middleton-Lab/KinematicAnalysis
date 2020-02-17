#' Calculate Rodrigues rotation matrix
#'
#' @param v_zero Numeric vector of axis to zero
#' @param v_axis Numeric vector of axis to rotate about
#' @param verbose Logical indicating whether to print message about rotation
#' angle
#'
#' @return Matrix of Rodrigues rotation parameters
#' @export
#'
rodrigues_rotation <- function(v_zero, v_axis, verbose = FALSE) {
  # Length of line from origin to v_zero
  v_zero_L <- sqrt(dot(v_zero, v_zero))

  # Coordinates of line from origin to length(v_zero)
  v_zero_rot <- c(0, v_zero_L, 0)

  # Angle between v_zero and v_zero_rot
  Rot_Angle <- acos(dot(v_zero, v_zero_rot) / v_zero_L ^ 2)
  if (verbose) message("Rotation angle: ",
                       round(rad2deg(Rot_Angle), 2))

  # Rotate all points around vector from origin to v_axis by Rot_Angle
  v_axis_L <- sqrt(dot(v_axis, v_axis))
  v_axis_unit <- v_axis / v_axis_L

  W <- matrix(c(0, -1 * v_axis_unit[3], v_axis_unit[2],
                v_axis_unit[3], 0, -1 * v_axis_unit[1],
                -1 * v_axis_unit[2], v_axis_unit[1], 0),
              nrow = 3, ncol = 3, byrow = TRUE)
  eye <- diag(c(1, 1, 1))

  R <- eye + sin(Rot_Angle) * W + (1 - cos(Rot_Angle)) * W %*% W
  return(R)
}
