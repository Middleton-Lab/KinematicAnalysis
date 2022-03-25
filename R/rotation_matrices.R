#' Calculate rotation matrices for a set of calibration coordinates
#'
#' @param cal_coords Coordinates of calibration object in wide format.
#'
#' @return List containing:
#' `Rx` and `Ry` Rodrigues rotation matrices to zero about the x and y axes
#' `cal_rot` Calibration points rotated about x and y
#'
#' @export
#'
rotation_matrices <- function(cal_coords) {
  # Find origin
  origin <- cal_coords |>
    dplyr::select(starts_with("Intersection")) |>
    as.numeric()

  # Move points to origin
  cal_coords <- translate_points(cal_coords, origin)

  # Get vertical line and x-axis vectors
  v_zero <- cal_coords |>
    dplyr::select(starts_with("Top_Line")) |>
    as.numeric()
  v_axis <- cal_coords |>
    dplyr::select(starts_with("Right_Line")) |>
    as.numeric()

  # Rotate around x-axis to line up z-axis with origin
  Rx <- rodrigues_rotation(v_zero = v_zero, v_axis = v_axis)
  cal_rotx <- Rx %*% make_xyz_matrix(cal_coords)

  # Rotate about y-axis to line up x-axis
  Ry <- rodrigues_rotation(v_zero = as.numeric(cal_rotx[, 11]),
                           v_axis = as.numeric(cal_rotx[, 10]))
  cal_rot <- Ry %*% cal_rotx
  return(list(Rx = Rx, Ry = Ry, cal_rot = cal_rot))
}
