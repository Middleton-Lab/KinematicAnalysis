#' Read in 3d points from DeepLabCut
#'
#' @param f
#' @param frame_start
#' @param frame_end
#' @param conv
#'
#' @return
#' @export
#'
read_xyz <- function(f, frame_start, frame_end, conv = 0.1004758) {
  M <- suppressWarnings(read_csv(file = f, skip = 1,
                                 col_types = "ccccccccccccccccccc")) %>%
    slice(-1)
  names(M) <- c("frame",
                paste(rep(c("eye", "sacrum", "hip", "knee", "ankle", "foot"),
                          each = 3),
                      c("x", "y", "z"), sep = "_"))
  M <- M %>% slice((frame_start + 1):(frame_end + 1))
  M <- apply(M, MARGIN = 2, as.numeric)

  # Convert to world units
  M[, -1] <- apply(M[, -1], MARGIN = c(1, 2), function(x) {x / conv})

  # Translate frame to time
  t <- (M[, 1] - min(M[, 1])) * (1 / 120)

  return(bind_cols(time = t, as_tibble(M)))
}


#' Translate points
#'
#' @param d
#' @param origin
#'
#' @return
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


#' Calculate Rodrigues rotation matrix
#'
#' @param v_zero
#' @param v_axis
#' @param verbose
#'
#' @return
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


#' Calculate rotation matrices for a set of calibration coordinates
#'
#' @param cal_coords
#'
#' @return
#' @export
#'
rotation_matrices <- function(cal_coords) {
  # Find origin
  origin <- cal_coords %>%
    dplyr::select(starts_with("Intersection")) %>%
    as.numeric()

  # Move points to origin
  cal_coords <- translate_points(cal_coords, origin)

  # Get vertical line and x-axis vectors
  v_zero <- cal_coords %>%
    dplyr::select(starts_with("Top_Line")) %>%
    as.numeric()
  v_axis <- cal_coords %>%
    dplyr::select(starts_with("Right_Line")) %>%
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


#' Rotate a wide set of points
#'
#' @param x
#' @param Rx
#' @param Ry
#'
#' @return
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


#' Plot point in 3D with plotly
#'
#' @param M
#' @param cal_rotate
#'
#' @return
#' @export
#'
plot3d <- function(M, cal_rotate) {
  axx <- list(range = c(-150, 150))
  scene <- list(camera = list(eye = list(x = 0, y = 0, z = 0),
                              up = list(x = 0, y = 1, z = 0),
                              projection = "orthographic",
                              aspectmode = "cube"),
                xaxis = axx, yaxis = axx, zaxis = axx)

  d <- data.frame(
    x = as.numeric(cal_rotate[1, ]),
    y = as.numeric(cal_rotate[2, ]),
    z = as.numeric(cal_rotate[3, ]))

  plot_ly() %>%
    add_markers(x = ~x, y = ~y, z = ~z, data = d,
                name = "back_wall",
                size = 1) %>%
    add_paths(x = ~eye_x, y = ~eye_y, z = ~eye_z,
              data = M, name = "eye") %>%
    add_paths(x = ~sacrum_x, y = ~sacrum_y, z = ~sacrum_z,
              data = M, name = "sacrum") %>%
    add_paths(x = ~hip_x, y = ~hip_y, z = ~hip_z,
              data = M, name = "hip") %>%
    add_paths(x = ~knee_x, y = ~knee_y, z = ~knee_z,
              data = M, name = "knee") %>%
    add_paths(x = ~ankle_x, y = ~ankle_y, z = ~ankle_z,
              data = M, name = "ankle") %>%
    add_paths(x = ~foot_x, y = ~foot_y, z = ~foot_z,
              data = M, name = "foot") %>%
    layout(scene = scene)
}

