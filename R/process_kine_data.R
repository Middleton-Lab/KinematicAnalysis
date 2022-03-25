#' Process kinematic data
#'
#' @param Trial_Info data.frame with information about trials
#' @param ii integer index for the row of Trial_Info to analyze
#' @param calibration_coords Coordinates for calibration object
#' @param bef_aft String for "Before" or "After"
#'
#' @export
#'
process_kine_data <- function(Trial_Info, ii, calibration_coords, bef_aft) {

  # Set the pixel to mm conversion
  conv <- if_else(bef_aft == "Before", 0.1004758, 0.09963728)

  # Extract Date, ID, and Trial number
  Date <- paste(Trial_Info$Year[ii], Trial_Info$Month[ii],
                Trial_Info$Day[ii], sep = "-")
  ID <- Trial_Info$MouseID[ii]
  Tr <- Trial_Info$Trial[ii]

  # Data file name
  f <- system.file("extdata",
                   paste0(Date, "_", ID, "_T", Tr, "_josh.csv"),
                   package = "KinematicAnalysis")

  # Start and end frames from trial info spreadsheet
  frame_start <- Trial_Info |>
    dplyr::filter(MouseID == ID, Trial == Tr) |>
    pull(Start_frame)
  frame_end <- Trial_Info |>
    dplyr::filter(MouseID == ID, Trial == Tr) |>
    pull(End_frame)

  # Load raw data
  M_raw <- read_xyz(f, frame_start, frame_end, conv)
  M_xyz <- M_raw |>
    dplyr::select(-time, -frame)

  # Find origin
  origin <- calibration_coords |>
    dplyr::select(starts_with("Intersection")) |>
    as.numeric()

  # Translate calibration coordinates
  cal_mat <- matrix(as.numeric(calibration_coords), ncol = 3, byrow = TRUE) |>
    as.data.frame()
  rownames(cal_mat) <- names(calibration_coords) |>
    str_remove(c("_x", "_y", "_z")) |>
    unique()
  names(cal_mat) <- c("x", "y", "z")
  cal_mat$x <- cal_mat$x - origin[1]
  cal_mat$y <- cal_mat$y - origin[2]
  cal_mat$z <- cal_mat$z - origin[3]

  # Translate XYZ points
  n <- ncol(M_xyz)
  x_cols <- seq(1, n, by = 3)
  y_cols <- seq(2, n, by = 3)
  z_cols <- seq(3, n, by = 3)

  M_xyz[, x_cols] <- M_xyz[, x_cols] - origin[1]
  M_xyz[, y_cols] <- M_xyz[, y_cols] - origin[2]
  M_xyz[, z_cols] <- M_xyz[, z_cols] - origin[3]

  # 3d rotation ##############################################################
  # Rotation about z = rotation so y lines up with origin
  # "Right_Line" point
  Right_Line <- cal_mat["Right_Line", ] |>
    as.numeric()
  Right_Line_zero <- c(Right_Line[1],
                       0,
                       Right_Line[3]) |>
    as.numeric()

  # Angle between (origin -> Right_Line) and (origin -> Right_line_zero)
  theta <- vector_angle(Right_Line, Right_Line_zero)

  # z-axis rotation matrix
  rot_mat_z <- xyz_rotation_matrix(theta, axis = "z")

  # Rotate points about x-axis
  cal_mat_z <- rot_mat_z %*% t(cal_mat) |>
    t()

  # Rotate M_xyz around z landmark by landmark
  M_xyz_z <- rotate_landmark_points(M_xyz, rot_mat_z)

  # Rotate about x: "Bottom Line"
  Bottom_Line <- cal_mat_z["Bottom_Line", ] |>
    as.numeric()
  Bottom_Line_zero <- c(0,
                        0,
                        Bottom_Line[3]) |>
    as.numeric()

  theta <- vector_angle(Bottom_Line, Bottom_Line_zero)
  if (Bottom_Line[3] < 0) theta <- -1 * theta
  rot_mat_x <- xyz_rotation_matrix(theta, axis = "x")
  cal_mat_zx <- rot_mat_x %*% t(cal_mat_z) |>
    t()
  M_xyz_zx <- rotate_landmark_points(M_xyz_z, rot_mat_x)

  # Rotate about y
  Bottom_Line <- cal_mat_zx["Bottom_Line", ] |>
    as.numeric()
  Bottom_Line_zero <- c(0,
                        Bottom_Line[2],
                        Bottom_Line[3]) |>
    as.numeric()
  theta <- vector_angle(Bottom_Line, Bottom_Line_zero)
  rot_mat_y <- xyz_rotation_matrix(theta, axis = "y")
  cal_mat_zxy <- rot_mat_y %*% t(cal_mat_zx) |>
    t()
  M_xyz_zxy <- rotate_landmark_points(M_xyz_zx, rot_mat_y)

  # 180 degree rotation around z
  rot_mat_z <- xyz_rotation_matrix(theta = pi, "z")
  cal_mat_zxy <- rot_mat_z %*% t(cal_mat_zxy) |>
    t()
  M_xyz_zxy <- rotate_landmark_points(M_xyz_zxy, rot_mat_z)

  # One last rotation about z
  # Average the y value from the 4 floor points
  dy <- mean(abs(cal_mat_zxy[5:8, 2]))
  v1 <- c(cal_mat_zxy["Right_Back", 1], 0, 0)
  v2 <- c(cal_mat_zxy["Right_Back", 1], dy, 0)
  theta <- vector_angle(v1, v2) * -1 # Rotate counterclockwise
  rot_mat_z <- xyz_rotation_matrix(theta, "z")
  cal_mat_zxyz <- rot_mat_z %*% t(cal_mat_zxy) |>
    t()
  M_xyz_zxyz <- rotate_landmark_points(M_xyz_zxy, rot_mat_z)


  # Convert cal_mat back into a data.frame
  cal_mat_zxyz <- as.data.frame(cal_mat_zxyz)
  names(cal_mat_zxyz) <- c("x", "y", "z")

  # Reattach time and frame
  M_xyz_zxyz <- M_xyz_zxyz |>
    mutate(time = M_raw$time,
           frame = M_raw$frame)

  return(list(M = M_xyz_zxyz, cal_rotate = cal_mat_zxyz))
}
