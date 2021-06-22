#' Process kinematic data
#'
#' @param Trial_Info data.frame with information about trials
#' @param ii integer index for the row of Trial_Info to analyze
#' @param calibration_coords Coordinates for calibration object
#' @param bef_aft String for "Before" or "After"
#'
#' @return
#' @export
#'
process_kine_data <- function(Trial_Info, ii, calibration_coords, bef_aft) {

  conv <- if_else(bef_aft == "Before", 0.1004758, 0.09963728)

  Date <- paste(Trial_Info$Year[ii], Trial_Info$Month[ii],
                Trial_Info$Day[ii], sep = "-")
  ID <- Trial_Info$MouseID[ii]
  Tr <- Trial_Info$Trial[ii]

  # Data file name
  f <- system.file("extdata",
                   paste0(Date, "_", ID, "_T", Tr, "_josh.csv"),
                   package = "KinematicAnalysis")

  # Start and end frames from trial info spreadsheet
  frame_start <- Trial_Info %>% filter(MouseID == ID, Trial == Tr) %>%
    pull(Start_frame)
  frame_end <- Trial_Info %>% filter(MouseID == ID, Trial == Tr) %>%
    pull(End_frame)

  M_raw <- read_xyz(f, frame_start, frame_end, conv)
  M_xyz <- M_raw %>% dplyr::select(-time, -frame)

  # Translate to origin
  origin <- calibration_coords %>%
    dplyr::select(starts_with("Intersection")) %>%
    as.numeric()

  # Translate calibration coordinates
  cal_mat <- matrix(as.numeric(calibration_coords), ncol = 3, byrow = TRUE) %>%
    as.data.frame()
  rownames(cal_mat) <- names(calibration_coords) %>%
    str_remove(c("_x", "_y", "_z")) %>% unique()
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


  # 3d rotation
  c(Rx, Ry, cal_rotate) %<-% rotation_matrices(calibration_coords)

  M <- apply(as.matrix(M_xyz), MARGIN = 1,
             rotate_points, Rx = Rx, Ry = Ry) %>%
    purrr::map_dfr(., as.list)
  names(M) <- names(M_xyz)

  # Reattach time and frame
  M <- M %>%
    mutate(time = M_raw$time,
           frame = M_raw$frame)

  return(list(M = M, cal_rotate = cal_rotate))
}


