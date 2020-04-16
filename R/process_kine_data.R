#' Process kinematic data
#'
#' @param Trial_Info data.frame with information about trials
#' @param ii integer index for the row of Trial_Info to analyze
#' @param cal_coords Corrdinates for calibration object
#'
#' @return List containing: `M` data.frame with points in wide format and
#' `cal_rotate` Rotated calibration points.
#'
#' @export
#'
process_kine_data <- function(Trial_Info, ii, cal_coords) {
  Date <- paste(Trial_Info$Year[ii], Trial_Info$Month[ii],
                Trial_Info$Day[ii], sep = "-")
  ID <- Trial_Info$MouseID[ii]
  Tr <- Trial_Info$Trial[ii]
  f <- paste0("../", Trial_Info$Base_Dir[ii], "/videos/",
              Date, "_", ID, "_T", Tr, "_josh.csv")

  message("Reading file: ", f)

  frame_start <- Trial_Info %>% filter(MouseID == ID, Trial == Tr) %>%
    pull(Start_frame)
  frame_end <- Trial_Info %>% filter(MouseID == ID, Trial == Tr) %>%
    pull(End_frame)

  M_raw <- read_xyz(f, frame_start, frame_end)
  M_xyz <- M_raw %>% dplyr::select(-time, -frame)

  # Translate to origin
  origin <- cal_coords %>% dplyr::select(starts_with("Intersection")) %>%
    as.numeric()
  M_xyz <- translate_points(M_xyz, origin)

  # 3d rotation
  c(Rx, Ry, cal_rotate) %<-% rotation_matrices(cal_coords)

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
