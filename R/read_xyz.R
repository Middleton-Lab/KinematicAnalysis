#' Read in 3d points from DeepLabCut
#'
#' @param f String of file name to read
#' @param frame_start Integer of frame to start reading from
#' @param frame_end Integer of frame to end reading from
#' @param conv Numeric value for conversion from pixels to mm
#'
#' @return Tibble with time and (x, y, z) coordinates
#' @export
#'
read_xyz <- function(f, frame_start, frame_end, conv) {
  M <- suppressWarnings(read_csv(file = f, skip = 1,
                                 col_types = "ccccccccccccccccccc")) %>%
    dplyr::slice(-1)
  names(M) <- c("frame",
                paste(rep(c("eye", "sacrum", "hip", "knee", "ankle", "foot"),
                          each = 3),
                      c("x", "y", "z"), sep = "_"))
  M <- M %>% dplyr::slice(frame_start:frame_end)
  M <- apply(M, MARGIN = 2, as.numeric)

  # Convert to world units
  M[, -1] <- apply(M[, -1], MARGIN = c(1, 2), function(x) {x / conv})

  # Translate frame to time
  t <- (M[, 1] - min(M[, 1])) * (1 / 120)

  return(bind_cols(time = t, as_tibble(M)))
}
