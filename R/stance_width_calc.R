#' Calculate stance width
#'
#' @param ii integer: row of Trial_Info to process
#' @param M data.frame: strides to analyze
#'
#' @return list
#' \enumerate{
#'   \item \code{mean_stance_width} numeric: mean strance width
#'   \item \code{sacrum_vert_displacement} numeric: vertical displacement of the sacrum
#' }
#'
#' @export
#'
stance_width_calc <- function(ii, M) {
  # Initialize mean_stance_width, overwrite later if successful
  mean_stance_width <- NA

  # Initialize vertical sacrum movement
  sacrum_vert_displacement <- NA

  # Extract row from Trial_Info
  ti <- Trial_Info %>% dplyr::slice(ii)

  # Find rows corresponding to footfalls
  # Extract footfall frames in original timing
  # Remove NAs
  ff <- ti %>% select(starts_with("Footfall")) %>% as.numeric()
  ff <- ff[!is.na(ff)]

  # Extract points for sacrum, hip, foot
  d <- M %>%
    select(frame, starts_with("sacrum"), starts_with("hip"),
           starts_with("foot")) %>%
    drop_na()

  # Check that the extracted frames are contiguous
  if (!all(diff(d$frame) == 1)) {
    message("Noncontiguous frames. Skipping stance width for trial.")
  } else {

    if (nrow(d) >= 2) {

      d$row_num <- 1:nrow(d)

      # Calculate sacrum vertical displacement
      # Filter out points where sacrum is not tracked properly.
      # xxxFIXME Move this out of the loop
      d_sub <- d %>% filter(sacrum_y <= 30)
      sacrum_vert_displacement <- max(d_sub$sacrum_y) - min(d_sub$sacrum_y)

      # Calculate 3D distance from hip to foot --
      #   minimum distance should happen at mid-stance
      d <- by_row(.d = d,
                  ..f = function(x) {
                    dist_3d(c(x$hip_x, x$hip_y, x$hip_z),
                            c(x$foot_x, x$foot_y, x$foot_z))
                  }, .collate = "rows", .to = "d_hip_foot")

      # Search a range around footfalls for minimum (+/- 5 frames)
      mid_stance_frames <- tibble(
        ff = ff,
        ff_low = ff - 5,
        ff_hi = ff + 5
      )

      # Check that the first frame in d is greater than the first
      # mid-stance frame
      if (min(mid_stance_frames$ff_low) < min(d$frame)) {
        mid_stance_frames <- dplyr::slice(mid_stance_frames, -1)
      }

      # Yields frames at mid-stance
      min_d_hip_foot <- invoke_rows(
        .d = mid_stance_frames,
        .f = function(ff, ff_low, ff_hi, d) {
          d_sub <- d %>% filter(frame >= ff_low, frame <= ff_hi)
          (min_d_hip_foot <- min(d_sub$d_hip_foot))
          (min_frame <- d_sub$frame[which.min(d_sub$d_hip_foot)])
          (min_row <- d_sub$row_num[which.min(d_sub$d_hip_foot)])
          return(c(min_d_hip_foot, min_frame, min_row))
        },
        d = d,
        .collate = "cols") %>%
        dplyr::rename(min_d_hip_foot = `.out1`,
                      min_frame = `.out2`,
                      min_row = `.out3`)

      # ggplot() +
      #   geom_vline(xintercept = ff, color = "red") +
      #   geom_point(data = d, aes(frame, d_hip_foot), size = 0.5) +
      #   geom_point(data = min_d_hip_foot, aes(x = min_frame, y = min_d_hip_foot),
      #              color = "blue", size = 1)

      # Calculate x (mediolateral) component of 3D vector: sacrum to foot
      #   then double the width to get stance width.
      # Subset rows matching min_d_hip_foot
      ms <- d %>% dplyr::slice(min_d_hip_foot$min_row)

      ms <- by_row(.d = ms,
                   ..f = function(x) {
                     # Vectors of hip and foot points
                     sacrum <- x %>% dplyr::select(starts_with("sacrum")) %>%
                       as.numeric()
                     foot <- x %>% dplyr::select(starts_with("foot")) %>%
                       as.numeric()

                     # Move sacrum to origin (subtract sacrum from foot)
                     foot_zero <- foot - sacrum
                     half_stance_width <- foot_zero[1]
                     return(stance_width = 2 * half_stance_width)
                   }, .collate = "rows", .to = "stance_width")
      mean_stance_width <- mean(ms$stance_width)
    }
  }

  # Manually drop sacrum points
  if (!is.na(ti$Drop_Sacrum)) sacrum_vert_displacement <- NA

  return(list(mean_stance_width = mean_stance_width,
              sacrum_vert_displacement = sacrum_vert_displacement))
}
