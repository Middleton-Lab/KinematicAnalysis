#' Calculate duty factor
#'
#' @param M data.frame:
#' @param thresh numeric: Threshold for considering the foot off or on the substrate
#' @param plot_strides boolean: Plot the strides
#'
#' @return list
#' \enumerate{
#'   \item \code{n_strides_df} data.frame: number of strides
#'   \item \code{duty_factor} numeric: mean duty factor
#' }
#'
#' @export
#'
duty_factor_calc <- function(M, thresh, plot_strides = FALSE) {
  foot <- M |>
    dplyr::select(starts_with("foot"))

  foot_z <- tibble::tibble(
    dz = diff(foot$foot_z),
    on_off = if_else(dz > thresh, 1, 0),
    time = 1 / 120 * seq_len(length(dz)),
    frame = seq_len(length(dz)))
  if (plot_strides) {
    ggplot(foot_z, aes(x = frame, y = dz, color = factor(on_off))) +
      geom_point()
  }

  # foot_z contains partial footfalls. Find the first whole stride.
  # Get index for first on and off. If off < on, then the foot is off
  # at the start of the recording.
  # Figure out if the foot starts out on or off. Then cut out all frames
  # before the other condition.

  min_on <- min(which(foot_z$on_off == 0))
  min_off <- min(which(foot_z$on_off == 1))

  if (min_off < min_on) { # foot starts out off
    # Remove everything before first "on"
    foot_z <- foot_z |>
      slice(-(seq(1, min_on - 1, 1)))

    # 1 = on to off; -1 = off to on
    diff_z <- c(0, diff(foot_z$on_off))
    max_off_to_on <- max(which(diff_z == -1))
    foot_z <- foot_z  |>
      slice(1:(max_off_to_on - 1))
    n_strides_df <- sum(diff(foot_z$on_off) == -1) + 1
    duty_factor <- sum(foot_z$on_off == 0) / nrow(foot_z)
  } else { # foot starts out on
    # Remove everything before first "off"
    foot_z <- foot_z |>
      slice(-(seq(1, min_off - 1, 1)))

    # 1 = on to off; -1 = off to on
    diff_z <- c(0, diff(foot_z$on_off))
    if (any(diff_z == 1, na.rm = TRUE)) {
      max_on_to_off <- max(which(diff_z == 1))
      foot_z <- foot_z |>
        slice(1:(max_on_to_off - 1))

      n_strides_df <- sum(diff(foot_z$on_off) == 1) + 1
      duty_factor <- sum(foot_z$on_off == 0) / nrow(foot_z)
    } else { # There are missing frames
      n_strides_df <- NA
      duty_factor <- NA
    }
  }
  return(list(n_strides_df = n_strides_df, duty_factor = duty_factor))
}
