#' Process video file
#'
#' @param ii integer: row od \code{Trial_Info} to process
#' @param Trial_Info data.frame: Structure containing all the trials to process
#'
#' @return data.frame
#' \enumerate{
#'     \item \code{MouseID}
#'     \item \code{Trial}
#'     \item \code{Before_After}
#'     \item \code{mean_speed}
#'     \item \code{stride_frequency}
#'     \item \code{mean_stride_length}
#'     \item \code{n_strides_df}
#'     \item \code{duty_factor}
#'     \item \code{stance_width}
#'     \item \code{sacrum_vert_displacement}
#' }
#'
#' @export
#'
process_video <- function(ii, Trial_Info, bef_aft, calibration_coords) {
  # M is tibble of xyz points for eye, sacrum, hip, knee, ankle, foot with
  # time and frame number
  # cal_rotate is the rotated calibration points
  message(ii)
  c(M, cal_rotate) %<-% process_kine_data(Trial_Info,
                                          ii,
                                          calibration_coords,
                                          bef_aft)

  # Calculate variables of interest
  mean_speed <- preferred_speed(M)
  strides <- stride_parameters(ii, M)
  duty_factor <- duty_factor_calc(M, thresh = 0.7)
  stance_width <- stance_width_calc(ii, M)

  return(data.frame(
    MouseID = Trial_Info$MouseID[ii],
    Trial = Trial_Info$Trial[ii],
    Before_After = Trial_Info$Before_After[ii],
    mean_speed = mean_speed,
    stride_frequency = strides$stride_frequency,
    mean_stride_length = strides$mean_stride_length,
    n_strides_df = duty_factor$n_strides_df,
    duty_factor = duty_factor$duty_factor,
    stance_width = stance_width$mean_stance_width,
    sacrum_vert_displacement = stance_width$sacrum_vert_displacement))
}

