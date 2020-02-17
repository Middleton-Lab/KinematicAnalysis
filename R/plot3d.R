#' Plot point in 3D with plotly
#'
#' @param M Data.frame of points in wide format
#' @param cal_rotate Calibration points
#'
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

