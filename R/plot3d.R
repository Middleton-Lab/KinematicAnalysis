#' Plot point in 3D with plotly
#'
#' @param M Data.frame of points in wide format
#' @param cal_rotate Calibration points
#'
#' @export
#'
plot3d <- function(M, cal_rotate) {
  axx <- list(range = c(-150, 150))
  axy <- list(range = c(-50, 50))
  scene <- list(camera = list(eye = list(x = 0, y = 0, z = 0),
                              up = list(x = 0, y = 0, z = 0),
                              projection = "orthographic",
                              aspectmode = "cube"),
                xaxis = axx,
                yaxis = axy,
                zaxis = axx)

  # d <- data.frame(
  #   x = as.numeric(cal_rotate[1, ]),
  #   y = as.numeric(cal_rotate[2, ]),
  #   z = as.numeric(cal_rotate[3, ]))

  floor_pts <- cal_rotate[c(5:8, 9, 11, 12), ]
  wall_pts <- cal_rotate[-c(5:8, 9, 11, 12), ]

  plot_ly() %>%
    add_markers(x = ~x, y = ~y, z = ~z, data = floor_pts,
                name = "Floor",
                marker = list(size = 3, color = "red")) %>%
    add_markers(x = ~x, y = ~y, z = ~z, data = wall_pts,
                name = "Wall",
                marker = list(size = 3, color = "blue")) %>%
    add_paths(x = ~eye_x, y = ~eye_y, z = ~eye_z,
              data = M, name = "Eye") %>%
    add_paths(x = ~sacrum_x, y = ~sacrum_y, z = ~sacrum_z,
              data = M, name = "Tail") %>%
    add_paths(x = ~hip_x, y = ~hip_y, z = ~hip_z,
              data = M, name = "Hip") %>%
    add_paths(x = ~knee_x, y = ~knee_y, z = ~knee_z,
              data = M, name = "Knee") %>%
    add_paths(x = ~ankle_x, y = ~ankle_y, z = ~ankle_z,
              data = M, name = "Ankle") %>%
    add_paths(x = ~foot_x, y = ~foot_y, z = ~foot_z,
              data = M, name = "Foot") %>%
    layout(scene = scene)
}

