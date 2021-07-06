library(plotly)

d <- data.frame(x = 0, y = 0, z = 0)

#d <- data.frame(cal_mat_zxy)
names(d) <- c("x", "y", "z")

axx <- list(range = c(-150, 150))
axy <- list(range = c(-10, 50))
scene <- list(camera = list(eye = list(x = 1, y = 0, z = 0),
                            up = list(x = 0, y = 1, z = 0),
                            projection = "orthographic",
                            aspectmode = "cube"),
              xaxis = axx,
              yaxis = axy,
              zaxis = axx)


origin_pts <- d[9:12, ]
floor_pts <- d[c(5:8), ]
wall_pts <- d[c(1:4), ]

plot_ly() %>%
  add_markers(x = ~x, y = ~y, z = ~z, data = wall_pts,
              name = "Wall",
              marker = list(size = 3, color = "blue")) %>%
  add_markers(x = ~x, y = ~y, z = ~z, data = origin_pts,
              name = "Origin",
              marker = list(size = 3, color = "purple")) %>%
  add_markers(x = ~x, y = ~y, z = ~z, data = floor_pts,
              name = "Floor",
              marker = list(size = 3, color = "red"))
