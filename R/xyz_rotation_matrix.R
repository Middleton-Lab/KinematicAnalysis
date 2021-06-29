#' Calculate rotation matrix
#'
#' For an given theta and axis for rotation, return the appropriate 3x3
#' rotation matrix.
#'
#' @param theta numeric angle in radians
#' @param axis character for axis of rotation, "x", "y", or "z"
#'
#' @return 3x3 rotation matrix
#' @export
#'
xyz_rotation_matrix <- function(theta, axis) {
  if (!(axis %in% c("x", "y", "z"))) stop("'axis' should be x, y, or z")

  if (axis == "x")
    rot_mat <- matrix(c(1,          0,           0,
                        0, cos(theta), -sin(theta),
                        0, sin(theta), cos(theta)),
                      byrow = TRUE,
                      nrow = 3, ncol = 3)

  if (axis == "y")
    rot_mat <- matrix(c( cos(theta), 0, sin(theta),
                         0,          1,          0,
                         -sin(theta), 0, cos(theta)),
                      byrow = TRUE,
                      nrow = 3, ncol = 3)

  if(axis == "z")
    rot_mat <- matrix(c(cos(theta), -sin(theta), 0,
                        sin(theta),  cos(theta), 0,
                        0,           0,         1),
                      byrow = TRUE,
                      nrow = 3, ncol = 3)
  return(rot_mat)
}
