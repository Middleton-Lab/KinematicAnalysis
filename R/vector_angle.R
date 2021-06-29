#' Vector angle
#'
#' Use the Law of Cosines to calculate the 3d angle between two vectors.
#'
#' @param A numeric vector
#' @param B numeric vector
#'
#' @return angle between A and B
#' @export
#'
#' @examples
#' vector_angle(c(0, 0, 1), c(0, 1, 0))
#'
vector_angle <- function(A, B) {
  # (A^2 + B^2 - C^2) / 2AB = cos(theta)

  A_norm <- norm(A, type = "2")
  B_norm <- norm(B, type = "2")
  C_norm <- norm(A - B, type = "2")

  cos_theta <- (A_norm ^ 2 + B_norm ^ 2 - C_norm ^ 2) / (2 * A_norm * B_norm)
  theta <- acos(cos_theta)
  return(theta)
}
