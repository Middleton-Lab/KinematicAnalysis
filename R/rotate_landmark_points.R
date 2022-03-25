#' Rotate landmark points
#'
#' Given a data.frame of landmarks in x, y, z, x, y, z... configuration
#' and a rotation matrix, rotate the points using the matrix and
#' reconstruct the original format.
#'
#' Currently this function assumes that there will be points for "eye",
#' "sacrum", "hip", "knee", "ankle", and "foot".
#'
#' @param M data.frame of landmark points
#' @param rot_mat 3x3 numeric rotation matrix
#'
#' @return M points rotated using rot_mat
#' @export
#'
rotate_landmark_points <- function(M, rot_mat) {
  purrr::map(
    .x = c("eye", "sacrum", "hip", "knee", "ankle", "foot"),
    .f = function(lm_name, M) {
      M_sub <- M |>
        dplyr::select(starts_with(lm_name))
      M_sub_rot <- rot_mat %*% t(M_sub) |>
        t() |>
        as.data.frame()
      names(M_sub_rot) <- paste(lm_name, c("x", "y", "z"), sep = "_")
      return(M_sub_rot)
    }, M = M) |>
    do.call("cbind", .)
}
