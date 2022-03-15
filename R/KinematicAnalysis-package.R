#' KinematicAnalysis
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab KinematicAnalysis\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.2.0\cr
#'               Date: \tab 2022-03-15\cr
#'               License: \tab MIT\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name KinematicAnalysis-package
#' @aliases KinematicAnalysis-package KinematicAnalysis
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package

## usethis namespace: start
#' @import tidyverse plotly zeallot
#' @importFrom dplyr select starts_with pull as_tibble bind_cols if_else
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %<>%
#' @importFrom purrr map map_dfr
#' @importFrom purrrlyr by_row
#' @importFrom purrrlyr invoke_rows
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
## usethis namespace: end

# Ignore some global variables
utils::globalVariables(c("End_frame",
                         "MouseID",
                         "Rx",
                         "Ry",
                         "Start_frame",
                         "Trial",
                         "cal_rotate",
                         "origin_pts",
                         "frame",
                         "time",
                         ".",
                         "dz",
                         ".out1",
                         ".out2",
                         ".out3"))
