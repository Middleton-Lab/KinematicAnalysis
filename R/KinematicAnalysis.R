#' KinematicAnalysis
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab KinematicAnalysis\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.1\cr
#'               Date: \tab 2020-02-15\cr
#'               License: \tab MIT\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name KinematicAnalysis-package
#' @aliases KinematicAnalysis-package KinematicAnalysis
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#' @import tidyverse plotly zeallot
#' @importFrom readxl read_excel
#' @importFrom dplyr select starts_with pull as_tibble bind_cols if_else
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom stringr str_remove
NULL

# Ignore some global variables
utils::globalVariables(c("End_frame",
                         "MouseID",
                         "Rx",
                         "Ry",
                         "Start_frame",
                         "Trial",
                         "cal_rotate",
                         "frame",
                         "time",
                         "."))
