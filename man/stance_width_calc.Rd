% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stance_width_calc.R
\name{stance_width_calc}
\alias{stance_width_calc}
\title{Calculate stance width}
\usage{
stance_width_calc(ii, M)
}
\arguments{
\item{ii}{integer: row of Trial_Info to process}

\item{M}{data.frame: strides to analyze}
}
\value{
list
\enumerate{
  \item \code{mean_stance_width} numeric: mean strance width
  \item \code{sacrum_vert_displacement} numeric: vertical displacement of the sacrum
}
}
\description{
Calculate stance width
}
