% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotlyGraphs.R
\name{plotlyCompile}
\alias{plotlyCompile}
\title{Compilation of HTML/plotly document}
\usage{
plotlyCompile(
  reportFile = "tmp.html",
  libFile = "path/to/file/plotly.min.js",
  lightWeight = F,
  css = "",
  font = "",
  name = "",
  reopen = F,
  debug = F
)
}
\arguments{
\item{all}{optional.}
}
\value{
none, modifies the given HTML template file.
}
\description{
Compilation of HTML/plotly document
}
\examples{
plotlyCompile()
}
