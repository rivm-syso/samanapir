% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetDataSamenmeten.R
\name{extract_coord}
\alias{extract_coord}
\title{Extract coordinates from API result}
\usage{
extract_coord(x)
}
\arguments{
\item{x}{}
}
\value{
dataframe with lat and lon as columns
}
\description{
Helper function (using lapply) of the function: GetSamenMetenAPIinfo
}
\details{
In the return of the SamenMeten API the coordinates are nested in lists,
this function extract the coordinates and checks if there are coordinates.
If no coordinates are available 0,0 is returned as coordinates.
}
