% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetDataSamenmeten.R
\name{GetSamenMetenAPIinfoMuni}
\alias{GetSamenMetenAPIinfoMuni}
\title{Get data Samen Meten API per Municipality}
\usage{
GetSamenMetenAPIinfoMuni(muni_code)
}
\arguments{
\item{muni_number}{string with the code of the municipality, for example '310'}
}
\value{
list with the info for each sensor in the municipality
}
\description{
This function will obtain the information of each sensor in a particular
municipality from the Samen Meten API. The name, location, closest reference stations,
measured components and the urls to the observations of each datastream.
}
\examples{
TEST <- GetSamenMetenAPIinfoMuni("330")
}
