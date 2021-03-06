% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fillGeneralSeedRate.R
\name{fillGeneralSeedRate}
\alias{fillGeneralSeedRate}
\title{Fill general seed rate}
\usage{
fillGeneralSeedRate(data, generalSeedData = getCountryGeneralSeedRate())
}
\arguments{
\item{data}{The data.table object containing the seed data, typically as
produced via getAreaData.}

\item{generalSeedData}{A data.table containing seeding data specific to
each commodity, typically as produced by getCountryGeneralSeedRate.}
}
\value{
No object is returned.  Instead, the underlying data object is
modified.
}
\description{
This function updates missing values of the Value_seedRate column in data.
If the value is missing, it is replaced with the commodity value.
}

