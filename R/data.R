#' Spring floods in Eastern Australia
#'
#' A data frame containing flood occurrences during the
#' spring (September - October - November)
#' for 42 sites in Eastern Australia
#' over the period 1951-2014.
#' It has 64 rows (64 years from 1951 to 2014) and
#' 43 variables (the year + flood occurrence at each of the 42 stations).
#' 1 denote occurrences, 0 on-occurrences.
#'
#' @source \url{http://www.bom.gov.au/water/hrs/}
"springFlood"

#' Spring average flows in Eastern Australia
#'
#' A data frame containing average flows during the
#' spring (September - October - November)
#' for 42 sites in Eastern Australia
#' over the period 1951-2014.
#' It has 64 rows (64 years from 1951 to 2014) and
#' 43 variables (the year + average flow in m3/s at each of the 42 stations).
#'
#' @source \url{http://www.bom.gov.au/water/hrs/}
"springFlow"

#' Spring maximum flows in Eastern Australia
#'
#' A data frame containing maximum flows during the
#' spring (September - October - November)
#' for 42 sites in Eastern Australia
#' over the period 1951-2014.#'
#' It has 64 rows (64 years from 1951 to 2014) and
#' 43 variables (the year + maximum flow in m3/s at each of the 42 stations).
#'
#' @source \url{http://www.bom.gov.au/water/hrs/}
"springMax"

#' 42 Hydrometric stations in Eastern Australia
#'
#' A dataset describing 42 hydrometric stations in Eastern Australia
#'
#' @format A data frame with 42 rows and 5 variables:
#' \describe{
#'   \item{ID}{Identifier}
#'   \item{OzID}{Original identifier}
#'   \item{lon}{longitude (°)}
#'   \item{lat}{latitude (°)}
#'   \item{area}{catchment area (km2)}
#' }
#' @source \url{http://www.bom.gov.au/water/hrs/}
"OZstations"

#' Nino 3.4 spring index, 1951-2014
#'
#' A dataset containing standardized sea surface temperature anomalies
#' during the spring (September - October - November)
#' over the Nino 3.4 region for the period 1951-2014.
#' See \url{https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni}
#'
#' @format A data frame with 64 rows and 2 variables:
#' \describe{
#'   \item{year}{year from 1951 to 2014}
#'   \item{nino}{nino 3.4 index}
#' }
#' @source \url{https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long.data}
"elNino"

#' Information on available distributions
#'
#' A named list containing information (parameters, contraints, notes, warnings, etc.)
#' for all available univariate distributions.
#'
#' @format A named list where each element is itself a list containing:
#' \describe{
#'   \item{parName}{parameters short names}
#'   \item{parLongName}{parameters long names}
#'   \item{parSymbol}{parameters typical symbols}
#'   \item{constraints}{constraints on parameters}
#'   \item{url}{link to more information}
#'   \item{note}{notes}
#'   \item{warning}{warnings: read carefully since this highlights in particular differences with "standard" parameterizations found in e.g. Wikipedia or R.}
#' }
"distInfo"
