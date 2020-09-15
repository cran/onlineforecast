#' Observations and weather forecasts from a single-family building, weather station and Danish Meteorological Institute (DMI)
#'
#' Data of the period from 2010-12-15 to 2011-03-01. The weather station was located within a range of 10 km from the building.
#'
#' Hourly average values. The time point is set in the end of the hour.
#'
#' Set in the format of a data.list used as input to forecast models in the onlineforecast package.
#'
#' @format A data list with 1854 rows and 7 variables:
#' \describe{
#'   \item{t}{Time in GMT as POSIXct}
#'   \item{heatload}{The heatload of a single family building in W}
#'   \item{heatloadtotal}{The average heatload of a 16 single family buildings in W}
#'   \item{Taobs}{Observed ambient temperature at the weather station in Celcius}
#'   \item{Iobs}{Observed global radiation at the weather station in W/m^2}
#'   \item{Ta}{Weather forecasts of ambient temperature up to 36 hours ahead from DMI in Celcius}
#'   \item{Ta}{Weather forecasts of global radiation up to 36 hours ahead from DMI in W/m^2}
#' }
#' @source See \url{https://onlineforecasting.org/examples/datasets.html}.
"Dbuilding"
