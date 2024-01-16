#' Meteorological data for calculating Water Balance
#'
#' Daily meteorological data from a weather station in Campinas, Brazil.
#' Belongs to the Agronomic Institute \acronym{IAC}
#'
#'  @format ## `DataForCWB`
#'  A data frame with 10 columns and 31 rows:
#'  \describe{
#'    \item{date}{date}
#'    \item{tmed}{Average air temperature in Celsius degrees}
#'    \item{tmax}{Maximum air temperature in Celsius degrees}
#'    \item{tmin}{Minimum air temperature in Celsius degrees}
#'    \item{Ra}{Extraterrestrial solar radiation in \acronym{MJ m-2 day-1}}
#'    \item{Rn}{Net radiation in \acronym{MJ m-2 day-1}}
#'    \item{W}{Wind speed in \acronym{m s-1}}
#'    \item{RH}{Relative Humidity  in %}
#'    \item{G}{Soil Heat Flux  in \acronym{MJ m-2 day-1}}
#'    \item{Rain}{Rain in millimeters}
#'    }
#'    @source <https://clima.iac.sp.gov.br/>
"DataForCWB"
