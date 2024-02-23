#' Data for Water Balance Accounting
#'
#' Daily meteorological data from a weather station in Campinas, Brazil
#' and other parameters required for calculating the crop water balance.
#' The meteorological data belongs to the Agronomic Institute \acronym{(IAC)}.
#'
#'  @format ## `DataForCWB`
#'  A data frame with 15 columns and 129 rows:
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
#'    \item{Drz}{Depth of the root zone in meters}
#'    \item{AWC}{available water capacity (amount of water between field capacity and permanent wilting point)
#'               in millimeter of water per meter of soil}
#'    \item{MAD}{management allowed depletion (between 0 and 1)}
#'    \item{Kc}{Crop coefficient (between 0 and 1)}
#'    \item{Irrig}{Applied net irrigation in millimeters}
#'    }
#'    @source <http://www.ciiagro.org.br/>
"DataForCWB"
