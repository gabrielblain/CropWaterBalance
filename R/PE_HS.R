#' PE_HS
#'
#' @param Ra
#' Extraterrestrial solar radiation in \acronym{MJ m-2 day-1}.
#' @param Tmax
#' A vector, 1-column matrix or data frame with daily Maximum air temperature in Celsius degrees.
#' @param Tmin
#' A vector, 1-column matrix or data frame with daily Minimum air temperature in Celsius degrees.
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @return
#' Daily potential evapotranspiration values.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Tmax <- DataForCWB[,3]
#' Tmin <- DataForCWB[,4]
#' Ra <- DataForCWB[,5]
#' PE_HS(Ra, Tavg, Tmax, Tmin)

PE_HS <- function(Ra, Tavg, Tmax, Tmin){
  Ra <- as.matrix(Ra)
  Tavg <- as.matrix(Tavg)
  Tmax <- as.matrix(Tmax)
  Tmin <- as.matrix(Tmin)
  PE <- as.matrix(0.0023*(Ra[,1]/2.45)*(Tmax[,1]-Tmin[,1])^0.5*(Tavg[,1]+17.8))
  colnames(PE) <- c("PE_HS")
  return(PE)
}
