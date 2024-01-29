#' Reference 'evapotranspiration'
#'
#' Daily reference 'evapotranspiration' using 'Hargreaves' and 'Samani' method.
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
#' ETr_HS(Ra, Tavg, Tmax, Tmin)

ETr_HS <- function(Ra, Tavg, Tmax, Tmin){
  nRa <- length(Ra)
  nTavg <- length(Tavg)
  nTmax <- length(Tmax)
  nTmin <- length(Tmin)
  if (is.numeric(Ra) == FALSE ||
      nRa == 0 ||
      any(is.na(Ra)) == TRUE ||
      is.numeric(Tavg) == FALSE ||
      nTavg == 0 ||
        any(is.na(Tavg)) == TRUE ||
      is.numeric(Tmax) == FALSE ||
      nTmax == 0 ||
        any(is.na(Tmax)) == TRUE ||
      is.numeric(Tmin) == FALSE ||
      nTmin == 0 ||
        any(is.na(Tmin)) == TRUE ||
      nRa !=nTavg || nRa !=nTmax || nRa !=nTmin ||
      nTavg !=nTmax || nTavg !=nTmin || nTmax !=nTmin)
  {stop("Ra, Tavg, Tmax, and  Tmin must be numerical single-column variable with no missing data and same length.")}
  Ra <- as.matrix(Ra)
  Tavg <- as.matrix(Tavg)
  Tmax <- as.matrix(Tmax)
  Tmin <- as.matrix(Tmin)
  ETr <- as.matrix(0.0023*(Ra[,1]/2.45)*(Tmax[,1]-Tmin[,1])^0.5*(Tavg[,1]+17.8))
  colnames(ETr) <- c("ETr_HS")
  return(ETr)
}
