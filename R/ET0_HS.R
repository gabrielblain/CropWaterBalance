#' Reference 'evapotranspiration' Using Hargreaves-Samani Method
#'
#' Calculates daily reference evapotranspiration amounts using the Hargreaves-Samani method.
#'
#' @param Ra
#' Extraterrestrial solar radiation in \acronym{MJ m-2 day-1}.
#' @param Tmax
#' A vector, 1-column matrix or data frame with daily maximum air temperature
#'  in Celsius degrees.
#' @param Tmin
#' A vector, 1-column matrix or data frame with daily minimum air temperature
#'  in Celsius degrees.
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @return
#' A maxtrix with the daily potential evapotranspiration values in millimetres.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[, 2]
#' Tmax <- DataForCWB[, 3]
#' Tmin <- DataForCWB[, 4]
#' Ra <- DataForCWB[, 5]
#' ET0_HS(Ra = Ra, Tavg = Tavg, Tmax = Tmax, Tmin = Tmin)
ET0_HS <- function(Ra, Tavg, Tmax, Tmin) {
  nRa <- length(Ra)
  nTavg <- length(Tavg)
  nTmax <- length(Tmax)
  nTmin <- length(Tmin)
  if (!is.numeric(Ra) ||
    nRa == 0 ||
    any(is.na(Ra)) ||
    !is.numeric(Tavg) ||
    nTavg == 0 ||
    any(is.na(Tavg)) ||
    !is.numeric(Tmax) ||
    nTmax == 0 ||
    any(is.na(Tmax)) ||
    !is.numeric(Tmin) ||
    nTmin == 0 ||
    any(is.na(Tmin)) ||
    nRa != nTavg || nRa != nTmax || nRa != nTmin ||
    nTavg != nTmax || nTavg != nTmin || nTmax != nTmin) {
    stop("Ra, Tavg, Tmax, and  Tmin must be numerical single-column variable
          with no missing data and same length.")
  }
  Ra <- as.matrix(Ra)
  Tavg <- as.matrix(Tavg)
  Tmax <- as.matrix(Tmax)
  Tmin <- as.matrix(Tmin)
  ET0 <- as.matrix(0.0023 * (Ra[, 1] / 2.45) *
    (Tmax[, 1] - Tmin[, 1])^0.5 * (Tavg[, 1] + 17.8))
  colnames(ET0) <- "ET0_HS"
  return(ET0)
}
