#' Reference Evapotranspiration Using Hargreaves-Samani Method
#'
#' Calculates daily reference evapotranspiration amounts using the
#'   Hargreaves-Samani method.
#'
#' @param Ra
#' A `vector`, 1-column `matrix` or `data.frame` with extraterrestrial solar
#'  radiation in \acronym{MJ m-2 day-1}.
#' @param Tmax
#' A `vector`, 1-column `matrix` or `data.frame` with daily maximum air
#'  temperature in Celsius degrees.
#' @param Tmin
#' A `vector`, 1-column `matrix` or `data.frame` with daily minimum air
#'  temperature in Celsius degrees.
#' @param Tavg
#' A `vector`, 1-column `matrix` or `data.frame` column with daily average air
#'  temperature.
#' @return
#' A `matrix` of 1-column with the same length as `the input values with the
#'  daily potential evapotranspiration values in millimetres.
#' @export
#' @seealso [ET0_PM()] [ET0_PT()]
#' @examples
#' # See `?DataForCWB` for more on this data set
#' Tavg <- DataForCWB[, 2]
#' Tmax <- DataForCWB[, 3]
#' Tmin <- DataForCWB[, 4]
#' Ra <- DataForCWB[, 5]
#' ET0_HS(Ra = Ra, Tavg = Tavg, Tmax = Tmax, Tmin = Tmin)
ET0_HS <- function(Ra, Tavg, Tmax, Tmin) {
  w <- list(
    "Ra" = Ra,
    "Tavg" = Tavg,
    "Tmax" = Tmax,
    "Tmin" = Tmin
  )

  if (!all(unlist(lapply(w, is.numeric))) ||
      any(lapply(w, length) == 0) ||
      any(unlist(lapply(w, anyNA))) ||
      any(unlist(lapply(w, length)) != max(unlist(lapply(w, length)))) ||
      any(unlist(lapply(w, length)) != min(unlist(lapply(w, length))))) {
    stop(
      "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be a numerical vector object with
      no missing data and all of the same length."
    )
  }

  ET0 <-
    as.matrix(0.0023 * (w$Ra / 2.45) * (w$Tmax - w$Tmin) ^ 0.5 *
                (w$Tavg + 17.8))
  colnames(ET0) <- "ET0"
  return(ET0)
}
