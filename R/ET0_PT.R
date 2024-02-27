#' Reference 'evapotranspiration'
#'
#' Calculates daily reference evapotranspiration amounts using the  Priestley-Taylor method.
#'
#' @param Rn
#' A vector, 1-column matrix or data frame with daily net radiation in \acronym{MJ m-2 day-1}
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @param G
#' Optional. A vector, 1-column matrix or data frame with daily soil Heat flux in \acronym{MJ m-2 day-1}.
#' May be provided by \code{\link{Soil_Heat_Flux}}
#' @param Coeff
#' Single number defining the 'Priestley' and 'Taylor' coefficient. Default is 1.26
#
#' @return
#' Daily potential evapotranspiration values in millimetres.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Rn <- DataForCWB[,6]
#' G <- DataForCWB[,9]
#' ET0_PT(Tavg=Tavg, Rn=Rn,G=G)

ET0_PT <- function(Tavg, Rn, G = NULL, Coeff = 1.26){
  Tavg <- as.matrix(Tavg)
  if (!is.numeric(Tavg) || any(is.na(Tavg)) ||
      length(Tavg[Tavg > 70]) != 0 || length(Tavg[Tavg < -70]) != 0 ||
      ncol(Tavg)!= 1){
    stop("Physically impossible or missing Tavg values")}
  n <- length(Tavg)
  if (is.null(G)) {G <- Soil_Heat_Flux(Tavg)}
  G <- as.matrix(G)
  Rn <- as.matrix(Rn)
  if (!is.numeric(Rn) || any(is.na(Rn)) ||
      length(Rn[Rn > 70]) != 0 || length(Rn[Rn < -70]) != 0 ||
      ncol(Rn)!= 1 || length(Rn) != n ||
      !is.numeric(G) || any(is.na(G)) ||
      length(G[G > 20]) != 0 || length(G[G < -20]) != 0 ||
      ncol(G)!= 1 || length(G) != n || !is.numeric(Coeff)
  ){
    stop("Physically impossible or missing Coeff, Rn or G values")}

  S <- (2503.058/(Tavg+237.3))*exp((17.27*Tavg)/(Tavg+237.3))
  ET0 <-  Coeff*(Rn-G)*(S/(2.45*(S+0.063)))
  colnames(ET0) <- c("ET0_PT")
  return(ET0)
}
