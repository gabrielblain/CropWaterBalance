#' Reference 'evapotranspiration'
#'
#' Calculates daily reference evapotranspiration amounts using the Penman and Monteith method.
#'
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @param Tmax
#' A vector, 1-column matrix or data frame with daily maximum air temperature in Celsius degrees.
#' @param Tmin
#' A vector, 1-column matrix or data frame with daily minimum air temperature in Celsius degrees.
#' @param Rn
#' A vector, 1-column matrix or data frame with daily net radiation in \acronym{MJ m-2 day-1}.
#' @param RH
#' A vector, 1-column matrix or data frame with daily relative Humidity  in %.
#' @param WS
#' A vector, 1-column matrix or data frame with daily wind speed in \acronym{m s-1}.
#' @param G
#' Optional. A vector, 1-column matrix or data frame with daily soil Heat flux in \acronym{MJ m-2 day-1}.
#' Default is `NULL` and if `NULL` it is assumed to be zero.
#' May be provided by \code{\link{Soil_Heat_Flux}}
#' @return
#' Daily reference evapotranspiration amounts in millimetres.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Tmax <- DataForCWB[,3]
#' Tmin <- DataForCWB[,4]
#' Rn <- DataForCWB[,6]
#' WS <- DataForCWB[,7]
#' RH <- DataForCWB[,8]
#' G <- DataForCWB[,9]
#' ET0_PM(Tavg=Tavg, Tmax=Tmax, Tmin=Tmin, Rn=Rn, RH=RH, WS=WS,G=G)

ET0_PM <- function(Tavg, Tmax, Tmin, Rn, RH, WS,G = NULL){
  Tavg <- as.matrix(Tavg)
  if (is.numeric(Tavg) == FALSE || any(is.na(Tavg)) == TRUE ||
      length(Tavg[Tavg > 70]) != 0 || length(Tavg[Tavg < -70]) != 0 ||
      ncol(Tavg)!= 1){
    stop("Physically impossible or missing Tavg values")}
  n <- length(Tavg)
  if (is.null(G) == TRUE) {
    G <- Soil_Heat_Flux(Tavg)}
  Tmax <- as.matrix(Tmax)
  Tmin <- as.matrix(Tmin)
  Rn <- as.matrix(Rn)
  RH <- as.matrix(RH)
  WS <- as.matrix(WS)
  G <- as.matrix(G)
  if (is.numeric(Tavg) == FALSE || any(is.na(Tavg)) == TRUE ||
      length(Tavg[Tavg > 70]) != 0 || length(Tavg[Tavg < -70]) != 0 ||
      is.numeric(Tmax) == FALSE || any(is.na(Tmax)) == TRUE ||
      length(Tmax[Tmax > 70]) != 0 || length(Tmax[Tmax < -70]) != 0 ||
      is.numeric(Tmin) == FALSE || any(is.na(Tmin)) == TRUE ||
      length(Tmin[Tmin > 70]) != 0 || length(Tmin[Tmin < -70]) != 0 ||
      is.numeric(Rn) == FALSE || any(is.na(Rn)) == TRUE ||
      length(Rn[Rn > 70]) != 0 || length(Rn[Rn < -70]) != 0 ||
      is.numeric(RH) == FALSE || any(is.na(RH)) == TRUE ||
      length(RH[RH > 100]) != 0 || length(RH[RH < 0]) != 0 ||
      is.numeric(WS) == FALSE || any(is.na(WS)) == TRUE ||
      length(WS[WS > 150]) != 0 || length(WS[WS < 0]) != 0 ||
      is.numeric(G) == FALSE || any(is.na(G)) == TRUE ||
      length(G[G > 20]) != 0 || length(G[G < -20]) != 0 ||
      ncol(Tmax)!= 1 || ncol(Tmin)!= 1 || ncol(Rn)!= 1 ||
      ncol(RH)!= 1 || ncol(WS)!= 1 || ncol(G)!= 1 ||
      length(Tmax) != n || length(Tmin) != n || length(Rn) != n ||
      length(RH) != n || length(WS) != n|| length(G) != n
  ){
    stop("Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values")}
  es=0.6108*exp((17.27*Tavg)/(Tavg+273.3))
  ea=(RH*es)/100
  slope.pressure=(4098*es)/((Tavg+237.3)^2)
  ET0 <- as.matrix((0.408*slope.pressure*
                      (Rn-G)+0.063*(900/(Tavg+273))*WS*(es-ea))/
                     (slope.pressure+0.063*(1+0.34*WS)))
  colnames(ET0) <- c("ET0_PM")
  return(ET0)
}
