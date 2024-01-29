#' Reference 'evapotranspiration'
#'
#' Daily reference 'evapotranspiration' using 'Priestley' and 'Taylor' method.
#'
#' @param Rn
#' A vector, 1-column matrix or data frame with daily net radiation in \acronym{MJ m-2 day-1}
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @param G
#' Optional. A vector, 1-column matrix or data frame with daily soil Heat flux in \acronym{MJ m-2 day-1}.
#' May be provided by \code{\link{Soil_Heat_Flux}}
#' @return
#' Daily potential evapotranspiration values.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Rn <- DataForCWB[,4]
#' G <- DataForCWB[,9]
#' ETr_PT(Tavg, Rn,G)

ETr_PT <- function(Tavg, Rn, G = NULL){
  Rn <- as.matrix(Rn)
  Tavg <- as.matrix(Tavg)
  if (is.null(G) == TRUE) {
    G <- Soil_Heat_Flux(Tavg)}
  G <- as.matrix(G)
  n <- length(Rn)
  W <- matrix (NA,n,1)
  for (i in 1:n){
    if (Tavg[i,1] < 16){W[i,1] <- 0.407+0.0145*Tavg[i,1]} else {
      W[i,1] <- 0.483+0.01*Tavg[i,1]}
  }
  ETr <- as.matrix(1.26*W*(Rn-G)/2.45)
  colnames(ETr) <- c("ETr_PT")
  return(ETr)
}


