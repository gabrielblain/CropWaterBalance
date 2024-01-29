#' Soil_Heat_Flux
#'
#' Calculates the Soil Heat Flux
#'
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @return
#' Daily values for soil Heat flux in \acronym{MJ/m^2/day}.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Soil_Heat_Flux(Tavg)

Soil_Heat_Flux <- function(Tavg){
  Tavg <-as.matrix(Tavg)
  if(ncol(Tavg)!= 1){stop("Tavg must be a single column variable")}
  n <- length(Tavg)
  G <- matrix(NA,n,1)
  if (n<4){stop("At least four days of Tavg are required.")}
  for (i in 4:n){
    G[i,1] <- 0.38*(Tavg[i]-mean(Tavg[(i-1):(i-3)]))
  }
  return(G)
}
