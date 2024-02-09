#' Soil Heat Flux
#'
#' Calculates the daily amounts of Soil Heat Flux.
#'
#' @param Tavg
#' A vector, 1-column matrix or data frame with daily average air temperature.
#' @return
#' Daily amounts of soil Heat flux in \acronym{MJ m-2 day-1}.
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Soil_Heat_Flux(Tavg)

Soil_Heat_Flux <- function(Tavg){
  Tavg <-as.matrix(Tavg)
  if(ncol(Tavg)!= 1 || any(is.na(Tavg)) == TRUE)
    {stop("Tavg must be a single column variable with no missing value")}
  n <- length(Tavg)
  G <- matrix(NA,n,1)
  if (n<4){stop("At least four days of Tavg are required.")}
  for (i in 4:n){
    G[i,1] <- 0.38*(Tavg[i]-mean(Tavg[(i-1):(i-3)]))
  }
  G[1:3,1] <- 0
  warning("The first 3 G values were set to zero")
  return(G)
}
