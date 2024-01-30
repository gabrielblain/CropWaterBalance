#' Water stress coefficient
#'
#' Calculates the water stress coefficient (Ks).
#'
#' @param AWC
#' Vector, 1-column matrix or data frame defining the available water capacity of the soil, that is:
#' the amount of water between field capacity and permanent wilting point in centimeter of water per centimeter of soil.
#' @param MAD
#' Vector, 1-column matrix or data frame defining the management allowed depletion.
#' Varies between 0.1 and 1, default = 0.8.
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
#' @param SoilWaterDeficit
#' Vector, 1-column matrix or data frame with soil water deficit in millimeters.
#' It is provided by the \code{\link{CWB}} function.
#' @return
#' Water stress coefficient \acronym{Ks}.
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
#' ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Rain <- DataForCWB[,10]
#' Drz <- DataForCWB[,11]
#' AWC <- DataForCWB[,12]
#' MAD <- DataForCWB[,13]
#' CWB.been <- CWB(Rain, ETr, AWC,Drz, start.date = "2023-11-23")
#' D <- CWB.been$SoilWaterDeficit
#' Ks(AWC = AWC,MAD = MAD, Drz = Drz, SoilWaterDeficit = D)

Ks <- function(AWC, MAD = NULL, Drz, SoilWaterDeficit){
  if (is.numeric(AWC) == FALSE || any(is.na(AWC)) == TRUE || length(AWC[AWC<0]) != 0){
    stop("Physically impossible (negative or missing) AWC values")}
  n <- length(AWC)
  Ks <- matrix(NA,n,1)
  AWC <- as.matrix(AWC)
  if (is.null(MAD) == TRUE) {MAD <- matrix(0.8,n,1)}
  if (is.numeric(SoilWaterDeficit) == FALSE || length(SoilWaterDeficit) != n || any(is.na(SoilWaterDeficit)) == TRUE ||
      length(MAD[MAD<0.1]) != 0 || is.numeric(MAD) == FALSE ||
      length(MAD) != n || any(is.na(MAD)) == TRUE || length(MAD[MAD>1]) != 0 ||
      is.numeric(Drz) == FALSE || length(Drz) != n || any(is.na(Drz)) == TRUE ||
      length(Drz[Drz<0]) != 0 || length(SoilWaterDeficit[SoilWaterDeficit<0])!= 0)
  {stop("AWC, MAD and SoilWaterDeficit must be numerical variables (same length) with no missing nor negative value.
        MAD varies between 0.1 and 1.")}
  MAD <- as.matrix(MAD)
  SoilWaterDeficit<- as.matrix(SoilWaterDeficit)
  Drz <- Drz*10
  TAW <- matrix((AWC*Drz),n,1) # sai em mm
  dmad <- matrix((MAD*TAW),n,1) #sai em mm
  for (i in 1:n){
    if (SoilWaterDeficit[i,1] < dmad[i,1]){Ks[i,1] <- 1} else{
      Ks[i,1] <- ((TAW[i,1]-SoilWaterDeficit[i,1])/((1-MAD[i,1])*TAW[i,1]))}
    colnames(Ks) <- c("Ks")}
  Ks[Ks<0,] <- 0
  return(Ks)
}
