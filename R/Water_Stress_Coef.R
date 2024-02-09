#' Water stress coefficient
#'
#' Calculates the water stress coefficient (Ks).
#'
#' @param AWC
#' Vector, 1-column matrix or data frame defining the available water capacity of the soil, that is:
#' the amount of water between field capacity and permanent wilting point in millimeters of water
#' per centimeter of soil.
#' @param MAD
#' Vector, 1-column matrix or data frame defining the management allowed depletion.
#' Varies between 0 and 1, default is 0.3.
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
#' @param SoilWaterDeficit
#' Vector, 1-column matrix or data frame with soil water deficit in millimeters.
#' It may be provided by the \code{\link{CWB}} function.
#' @return
#' Water stress coefficient (Ks).
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
#' ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Rain <- DataForCWB[,10]
#' Drz <- DataForCWB[,11]
#' AWC <- DataForCWB[,12]
#' MAD <- DataForCWB[,13]
#' Kc <- DataForCWB[,14]
#' Ks <- DataForCWB[,15]
#' Irrig <- DataForCWB[,16]
#' CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
#'     Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
#' D <- CWB.been$SoilWaterDeficit
#' Water_Stress_Coef(AWC = AWC,MAD = MAD, Drz = Drz, SoilWaterDeficit = D)

Water_Stress_Coef <- function(AWC,
                              MAD,
                              Drz,
                              SoilWaterDeficit
                              ){
  AWC <- as.matrix(AWC)
  if (!is.numeric(AWC) || any(is.na(AWC)) || length(AWC) < 3 ||
      length(AWC[AWC<0]) != 0 || ncol(AWC) != 1) {
    stop("Physically impossible, negative or missing AWC values")}
  n <- length(AWC)
  Water_Stress_Coef <- matrix(NA,n,1)
  AWC <- as.matrix(AWC)
  MAD <- as.matrix(MAD)
  Drz <- as.matrix(Drz)
  SoilWaterDeficit <- as.matrix(SoilWaterDeficit)
  if (!is.numeric(SoilWaterDeficit)|| length(SoilWaterDeficit) != n || any(is.na(SoilWaterDeficit)) ||
      length(MAD[MAD<0]) != 0 || is.numeric(MAD) == FALSE ||
      length(MAD) != n || any(is.na(MAD)) == TRUE || length(MAD[MAD>1]) != 0 ||
      !is.numeric(Drz) || length(Drz) != n || any(is.na(Drz)) ||
      length(Drz[Drz<0]) != 0 || length(SoilWaterDeficit[SoilWaterDeficit<0])!= 0 ||
      length(SoilWaterDeficit)!= n)
  {stop("AWC, MAD and SoilWaterDeficit must be numerical variables with same length and no missing nor negative value.
        MAD varies between 0 and 1.")}
  SoilWaterDeficit<- as.matrix(SoilWaterDeficit)
  TAW <- matrix((AWC*Drz),n,1) # sai em mm
  dmad <- matrix((MAD*TAW),n,1) #sai em mm
  for (i in 1:n){
    if (SoilWaterDeficit[i,1] < dmad[i,1]){Water_Stress_Coef[i,1] <- 1} else{
      Water_Stress_Coef[i,1] <- ((TAW[i,1]-SoilWaterDeficit[i,1])/((1-MAD[i,1])*TAW[i,1]))}
    colnames(Water_Stress_Coef) <- c("Ks")}
  Water_Stress_Coef[Water_Stress_Coef<0,] <- 0
  return(Water_Stress_Coef)
}
