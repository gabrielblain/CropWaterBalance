#' Crop Water Balance Accounting
#'
#' Calculates several parameters of the crop water balance.
#' It also suggests when irrigate.
#'
#' @param Rain
#' Vector, 1-column matrix or data frame with daily rainfall totals in millimeters.
#' @param ETr
#' Vector, 1-column matrix or data frame with daily reference evapotranspiration in millimeters.
#' @param AWC
#' Vector, 1-column matrix or data frame defining the available water capacity of the soil, that is:
#' the amount of water between field capacity and permanent wilting point in millimeter of water per centimeter of soil.
#' @param Kc
#' Single number defining the crop coefficient. Default is 1.
#' @param Ks
#' Single number defining the water stress coefficient. Default is 1.
#' @param MAD
#' Vector, 1-column matrix or data frame defining the management allowed depletion.
#' Varies between 0 and 1, default = 0.3.
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
#' @param Irrig
#' Vector, 1-column matrix or data frame with net irrigation amount infiltrated into the soil
#' for the current day in millimeters. Default is 0.
#' @return
#' Water Balance Accounting, including the soil water defict.
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
#' CWB(Rain, ETr, AWC,Drz,MAD)

CWB <- function(Rain, ETr, AWC, Drz,Kc = NULL, Ks = NULL,Irrig = NULL, MAD= NULL){
  if (is.numeric(Rain) == FALSE || any(is.na(Rain)) == TRUE || length(Rain[Rain<0]) != 0){
    stop("Physically impossible or missing rain values")}
  n <- length(Rain)
  Rain <- as.matrix(Rain)
  ETactul <- matrix(NA,n,1)
  Arm <- matrix(NA,n,1)
  Alt <- matrix(NA,n,1)
  Exd <- matrix(NA,n,1)
  Def <- matrix(NA,n,1)
  NegAc <- matrix(NA,n,1)
  P_ETc <- matrix(NA,n,1)
  D <- matrix(NA,n,1)
  recom <- matrix(NA,n,1)
  if (is.null(Kc) == TRUE) {Kc <- matrix(1,n,1)}
  if (is.null(Ks) == TRUE) {Ks <- matrix(1,n,1)}
  if (is.null(MAD) == TRUE) {MAD <- matrix(0.3,n,1)}
  if (is.null(Irrig) == TRUE) {Irrig <- matrix(0,n,1)}
  if (is.numeric(Ks) == FALSE || length(Ks) != n || any(is.na(Ks)) == TRUE ||
      is.numeric(Kc) == FALSE || length(Kc) != n || any(is.na(Kc)) == TRUE ||
      is.numeric(ETr) == FALSE || length(ETr) != n || any(is.na(ETr)) == TRUE ||
      is.numeric(Irrig) == FALSE || length(Irrig) != n || any(is.na(Irrig)) == TRUE ||
      is.numeric(AWC) == FALSE || length(AWC) != n || any(is.na(AWC)) == TRUE ||
      is.numeric(MAD) == FALSE || length(MAD) != n || any(is.na(MAD)) == TRUE ||
      length(MAD[MAD>1]) != 0 || length(MAD[MAD<0])!= 0 ||
      length(AWC[AWC<=0])!= 0 ||
      is.numeric(Drz) == FALSE || length(Drz) != n || any(is.na(Drz)) == TRUE ||
      length(Irrig[Irrig<0]) != 0 || length(ETr[ETr<0]) != 0 ||
      length(Drz[Drz<0]) != 0 || length(Drz[Drz<0]) != 0)
      {stop("Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound.")}
  if (length(Ks[Ks < 0]) != 0 || length(Ks[Ks < 1]) != 0 ||
      length(Kc[Kc < 0.1]) != 0 || length(Kc[Kc > 3]) != 0){
    stop("Physically impossible Ks or Kc values")}
  ETc <-ETr*Kc*Ks
  Drz <- Drz # entra em cm?
  TAW <- matrix((AWC*Drz),n,1) # sai em mm
  dmad <- matrix((MAD*TAW),n,1) #sai em mm
  RainIrrig <- Rain + Irrig #sai em mm
  DaysSeason <- as.matrix(seq(1:n))
  P_ETc[,1] <- RainIrrig[,1]-ETc[,1] #sai em mm
  if (P_ETc[1,1]<0){
    NegAc[1,1] <- P_ETc[1,1]
    Arm[1,1] <- TAW[1,1]*exp(NegAc[1,1]/TAW[1,1])
    Alt[1,1] <- 0
    ETactul[1,1] <- ETc[1,1]
    Exd[1,1] <- 0
    warning("On day 1, ETC was larger than Rain. The first days should be analyzed with caution.")
  } else {
    Arm[1,1] <- TAW[1,1]
    NegAc[1,1] <- 0
    Alt[1,1] <- 0
    ETactul[1,1] <- ETc[1,1]
    Exd[1,1] <- P_ETc[1,1]
  }
  D[1,1] <- 0 + ETc[1,1] - RainIrrig[1,1]
  if(D[1,1] < 0){
    D[1,1] <- 0
    recom[1,1]=c("No")} else {recom[1,1]=c("Yes. Consider Irrig")}
  for (i in 2:n){
    if (P_ETc[i,1]<0){
      NegAc[i,1] <- NegAc[(i-1),1]+P_ETc[i,1]
      Arm[i,1] <- TAW[i,1]*exp(NegAc[i,1]/TAW[i,1])
      Alt[i,1] <- Arm[i,1] - Arm[(i-1),1]
      ETactul[i,1] <- RainIrrig[i,1]+abs(Alt[i,1])
    } else {
      Arm[i,1] <- Arm[(i-1),1]+P_ETc[i,1]
      if(Arm[i,1] > TAW[i,1]){Arm[i,1] <- TAW[i,1]}
      NegAc[i,1] <- TAW[i,1]*log(Arm[i,1]/TAW[i,1])
      Alt[i,1] <- Arm[i,1] - Arm[(i-1),1]
      ETactul[i,1] <- ETc[i,1]
    }

    if (ETactul[i,1] > ETc[i,1]){ETactul[i,1] <- ETc[i,1]}
    if (Arm[i,1]<TAW[i,1]){Exd[i,1] <- 0}else{Exd[i,1] <- P_ETc[i,1]-Alt[i,1]}
    D[i,1] <- D[(i-1),1] + ETc[i,1] - RainIrrig[i,1]
    if(D[i,1] < 0){D[i,1] <- 0}
    if (D[i,1] >= dmad[i,1]) {recom[i,1]=c("Yes. Consider Irrig")} else {recom[i,1]=c("No")}
  }
  Def[,1] <- ETc[,1] - ETactul[,1]

  WB=data.frame(DaysSeason,Rain,Irrig,ETr,Kc,Ks,ETc,P_ETc,ETactul,Arm,Alt,Exd,Def,
                         TAW,D,dmad,recom)
  colnames(WB) <- c("DaysSeason","Rain","Irrig","ETr","Kc","Ks","ETc", "P-ETc","ActualCropEvap",
                    "StoredWaterRoot","DeltaWaterRoot","Perc","ET_Defict",
                    "TAW","SoilWaterDeficit","d_MAD", "D>=d_MAD")
  return(WB)
}
