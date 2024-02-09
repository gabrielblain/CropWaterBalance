#' Crop Water Balance Accounting
#'
#' Calculates several parameters of the crop water balance.
#' It also suggests when irrigate.
#'
#' @param Rain
#' Vector, 1-column matrix or data frame with daily rainfall totals in millimeters.
#' @param ET0
#' Vector, 1-column matrix or data frame with daily reference evapotranspiration in millimeters.
#' @param AWC
#' Vector, 1-column matrix or data frame with the available water capacity of the soil, that is:
#' the amount of water between field capacity and permanent wilting point
#' in millimeter of water per centimeter of soil.
#' @param InitialSoilWater
#' Single number defining in millimeter the initial amount of soil water.
#' It is used to start the water balance accounting.
#' If NULL it is assumed to be at the field capacity.
#' @param InitialD
#' Single number defining in millimeter, the initial soil water deficit.
#' It is used to start the water balance accounting.
#' Default value is zero, which assumes the root zone is at the field capacity.
#' @param Kc
#' Vector, 1-column matrix or data frame defining the crop coefficient.
#' If NULL its values are assumed to be 1.
#' @param Ks
#' Vector, 1-column matrix or data frame defining the water stress coefficient.
#' If NULL its values are assumed to be 1.
#' @param MAD
#' Vector, 1-column matrix or data frame defining the management allowed depletion.
#' Varies between 0 and 1.
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
#' @param Irrig
#' Vector, 1-column matrix or data frame with net irrigation amount infiltrated into the soil
#' for the current day in millimeters.
#' @param start.date
#' Date at which the accounting should start. Formats:
#' \dQuote{YYYY-MM-DD}, \dQuote{YYYY/MM/DD}.
#' @return
#' Water balance accounting, including the soil water deficit.
#' @export
#' @importFrom lubridate year is.Date
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
#' CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
#'     Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")

CWB <- function(Rain,
                ET0,
                AWC,
                Drz,
                Kc = NULL,
                Ks = NULL,
                Irrig = NULL,
                MAD = NULL,
                InitialSoilWater = NULL,
                InitialD = 0,
                start.date = "2011-11-23"){

  Rain <- as.matrix(Rain)
  if (is.numeric(Rain) == FALSE || any(is.na(Rain)) == TRUE ||
      length(Rain[Rain<0]) != 0 || ncol(Rain) != 1){
    stop("Physically impossible or missing rain values")}
  n <- length(Rain)
  start.date <- as.Date(start.date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
  end.date <- start.date + (n-1)
  all.period <- seq(start.date, end.date, "days")
  ETactul <- matrix(NA,n,1)
  Arm <- matrix(NA,n,1)
  Alt <- matrix(NA,n,1)
  Exd <- matrix(NA,n,1)
  Def <- matrix(NA,n,1)
  NegAc <- matrix(NA,n,1)
  P_ETc <- matrix(NA,n,1)
  D <- matrix(NA,n,1)
  recom <- matrix(NA,n,1)
  if (is.null(Kc) == TRUE) {Kc <- matrix(1,n,1)} else {Kc <- as.matrix(Kc)}
  if (is.null(Ks) == TRUE) {Ks <- matrix(1,n,1)} else {Ks <- as.matrix(Ks)}
  if (is.null(MAD) == TRUE) {MAD <- matrix(0.3,n,1)} else {MAD <- as.matrix(MAD)}
  if (is.null(Irrig) == TRUE) {Irrig <- matrix(0,n,1)} else {Irrig <- as.matrix(Irrig)}
  ET0 <- as.matrix(ET0)
  Drz <- as.matrix(Drz)
  if (is.numeric(Ks) == FALSE || length(Ks) != n || any(is.na(Ks)) == TRUE ||
      is.numeric(Kc) == FALSE || length(Kc) != n || any(is.na(Kc)) == TRUE ||
      is.numeric(ET0) == FALSE || length(ET0) != n || any(is.na(ET0)) == TRUE ||
      is.numeric(Irrig) == FALSE || length(Irrig) != n || any(is.na(Irrig)) == TRUE ||
      is.numeric(AWC) == FALSE || length(AWC) != n || any(is.na(AWC)) == TRUE ||
      is.numeric(MAD) == FALSE || length(MAD) != n || any(is.na(MAD)) == TRUE ||
      length(MAD[MAD>1]) != 0 || length(MAD[MAD<0])!= 0 ||
      length(AWC[AWC<=0])!= 0 ||
      is.numeric(Drz) == FALSE || length(Drz) != n || any(is.na(Drz)) == TRUE ||
      length(Irrig[Irrig<0]) != 0 || length(ET0[ET0<0]) != 0 ||
      length(Drz[Drz<0]) != 0 || length(Drz[Drz<0]) != 0 ||
      length(Ks[Ks < 0]) != 0 || length(Ks[Ks > 1]) != 0 ||
      length(Kc[Kc < 0.1]) != 0 || length(Kc[Kc > 3]) != 0 ||
      ncol(Drz) != 1 || ncol(Kc) != 1 || ncol(Ks) != 1 || ncol(MAD) != 1 ||
      ncol(ET0) != 1) # limits Ks e Kc ok?
  {stop("Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound.")}
  ETc <-ET0*Kc*Ks
  TAW <- matrix((AWC*Drz),n,1)
  dmad <- matrix((MAD*TAW),n,1)
  RainIrrig <- Rain + Irrig
  DaysSeason <- as.matrix(seq(1:n))
  P_ETc[,1] <- RainIrrig[,1]-ETc[,1]
  if (is.null(InitialSoilWater)) {InitialSoilWater <- as.matrix(TAW[1,1])}
  else if (!is.numeric(InitialSoilWater) || length(InitialSoilWater) != 1 ||
           InitialSoilWater > TAW[1,1] || InitialSoilWater < 0)
  {stop("InitialSoilWater must be a single positive number no larger than TAW")}
  else{InitialSoilWater <- as.matrix(InitialSoilWater)}

  if (!is.numeric(InitialD) || length(InitialD) != 1 ||
      InitialD > TAW[1,1] || InitialD < 0)
  {stop("InitialD must be a single positive number no larger than TAW")}

  startNegAc <- TAW[1,1]*log(InitialSoilWater/TAW[1,1])
  if (P_ETc[1,1]<0){
    NegAc[1,1] <- startNegAc + P_ETc[1,1]
    Arm[1,1] <- TAW[1,1]*exp(NegAc[1,1]/TAW[1,1])
    Alt[1,1] <- Arm[1,1]-InitialSoilWater
    ETactul[1,1] <- ETc[1,1]
    warning("On day 1, ETC was larger than Rain. The first days should be analyzed with caution.")
  } else {
    Arm[1,1] <- TAW[1,1]
    NegAc[1,1] <- TAW[1,1]*log(InitialSoilWater/TAW[1,1])
    Alt[1,1] <- Arm[1,1]-InitialSoilWater
    ETactul[1,1] <- ETc[1,1]
  }
  if (Arm[1,1]<TAW[1,1]){Exd[1,1] <- 0}else{Exd[1,1] <- P_ETc[1,1]-Alt[1,1]}
  D[1,1] <- 0 + ETc[1,1] - RainIrrig[1,1]
  if(D[1,1] < 0){D[1,1] <- 0}
    if (D[1,1] >= (dmad[1,1]-(MAD[1,1]*dmad[1,1]))) {recom[1,1]=c("Yes. Consider Irrig")}
    else {recom[1,1]=c("No")}

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
    if (D[i,1] >= (dmad[i,1]-(MAD[i,1]*dmad[i,1]))) {recom[i,1]=c("Yes. Consider Irrig")}
    else {recom[i,1]=c("No")}
  }
  Def[,1] <- ETc[,1] - ETactul[,1]

  WB=data.frame(DaysSeason,Rain,Irrig,ET0,Kc,Ks,ETc,P_ETc,ETactul,Arm,Alt,Exd,Def,
                TAW,D,dmad,recom)
  colnames(WB) <- c("DaysSeason","Rain","Irrig","ET0","Kc","Ks","ETc", "(P+Irrig)-ETc","ActualCropEvap",
                    "StoredWaterRoot","DeltaWaterRoot","Perc","ET_Defict",
                    "TAW","SoilWaterDeficit","d_MAD", "D>=dmad-(MAD*dmad)")
  rownames(WB) <- all.period
  return(WB)
}
