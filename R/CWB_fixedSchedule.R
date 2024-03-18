#' Crop Water Balance Accounting with fixed time periods for irrigation
#'
#' Calculates several parameters of the crop water balance.
#' It also suggests how much irrigate.
#'
#' @param Rain
#' Vector, 1-column matrix or data frame with daily rainfall totals in millimeters.
#' @param ET0
#' Vector, 1-column matrix or data frame with daily reference evapotranspiration in millimeters.
#' @param AWC
#' Vector, 1-column matrix or data frame with the available water capacity of the soil, that is:
#' the amount of water between field capacity and permanent wilting point
#' in millimeter of water per meters of soil.
#' @param InitialD
#' Single number defining in millimeter, the initial soil water deficit.
#' It is used to start the water balance accounting.
#' Default value is zero, which assumes the root zone is at the field capacity.
#' @param Kc
#' Vector, 1-column matrix or data frame defining the crop coefficient.
#' If NULL its values are assumed to be 1.
#' @param MAD
#' Vector, 1-column matrix or data frame defining the management allowed depletion.
#' Varies between 0 and 1.
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in meters.
#' @param Irrig
#' Vector, 1-column matrix or data frame with net irrigation amount infiltrated into the soil
#' for the current day in millimeters.
#' @param Scheduling
#' Single integer number defining the number of days between two consecutive irrigations.
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
#' ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G,Alt=700)
#' Rain <- DataForCWB[,10]
#' Drz <- DataForCWB[,11]
#' AWC <- DataForCWB[,12]
#' MAD <- DataForCWB[,13]
#' Kc <- DataForCWB[,14]
#' Irrig <- DataForCWB[,15]
#' Scheduling <- 5
#' CWB_FixedSchedule(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
#'     Kc=Kc, Irrig=Irrig, MAD=MAD, Scheduling=Scheduling, start.date = "2023-11-23")

CWB_FixedSchedule <- function(Rain,
                          ET0,
                          AWC,
                          Drz,
                          Kc = NULL,
                          Irrig = NULL,
                          MAD = NULL,
                          InitialD = 0,
                          Scheduling,
                          start.date = "2011-11-23"){
  if (!is.numeric(Scheduling) || length(Scheduling) != 1 || Scheduling < 1) {
    stop("Scheduling must be a single positive number")
  }
  if (Scheduling > 20){
    message("Period between irrigations is longer than 20 days. Sure about it?")
  }
  Rain <- as.matrix(Rain)
  if (!is.numeric(Rain) || any(is.na(Rain)) ||
      length(Rain[Rain<0]) != 0 || ncol(Rain) != 1){
    stop("Physically impossible or missing rain values")}
  n <- length(Rain)
  start.date <- as.Date(start.date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
  end.date <- start.date + (n-1)
  all.period <- seq(start.date, end.date, "days")
  Ks <- matrix(1,n,1)
  ETactul <- matrix(NA,n,1)
  Arm <- matrix(NA,n,1)
  Alt <- matrix(NA,n,1)
  Exd <- matrix(NA,n,1)
  Def <- matrix(NA,n,1)
  NegAc <- matrix(NA,n,1)
  P_ETc <- matrix(NA,n,1)
  D <- matrix(NA,n,1)
  recom <- matrix(NA,n,1)
  if (is.null(Kc)) {Kc <- matrix(1,n,1)} else {Kc <- as.matrix(Kc)}
  if (is.null(MAD)) {MAD <- matrix(0.3,n,1)} else {MAD <- as.matrix(MAD)}
  if (is.null(Irrig)) {Irrig <- matrix(0,n,1)} else {Irrig <- as.matrix(Irrig)}
  ET0 <- as.matrix(ET0)
  Drz <- as.matrix(Drz)
  if (!is.numeric(Kc) || length(Kc) != n || any(is.na(Kc)) ||
      !is.numeric(ET0) || length(ET0) != n || any(is.na(ET0)) ||
      !is.numeric(Irrig) || length(Irrig) != n || any(is.na(Irrig)) ||
      !is.numeric(AWC) || length(AWC) != n || any(is.na(AWC)) ||
      !is.numeric(MAD) || length(MAD) != n || any(is.na(MAD)) ||
      length(MAD[MAD>1]) != 0 || length(MAD[MAD<0])!= 0 ||
      length(AWC[AWC<=0])!= 0 ||
      !is.numeric(Drz) || length(Drz) != n || any(is.na(Drz)) ||
      length(Irrig[Irrig<0]) != 0 || length(ET0[ET0<0]) != 0 ||
      length(Drz[Drz<0]) != 0 || length(Drz[Drz<0]) != 0 ||
      length(Kc[Kc < 0.1]) != 0 || length(Kc[Kc > 3]) != 0 ||
      ncol(Drz) != 1 || ncol(Kc) != 1 || ncol(MAD) != 1 || ncol(ET0) != 1)
  {stop("Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound.")}
  ETc <-ET0*Kc
  TAW <- matrix((AWC*Drz),n,1)
  dmad <- matrix((MAD*TAW),n,1)
  RainIrrig <- Rain + Irrig
  DaysSeason <- as.matrix(seq(1:n))
  P_ETc[,1] <- RainIrrig[,1]-ETc[,1]

  if (!is.numeric(InitialD) || length(InitialD) != 1 ||
      InitialD > TAW[1,1] || InitialD < 0)
  {stop("InitialD must be a single positive number no larger than TAW")}
  D[1,1] <- InitialD + ETc[1,1] - RainIrrig[1,1]
  if(D[1,1] < 0){D[1,1] <- 0}
  if(D[1,1] > TAW[1,1]){D[1,1] <- TAW[1,1]}
  if (D[1,1] >= (dmad[1,1]-(MAD[1,1]*dmad[1,1]))) {recom[1,1]=c("Yes. Consider Irrig")}
  else {recom[1,1]=c("No")}
  if (D[1,1] > dmad[1,1]) {Ks[1,1] <- ((TAW[1,1]-D[1,1])/((1-MAD[1,1])*TAW[1,1]))}
  days.irrig <- 1
  if (days.irrig == Scheduling) {
    recom[1,1]=c("Time to Irrig")
    days.irrig <- 1}
  else {
    recom[1,1]=c("No")
    days.irrig <- days.irrig+1}
  for (i in 2:n){
    D[i,1] <- D[(i-1),1] + ETc[i,1] - RainIrrig[i,1]
    if(D[i,1] < 0){D[i,1] <- 0}
    if(D[i,1] > TAW[i,1]){D[i,1] <- TAW[i,1]}
    if (days.irrig == Scheduling) {
      recom[i,1]=paste("Irrigate", round(D[i,1],3),"mm")
      days.irrig <- 1}
    else {
      recom[i,1]=c("No")
      days.irrig <- days.irrig+1}
    if (D[i,1] >= dmad[i,1]) {Ks[i,1] <- ((TAW[i,1]-D[i,1])/((1-MAD[i,1])*TAW[i,1]))}
  }

  ETactul[,1] <- ETc[,1]*Ks[,1]
  Def[,1] <- ETc[,1] - ETactul[,1]
  WB=data.frame(DaysSeason,Rain,Irrig,ET0,Kc,Ks,ETc,P_ETc,ETactul,Def,TAW,D,dmad,recom)
  WB[,2:13] <- round(WB[,2:13],3)
  colnames(WB) <- c("DaysSeason","Rain","Irrig","ET0","Kc","WaterStressCoef_Ks","ETc", "(P+Irrig)-ETc","NonStandardCropEvap",
                    "ET_Defict","TAW","SoilWaterDeficit","d_MAD", "Scheduling")
  rownames(WB) <- all.period
  return(WB)
}
