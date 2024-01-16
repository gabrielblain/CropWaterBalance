#' CWB
#'
#' @param Rain
#' A vector, 1-column matrix or data frame with daily rainfall totals in millimeters.
#' @param PE
#' A vector, 1-column matrix or data frame with daily potential evapotranspiration in millimeters.
#' @param AWC
#' A single number defining the available water capacity in the root zone.
#' @param STinit
#' A single number defining the water capacity in the root zone at the beginning
#' of the water balance accounting. Default is \dQuote{STinit=AWC}
#' @param Kc
#' A single number defining the crop coefficient. Default is 1.
#' @param Ks
#' A single number defining the water stress coefficient. Default is 1.
#' @param Irrig
#' A vector, 1-column matrix or data frame with net irrigation amount infiltrated into the soil
#' for the current day in millimeters. Default is 0.
#' @return
#' Daily potential evapotranspiration values (Penman & Monteith \acronym{FAO-1998}).
#' @export
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Tmax <- DataForCWB[,3]
#' Tmin <- DataForCWB[,4]
#' Rn <- DataForCWB[,6]
#' WS <- DataForCWB[,7]
#' RH <- DataForCWB[,7]
#' G <- DataForCWB[,9]
#' PE <- PE_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Rain <- DataForCWB[,10]
#' CWB(Rain, PE, AWC=50)

CWB <- function(Rain, PE, AWC, STinit = AWC, Kc = NULL, Ks = NULL,Irrig = NULL){
  Rain <- as.matrix(Rain)
  n <- length(Rain)
  if (is.numeric(Rain) == FALSE || n < 5) {
    stop("Rain must be numerical single column variable with at least 5 records")}
  # 5 por causa do G (Ver Regina)
  ETR <- matrix(NA,n,1)
  Arm <- matrix(NA,n,1)
  Alt <- matrix(NA,n,1)
  Exd <- matrix(NA,n,1)
  Def <- matrix(NA,n,1)
  NegAc <- matrix(NA,n,1)
  PPE <- matrix(NA,n,1)
  if (is.numeric(AWC) == FALSE || length(AWC) != 1) {stop ("AWC must be a single number")}
  if (is.numeric(STinit) == TRUE &&
      length(STinit) == 1 &&
      STinit <= AWC) {Arm[1,1] <- STinit} else {
        stop("STinit must be a single number and cannot be larger than AWC")}
  if (is.null(Kc) == TRUE) {Kc <- matrix(1,n,1)}
  if (is.null(Ks) == TRUE) {Ks <- matrix(1,n,1)}
  if (is.null(Irrig) == TRUE) {Irrig <- matrix(0,n,1)}
  if (is.numeric(Ks) == FALSE || length(Ks) != n ||
      is.numeric(Kc) == FALSE || length(Kc) != n ||
      is.numeric(PE) == FALSE || length(PE) != n ||
      is.numeric(Irrig) == FALSE || length(Irrig) != n) {
    stop("Rain, PE, AWC, STinit, Kc, Ks, and Irrig must be numerical variables")}
  ETc <-PE*Kc*Ks
  RainIrrig <- Rain + Irrig
  DaysSeason <- as.matrix(seq(1:n))
  PPE[,1] <- RainIrrig[,1]-ETc[,1]
  Alt[1,1]=AWC-Arm[1,1]
  if (PPE[1,1]<0){
    NegAc[1,1]=0+PPE[1,1]
    ETR[1,1]=RainIrrig[1,1]+abs(Alt[1,1])
    Def[1,1]=ETc[1,1]-ETR[1,1]
  } else {
    NegAc[1,1]=AWC*log(Arm[1,1]/AWC)
    ETR[1,1]=ETc[1,1]
  }
  if (Arm[1,1]<AWC){Exd[1,1] <- 0}else{Exd[1,1] <- PPE[1,1]-Alt[1,1]}
  for (i in 2:n){
    if (PPE[i,1]<0){
      NegAc[i,1] <- NegAc[(i-1),1]+PPE[i,1]
      Arm[i,1] <- AWC*exp(NegAc[i,1]/AWC)
      Alt[i,1] <- Arm[i,1] - Arm[(i-1),1]
      ETR[i,1] <- RainIrrig[i,1]+abs(Alt[i,1])
    } else {
      Arm[i,1]=Arm[(i-1),1]+PPE[i,1]
      if(Arm[i,1] > AWC){Arm[i,1] <- AWC}
      NegAc[i,1]=AWC*log(Arm[i,1]/AWC)
      Alt[i,1] <- Arm[i,1] - Arm[(i-1),1]
      ETR[i,1]=ETc[i,1]
    }
    if (Arm[i,1]<AWC){Exd[i,1] <- 0}else{Exd[i,1] <- PPE[i,1]-Alt[i,1]}

  }
  Def[,1]=ETc[,1] - ETR[,1]
  WB=as.matrix(cbind(DaysSeason,Rain,Irrig,PE,Kc,Ks,ETc,PPE,ETR,Arm,Alt,Exd,Def))
  colnames(WB) <- c("DaysSeason","Rain","Irrig","PE","Kc","Ks","ETc", "P-ETc","ActualPE","Water_Root","DeltaWater_Root","Perc","WaterDefict")
  return(WB)
}
