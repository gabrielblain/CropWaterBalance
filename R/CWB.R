#' Crop Water Balance Accounting
#'
#' Calculates several parameters of the crop water balance.  It also suggests
#'   when to irrigate.
#'
#' @param Rain
#' A `vector`, 1-column `matrix` or `data.frame` with daily rainfall totals in
#'   millimeters.
#' @param ET0
#' A `vector`, 1-column `matrix` or `data.frame` with daily reference
#'   evapotranspiration in millimetres.
#' @param AWC
#' A `vector`, 1-column `matrix` or `data.frame` with the available water
#'   capacity of the soil, that is: the amount of water between field capacity
#'   and permanent wilting point in millimetre of water per meters of soil, must
#'   be greater than or equal to 0.
#' @param InitialD
#' Single number defining in millimetres, the initial soil water deficit.  It
#'    is used to start the water balance accounting.  Default value is 0, which
#'    assumes the root zone is at the field capacity.
#' @param Kc
#' A `vector`, 1-column `matrix` or `data.frame` defining the crop coefficient.
#'    If `NULL` its values are assumed to be 1.
#' @param MAD
#' A `vector`, 1-column `matrix` or `data.frame` defining the management
#'   allowed depletion.  Varies between 0 and 1.
#' @param Drz
#' A `vector`, 1-column `matrix` or `data.frame` defining the root zone depth
#'   in metres.
#' @param Irrig
#' A `vector`, 1-column `matrix` or `data.frame` with  net irrigation amount
#'   infiltrated into the soil for the current day in millimeters.
#' @param start.date
#' Date at which the accounting should start. Formats:
#'   \dQuote{YYYY-MM-DD}, \dQuote{YYYY/MM/DD}.
#' @return
#' A `data.frame` of water balance accounting, including the soil water deficit.
#' @export
#' @importFrom lubridate year is.Date
#' @examples
#' # See `?DataForCWB` for more on this data set
#'
#' Tavg <- DataForCWB[, 2]
#' Tmax <- DataForCWB[, 3]
#' Tmin <- DataForCWB[, 4]
#' Rn <- DataForCWB[, 6]
#' WS <- DataForCWB[, 7]
#' RH <- DataForCWB[, 8]
#' G <- DataForCWB[, 9]
#' ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Rain <- DataForCWB[, 10]
#' Drz <- DataForCWB[, 11]
#' AWC <- DataForCWB[, 12]
#' MAD <- DataForCWB[, 13]
#' Kc <- DataForCWB[, 14]
#' Irrig <- DataForCWB[, 15]
#' CWB(
#' Rain = Rain,
#'   ET0 = ET0,
#'   AWC = AWC,
#'   Drz = Drz,
#'   Kc = Kc,
#'   Irrig = Irrig,
#'   MAD = MAD,
#'   start.date = "2023-11-23"
#' )

CWB <- function(Rain,
                ET0,
                AWC,
                Drz,
                Kc = NULL,
                Irrig = NULL,
                MAD = NULL,
                InitialD = 0,
                start.date = "2011-11-23") {
  Rain <- as.matrix(Rain)
  if (!is.numeric(Rain) || any(is.na(Rain)) ||
      length(Rain[Rain < 0]) != 0 || ncol(Rain) != 1) {
    stop("Physically impossible or missing rain values")
  }
  n <- length(Rain)

  start.date <- .check_date(start.date)
  end.date <- start.date + (n - 1)
  all.period <- seq(start.date, end.date, "days")

  Ks <- matrix(1, n, 1)
  ETactual <- Def <- P_ETc <- D <- recom <- matrix(NA, n, 1)

  if (is.null(Kc)) {
    Kc <- matrix(1, n, 1)
  } else {
    Kc <- as.matrix(Kc)
  }
  if (is.null(MAD)) {
    MAD <- matrix(0.3, n, 1)
  } else {
    MAD <- as.matrix(MAD)
  }
  if (is.null(Irrig)) {
    Irrig <- matrix(0, n, 1)
  } else {
    Irrig <- as.matrix(Irrig)
  }
  ET0 <- as.matrix(ET0)
  Drz <- as.matrix(Drz)

  pars <-
    list(
      "Kc" = Kc,
      "ET0" = ET0,
      "Irrig" = Irrig,
      "AWC" = AWC,
      "MAD" = MAD,
      "Drz" = Drz
    )

  if (!all(unlist(lapply(pars, is.numeric))) ||
      any(lapply(pars, length) == 0) ||
      any(unlist(lapply(pars, anyNA))) ||
      any(unlist(lapply(pars, length)) != n) ||
      any(unlist(lapply(pars, ncol)) != 1) ||
      any(pars$MAD < 0) || any(pars$MAD > 1) ||
      any(pars$AWC <= 0) ||
      any(pars$Irrig < 0) ||
      any(pars$ET0 < 0) ||
      any(pars$Drz < 0) ||
      any(pars$Kc < 0.1) ||
      any(pars$Kc > 3)) {
    stop(
      "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
    )
  }

  ETc <- ET0 * Kc
  TAW <- matrix((AWC * Drz), n, 1)
  dmad <- matrix((MAD * TAW), n, 1)
  RainIrrig <- Rain + Irrig
  DaysSeason <- as.matrix(seq(1:n))
  P_ETc[, 1] <- RainIrrig[, 1] - ETc[, 1]

  if (!is.numeric(InitialD) || length(InitialD) != 1 ||
      InitialD > TAW[1] || InitialD < 0) {
    stop("`InitialD` must be a single positive number no larger than `TAW`.")
  }
  D[1, 1] <- InitialD + ETc[1, 1] - RainIrrig[1, 1]
  if (D[1, 1] < 0) {
    D[1, 1] <- 0
  }
  if (D[1, 1] >= (dmad[1, 1] - (MAD[1, 1] * dmad[1, 1]))) {
    recom[1, 1] <- c("Yes. Consider Irrig")
  } else {
    recom[1, 1] <- c("No")
  }
  if (D[1, 1] > dmad[1, 1]) {
    Ks[1, 1] <- ((TAW[1, 1] - D[1, 1]) / ((1 - MAD[1, 1]) * TAW[1, 1]))
  }

  for (i in 2:n) {
    D[i, 1] <- D[(i - 1), 1] + ETc[i, 1] - RainIrrig[i, 1]
    if (D[i, 1] < 0) {
      D[i, 1] <- 0
    }
    if (D[i, 1] >= (dmad[i, 1] - (MAD[i, 1] * dmad[i, 1]))) {
      recom[i, 1] <- c("Yes. Consider Irrig")
    } else {
      recom[i, 1] <- c("No")
    }
    if (D[i, 1] >= dmad[i, 1]) {
      Ks[i, 1] <- ((TAW[i, 1] - D[i, 1]) / ((1 - MAD[i, 1]) * TAW[i, 1]))
    }
  }

  ETactual[, 1] <- ETc[, 1] * Ks[, 1]
  Def[, 1] <- ETc[, 1] - ETactual[, 1]
  WB <- data.frame(DaysSeason,
                   Rain,
                   Irrig,
                   ET0,
                   Kc,
                   Ks,
                   ETc,
                   P_ETc,
                   ETactual,
                   Def,
                   TAW,
                   D,
                   dmad,
                   recom)
  colnames(WB) <-
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  return(WB)
}

#' Check User Input Dates for Validity
#'
#' @param x User entered date value
#' @return Validated date string as a `POSIXct` object.
#' @note This was taken from \CRANpkg{nasapower}.
#' @example .check_date(x)
#' @author Adam H. Sparks \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @noRd
.check_date <- function(x) {
  tryCatch(
    x <- lubridate::parse_date_time(x,
                                    c(
                                      "Ymd",
                                      "dmY",
                                      "mdY",
                                      "BdY",
                                      "Bdy",
                                      "bdY",
                                      "bdy"
                                    ),
                                    tz = Sys.timezone()),
    warning = function(c) {
      stop(call. = FALSE,
           "\n`",
           x,
           "` is not in a valid date format. Please enter a valid date format.",
           "\n")
    }
  )
  return(x)
}
