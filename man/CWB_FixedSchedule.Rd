% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CWB_FixedSchedule.R
\name{CWB_FixedSchedule}
\alias{CWB_FixedSchedule}
\title{Crop Water Balance Accounting With Fixed Time Periods for Irrigation}
\usage{
CWB_FixedSchedule(
  Rain,
  ET0,
  AWC,
  Drz,
  Kc = NULL,
  Irrig = NULL,
  MAD = NULL,
  InitialD = 0,
  Scheduling,
  start.date
)
}
\arguments{
\item{Rain}{Vector, 1-column matrix or data frame with daily rainfall totals in
millimetres.}

\item{ET0}{Vector, 1-column matrix or data frame with daily reference evapotranspiration
in millimetres.}

\item{AWC}{Vector, 1-column matrix or data frame with the available water capacity of
the soil, that is: the amount of water between field capacity and permanent
wilting point in millimetres of water per metres of soil.}

\item{Drz}{Vector, 1-column matrix or data frame defining the root zone depth in metres.}

\item{Kc}{Vector, 1-column matrix or data frame defining the crop coefficient.
If NULL its values are assumed to be 1.}

\item{Irrig}{Vector, 1-column matrix or data frame with net irrigation amount infiltrated
into the soil for the current day in millimetres.}

\item{MAD}{Vector, 1-column matrix or data frame defining the management allowed
depletion.  Varies between 0 and 1.}

\item{InitialD}{Single number defining in millimetre, the initial soil water deficit.
It is used to start the water balance accounting.
Default value is zero, which assumes the root zone is at the field capacity.}

\item{Scheduling}{Single integer number defining the number of days between two consecutive
irrigations.}

\item{start.date}{Date at which the accounting should start. Formats:
\dQuote{YYYY-MM-DD}, \dQuote{YYYY/MM/DD}.}
}
\value{
Water balance accounting, including the soil water deficit.
}
\description{
Calculates several parameters of the crop water balance.
It also suggests how much irrigation to apply.
}
\examples{

Tavg <- DataForCWB[, 2]
Tmax <- DataForCWB[, 3]
Tmin <- DataForCWB[, 4]
Rn <- DataForCWB[, 6]
WS <- DataForCWB[, 7]
RH <- DataForCWB[, 8]
G <- DataForCWB[, 9]
ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
Rain <- DataForCWB[, 10]
Drz <- DataForCWB[, 11]
AWC <- DataForCWB[, 12]
MAD <- DataForCWB[, 13]
Kc <- DataForCWB[, 14]
Irrig <- DataForCWB[, 15]
Scheduling <- 5
CWB_FixedSchedule(
  Rain = Rain,
  ET0 = ET0,
  AWC = AWC,
  Drz = Drz,
  Kc = Kc,
  Irrig = Irrig,
  MAD = MAD,
  Scheduling = Scheduling,
  start.date = "2023-11-23"
)
}
