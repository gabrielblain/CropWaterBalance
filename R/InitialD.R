#' Soil water deficit in the root zone
#'
#' Estimates initial values for soil water deficit. Required to initiate the water balance accounting.
#'
#' @param teta_FC
#' Soil water content for the effective root zone at the field capacity \acronym{m3/m3}
#' @param teta_Obs
#' Soil water content for the effective root zone at the wilting point \acronym{m3/m3}
#' @param Drz
#' Vector, 1-column matrix or data frame defining the root zone depth in meters.
#' @return
#' Initial soil water deficit in the root zone (millimetres).
#' @export
#' @examples
#' teta_FC <- 0.30
#' teta_Obs <- 0.17
#' Drz <- 0.3048
#' Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz)

Dinitial <- function(teta_FC,teta_Obs,Drz){

  if (is.numeric(teta_FC) == FALSE || length(teta_FC) != 1 ||
      any(is.na(teta_FC)) == TRUE || teta_FC < 0 ||
      is.numeric(teta_Obs) == FALSE || length(teta_Obs) != 1 ||
      any(is.na(teta_Obs)) == TRUE || teta_Obs < 0 ||
      is.numeric(Drz) == FALSE || length(Drz) != 1 ||
      any(is.na(Drz)) == TRUE || Drz < 0 || teta_Obs > teta_FC)
      {
    stop("Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC")}
  Dinitial <- 1000*(teta_FC - teta_Obs)*Drz
  return(Dinitial)}
