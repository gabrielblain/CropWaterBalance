#' Compare Data from Two Samples
#'
#' Calculates measures of accuracy and agreement.
#'
#' @param Sample1
#' A `vector`, 1-column `matrix` or `data.frame` with evapotranspiration or
#'  other variable.
#' @param Sample2
#' A `vector`, 1-column `matrix` or `data.frame` with evapotranspiration or
#'  other variable.
#' @return
#' A \code{data.frame} with:
#'
#' \itemize{
#'  \item Absolute mean error (AME),
#'  \item square root of the mean squared error (RMSE),
#'  \item Willmott's indices of agreement:
#'    \itemize{
#'      \item original (dorig),
#'      \item modified (dmod) and
#'      \item refined (dref)
#'      }, and
#'  \item Pearson determination coefficient (RQuad).
#'  }
#'
#' @examples
#' # See `?DataForCWB` for more on this data set
#' Tavg <- DataForCWB[, 2]
#' Tmax <- DataForCWB[, 3]
#' Tmin <- DataForCWB[, 4]
#' Rn <- DataForCWB[, 6]
#' WS <- DataForCWB[, 7]
#' RH <- DataForCWB[, 8]
#' G <- DataForCWB[, 9]
#' Sample1 <-
#'   ET0_PM(
#'     Tavg = Tavg,
#'     Tmax = Tmax,
#'     Tmin = Tmin,
#'     Rn = Rn,
#'     RH = RH,
#'     WS = WS,
#'     G = G,
#'     Alt = 700)
#' Sample2 <- ET0_PT(Tavg = Tavg, Rn = Rn, G = G)
#' Compare(Sample1 = Sample1, Sample2 = Sample2)
#' @export
#' @importFrom PowerSDI Accuracy

Compare <- function(Sample1, Sample2) {
  ObsEst <- list(Sample1, Sample2)
  if (isFALSE(any(unlist(lapply(
    ObsEst, is.numeric
  )))) ||
  length(Sample1) < 5 ||
  length(Sample1) != length(Sample2) ||
  any(unlist(lapply(ObsEst, anyNA)))) {
    stop(
      "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed."
    )
  }

  Comp <-
    as.data.frame(Accuracy(obs_est = as.data.frame(ObsEst),
                           conf.int = "No"))[1, 3:8]
  colnames(Comp) <-
    c("AME", "RMSE", "dorig", "dmod", "dref", "RQuad")
  return(Comp)
}
