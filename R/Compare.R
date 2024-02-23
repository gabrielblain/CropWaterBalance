#' Compare data from two samples
#'
#' Calculates measures of accuracy and agreement.
#'
#' @param Sample1
#' A vector, 1-column matrix or data frame with evapotranspiration or other variable.
#' @param Sample2
#' A vector, 1-column matrix or data frame with evapotranspiration or other variable.
#' @return
#' a \code{list} with:
#'
#' \itemize{
#'  \item Absolute mean error (AME),
#'  \item square root of the mean squared error (RMSE),
#'  \item Willmott's indices of agreement:
#'    \itemize{
#'      \item original (dorig),
#'      \item modified (dmod) and
#'      \item refined (dref)
#'      }
#'  \item Pearson determination coefficient (R2), and
#'  }
#'
#' @examples
#' data(DataForCWB)
#' Tavg <- DataForCWB[,2]
#' Tmax <- DataForCWB[,3]
#' Tmin <- DataForCWB[,4]
#' Rn <- DataForCWB[,6]
#' WS <- DataForCWB[,7]
#' RH <- DataForCWB[,8]
#' G <- DataForCWB[,9]
#' Sample1 <- ET0_PM(Tavg=Tavg, Tmax=Tmax, Tmin=Tmin, Rn=Rn, RH=RH, WS=WS,G=G)
#' Sample2 <- ET0_PT(Tavg=Tavg, Rn=Rn,G=G)
#' Compare(Sample1=Sample1, Sample2=Sample2)
#' @export
#' @importFrom PowerSDI Accuracy

Compare <- function(Sample1, Sample2){
  if (is.numeric(Sample1) == FALSE ||
      is.numeric(Sample2) == FALSE ||
      length(Sample1) < 5 ||
      length(Sample1) != length(Sample2) ||
      any(is.na(Sample1)) == TRUE  ||
      any(is.na(Sample2)) == TRUE ) {
    stop("Samples 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed.")
  } else{
    ObsEst <- as.data.frame(cbind(Sample1,Sample2))
    Comp.orig <- as.data.frame((Accuracy(obs_est = ObsEst, conf.int = "No")))
    Comp <- as.data.frame(Comp.orig[1,3:8])
    colnames(Comp) <- c("AME","RMSE","dorig","dmod","dref","RQuad")
  }
  return(Comp)
}
