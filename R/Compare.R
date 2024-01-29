#' Compare two potential evapotranspiration methods
#'
#' Calculates measures of accuracy and agreement.
#'
#' @param Method1
#' A vector, 1-column matrix or data frame with potential evapotranspiration
#' calculated from Method1.
#' @param Method2
#' A vector, 1-column matrix or data frame with potential evapotranspiration
#' calculated from Method2.
#' @return
#' a \code{list}, which contains:
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
#' Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Method2 <- ETr_PT(Tavg, Rn,G)
#' Compare(Method1=Method1, Method2=Method2)
#' @export
#' @importFrom PowerSDI Accuracy

Compare <- function(Method1, Method2){
  if (is.numeric(Method1) == FALSE ||
      is.numeric(Method2) == FALSE ||
      length(Method1) < 5 ||
      length(Method1) != length(Method2) ||
      any(is.na(Method1)) == TRUE  ||
      any(is.na(Method2)) == TRUE ) {
    stop("Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed.")
  } else{
    ObsEst <- as.data.frame(cbind(Method1,Method2))
    Comp.orig <- as.data.frame((Accuracy(obs_est = ObsEst, conf.int = "No")))
    Comp <- as.data.frame(Comp.orig[1,3:8])
    colnames(Comp) <- c("AME","RMSE","dorig","dmod","dref","RQuad")
  }
  return(Comp)
}
