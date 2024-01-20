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
#' RH <- DataForCWB[,7]
#' G <- DataForCWB[,9]
#' Method1 <- PE_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
#' Method2 <- PE_PT(Tavg, Rn,G)
#' Comp <- Compare(Method1=Method1, Method2=Method2)
#' Comp
#' @export
#' @importFrom PowerSDI Accuracy

Compare <- function(Method1, Method2){
  if (is.numeric(Method1) == FALSE ||
      is.numeric(Method2) == FALSE ||
      length(Method1) < 5 ||
      length(Method1) != length(Method2)) {
    stop("Methods 1 and 2 must be numerical single column variables with at least 5 records each.")
  } else{
    ObsEst <- as.data.frame(cbind(Method1,Method2))
    Comp <- Accuracy(obs_est = ObsEst, conf.int = "No")
  }
  return(Comp)
}
