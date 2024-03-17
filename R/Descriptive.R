#' Descriptive Statistics for Weather Variables
#'
#' Calculates descriptive statistics for rainfall, evapotranspiration, or other
#'   variables.
#'
#' @param Sample
#' A vector, 1-column matrix or data frame with rainfall, evapotranspiration,
#'   or other variable.
#' @return
#' A \code{dataframe} with:
#'
#' \itemize{
#'  \item sample mean (Avg),
#'  \item sample median (Med),
#'  \item sample standard variation (SD)
#'  \item sample standard Error (SE)
#'  \item maximum value (MaxValue)
#'  \item minimum value (MinValue)
#'  \item frequency of zeros (FreqZero%)
#'  }
#'
#' @examples
#' data(DataForCWB)
#' Rain <- DataForCWB[,10]
#' Descriptive(Sample = Rain)
#' @export
#' @importFrom PowerSDI Accuracy
#' @importFrom stats median sd

Descriptive <- function(Sample) {
  Sample <- as.matrix(Sample)
  n <- length(Sample)
  if (!is.numeric(Sample) ||
      length(Sample) < 10)
  {
    stop("Sample must be a numerical single-column variable with at least
         10 records.")
  } else if (length(Sample[Sample < 0]) > 0) {
    stop("Negative or missing data in the sample is not allowed.")
  } else{
    Desc <- as.matrix(c(
      n,
      mean(Sample),
      median(Sample),
      sd(Sample),
      sd(Sample) / sqrt(n),
      max(Sample),
      min(Sample),
      100 * length(Sample[Sample == 0]) / n
    ))
  }
  Desc <- as.data.frame(t(Desc))
  Desc[2:8] <- round(Desc[2:8], 2)
  colnames(Desc) <-
    c("SampleSize",
      "Avg",
      "Med",
      "SD",
      "SE",
      "MaxValue",
      "MinValue",
      "FreqZero%")
  return(Desc)
}
