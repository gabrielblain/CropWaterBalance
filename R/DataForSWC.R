#' Typical Soil Water Characteristics for Different Soil Types (Teta)
#'
#' Soil water content at field capacity and at permanent wilting point.
#' Given in \acronym{m-3 m-3}.
#' Extracted from: Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M.
#' Crop evapotranspiration. In Guidelines for Computing Crop Water Requirements.
#' Irrigation and Drainage Paper 56; FAO: Rome, Italy, 1998; p. 300.
#'
#'  @format ## `DataForSWC`
#'  A data frame with 5 columns and 9 rows:
#'  \describe{
#'    \item{Soil type}{Soil Type}
#'    \item{Teta_FC_Min}{Minimum values for soil water content at field
#'       capacity}
#'    \item{Teta_FC_Max}{Maximum values for soil water content at field
#'      capacity}
#'    \item{Teta_PWP_Min}{Minimum values for soil water content at permanent
#'      wilting point}
#'    \item{Teta_PWP_Max}{Maximum values for soil water content at permanent
#'      wilting point}
#'    }
#'    @source <https://www.fao.org/home/en/>
"DataForSWC"
