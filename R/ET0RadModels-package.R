#' ET0RadModels: Reference Evapotranspiration Estimation Using Radiation-Based Models
#'
#' The \pkg{ET0RadModels} package provides functions for estimating daily reference
#' evapotranspiration (ET0) using 10 radiation-based empirical models, with
#' the FAO Penman-Monteith method included as the standard reference for model
#' comparison. The package also includes statistical evaluation metrics and
#' visualization tools.
#'
#' @section Radiation-Based Models (10):
#' \describe{
#'   \item{Jensen-Haise (1963)}{Solar radiation and mean temperature.}
#'   \item{McGuinness-Bordne (1972)}{Extraterrestrial radiation and mean temperature.}
#'   \item{Irmak (2003)}{Solar radiation-based regression equation.}
#'   \item{Makkink (1957)}{Solar radiation and slope of SVP curve.}
#'   \item{Turc (1961)}{Solar radiation and mean temperature (humid conditions).}
#'   \item{Caprio (1974)}{Linear function of solar radiation and temperature.}
#'   \item{Abtew (1996)}{Simple solar radiation-based approach (coefficient 0.53).}
#'   \item{Priestley-Taylor (1972)}{Net radiation with empirical coefficient alpha = 1.26.}
#'   \item{Jones-Ritchie (1990)}{Simplified Priestley-Taylor with alpha = 1.1.}
#'   \item{Hargreaves (1975)}{Radiation-based method using solar radiation.}
#' }
#'
#' @section Key Functions:
#' \describe{
#'   \item{\code{\link{et0_rad_all}}}{Compute ET0 using all 10 radiation-based models + FAO PM.}
#'   \item{\code{\link{et0_fao_pm}}}{FAO Penman-Monteith reference method.}
#'   \item{\code{\link{evaluate_models}}}{Compute all 9 statistical metrics.}
#'   \item{\code{\link{plot_taylor}}}{Generate Taylor diagrams.}
#'   \item{\code{\link{plot_scatter}}}{Generate scatter plots.}
#' }
#'
#' @section Data:
#' The package includes a sample meteorological dataset
#' (\code{\link{jhansi_weather}}) from a semi-arid region in central India
#' for demonstrating package functionality.
#'
#' @references
#' Satpute, A.N., Barman, S., Singh, A.K., Ghosh, A., & Gupta, G. (2026).
#' A multi-scale evaluation of 30 empirical models for reference
#' evapotranspiration estimation in a data-scarce semi-arid region:
#' A case study of Jhansi, India.
#' Journal of Hydrology: Regional Studies.
#' \doi{10.1016/j.ejrh.2026.103925}
#'
#' Allen, R.G., Pereira, L.S., Raes, D., & Smith, M. (1998). Crop
#' evapotranspiration: Guidelines for computing crop water requirements.
#' FAO Irrigation and Drainage Paper No. 56.
#'
#' @docType package
#' @name ET0RadModels-package
#' @aliases ET0RadModels
#'
#' @importFrom graphics abline axis barplot box grid layout legend lines mtext par plot points polygon segments text title
#' @importFrom grDevices adjustcolor rainbow
#' @importFrom stats cor lm sd coef complete.cases
"_PACKAGE"
