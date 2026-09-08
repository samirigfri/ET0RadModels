#' Compute ET0 Using All 10 Radiation-Based Models and FAO Penman-Monteith
#'
#' A convenience wrapper that computes daily reference evapotranspiration using
#' all 10 radiation-based empirical models plus the FAO Penman-Monteith
#' standard method as the reference/base for comparison.
#'
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param RH_morning Morning relative humidity (percent). Used by
#'   Priestley-Taylor and FAO Penman-Monteith.
#' @param RH_evening Evening relative humidity (percent). Used by
#'   Priestley-Taylor and FAO Penman-Monteith.
#' @param u2 Wind speed at 2 m height (m/s). Used by FAO Penman-Monteith only.
#' @param n Actual sunshine hours.
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees, negative for Southern Hemisphere).
#' @param z Elevation above sea level (m). Default is 0.
#' @return A \code{data.frame} with 11 columns: \code{FAO_PM} (reference) plus
#'   one column per radiation-based model. The number of rows equals the
#'   length of the input vectors.
#' @details
#' The 10 radiation-based models included are:
#' \itemize{
#'   \item Jensen-Haise (1963)
#'   \item McGuinness-Bordne (1972)
#'   \item Irmak (2003)
#'   \item Makkink (1957)
#'   \item Turc (1961)
#'   \item Caprio (1974)
#'   \item Abtew (1996)
#'   \item Priestley-Taylor (1972)
#'   \item Jones-Ritchie (1990)
#'   \item Hargreaves (1975)
#' }
#' The \strong{FAO Penman-Monteith} column serves as the standard reference
#' against which models can be evaluated using \code{\link{evaluate_models}}.
#' @export
#' @examples
#' result <- et0_rad_all(Tmin = 15, Tmax = 35, RH_morning = 90,
#'                       RH_evening = 40, u2 = 1.5, n = 9,
#'                       J = 172, lat = 25.43, z = 216)
#' print(result)
et0_rad_all <- function(Tmin, Tmax, RH_morning, RH_evening, u2, n, J,
                        lat, z = 0) {
  Tmean <- (Tmin + Tmax) / 2

  result <- data.frame(
    # FAO Penman-Monteith (reference/base model)
    FAO_PM            = et0_fao_pm(Tmin, Tmax, RH_morning, RH_evening,
                                   u2, n, J, lat, z),

    # Radiation-based models
    Jensen_Haise      = et0_jensen_haise(Tmean, J, lat, n),
    McGuinness_Bordne = et0_mcguinness_bordne(Tmean, J, lat, n),
    Irmak             = et0_irmak(Tmean, J, lat, n),
    Makkink           = et0_makkink(Tmean, J, lat, n, z),
    Turc              = et0_turc(Tmean, J, lat, n),
    Caprio            = et0_caprio(Tmean, J, lat, n),
    Abtew             = et0_abtew(Tmean, J, lat, n),
    Priestley_Taylor  = et0_priestley_taylor(Tmin, Tmax, RH_morning,
                                             RH_evening, n, J, lat, z),
    Jones_Ritchie     = et0_jones_ritchie(Tmin, Tmax, J, lat, n),
    Hargreaves_1975   = et0_hargreaves(Tmean, J, lat, n),

    stringsAsFactors = FALSE
  )

  result
}
