#' @title Radiation-Based ET0 Models
#' @name radiation_models
#' @description Functions implementing 10 radiation-based empirical models
#'   for reference evapotranspiration (ET0) estimation.
NULL

#' Jensen-Haise ET0 Model
#'
#' Estimates daily ET0 using the Jensen and Haise (1963) method based on
#' solar radiation and mean air temperature.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Jensen, M.E. & Haise, H.R. (1963). Estimating
#'   evapotranspiration from solar radiation. Journal of Irrigation and
#'   Drainage Division, ASCE, 89(4), 15-41.
#' @family radiation models
#' @export
#' @examples
#' et0_jensen_haise(25, 172, 25.43, 9)
et0_jensen_haise <- function(Tmean, J, lat, n) {
  Rs     <- solar_radiation(J, lat, n)
  lambda <- latent_heat(Tmean)
  (0.025 * Tmean + 0.08) * Rs / lambda
}

#' McGuinness-Bordne ET0 Model
#'
#' Estimates daily ET0 using the McGuinness and Bordne (1972) method based
#' on extraterrestrial radiation and mean air temperature.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references McGuinness, J.L. & Bordne, E.F. (1972). A comparison of
#'   lysimeter-derived potential evapotranspiration with computed values.
#'   USDA Technical Bulletin No. 1452.
#' @family radiation models
#' @export
#' @examples
#' et0_mcguinness_bordne(25, 172, 25.43, 9)
et0_mcguinness_bordne <- function(Tmean, J, lat, n) {
  Ra     <- extraterrestrial_radiation(J, lat)
  lambda <- latent_heat(Tmean)
  Ra / lambda * (Tmean + 5) / 68
}

#' Irmak ET0 Model
#'
#' Estimates daily ET0 using the Irmak et al. (2003) solar radiation-based
#' regression equation.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Irmak, S., Irmak, A., Allen, R.G., & Jones, J.W. (2003).
#'   Solar and net radiation-based equations to estimate reference
#'   evapotranspiration in humid climates. Journal of Irrigation and
#'   Drainage Engineering, 129(5), 336-347.
#' @family radiation models
#' @export
#' @examples
#' et0_irmak(25, 172, 25.43, 9)
et0_irmak <- function(Tmean, J, lat, n) {
  Rs <- solar_radiation(J, lat, n)
  pmax(-0.611 + 0.149 * Rs + 0.079 * Tmean, 0)
}

#' Makkink ET0 Model
#'
#' Estimates daily ET0 using the Makkink (1957) method based on solar
#' radiation and the slope of the saturation vapor pressure curve.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @param z Elevation above sea level (m). Default is 0.
#' @return Daily ET0 (mm/day).
#' @references Makkink, G.F. (1957). Testing the Penman formula by means
#'   of lysimeters. Journal of the Institution of Water Engineers, 11(3),
#'   277-288.
#' @family radiation models
#' @export
#' @examples
#' et0_makkink(25, 172, 25.43, 9, 216)
et0_makkink <- function(Tmean, J, lat, n, z = 0) {
  Rs     <- solar_radiation(J, lat, n)
  lambda <- latent_heat(Tmean)
  delta  <- slope_vapor_pressure(Tmean)
  gamma  <- psychrometric_constant(z = z)
  pmax(0.61 * (delta / (delta + gamma)) * Rs / lambda - 0.12, 0)
}

#' Turc ET0 Model
#'
#' Estimates daily ET0 using the Turc (1961) method based on solar radiation
#' and mean air temperature. Assumes humid conditions (RH >= 50%).
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Turc, L. (1961). Estimation of irrigation water requirements,
#'   potential evapotranspiration: a simple climatic formula evolved up to
#'   date. Annals of Agronomy, 12, 13-49.
#' @family radiation models
#' @export
#' @examples
#' et0_turc(25, 172, 25.43, 9)
et0_turc <- function(Tmean, J, lat, n) {
  Rs     <- solar_radiation(J, lat, n)
  Rs_cal <- Rs * 23.8856   # MJ/m2/day -> cal/cm2/day
  pmax(0.013 * (Tmean / (Tmean + 15)) * (Rs_cal + 50), 0)
}

#' Caprio ET0 Model
#'
#' Estimates daily ET0 using the Caprio (1974) method, a linear function
#' of solar radiation and mean temperature.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Caprio, J.M. (1974). The solar thermal unit concept in problems
#'   related to plant development and potential evapotranspiration. In:
#'   Phenology and Seasonality Modeling. Springer, Berlin, 353-364.
#' @family radiation models
#' @export
#' @examples
#' et0_caprio(25, 172, 25.43, 9)
et0_caprio <- function(Tmean, J, lat, n) {
  Rs     <- solar_radiation(J, lat, n)
  Rs_cal <- Rs * 23.8856   # MJ/m2/day -> cal/cm2/day
  pmax((7.0e-6 * Tmean + 3.2e-4) * Rs_cal, 0)
}

#' Abtew ET0 Model
#'
#' Estimates daily ET0 using the Abtew (1996) method, a simple solar
#' radiation-based approach developed for subtropical wetland conditions.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Abtew, W. (1996). Evapotranspiration measurements and
#'   modelling for three wetland systems in south Florida. JAWRA,
#'   32(3), 465-473.
#' @family radiation models
#' @export
#' @examples
#' et0_abtew(25, 172, 25.43, 9)
et0_abtew <- function(Tmean, J, lat, n) {
  Rs     <- solar_radiation(J, lat, n)
  lambda <- latent_heat(Tmean)
  pmax(0.53 * Rs / lambda, 0)
}

#' Priestley-Taylor ET0 Model
#'
#' Estimates daily ET0 using the Priestley and Taylor (1972) method, an
#' equilibrium evaporation approach using net radiation with an empirical
#' coefficient (alpha = 1.26).
#'
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param RH_morning Morning relative humidity (percent).
#' @param RH_evening Evening relative humidity (percent).
#' @param n Actual sunshine hours.
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param z Elevation above sea level (m). Default is 0.
#' @return Daily ET0 (mm/day).
#' @references Priestley, C.H.B. & Taylor, R.J. (1972). On the assessment
#'   of surface heat flux and evaporation using large-scale parameters.
#'   Monthly Weather Review, 100(2), 81-92.
#' @family radiation models
#' @export
#' @examples
#' et0_priestley_taylor(15, 35, 90, 40, 9, 172, 25.43, 216)
et0_priestley_taylor <- function(Tmin, Tmax, RH_morning, RH_evening,
                                 n, J, lat, z = 0) {
  Tmean  <- (Tmin + Tmax) / 2
  ea     <- actual_vapor_pressure(Tmin, Tmax, RH_morning, RH_evening)
  Rn     <- net_radiation(J, lat, n, Tmin, Tmax, ea)
  delta  <- slope_vapor_pressure(Tmean)
  gamma  <- psychrometric_constant(z = z)
  lambda <- latent_heat(Tmean)
  alpha  <- 1.26   # Priestley-Taylor coefficient
  G      <- 0      # Soil heat flux (zero for daily)
  pmax(alpha * (delta / (delta + gamma)) * (Rn - G) / lambda, 0)
}

#' Jones-Ritchie ET0 Model
#'
#' Estimates daily ET0 using the Jones and Ritchie (1990) radiation-based
#' approach, a simplified Priestley-Taylor variant with alpha = 1.1.
#'
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Jones, C.A. & Ritchie, J.T. (1990). Chapter 4: crop growth
#'   models. In: Jones, J.W. & Kiniry, J.R. (Eds.), CERES-Maize: A Simulation
#'   Model of Maize Growth and Development. Texas A&M University Press, 49-75.
#' @family radiation models
#' @export
#' @examples
#' et0_jones_ritchie(15, 35, 172, 25.43, 9)
et0_jones_ritchie <- function(Tmin, Tmax, J, lat, n) {
  Tmean  <- (Tmin + Tmax) / 2
  Rs     <- solar_radiation(J, lat, n)
  lambda <- latent_heat(Tmean)
  delta  <- slope_vapor_pressure(Tmean)
  gamma  <- psychrometric_constant()
  Rns    <- 0.77 * Rs   # simplified net shortwave radiation
  pmax(1.1 * (delta / (delta + gamma)) * Rns / lambda, 0)
}

#' Hargreaves (1975) Radiation-Based ET0 Model
#'
#' Estimates daily ET0 using the Hargreaves (1975) radiation-based method,
#' distinct from the Hargreaves-Samani (1985) temperature-range method.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @return Daily ET0 (mm/day).
#' @references Hargreaves, G.H. (1975). Moisture availability and crop
#'   production. Transactions of the ASAE, 18(5), 980-984.
#' @family radiation models
#' @export
#' @examples
#' et0_hargreaves(25, 172, 25.43, 9)
et0_hargreaves <- function(Tmean, J, lat, n) {
  Rs <- solar_radiation(J, lat, n)
  pmax(0.0135 * (Tmean + 17.78) * Rs * 0.408, 0)
}
