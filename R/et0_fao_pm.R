#' FAO Penman-Monteith Reference Evapotranspiration
#'
#' Computes daily reference evapotranspiration (ET0) using the FAO
#' Penman-Monteith equation (Allen et al., 1998). This is the standard
#' reference method recommended by FAO.
#'
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param RH_morning Morning relative humidity (percent).
#' @param RH_evening Evening relative humidity (percent).
#' @param u2 Wind speed at 2 m height (m/s).
#' @param n Actual sunshine hours.
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees, negative for Southern Hemisphere).
#' @param z Elevation above sea level (m).
#' @return Daily ET0 (mm/day).
#' @references
#' Allen, R.G., Pereira, L.S., Raes, D., & Smith, M. (1998). Crop
#' evapotranspiration: Guidelines for computing crop water requirements.
#' FAO Irrigation and Drainage Paper No. 56.
#' @export
#' @examples
#' et0_fao_pm(Tmin = 15, Tmax = 30, RH_morning = 90, RH_evening = 40,
#'            u2 = 1.5, n = 8, J = 172, lat = 25.43, z = 216)
et0_fao_pm <- function(Tmin, Tmax, RH_morning, RH_evening, u2, n, J,
                       lat, z = 0) {
  Tmean <- (Tmin + Tmax) / 2

  # Intermediate calculations
  delta <- slope_vapor_pressure(Tmean)
  gamma <- psychrometric_constant(z = z)
  es <- (saturation_vapor_pressure(Tmax) + saturation_vapor_pressure(Tmin)) / 2
  ea <- actual_vapor_pressure(Tmin, Tmax, RH_morning, RH_evening)

  # Radiation
  Rn <- net_radiation(J, lat, n, Tmin, Tmax, ea)
  G <- 0  # Soil heat flux assumed zero for daily calculation

  # FAO-PM equation
  numerator <- 0.408 * delta * (Rn - G) +
    gamma * (900 / (Tmean + 273)) * u2 * (es - ea)
  denominator <- delta + gamma * (1 + 0.34 * u2)

  ET0 <- numerator / denominator
  ET0 <- pmax(ET0, 0)  # ET0 cannot be negative
  ET0
}
