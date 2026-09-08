#' Latent Heat of Vaporization
#'
#' Computes the latent heat of vaporization as a function of air temperature
#' following Allen et al. (1998).
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @return Latent heat of vaporization (MJ/kg).
#' @references Allen, R.G., Pereira, L.S., Raes, D., & Smith, M. (1998).
#'   FAO Irrigation and Drainage Paper No. 56.
#' @export
#' @examples
#' latent_heat(25)
latent_heat <- function(Tmean) {
  2.501 - 0.002361 * Tmean
}

#' Psychrometric Constant
#'
#' Computes the psychrometric constant as a function of atmospheric pressure.
#'
#' @param P Atmospheric pressure (kPa). If \code{NULL}, computed from elevation.
#' @param z Elevation above sea level (m). Used only if \code{P} is \code{NULL}.
#' @return Psychrometric constant (kPa/degrees C).
#' @references Allen et al. (1998), FAO-56, Eq. 8.
#' @export
#' @examples
#' psychrometric_constant(z = 216)
psychrometric_constant <- function(P = NULL, z = 0) {
  if (is.null(P)) {
    P <- 101.3 * ((293 - 0.0065 * z) / 293)^5.26
  }
  0.665e-3 * P
}

#' Saturation Vapor Pressure
#'
#' Computes the saturation vapor pressure at a given temperature using the
#' Tetens formula (Allen et al., 1998).
#'
#' @param T Air temperature (degrees Celsius).
#' @return Saturation vapor pressure (kPa).
#' @references Allen et al. (1998), FAO-56, Eq. 11.
#' @export
#' @examples
#' saturation_vapor_pressure(25)
saturation_vapor_pressure <- function(T) {
  0.6108 * exp((17.27 * T) / (T + 237.3))
}

#' Actual Vapor Pressure
#'
#' Computes actual vapor pressure from relative humidity and temperature.
#'
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param RH_morning Morning relative humidity (percent).
#' @param RH_evening Evening relative humidity (percent).
#' @return Actual vapor pressure (kPa).
#' @references Allen et al. (1998), FAO-56, Eq. 17.
#' @export
#' @examples
#' actual_vapor_pressure(15, 30, 90, 40)
actual_vapor_pressure <- function(Tmin, Tmax, RH_morning, RH_evening) {
  e_tmin <- saturation_vapor_pressure(Tmin)
  e_tmax <- saturation_vapor_pressure(Tmax)
  (e_tmin * (RH_morning / 100) + e_tmax * (RH_evening / 100)) / 2
}

#' Slope of Saturation Vapor Pressure Curve
#'
#' Computes the slope of the saturation vapor pressure-temperature curve.
#'
#' @param Tmean Mean air temperature (degrees Celsius).
#' @return Slope of saturation vapor pressure curve (kPa/degrees C).
#' @references Allen et al. (1998), FAO-56, Eq. 13.
#' @export
#' @examples
#' slope_vapor_pressure(25)
slope_vapor_pressure <- function(Tmean) {
  (4098 * 0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3))) /
    (Tmean + 237.3)^2
}

#' Dew Point Temperature
#'
#' Estimates dew point temperature from actual vapor pressure.
#'
#' @param ea Actual vapor pressure (kPa).
#' @return Dew point temperature (degrees Celsius).
#' @export
#' @examples
#' ea <- actual_vapor_pressure(15, 30, 90, 40)
#' dew_point_temperature(ea)
dew_point_temperature <- function(ea) {
  (237.3 * log(ea / 0.6108)) / (17.27 - log(ea / 0.6108))
}

#' Daylight Hours
#'
#' Computes the maximum possible daylight hours for a given day and latitude.
#'
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees, negative for Southern Hemisphere).
#' @return Maximum daylight hours.
#' @references Allen et al. (1998), FAO-56, Eq. 34.
#' @export
#' @examples
#' daylight_hours(172, 25.43)
daylight_hours <- function(J, lat) {
  lat_rad <- lat * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi / 365 * J)
  delta <- 0.409 * sin(2 * pi / 365 * J - 1.39)
  ws <- acos(-tan(lat_rad) * tan(delta))
  24 / pi * ws
}

#' Extraterrestrial Radiation
#'
#' Computes the extraterrestrial radiation (Ra) for a given day and latitude
#' following Allen et al. (1998).
#'
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees, negative for Southern Hemisphere).
#' @return Extraterrestrial radiation (MJ/m2/day).
#' @references Allen et al. (1998), FAO-56, Eq. 21.
#' @export
#' @examples
#' extraterrestrial_radiation(172, 25.43)
extraterrestrial_radiation <- function(J, lat) {
  Gsc <- 0.0820  # Solar constant (MJ/m2/min)
  lat_rad <- lat * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi / 365 * J)
  delta <- 0.409 * sin(2 * pi / 365 * J - 1.39)
  ws <- acos(-tan(lat_rad) * tan(delta))
  Ra <- (24 * 60 / pi) * Gsc * dr *
    (ws * sin(lat_rad) * sin(delta) +
       cos(lat_rad) * cos(delta) * sin(ws))
  Ra
}

#' Solar Radiation (Angstrom)
#'
#' Estimates incoming solar radiation (Rs) using the Angstrom formula with
#' sunshine hours.
#'
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @param a_s Angstrom coefficient a (default 0.25).
#' @param b_s Angstrom coefficient b (default 0.50).
#' @return Solar radiation (MJ/m2/day).
#' @references Allen et al. (1998), FAO-56, Eq. 35.
#' @export
#' @examples
#' solar_radiation(172, 25.43, 8)
solar_radiation <- function(J, lat, n, a_s = 0.25, b_s = 0.50) {
  Ra <- extraterrestrial_radiation(J, lat)
  N <- daylight_hours(J, lat)
  N[N == 0] <- NA
  Rs <- (a_s + b_s * (n / N)) * Ra
  Rs
}

#' Net Radiation
#'
#' Computes net radiation (Rn) following the FAO-56 approach.
#'
#' @param J Day of the year (1-366).
#' @param lat Latitude (degrees).
#' @param n Actual sunshine hours.
#' @param Tmin Minimum air temperature (degrees Celsius).
#' @param Tmax Maximum air temperature (degrees Celsius).
#' @param ea Actual vapor pressure (kPa).
#' @param albedo Surface albedo (default 0.23 for reference crop).
#' @param a_s Angstrom coefficient a (default 0.25).
#' @param b_s Angstrom coefficient b (default 0.50).
#' @return Net radiation (MJ/m2/day).
#' @references Allen et al. (1998), FAO-56.
#' @export
#' @examples
#' ea <- actual_vapor_pressure(15, 30, 90, 40)
#' net_radiation(172, 25.43, 8, 15, 30, ea)
net_radiation <- function(J, lat, n, Tmin, Tmax, ea,
                          albedo = 0.23, a_s = 0.25, b_s = 0.50) {
  Rs <- solar_radiation(J, lat, n, a_s, b_s)
  Ra <- extraterrestrial_radiation(J, lat)
  N <- daylight_hours(J, lat)

  # Net shortwave radiation
  Rns <- (1 - albedo) * Rs

  # Net longwave radiation (FAO-56 Eq. 39)
  sigma <- 4.903e-9  # Stefan-Boltzmann (MJ/m2/day/K4)
  Rso <- (0.75 + 2e-5 * 0) * Ra  # clear-sky radiation (z=0 as default)

  # Guard against division by zero
  Rs_Rso_ratio <- ifelse(Rso > 0, pmin(Rs / Rso, 1), 0.5)

  Rnl <- sigma * ((Tmax + 273.16)^4 + (Tmin + 273.16)^4) / 2 *
    (0.34 - 0.14 * sqrt(ea)) *
    (1.35 * Rs_Rso_ratio - 0.35)

  Rn <- Rns - Rnl
  Rn
}

#' Convert Vapor Pressure from kPa to hPa
#'
#' @param kPa Vapor pressure in kPa.
#' @return Vapor pressure in hPa.
#' @keywords internal
kpa_to_hpa <- function(kPa) {
  kPa * 10
}

#' Convert Vapor Pressure from kPa to mmHg
#'
#' @param kPa Vapor pressure in kPa.
#' @return Vapor pressure in mmHg.
#' @keywords internal
kpa_to_mmhg <- function(kPa) {
  kPa * 7.50062
}

#' Convert Wind Speed from m/s to miles/day
#'
#' @param u Wind speed in m/s.
#' @return Wind speed in miles/day.
#' @keywords internal
ms_to_miles_day <- function(u) {
  u * 86.4 / 1.609344
}
