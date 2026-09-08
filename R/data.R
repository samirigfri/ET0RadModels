#' Sample Meteorological Data from Jhansi, India
#'
#' A dataset containing daily meteorological observations from the Central
#' Research Farm of ICAR-Indian Grassland and Fodder Research Institute
#' (IGFRI), Jhansi, Uttar Pradesh, India (25 deg 26' N, 78 deg 30' E,
#' 216 m above sea level). The data covers one year (2010) and is
#' representative of semi-arid climate conditions.
#'
#' @format A data frame with 365 rows and 8 columns:
#' \describe{
#'   \item{Date}{Date of observation (Date class).}
#'   \item{J}{Day of the year (1-365).}
#'   \item{Tmax}{Maximum air temperature (degrees Celsius).}
#'   \item{Tmin}{Minimum air temperature (degrees Celsius).}
#'   \item{RH_morning}{Morning relative humidity (percent).}
#'   \item{RH_evening}{Evening relative humidity (percent).}
#'   \item{SSH}{Bright sunshine hours.}
#'   \item{WS}{Wind speed at 2 m height (m/s).}
#' }
#' @details
#' The study area has a semi-arid climate with extreme summers (average
#' 32.7 degrees C) and mild winters (average 25.1 degrees C). The average
#' annual precipitation is 840 mm, with 90 percent contributed by southwest
#' monsoons between July and September. This dataset is suitable for
#' demonstrating the package functions and comparing ET0 model performance
#' in data-scarce semi-arid conditions.
#'
#' The latitude for this station is 25.43 degrees N and the elevation is
#' 216 m above sea level.
#'
#' @source ICAR-Indian Grassland and Fodder Research Institute (IGFRI),
#'   Jhansi, India.
#' @examples
#' data(jhansi_weather)
#' head(jhansi_weather)
#'
#' # Compute FAO-PM ET0 for the first day
#' with(jhansi_weather[1, ],
#'   et0_fao_pm(Tmin, Tmax, RH_morning, RH_evening, WS, SSH, J,
#'              lat = 25.43, z = 216))
"jhansi_weather"
