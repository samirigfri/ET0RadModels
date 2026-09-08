test_that("saturation_vapor_pressure returns correct values", {
  # At 20 C, es should be approximately 2.338 kPa (FAO-56)
  expect_equal(saturation_vapor_pressure(20), 2.338, tolerance = 0.01)

  # At 0 C, es should be approximately 0.611 kPa
  expect_equal(saturation_vapor_pressure(0), 0.6108, tolerance = 0.001)

  # Vectorized
  result <- saturation_vapor_pressure(c(0, 20, 40))
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("slope_vapor_pressure returns correct values", {
  # At 20 C, delta should be approximately 0.1447 kPa/C (FAO-56)
  expect_equal(slope_vapor_pressure(20), 0.1447, tolerance = 0.01)
})

test_that("psychrometric_constant works with elevation", {
  # At sea level, gamma ~ 0.0665 kPa/C
  gamma_0 <- psychrometric_constant(z = 0)
  expect_equal(gamma_0, 0.0674, tolerance = 0.005)

  # At higher elevation, gamma should be smaller
  gamma_216 <- psychrometric_constant(z = 216)
  expect_true(gamma_216 < gamma_0)
})

test_that("latent_heat returns correct values", {
  # At 20 C, lambda = 2.501 - 0.002361*20 = 2.4538 MJ/kg
  expect_equal(latent_heat(20), 2.4538, tolerance = 0.001)
})

test_that("extraterrestrial_radiation is positive and seasonal", {
  lat <- 25.43
  # Summer solstice (J=172) should have more Ra than winter solstice (J=355)
  Ra_summer <- extraterrestrial_radiation(172, lat)
  Ra_winter <- extraterrestrial_radiation(355, lat)
  expect_true(Ra_summer > 0)
  expect_true(Ra_winter > 0)
  expect_true(Ra_summer > Ra_winter)
})

test_that("solar_radiation is less than extraterrestrial radiation", {
  J <- 172
  lat <- 25.43
  Ra <- extraterrestrial_radiation(J, lat)
  Rs <- solar_radiation(J, lat, n = 8)
  expect_true(Rs > 0)
  expect_true(Rs < Ra)
})

test_that("daylight_hours are between 0 and 24", {
  for (J in c(1, 91, 172, 266)) {
    N <- daylight_hours(J, 25.43)
    expect_true(N > 0 && N < 24)
  }
})

test_that("actual_vapor_pressure is less than saturation", {
  Tmin <- 15
  Tmax <- 30
  es <- (saturation_vapor_pressure(Tmax) + saturation_vapor_pressure(Tmin)) / 2
  ea <- actual_vapor_pressure(Tmin, Tmax, 90, 40)
  expect_true(ea > 0)
  expect_true(ea < es)
})

test_that("dew_point_temperature returns reasonable values", {
  ea <- actual_vapor_pressure(15, 30, 90, 40)
  Td <- dew_point_temperature(ea)
  expect_true(Td < 30)  # Dew point should be less than Tmax
  expect_true(Td > -10)  # Should be reasonable
})
