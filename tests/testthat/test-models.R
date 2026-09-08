# Test parameters representing a typical semi-arid summer day
Tmin <- 25
Tmax <- 40
Tmean <- (Tmin + Tmax) / 2
RH_morning <- 70
RH_evening <- 30
u2 <- 1.5
n  <- 9
J  <- 172   # June 21
lat <- 25.43
z   <- 216

# --- FAO Penman-Monteith (reference/base model) ---
test_that("et0_fao_pm returns positive values", {
  result <- et0_fao_pm(Tmin, Tmax, RH_morning, RH_evening, u2, n, J, lat, z)
  expect_true(result > 0)
  expect_true(result < 20)
})

test_that("et0_fao_pm is vectorized", {
  result <- et0_fao_pm(
    Tmin = c(10, 20, 25), Tmax = c(25, 35, 40),
    RH_morning = c(90, 80, 70), RH_evening = c(50, 40, 30),
    u2 = c(1, 1.5, 2), n = c(7, 8, 9),
    J = c(15, 91, 172), lat = 25.43, z = 216
  )
  expect_length(result, 3)
  expect_true(all(result > 0))
})

# --- Radiation-based models ---
test_that("radiation-based models return non-negative values", {
  expect_true(et0_jensen_haise(Tmean, J, lat, n) > 0)
  expect_true(et0_mcguinness_bordne(Tmean, J, lat, n) > 0)
  expect_true(et0_irmak(Tmean, J, lat, n) > 0)
  expect_true(et0_makkink(Tmean, J, lat, n, z) > 0)
  expect_true(et0_turc(Tmean, J, lat, n) > 0)
  expect_true(et0_caprio(Tmean, J, lat, n) > 0)
  expect_true(et0_abtew(Tmean, J, lat, n) > 0)
  expect_true(et0_priestley_taylor(Tmin, Tmax, RH_morning, RH_evening,
                                    n, J, lat, z) > 0)
  expect_true(et0_jones_ritchie(Tmin, Tmax, J, lat, n) > 0)
  expect_true(et0_hargreaves(Tmean, J, lat, n) > 0)
})

test_that("radiation-based models return reasonable values (0-20 mm/day)", {
  expect_true(et0_jensen_haise(Tmean, J, lat, n) < 20)
  expect_true(et0_mcguinness_bordne(Tmean, J, lat, n) < 20)
  expect_true(et0_irmak(Tmean, J, lat, n) < 20)
  expect_true(et0_makkink(Tmean, J, lat, n, z) < 20)
  expect_true(et0_turc(Tmean, J, lat, n) < 20)
  expect_true(et0_caprio(Tmean, J, lat, n) < 20)
  expect_true(et0_abtew(Tmean, J, lat, n) < 20)
  expect_true(et0_priestley_taylor(Tmin, Tmax, RH_morning, RH_evening,
                                    n, J, lat, z) < 20)
  expect_true(et0_jones_ritchie(Tmin, Tmax, J, lat, n) < 20)
  expect_true(et0_hargreaves(Tmean, J, lat, n) < 20)
})

# --- Wrapper function ---
test_that("et0_rad_all returns correct structure", {
  result <- et0_rad_all(Tmin, Tmax, RH_morning, RH_evening, u2, n, J, lat, z)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 11)   # FAO_PM + 10 radiation-based models
  expect_true("FAO_PM" %in% names(result))
  expect_true("Jensen_Haise" %in% names(result))
  expect_true("Priestley_Taylor" %in% names(result))
  expect_true("Hargreaves_1975" %in% names(result))
  expect_true(all(result >= 0))
})

test_that("et0_rad_all handles vectorized input", {
  result <- et0_rad_all(
    Tmin = c(10, 25), Tmax = c(25, 40),
    RH_morning = c(90, 70), RH_evening = c(50, 30),
    u2 = c(1, 1.5), n = c(7, 9),
    J = c(15, 172), lat = 25.43, z = 216
  )
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 11)
})
