# Known test vectors
obs <- c(3.5, 4.2, 5.1, 2.8, 6.0, 3.9, 4.5)
pred <- c(3.3, 4.5, 4.9, 3.0, 5.8, 4.1, 4.3)

test_that("calc_nse returns correct values", {
  nse <- calc_nse(obs, pred)
  # NSE should be close to 1 for good predictions
  expect_true(nse > 0.9)
  expect_true(nse <= 1)

  # Perfect prediction
  expect_equal(calc_nse(obs, obs), 1)
})

test_that("calc_d returns values between 0 and 1", {
  d <- calc_d(obs, pred)
  expect_true(d >= 0 && d <= 1)

  # Perfect prediction
  expect_equal(calc_d(obs, obs), 1)
})

test_that("calc_mse is non-negative", {
  mse <- calc_mse(obs, pred)
  expect_true(mse >= 0)

  # Perfect prediction
  expect_equal(calc_mse(obs, obs), 0)
})

test_that("calc_rmse is the square root of MSE", {
  expect_equal(calc_rmse(obs, pred), sqrt(calc_mse(obs, pred)))
})

test_that("calc_nrmse is RMSE divided by mean observed", {
  nrmse <- calc_nrmse(obs, pred)
  expected <- calc_rmse(obs, pred) / mean(obs)
  expect_equal(nrmse, expected)
})

test_that("calc_mae is non-negative", {
  mae <- calc_mae(obs, pred)
  expect_true(mae >= 0)
  expect_equal(calc_mae(obs, obs), 0)
})

test_that("calc_mbe returns positive value for overestimation", {
  mbe <- calc_mbe(obs, pred)
  expect_true(mbe >= 0)
})

test_that("calc_r returns value between -1 and 1", {
  r <- calc_r(obs, pred)
  expect_true(r >= -1 && r <= 1)
  expect_equal(calc_r(obs, obs), 1)
})

test_that("calc_r2 is the square of calc_r", {
  expect_equal(calc_r2(obs, pred), calc_r(obs, pred)^2)
})

test_that("evaluate_models returns correct structure", {
  preds <- data.frame(Model_A = pred, Model_B = pred * 1.1)
  result <- evaluate_models(obs, preds)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("Model", "NSE", "d", "MSE", "RMSE", "NRMSE",
                     "MAE", "MBE", "r", "R2") %in% names(result)))
})

test_that("metrics handle NA values", {
  obs_na <- c(3.5, NA, 5.1, 2.8, 6.0)
  pred_na <- c(3.3, 4.5, NA, 3.0, 5.8)
  # Should not error
  expect_type(calc_nse(obs_na, pred_na), "double")
  expect_type(calc_rmse(obs_na, pred_na), "double")
})
