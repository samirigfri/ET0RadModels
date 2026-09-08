#' @title Statistical Evaluation Metrics for ET0 Models
#' @name metrics
#' @description Functions to compute statistical performance metrics for
#'   comparing ET0 model estimates against a reference (e.g., FAO
#'   Penman-Monteith).
NULL

#' Nash-Sutcliffe Efficiency (NSE)
#'
#' Computes the Nash-Sutcliffe efficiency coefficient. A value of 1 indicates
#' perfect agreement; values below 0 indicate the model is worse than the mean.
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return NSE value (numeric scalar).
#' @references Nash, J.E. & Sutcliffe, J.V. (1970). River flow forecasting
#'   through conceptual models. Journal of Hydrology, 10(3), 282-290.
#' @export
#' @examples
#' obs <- c(3.5, 4.2, 5.1, 2.8, 6.0)
#' pred <- c(3.3, 4.5, 4.9, 3.0, 5.8)
#' calc_nse(obs, pred)
calc_nse <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}

#' Willmott's Index of Agreement (d)
#'
#' Computes Willmott's index of agreement. Values range from 0 to 1, with 1
#' indicating perfect agreement.
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return Index of agreement (numeric scalar between 0 and 1).
#' @references Willmott, C.J. (1981). On the validation of models. Physical
#'   Geography, 2(2), 184-194.
#' @export
#' @examples
#' obs <- c(3.5, 4.2, 5.1, 2.8, 6.0)
#' pred <- c(3.3, 4.5, 4.9, 3.0, 5.8)
#' calc_d(obs, pred)
calc_d <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  obs_mean <- mean(observed)
  1 - sum((observed - predicted)^2) /
    sum((abs(predicted - obs_mean) + abs(observed - obs_mean))^2)
}

#' Mean Squared Error (MSE)
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return MSE value (numeric scalar).
#' @export
#' @examples
#' calc_mse(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_mse <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  mean((observed - predicted)^2)
}

#' Root Mean Squared Error (RMSE)
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return RMSE value (numeric scalar).
#' @export
#' @examples
#' calc_rmse(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_rmse <- function(observed, predicted) {
  sqrt(calc_mse(observed, predicted))
}

#' Normalized Root Mean Squared Error (NRMSE)
#'
#' RMSE normalized by the mean of observed values.
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return NRMSE value (numeric scalar).
#' @export
#' @examples
#' calc_nrmse(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_nrmse <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  rmse <- sqrt(mean((observed - predicted)^2))
  rmse / mean(observed)
}

#' Mean Absolute Error (MAE)
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return MAE value (numeric scalar).
#' @export
#' @examples
#' calc_mae(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_mae <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  mean(abs(observed - predicted))
}

#' Mean Bias Error (MBE)
#'
#' Computes the mean bias error as mean percentage absolute error.
#' Positive values indicate overall overestimation by the model.
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return MBE value (numeric scalar, as percentage).
#' @export
#' @examples
#' calc_mbe(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_mbe <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  observed <- observed[cc]
  predicted <- predicted[cc]
  # Guard against division by zero
  valid <- observed != 0
  if (sum(valid) == 0) return(NA_real_)
  mean(abs((observed[valid] - predicted[valid]) / observed[valid]) * 100)
}

#' Pearson Correlation Coefficient (r)
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return Pearson correlation coefficient (numeric scalar).
#' @export
#' @examples
#' calc_r(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_r <- function(observed, predicted) {
  cc <- complete.cases(observed, predicted)
  cor(observed[cc], predicted[cc])
}

#' Coefficient of Determination (R-squared)
#'
#' Computes R-squared as the square of the Pearson correlation coefficient.
#'
#' @param observed Numeric vector of observed (reference) values.
#' @param predicted Numeric vector of predicted (model) values.
#' @return R-squared value (numeric scalar between 0 and 1).
#' @export
#' @examples
#' calc_r2(c(3.5, 4.2, 5.1), c(3.3, 4.5, 4.9))
calc_r2 <- function(observed, predicted) {
  calc_r(observed, predicted)^2
}

#' Evaluate Multiple ET0 Models Against a Reference
#'
#' Computes all 9 statistical evaluation metrics for one or more ET0 models
#' compared against a reference method (typically FAO Penman-Monteith).
#'
#' @param observed Numeric vector of observed (reference) ET0 values.
#' @param predicted A named list or data.frame of predicted ET0 values. Each
#'   element/column represents a different model.
#' @return A \code{data.frame} with one row per model and columns for each
#'   metric: NSE, d, MSE, RMSE, NRMSE, MAE, MBE, r, R2.
#' @export
#' @examples
#' obs <- c(3.5, 4.2, 5.1, 2.8, 6.0)
#' preds <- data.frame(
#'   Model_A = c(3.3, 4.5, 4.9, 3.0, 5.8),
#'   Model_B = c(4.0, 4.0, 5.5, 2.5, 6.5)
#' )
#' evaluate_models(obs, preds)
evaluate_models <- function(observed, predicted) {
  if (is.data.frame(predicted)) {
    predicted <- as.list(predicted)
  }
  if (!is.list(predicted)) {
    stop("'predicted' must be a named list or data.frame")
  }

  results <- lapply(names(predicted), function(nm) {
    pred <- predicted[[nm]]
    data.frame(
      Model = nm,
      NSE   = calc_nse(observed, pred),
      d     = calc_d(observed, pred),
      MSE   = calc_mse(observed, pred),
      RMSE  = calc_rmse(observed, pred),
      NRMSE = calc_nrmse(observed, pred),
      MAE   = calc_mae(observed, pred),
      MBE   = calc_mbe(observed, pred),
      r     = calc_r(observed, pred),
      R2    = calc_r2(observed, pred),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
