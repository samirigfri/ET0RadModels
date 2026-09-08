#' @title Visualization Functions for ET0 Model Comparison
#' @name plots
#' @description Functions for graphical evaluation of ET0 model performance
#'   including scatter plots, Taylor diagrams, monthly comparison plots, and
#'   annual bar charts.
NULL

#' Scatter Plot of Predicted vs Observed ET0
#'
#' Creates a scatter plot comparing predicted ET0 values from one or more
#' models against observed (reference) values, with 1:1 line and regression
#' lines.
#'
#' @param observed Numeric vector of observed (reference) ET0 values.
#' @param predicted A named list or data.frame of predicted ET0 values.
#' @param model_names Character vector of model names to plot. If \code{NULL},
#'   all models in \code{predicted} are plotted.
#' @param main Title for the plot.
#' @param colors Character vector of colors. If \code{NULL}, colors are
#'   generated automatically.
#' @param ... Additional arguments passed to \code{plot}.
#' @return Invisible \code{NULL}. Called for its side effect of producing a plot.
#' @export
#' @examples
#' obs <- c(3.5, 4.2, 5.1, 2.8, 6.0, 3.1, 4.8)
#' preds <- data.frame(
#'   Model_A = c(3.3, 4.5, 4.9, 3.0, 5.8, 3.4, 4.6),
#'   Model_B = c(4.0, 4.0, 5.5, 2.5, 6.5, 3.8, 5.0)
#' )
#' plot_scatter(obs, preds)
plot_scatter <- function(observed, predicted, model_names = NULL,
                         main = "Predicted vs Observed ET0",
                         colors = NULL, ...) {
  if (is.data.frame(predicted)) predicted <- as.list(predicted)
  if (!is.null(model_names)) {
    predicted <- predicted[model_names]
  }
  n_models <- length(predicted)

  if (is.null(colors)) {
    colors <- grDevices::rainbow(n_models, s = 0.7, v = 0.8)
  }

  all_vals <- c(observed, unlist(predicted))
  lim <- range(all_vals, na.rm = TRUE)

  plot(NA, xlim = lim, ylim = lim,
       xlab = "FAO-PM ET0 (mm/day)", ylab = "Model ET0 (mm/day)",
       main = main, ...)
  abline(0, 1, lty = 2, col = "gray40", lwd = 1.5)
  grid(col = "gray90")

  for (i in seq_along(predicted)) {
    points(observed, predicted[[i]],
           col = adjustcolor(colors[i], alpha.f = 0.6),
           pch = 16, cex = 0.6)
    fit <- lm(predicted[[i]] ~ observed)
    abline(fit, col = colors[i], lwd = 1.5)
  }

  legend("topleft", legend = names(predicted), col = colors,
         pch = 16, lty = 1, lwd = 1.5, cex = 0.7, bg = "white")
  invisible(NULL)
}

#' Taylor Diagram
#'
#' Creates a Taylor diagram showing the correlation coefficient, standard
#' deviation, and centered RMSE of multiple models relative to the observed
#' reference.
#'
#' @param observed Numeric vector of observed (reference) ET0 values.
#' @param predicted A named list or data.frame of predicted ET0 values.
#' @param model_names Character vector of model names to plot. If \code{NULL},
#'   all models are plotted.
#' @param main Title for the plot.
#' @param colors Character vector of colors. If \code{NULL}, colors are
#'   generated automatically.
#' @param normalize Logical; if \code{TRUE}, standard deviations are normalized
#'   by the observed standard deviation (default \code{FALSE}).
#' @return Invisible \code{NULL}. Called for its side effect of producing a plot.
#' @export
#' @examples
#' set.seed(42)
#' obs <- rnorm(100, mean = 5, sd = 2)
#' preds <- list(
#'   Model_A = obs + rnorm(100, 0, 0.5),
#'   Model_B = obs * 1.1 + rnorm(100, 0, 1)
#' )
#' plot_taylor(obs, preds)
plot_taylor <- function(observed, predicted, model_names = NULL,
                        main = "Taylor Diagram", colors = NULL,
                        normalize = FALSE) {
  if (is.data.frame(predicted)) predicted <- as.list(predicted)
  if (!is.null(model_names)) predicted <- predicted[model_names]

  cc_obs <- complete.cases(observed)
  observed <- observed[cc_obs]

  n_models <- length(predicted)
  if (is.null(colors)) {
    colors <- grDevices::rainbow(n_models, s = 0.7, v = 0.8)
  }

  sd_obs <- sd(observed)

  # Compute stats for each model
  stats_list <- lapply(predicted, function(pred) {
    pred <- pred[cc_obs]
    cc <- complete.cases(pred)
    obs_c <- observed[cc]
    pred_c <- pred[cc]
    list(
      sd = sd(pred_c),
      r = cor(obs_c, pred_c),
      crmse = sqrt(mean(((pred_c - mean(pred_c)) -
                            (obs_c - mean(obs_c)))^2))
    )
  })

  # Normalize if requested
  if (normalize) {
    for (i in seq_along(stats_list)) {
      stats_list[[i]]$sd <- stats_list[[i]]$sd / sd_obs
      stats_list[[i]]$crmse <- stats_list[[i]]$crmse / sd_obs
    }
    sd_obs_plot <- 1
  } else {
    sd_obs_plot <- sd_obs
  }

  # Maximum SD for axis
  all_sd <- c(sd_obs_plot, sapply(stats_list, function(x) x$sd))
  max_sd <- max(all_sd, na.rm = TRUE) * 1.3

  # Set up polar plot
  old_par <- par(mar = c(5, 5, 4, 2))
  on.exit(par(old_par))

  plot(NA, xlim = c(0, max_sd), ylim = c(0, max_sd),
       xlab = "Standard Deviation", ylab = "Standard Deviation",
       main = main, asp = 1)

  # Draw correlation arcs
  angles <- seq(0, pi / 2, length.out = 200)
  r_labels <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
  for (r_val in r_labels) {
    theta <- acos(r_val)
    lines(c(0, max_sd * cos(theta)), c(0, max_sd * sin(theta)),
          col = "gray85", lty = 3)
    text(max_sd * 0.97 * cos(theta), max_sd * 0.97 * sin(theta),
         labels = r_val, cex = 0.55, col = "gray50")
  }

  # Draw SD arcs
  sd_ticks <- pretty(c(0, max_sd), n = 5)
  sd_ticks <- sd_ticks[sd_ticks > 0 & sd_ticks <= max_sd]
  for (s in sd_ticks) {
    arc_angles <- seq(0, pi / 2, length.out = 100)
    lines(s * cos(arc_angles), s * sin(arc_angles),
          col = "gray90", lty = 1)
  }

  # Draw CRMSE circles centered on the reference point
  crmse_ticks <- pretty(c(0, max_sd), n = 4)
  crmse_ticks <- crmse_ticks[crmse_ticks > 0]
  for (cr in crmse_ticks) {
    arc_angles <- seq(0, 2 * pi, length.out = 200)
    cx <- sd_obs_plot + cr * cos(arc_angles)
    cy <- cr * sin(arc_angles)
    valid <- cx >= 0 & cy >= 0 & cx <= max_sd & cy <= max_sd
    lines(cx[valid], cy[valid], col = "gray80", lty = 2)
  }

  # Plot reference point
  points(sd_obs_plot, 0, pch = 18, col = "black", cex = 2)
  text(sd_obs_plot, -max_sd * 0.04, "REF", cex = 0.7, font = 2)

  # Plot each model
  pchs <- rep(c(16, 17, 15, 18, 8), length.out = n_models)
  for (i in seq_along(stats_list)) {
    s <- stats_list[[i]]
    x <- s$sd * s$r
    y <- s$sd * sin(acos(s$r))
    points(x, y, pch = pchs[i], col = colors[i], cex = 1.5)
  }

  legend("topright", legend = names(predicted), col = colors,
         pch = pchs, cex = 0.6, bg = "white", ncol = 2)

  invisible(NULL)
}

#' Monthly Comparison Plot
#'
#' Creates a line plot comparing the average monthly ET0 from multiple models
#' against the FAO-PM reference.
#'
#' @param monthly_data A data.frame with columns: \code{Month} (1-12) and one
#'   column per model containing mean monthly ET0 values. Must include a
#'   \code{FAO_PM} column.
#' @param model_names Character vector of model names to plot (in addition to
#'   FAO_PM). If \code{NULL}, all models are plotted.
#' @param main Title for the plot.
#' @param colors Named character vector of colors per model. If \code{NULL},
#'   colors are generated automatically.
#' @return Invisible \code{NULL}.
#' @export
#' @examples
#' monthly <- data.frame(
#'   Month = 1:12,
#'   FAO_PM = c(2.0, 2.5, 3.5, 5.0, 6.5, 7.0, 6.0, 5.5, 4.5, 3.5, 2.5, 2.0),
#'   Model_A = c(2.2, 2.8, 3.8, 5.3, 6.8, 7.3, 6.3, 5.8, 4.8, 3.8, 2.8, 2.2)
#' )
#' plot_monthly_comparison(monthly)
plot_monthly_comparison <- function(monthly_data, model_names = NULL,
                                    main = "Monthly ET0 Comparison",
                                    colors = NULL) {
  if (!"Month" %in% names(monthly_data)) {
    stop("'monthly_data' must contain a 'Month' column")
  }
  if (!"FAO_PM" %in% names(monthly_data)) {
    stop("'monthly_data' must contain a 'FAO_PM' column")
  }

  models <- setdiff(names(monthly_data), "Month")
  if (!is.null(model_names)) {
    models <- intersect(c("FAO_PM", model_names), models)
  }

  n_models <- length(models)
  if (is.null(colors)) {
    colors <- c("black", grDevices::rainbow(n_models - 1, s = 0.7, v = 0.8))
    names(colors) <- models
  }

  ylim <- range(monthly_data[, models], na.rm = TRUE)
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  plot(monthly_data$Month, monthly_data$FAO_PM, type = "l",
       lwd = 3, col = colors["FAO_PM"],
       xlim = c(1, 12), ylim = ylim,
       xlab = "Month", ylab = "ET0 (mm/day)",
       main = main, xaxt = "n")
  axis(1, at = 1:12, labels = month_labels)
  grid(col = "gray90")

  for (m in setdiff(models, "FAO_PM")) {
    lines(monthly_data$Month, monthly_data[[m]],
          col = colors[m], lwd = 1.5, lty = 2)
  }

  legend("topright", legend = models, col = colors[models],
         lwd = c(3, rep(1.5, n_models - 1)),
         lty = c(1, rep(2, n_models - 1)),
         cex = 0.6, bg = "white", ncol = 2)

  invisible(NULL)
}

#' Annual ET0 Bar Chart
#'
#' Creates a bar chart comparing total annual ET0 estimated by each model
#' against the FAO-PM reference.
#'
#' @param annual_et0 A named numeric vector of total annual ET0 values for
#'   each model. Must include \code{FAO_PM}.
#' @param main Title for the plot.
#' @param colors Character vector of bar colors. If \code{NULL}, generated
#'   automatically.
#' @return Invisible \code{NULL}.
#' @export
#' @examples
#' annual <- c(FAO_PM = 1571, Blaney_Criddle = 1933, Schendel = 2593,
#'             Hargreaves_Samani = 1393)
#' plot_annual_bar(annual)
plot_annual_bar <- function(annual_et0, main = "Annual ET0 by Model",
                            colors = NULL) {
  if (!"FAO_PM" %in% names(annual_et0)) {
    stop("'annual_et0' must include 'FAO_PM'")
  }

  n <- length(annual_et0)
  if (is.null(colors)) {
    colors <- ifelse(names(annual_et0) == "FAO_PM", "black",
                     grDevices::rainbow(n, s = 0.6, v = 0.85))
  }

  old_par <- par(mar = c(8, 5, 4, 2))
  on.exit(par(old_par))

  bp <- barplot(annual_et0, col = colors, main = main,
                ylab = "Annual ET0 (mm/year)",
                las = 2, cex.names = 0.65, border = NA)

  # Reference line for FAO-PM
  abline(h = annual_et0["FAO_PM"], lty = 2, col = "red", lwd = 1.5)

  invisible(NULL)
}
