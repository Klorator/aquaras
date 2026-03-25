#' Fit and Plot a Sigmoidal Regression Curve
#'
#' Fits a logistic (sigmoidal) curve using [stats::nls()] with [stats::SSlogis()],
#' optionally excluding selected rows from model fitting while still displaying them
#' on the plot. Included and excluded points can be styled independently, and optional
#' SEM error bars and a 95% confidence interval ribbon are added to the plot.
#'
#' @param data A data frame containing all columns used for fitting and plotting.
#' @param x_name A string naming the numeric predictor column in `data`.
#' @param y_name A string naming the numeric response column in `data`.
#' @param sem_name Optional string naming the SEM column in `data`. If provided,
#'   y ± SEM error bars are added.
#' @param outlier_indices Optional integer vector of row indices in `data` to
#'   exclude from model fitting.
#' @param point_color Color for included data points and their error bars.
#' @param point_shape Shape for included data points.
#' @param point_size Size for all points.
#' @param excluded_point_color Color for excluded points and their error bars.
#' @param excluded_point_shape Shape for excluded data points.
#' @param line_color Color of the fitted sigmoidal line.
#' @param line_type Linetype of the fitted line (e.g., `"solid"`, `"dashed"`).
#' @param line_size Line width of the fitted line.
#' @param ci_fill Fill color of the 95% confidence interval ribbon.
#' @param text_size Base text size used in [ggplot2::theme_classic()].
#' @param x_label Optional custom x-axis label. Defaults to `x_name` when `NULL`.
#' @param y_label Optional custom y-axis label. Defaults to `y_name` when `NULL`.
#'
#' @return A named list with:
#' \describe{
#'   \item{`model`}{The fitted `nls` model object.}
#'   \item{`coefficients`}{Named numeric vector of fitted coefficients.}
#'   \item{`plot`}{A `ggplot` object containing points, fit, and confidence band.}
#'   \item{`predicted_curve`}{Data frame with x values and fitted/CI values.}
#'   \item{`fit_data`}{Data used for model fitting (after exclusions).}
#'   \item{`excluded_data`}{Data excluded from fitting and highlighted in the plot.}
#' }
#'
#' @examples
#' my_data <- data.frame(
#'   logd = c(-1.56, -0.13, 0.14, 0.19, 1.93, 2.2, 3.3, 3.53, 4.3, 4.8, 5.46),
#'   mean = c(
#'     1,
#'     0.994,
#'     1,
#'     0.0836,
#'     1,
#'     0.158,
#'     0.723,
#'     0.0664,
#'     0.0286,
#'     0.00879,
#'     0.00834
#'   ),
#'   sem = c(
#'     0.000000,
#'     0.053012,
#'     0.000000,
#'     0.042940,
#'     0.000000,
#'     0.086477,
#'     0.370647,
#'     0.043030,
#'     0.021356,
#'     0.000411,
#'     0.001492
#'   )
#' )
#'
#' outliers <- c(4, 6)
#' my_data_result <- sigmoid_regression(
#'   data = my_data,
#'   x_name = "logd",
#'   y_name = "mean",
#'   sem_name = "sem",
#'   outlier_indices = outliers
#' )
#'
#' my_data_result$coefficients
#' my_data_result$plot
#'
#' @export
sigmoid_regression <- function(
  data,
  x_name,
  y_name,
  sem_name = NULL,
  outlier_indices = NULL,
  point_color = "black",
  point_shape = 16,
  point_size = 3,
  excluded_point_color = "red",
  excluded_point_shape = 16,
  line_color = "blue",
  line_type = "solid",
  line_size = 1,
  ci_fill = "blue",
  text_size = 16,
  x_label = NULL,
  y_label = NULL
) {
  # Validate required column names and numeric types
  if (!(x_name %in% names(data))) {
    stop("x_name must be a column name in data.")
  }

  if (!(y_name %in% names(data))) {
    stop("y_name must be a column name in data.")
  }

  if (!is.numeric(data[[x_name]])) {
    stop("The x_name column must be numeric.")
  }

  if (!is.numeric(data[[y_name]])) {
    stop("The y_name column must be numeric.")
  }

  # Optionally exclude rows from model fitting (still shown on the plot)
  valid_indices <- seq_len(nrow(data))
  excluded_data <- data[0, , drop = FALSE]

  if (!is.null(outlier_indices)) {
    outlier_indices <- unique(stats::na.omit(as.integer(outlier_indices)))
    outlier_indices <- outlier_indices[
      outlier_indices >= 1 & outlier_indices <= nrow(data)
    ]
    valid_indices <- setdiff(valid_indices, outlier_indices)
    excluded_data <- data[outlier_indices, , drop = FALSE]
  }

  fit_data <- data[valid_indices, , drop = FALSE]

  if (!is.null(sem_name) && !(sem_name %in% names(data))) {
    stop("sem_name must be a column name in data.")
  }

  # Extract x and y data from fit_data
  x_data <- fit_data[[x_name]]
  y_data <- fit_data[[y_name]]

  # Fit the sigmoidal model using the self-starting logistic curve
  fit <- nls(
    y_data ~ SSlogis(x_data, Asym, xmid, scal),
    data = fit_data,
    start = list(
      Asym = max(y_data, na.rm = TRUE),
      xmid = stats::median(x_data, na.rm = TRUE),
      scal = -diff(range(x_data, na.rm = TRUE)) / 4
    )
  )

  # Compute pseudo-R-squared for display in plot subtitle
  rss <- sum(stats::residuals(fit)^2)
  tss <- sum((y_data - mean(y_data, na.rm = TRUE))^2)
  r_squared <- if (tss > 0) 1 - (rss / tss) else NA_real_
  r2_label <- if (is.na(r_squared)) {
    "R² = NA"
  } else {
    paste0("R² = ", formatC(r_squared, format = "f", digits = 3))
  }

  # Generate x-grid and predictions for smooth fitted curve
  pred_grid <- data.frame(
    x_data = seq(
      min(x_data, na.rm = TRUE),
      max(x_data, na.rm = TRUE),
      length.out = 100
    )
  )
  pred_grid$y_hat <- predict(fit, newdata = pred_grid)

  # Compute 95% CI around fitted curve using delta method
  coef_fit <- stats::coef(fit)
  vcov_fit <- stats::vcov(fit)

  Asym <- unname(coef_fit[["Asym"]])
  xmid <- unname(coef_fit[["xmid"]])
  scal <- unname(coef_fit[["scal"]])

  z <- (xmid - pred_grid$x_data) / scal
  g <- 1 / (1 + exp(z))

  d_Asym <- g
  d_xmid <- -Asym * g * (1 - g) / scal
  d_scal <- Asym * g * (1 - g) * (xmid - pred_grid$x_data) / (scal^2)

  grad_mat <- cbind(d_Asym, d_xmid, d_scal)
  pred_var <- rowSums((grad_mat %*% vcov_fit) * grad_mat)
  pred_se <- sqrt(pmax(pred_var, 0))

  t_crit <- stats::qt(0.975, df = stats::df.residual(fit))
  pred_grid$y_lower <- pred_grid$y_hat - t_crit * pred_se
  pred_grid$y_upper <- pred_grid$y_hat + t_crit * pred_se

  # Build plot layers
  p <- ggplot2::ggplot(
    fit_data,
    ggplot2::aes(x = .data[[x_name]], y = .data[[y_name]])
  )

  if (!is.null(sem_name)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data[[y_name]] - .data[[sem_name]],
          ymax = .data[[y_name]] + .data[[sem_name]]
        ),
        color = point_color,
        width = 0.08
      )
  }

  p <- p +
    ggplot2::geom_point(
      color = point_color,
      shape = point_shape,
      size = point_size
    ) +
    ggplot2::geom_ribbon(
      data = pred_grid,
      ggplot2::aes(x = x_data, ymin = y_lower, ymax = y_upper),
      inherit.aes = FALSE,
      fill = ci_fill,
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = pred_grid,
      ggplot2::aes(x = x_data, y = y_hat),
      color = line_color,
      linetype = line_type,
      linewidth = line_size
    ) +
    ggplot2::labs(
      title = paste("Sigmoidal Fit for", unique(data$matrix)),
      subtitle = r2_label,
      x = if (is.null(x_label)) x_name else x_label,
      y = if (is.null(y_label)) y_name else y_label
    ) +
    ggplot2::theme_classic(base_size = text_size)

  if (nrow(excluded_data) > 0) {
    if (!is.null(sem_name)) {
      p <- p +
        ggplot2::geom_errorbar(
          data = excluded_data,
          ggplot2::aes(
            x = .data[[x_name]],
            ymin = .data[[y_name]] - .data[[sem_name]],
            ymax = .data[[y_name]] + .data[[sem_name]]
          ),
          color = excluded_point_color,
          width = 0.08
        )
    }

    p <- p +
      ggplot2::geom_point(
        data = excluded_data,
        ggplot2::aes(x = .data[[x_name]], y = .data[[y_name]]),
        color = excluded_point_color,
        shape = excluded_point_shape,
        size = point_size
      )
  }

  # Rename columns in prediction table for consistency with input names
  colnames(pred_grid) <- c(x_name, paste0("predicted_", y_name))

  # Bundle outputs and return
  result <- list(
    model = fit,
    coefficients = coef(fit),
    plot = p,
    predicted_curve = pred_grid,
    fit_data = fit_data,
    excluded_data = excluded_data
  )

  return(result)
}

