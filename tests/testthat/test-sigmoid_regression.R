test_that("sigmoid_regression returns expected output structure", {
  df_ex <- data.frame(
    x = c(-2, -1, 0, 1, 2, 3),
    y = c(0.98, 0.90, 0.60, 0.25, 0.10, 0.05),
    se = rep(0.02, 6),
    matrix = "M1"
  )

  result <- suppressWarnings(
    sigmoid_regression(
      data = df_ex,
      x_name = "x",
      y_name = "y",
      sem_name = "se",
      outlier_indices = c(2, 200, NA)
    )
  )

  expect_named(
    result,
    c("model", "coefficients", "plot", "predicted_curve", "fit_data", "excluded_data")
  )
  expect_s3_class(result$model, "nls")
  expect_s3_class(result$plot, "ggplot")
  expect_equal(nrow(result$fit_data), 5)
  expect_equal(nrow(result$excluded_data), 1)
  expect_equal(nrow(result$predicted_curve), 100)
  expect_true(all(c("x", "predicted_y") %in% names(result$predicted_curve)))
})

test_that("sigmoid_regression validates required inputs", {
  df_ex <- data.frame(
    x = c(-2, -1, 0, 1, 2, 3),
    y = c(0.98, 0.90, 0.60, 0.25, 0.10, 0.05),
    se = rep(0.02, 6),
    matrix = "M1"
  )

  expect_error(
    sigmoid_regression(df_ex, "missing_x", "y"),
    "x_name must be a column name in data"
  )

  df_bad <- df_ex
  df_bad$x <- as.character(df_bad$x)
  expect_error(
    sigmoid_regression(df_bad, "x", "y"),
    "x_name column must be numeric"
  )

  expect_error(
    sigmoid_regression(df_ex, "x", "y", sem_name = "missing_se"),
    "sem_name must be a column name in data"
  )
})
