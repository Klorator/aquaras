# Fit and Plot a Sigmoidal Regression Curve

Fits a logistic (sigmoidal) curve using
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) with
[`stats::SSlogis()`](https://rdrr.io/r/stats/SSlogis.html), optionally
excluding selected rows from model fitting while still displaying them
on the plot. Included and excluded points can be styled independently,
and optional SEM error bars and a 95% confidence interval ribbon are
added to the plot.

## Usage

``` r
sigmoid_regression(
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
)
```

## Arguments

- data:

  A data frame containing all columns used for fitting and plotting.

- x_name:

  A string naming the numeric predictor column in `data`.

- y_name:

  A string naming the numeric response column in `data`.

- sem_name:

  Optional string naming the SEM column in `data`. If provided, y +/-
  SEM error bars are added.

- outlier_indices:

  Optional integer vector of row indices in `data` to exclude from model
  fitting.

- point_color:

  Color for included data points and their error bars.

- point_shape:

  Shape for included data points.

- point_size:

  Size for all points.

- excluded_point_color:

  Color for excluded points and their error bars.

- excluded_point_shape:

  Shape for excluded data points.

- line_color:

  Color of the fitted sigmoidal line.

- line_type:

  Linetype of the fitted line (e.g., `"solid"`, `"dashed"`).

- line_size:

  Line width of the fitted line.

- ci_fill:

  Fill color of the 95% confidence interval ribbon.

- text_size:

  Base text size used in
  [`ggplot2::theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- x_label:

  Optional custom x-axis label. Defaults to `x_name` when `NULL`.

- y_label:

  Optional custom y-axis label. Defaults to `y_name` when `NULL`.

## Value

A named list with:

- `model`:

  The fitted `nls` model object.

- `coefficients`:

  Named numeric vector of fitted coefficients.

- `plot`:

  A `ggplot` object containing points, fit, and confidence band.

- `predicted_curve`:

  Data frame with x values and fitted/CI values.

- `fit_data`:

  Data used for model fitting (after exclusions).

- `excluded_data`:

  Data excluded from fitting and highlighted in the plot.

## Examples

``` r
my_data <- data.frame(
  logd = c(-2, -1, 0, 1, 2, 3),
  mean = c(0.98, 0.90, 0.60, 0.25, 0.10, 0.05),
  sem = rep(0.02, 6),
)
#> Error in data.frame(logd = c(-2, -1, 0, 1, 2, 3), mean = c(0.98, 0.9,     0.6, 0.25, 0.1, 0.05), sem = rep(0.02, 6), ): argument is missing, with no default

outliers <- c(4, 6)
my_data_result <- sigmoid_regression(
  data = my_data,
  x_name = "logd",
  y_name = "mean",
  sem_name = "sem",
  outlier_indices = outliers
)
#> Error: object 'my_data' not found

my_data_result$coefficients
#> Error: object 'my_data_result' not found
my_data_result$plot
#> Error: object 'my_data_result' not found
```
