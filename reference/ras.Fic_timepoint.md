# Extract values for Cell & Medium (values w/ timepoints)

Extract values for Cell & Medium (values w/ timepoints)

## Usage

``` r
ras.Fic_timepoint(
  df,
  values = "Conc.",
  types = c("Cell", "Medium"),
  .summarize = TRUE
)
```

## Arguments

- df:

  Data frame from `ras.Fic_cleanup()`.

- values:

  Name of the column to use for values.

- types:

  Vector with the types to select timepoints for. (Cell & Medium)

- .summarize:

  Summarize values into averages.

## Value

A list of data frames with the selected timepoints.

## Examples

``` r
if (interactive()) {
  df_ex <- data.frame(
    Sample_ID = rep(c("A", "B"), each = 4),
    Sample_type = rep(c("Cell", "Cell", "Medium", "Medium"), times = 2),
    Timepoint = rep(c(30, 60, 30, 60), times = 2),
    `Conc.` = c(10, 15, 9, 11, 20, 24, 17, 19)
  )

  ras.Fic_timepoint(df_ex)
}
```
