# Random group

Populate a group with random observations/compounds. Used inside
[`ras.ran_series()`](https://klorator.github.io/aquaras/reference/ras.ran_series.md).

## Usage

``` r
ras.ran_group(
  masterList = master_list,
  groupName = c(),
  seriesList = series_list,
  seriesCheck = series_check,
  groupSize = group_size
)
```

## Arguments

- masterList:

  A character vector with observations/compound names

- groupName:

  Empty vector to fill with observations

- seriesList:

  Data frame used for control of how many times an observation occurs

- seriesCheck:

  Character vector that has the cumulative unique observations across
  the series

- groupSize:

  Integer of how many observations to attempt to place in each group

## Value

List with 1. populated `groupName`, 2. updated `seriesList`, and 3.
updated `seriesCheck`
