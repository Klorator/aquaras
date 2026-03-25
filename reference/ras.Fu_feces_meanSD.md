# Fu feces mean & SD

Add mean & SD columns, grouped by `grouping_col`, for all the calculated
numeric columns.

## Usage

``` r
ras.Fu_feces_meanSD(
  df,
  grouping_col = "Sample_ID",
  sample_type = "Sample Type",
  compound = "Analyte Peak Name"
)
```

## Arguments

- df:

  Data frame

- grouping_col:

  Column name to group by

- sample_type:

  Column name for sample type

- compound:

  Column name for compound

## Value

Data frame with mean & SD columns

## Examples

``` r
df_ex <- data.frame(
  Sample_ID = c("S1", "S1", "S2", "S2"),
  `Sample Type` = c("hom", "hom", "hom", "hom"),
  `Analyte Peak Name` = c("cmpd", "cmpd", "cmpd", "cmpd"),
  value = c(10, 12, 20, 22)
)

ras.Fu_feces_meanSD(df_ex)
#>   Sample_ID Sample.Type Analyte.Peak.Name value value_mean value_sd
#> 1        S1         hom              cmpd    10         11 1.414214
#> 2        S1         hom              cmpd    12         11 1.414214
#> 3        S2         hom              cmpd    20         21 1.414214
#> 4        S2         hom              cmpd    22         21 1.414214
```
