# TPA - Do all the TPA stuff

Clean, restructure, calculate, plot, and export/save plots to file
system.

## Usage

``` r
ras.TPAer(
  df,
  na.rm = FALSE,
  save = FALSE,
  file_type = "png",
  folder_path = tcltk::tk_choose.dir()
)
```

## Arguments

- df:

  Data frame with values.

- na.rm:

  If `TRUE`, `NA` values are dropped before determining calculations.

- save:

  If `TRUE`, writes files to system

- file_type:

  File extension/type for exported images

- folder_path:

  Path to destination folder

## Value

List containing (1) Data frame and (2) List of generated plots

## Examples

``` r
if (interactive()) {
  df_ex <- data.frame(
    gene_names = c("GeneA", "GeneB"),
    `1A_1` = c(10, 20),
    `1A_2` = c(12, 22),
    `2B_1` = c(8, 16),
    `2B_2` = c(9, 17)
  )

  ras.TPAer(df_ex, save = FALSE, folder_path = tempdir())
}
```
