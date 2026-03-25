# Insert analyte rows for a well type

Function used within
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md)
to add all analytes for a specified compound and well type.

## Usage

``` r
ras.add.type(Runlist, df.analyte, compound, wellType)
```

## Arguments

- Runlist:

  A data frame to add blanks to.

- df.analyte:

  A data frame with analytes to take from.

- compound:

  A string with what compound to filter by.

- wellType:

  A string with What well type to filter by.

## Value

Returns the same data frame that was supplied to the Runlist argument,
but with appended analytes.

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md),
[`ras.darkModeDF_Options()`](https://klorator.github.io/aquaras/reference/ras.darkModeDF_Options.md),
[`ras.dateSignAll.update()`](https://klorator.github.io/aquaras/reference/ras.dateSignAll.update.md),
[`ras.sanitizeInput()`](https://klorator.github.io/aquaras/reference/ras.sanitizeInput.md),
[`ras.server()`](https://klorator.github.io/aquaras/reference/ras.server.md),
[`ras.well.update()`](https://klorator.github.io/aquaras/reference/ras.well.update.md)

## Examples

``` r
# Setup
full.list = ras.Example_Runlist
blank.max = 5
df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
Runlist = tibble::tibble(
  Index        = double(),
  Plate        = double(),
  Row          = character(),
  Col          = double(),
  LC_Position  = character(),
  Date         = character(),
  Signature    = character(),
  Sample_name  = character(),
  Compound     = character(),
  Timepoint    = character(),
  Well_Type    = character(),
  LC_Well_Type = character(),
  Replicate    = character(),
  Sample_text  = character(),
  Draw_Max     = double(),
  Draw_Count   = double())

# Run function
Runlist1 <- ras.add.type(Runlist, df.analyte, "Paracetamol", "cell")
Runlist1
#> # A tibble: 18 × 16
#>    Index Plate Row     Col LC_Position Date     Signature Sample_name   Compound
#>    <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>         <chr>   
#>  1     1     3 A         1 3:A,1       20220725 RH        20220725_RH_… Paracet…
#>  2     2     3 A         2 3:A,2       20220725 RH        20220725_RH_… Paracet…
#>  3     3     3 A         3 3:A,3       20220725 RH        20220725_RH_… Paracet…
#>  4     4     3 A         4 3:A,4       20220725 RH        20220725_RH_… Paracet…
#>  5     5     3 A         5 3:A,5       20220725 RH        20220725_RH_… Paracet…
#>  6     6     3 A         6 3:A,6       20220725 RH        20220725_RH_… Paracet…
#>  7    32     3 C         8 3:C,8       20220725 RH        20220725_RH_… Paracet…
#>  8    33     3 C         9 3:C,9       20220725 RH        20220725_RH_… Paracet…
#>  9    34     3 C        10 3:C,10      20220725 RH        20220725_RH_… Paracet…
#> 10    35     3 C        11 3:C,11      20220725 RH        20220725_RH_… Paracet…
#> 11    36     3 C        12 3:C,12      20220725 RH        20220725_RH_… Paracet…
#> 12    37     3 D         1 3:D,1       20220725 RH        20220725_RH_… Paracet…
#> 13    38     3 D         2 3:D,2       20220725 RH        20220725_RH_… Paracet…
#> 14    39     3 D         3 3:D,3       20220725 RH        20220725_RH_… Paracet…
#> 15    40     3 D         4 3:D,4       20220725 RH        20220725_RH_… Paracet…
#> 16    41     3 D         5 3:D,5       20220725 RH        20220725_RH_… Paracet…
#> 17    42     3 D         6 3:D,6       20220725 RH        20220725_RH_… Paracet…
#> 18    43     3 D         7 3:D,7       20220725 RH        20220725_RH_… Paracet…
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>
```
