# Insert all analyte and blank rows for a compound

Function used within
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md)
to add all analyte and blank rows for a compound. Passes arguments to
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md)
and
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md).

## Usage

``` r
ras.add.compound(
  Runlist,
  df.analyte,
  df.blank,
  compound,
  wellType,
  blank.type = 1
)
```

## Arguments

- Runlist:

  A data frame to add blanks to.

- df.analyte:

  A data frame with analytes to take from.

- df.blank:

  A data frame consisting of only blanks to take from.

- compound:

  A string with what compound to filter by.

- wellType:

  A vector with all well types to filter by. (string passed to
  [`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md))

- blank.type:

  An integer of how many blanks to add between types. (default = 1;
  passed to
  [`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md))

## Value

Returns the same data frame that was supplied to the Runlist argument,
but appended with all analyte and blank rows for a compound.

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md),
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
df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
  dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
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
  # Adds 1 blank between every type.
Runlist1 <- ras.add.compound(Runlist, df.analyte, df.blank, "Paracetamol",
  c("bead", "medium", "cell", "STD", "blank"))
Runlist1
#> # A tibble: 47 × 16
#>    Index Plate Row     Col LC_Position Date     Signature Sample_name   Compound
#>    <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>         <chr>   
#>  1    25     3 C         1 3:C,1       20220725 RH        20220725_RH_… Paracet…
#>  2    26     3 C         2 3:C,2       20220725 RH        20220725_RH_… Paracet…
#>  3    27     3 C         3 3:C,3       20220725 RH        20220725_RH_… Paracet…
#>  4    28     3 C         4 3:C,4       20220725 RH        20220725_RH_… Paracet…
#>  5    29     3 C         5 3:C,5       20220725 RH        20220725_RH_… Paracet…
#>  6    30     3 C         6 3:C,6       20220725 RH        20220725_RH_… Paracet…
#>  7    31     3 C         7 3:C,7       20220725 RH        20220725_RH_… Paracet…
#>  8   136     4 D         4 4:D,4       20220725 RH        20220725_RH_… Paracet…
#>  9   137     4 D         5 4:D,5       20220725 RH        20220725_RH_… Paracet…
#> 10   138     4 D         6 4:D,6       20220725 RH        20220725_RH_… Paracet…
#> # ℹ 37 more rows
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>

  # Adds 3 blanks between every type.
Runlist2 <- ras.add.compound(Runlist, df.analyte, df.blank, "Ibuprofen",
  c("bead", "medium", "cell", "STD", "blank"), 3)
Runlist2
#> # A tibble: 55 × 16
#>    Index Plate Row     Col LC_Position Date     Signature Sample_name   Compound
#>    <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>         <chr>   
#>  1    62     3 F         2 3:F,2       20220725 RH        20220725_RH_… Ibuprof…
#>  2    63     3 F         3 3:F,3       20220725 RH        20220725_RH_… Ibuprof…
#>  3    64     3 F         4 3:F,4       20220725 RH        20220725_RH_… Ibuprof…
#>  4    65     3 F         5 3:F,5       20220725 RH        20220725_RH_… Ibuprof…
#>  5    66     3 F         6 3:F,6       20220725 RH        20220725_RH_… Ibuprof…
#>  6    67     3 F         7 3:F,7       20220725 RH        20220725_RH_… Ibuprof…
#>  7    68     3 F         8 3:F,8       20220725 RH        20220725_RH_… Ibuprof…
#>  8    69     3 F         9 3:F,9       20220725 RH        20220725_RH_… Ibuprof…
#>  9    70     3 F        10 3:F,10      20220725 RH        20220725_RH_… Ibuprof…
#> 10    71     3 F        11 3:F,11      20220725 RH        20220725_RH_… Ibuprof…
#> # ℹ 45 more rows
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>
```
