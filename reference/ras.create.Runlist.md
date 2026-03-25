# Create a Runlist from data frame

Creates a Runlist from the standardized data frame supplied by/made in
the Shiny app
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md).

## Usage

``` r
ras.create.Runlist(
  full.list,
  blank.start = 3,
  blank.end = 5,
  blank.comp = 2,
  blank.type = 1,
  blank.max = 5
)
```

## Arguments

- full.list:

  A full list, according to the package template, to be transformed into
  a runlist.

- blank.start:

  Integer of blanks to start with.

- blank.end:

  Integer of blanks to end with.

- blank.comp:

  Integer of blanks to insert between compounds.

- blank.type:

  Integer of blanks to insert between well types.

- blank.max:

  Max number of times to draw from the same blank well.

## Value

Returns a Runlist with blanks spaced as defined (with sensible
defaults).

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md),
[`ras.darkModeDF_Options()`](https://klorator.github.io/aquaras/reference/ras.darkModeDF_Options.md),
[`ras.dateSignAll.update()`](https://klorator.github.io/aquaras/reference/ras.dateSignAll.update.md),
[`ras.sanitizeInput()`](https://klorator.github.io/aquaras/reference/ras.sanitizeInput.md),
[`ras.server()`](https://klorator.github.io/aquaras/reference/ras.server.md),
[`ras.well.update()`](https://klorator.github.io/aquaras/reference/ras.well.update.md)

## Examples

``` r
# Setup
full.list = ras.Example_Runlist
head(full.list)
#> # A tibble: 6 × 14
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1     1     3 A         1 3:A,1       20220725 RH        20220725_RH_I… Paracet…
#> 2     2     3 A         2 3:A,2       20220725 RH        20220725_RH_I… Paracet…
#> 3     3     3 A         3 3:A,3       20220725 RH        20220725_RH_I… Paracet…
#> 4     4     3 A         4 3:A,4       20220725 RH        20220725_RH_I… Paracet…
#> 5     5     3 A         5 3:A,5       20220725 RH        20220725_RH_I… Paracet…
#> 6     6     3 A         6 3:A,6       20220725 RH        20220725_RH_I… Paracet…
#> # ℹ 5 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>

# Run function
Runlist1 <- ras.create.Runlist(full.list)
head(Runlist1)
#> # A tibble: 6 × 16
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 2    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 3    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 4    62     3 F         2 3:F,2       20220725 RH        20220725_RH_I… Ibuprof…
#> 5    63     3 F         3 3:F,3       20220725 RH        20220725_RH_I… Ibuprof…
#> 6    64     3 F         4 3:F,4       20220725 RH        20220725_RH_I… Ibuprof…
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>

Runlist2 <- ras.create.Runlist(full.list,
                               blank.start = 4,
                               blank.end   = 3,
                               blank.comp  = 2,
                               blank.type  = 1,
                               blank.max   = 6)
head(Runlist2)
#> # A tibble: 6 × 16
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 2    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 3    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 4    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 5    62     3 F         2 3:F,2       20220725 RH        20220725_RH_I… Ibuprof…
#> 6    63     3 F         3 3:F,3       20220725 RH        20220725_RH_I… Ibuprof…
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>
```
