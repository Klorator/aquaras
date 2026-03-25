# Update Date, Signature, & Sample_name

Update Date, Signature, & Sample_name for the entire data frame.
Sample_name is a composite in the format "date_signature_index".

## Usage

``` r
ras.dateSignAll.update(df, date, signature)
```

## Arguments

- df:

  Data frame to update

- date:

  New date value (character string)

- signature:

  New signature value

## Value

Updated data frame

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md),
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md),
[`ras.darkModeDF_Options()`](https://klorator.github.io/aquaras/reference/ras.darkModeDF_Options.md),
[`ras.sanitizeInput()`](https://klorator.github.io/aquaras/reference/ras.sanitizeInput.md),
[`ras.server()`](https://klorator.github.io/aquaras/reference/ras.server.md),
[`ras.well.update()`](https://klorator.github.io/aquaras/reference/ras.well.update.md)

## Examples

``` r
head(ras.Example_Runlist)
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
Example_Runlist1 = ras.dateSignAll.update(df = ras.Example_Runlist,
                                          date      = "20220730",
                                          signature = "RH")
head(Example_Runlist1)
#> # A tibble: 6 × 14
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1     1     3 A         1 3:A,1       20220730 RH        20220730_RH_I… Paracet…
#> 2     2     3 A         2 3:A,2       20220730 RH        20220730_RH_I… Paracet…
#> 3     3     3 A         3 3:A,3       20220730 RH        20220730_RH_I… Paracet…
#> 4     4     3 A         4 3:A,4       20220730 RH        20220730_RH_I… Paracet…
#> 5     5     3 A         5 3:A,5       20220730 RH        20220730_RH_I… Paracet…
#> 6     6     3 A         6 3:A,6       20220730 RH        20220730_RH_I… Paracet…
#> # ℹ 5 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>
```
