# Update well info

Update well info

## Usage

``` r
ras.well.update(
  df,
  well.current,
  date,
  signature,
  compound,
  timepoint,
  type,
  replicate
)
```

## Arguments

- df:

  Data frame to update

- well.current:

  Current well plate position (LC_Position)

- date:

  New date value (character string)

- signature:

  New signature value

- compound:

  New compound value

- timepoint:

  New timepoint value

- type:

  New type value

- replicate:

  New replicate value

## Value

Same data frame, but with updated row

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md),
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md),
[`ras.darkModeDF_Options()`](https://klorator.github.io/aquaras/reference/ras.darkModeDF_Options.md),
[`ras.dateSignAll.update()`](https://klorator.github.io/aquaras/reference/ras.dateSignAll.update.md),
[`ras.sanitizeInput()`](https://klorator.github.io/aquaras/reference/ras.sanitizeInput.md),
[`ras.server()`](https://klorator.github.io/aquaras/reference/ras.server.md)

## Examples

``` r
ras.Example_Runlist[1,]
#> # A tibble: 1 × 14
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1     1     3 A         1 3:A,1       20220725 RH        20220725_RH_I… Paracet…
#> # ℹ 5 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>
Example_Runlist1 = ras.well.update(df = ras.Example_Runlist,
                                   well.current = "3:A,1",
                                   date         = "20220730",
                                   signature    = "RH",
                                   compound     = "Warfarin",
                                   timepoint    = "42",
                                   type         = "bead",
                                   replicate    = "6")
Example_Runlist1[1,]
#> # A tibble: 1 × 14
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1     1     3 A         1 3:A,1       20220730 RH        20220730_RH_I… Warfarin
#> # ℹ 5 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>
```
