# Insert blank rows

Function used within
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md)
to add blanks.

## Usage

``` r
ras.add.blank(Runlist, df.blank, blank.insert)
```

## Arguments

- Runlist:

  A data frame to add blanks to.

- df.blank:

  A data frame consisting of only blanks to take from.

- blank.insert:

  An integer of how many blanks to add.

## Value

Returns the same data frame that was supplied to the Runlist argument,
but with the specified number of appended blanks.

## Details

Function used within
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md)
to add blanks.

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
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
Runlist1 = ras.add.blank(Runlist, df.blank, 3) # Adds 3 blanks to Runlist.
Runlist1
#> # A tibble: 3 × 16
#>   Index Plate Row     Col LC_Position Date     Signature Sample_name    Compound
#>   <dbl> <dbl> <chr> <dbl> <chr>       <chr>    <chr>     <chr>          <chr>   
#> 1    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 2    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> 3    13     3 B         1 3:B,1       20220725 RH        20220725_RH_I… NA      
#> # ℹ 7 more variables: Timepoint <chr>, Well_Type <chr>, LC_Well_Type <chr>,
#> #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>
```
