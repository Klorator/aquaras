
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquaras

<!-- badges: start -->

[![R-CMD-check](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of aquaras is to make it easier to create runlists for use with
MassLynx (Waters LC/MS software) and also facilitate some of the data
processing steps with the MassLynx complete summary output file.

## Installation

You can install the development version of aquaras from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Klorator/aquaras")
```

## Example workflow:

### Summary

-   Call `RunlistGenerator()`
-   Input all the runlist info under “Well input”
    -   Optionally: download/upload the tsv under “Full list” to
        save/resume a project
-   Generate a runlist under “Runlist” and download the tsv
-   Do the experiments. Yeay!
-   Call `SplitOutput.ML()` and select the MassLynx complete summary
    output file (the file-selection window might be hiding behind
    RStudio)
-   Analyze the results for each compound
-   Publish paper!
-   Design new experiments (rinse and repeat)

### Pre-LC/MS

Here is a mock example of a populated runlist.

``` r
library(aquaras)
# str(Example_Runlist)
Example_Runlist[c(1:2, 7:8, 13:14), c(1, 5, 8, 12, 14)]
#>    Index LC_Position          Sample_name LC_Well_Type           Sample_text
#> 1      1       3:A,1  20220725_RH_Index.1      Analyte  Paracetamol_0_cell_1
#> 2      2       3:A,2  20220725_RH_Index.2      Analyte Paracetamol_15_cell_1
#> 7      7       3:A,7  20220725_RH_Index.7      Analyte    Paracetamol_0_STD_
#> 8      8       3:A,8  20220725_RH_Index.8      Analyte    Paracetamol_0_STD_
#> 13    13       3:B,1 20220725_RH_Index.13        blank                 blank
#> 14    14       3:B,2 20220725_RH_Index.14        blank                 blank
```

And this is what we get from generating a final runlist version with
default settings.

``` r
Final_Runlist = create.Runlist(Example_Runlist)
Final_Runlist[c(1, 5, 8, 12, 14)]
#> # A tibble: 150 × 5
#>    Index LC_Position Sample_name          LC_Well_Type Sample_text      
#>    <dbl> <chr>       <chr>                <chr>        <chr>            
#>  1    13 3:B,1       20220725_RH_Index.13 blank        blank            
#>  2    13 3:B,1       20220725_RH_Index.13 blank        blank            
#>  3    13 3:B,1       20220725_RH_Index.13 blank        blank            
#>  4    62 3:F,2       20220725_RH_Index.62 Analyte      Ibuprofen_0_bead_
#>  5    63 3:F,3       20220725_RH_Index.63 Analyte      Ibuprofen_0_bead_
#>  6    64 3:F,4       20220725_RH_Index.64 Analyte      Ibuprofen_0_bead_
#>  7    65 3:F,5       20220725_RH_Index.65 Analyte      Ibuprofen_0_bead_
#>  8    66 3:F,6       20220725_RH_Index.66 Analyte      Ibuprofen_0_bead_
#>  9    67 3:F,7       20220725_RH_Index.67 Analyte      Ibuprofen_0_bead_
#> 10    68 3:F,8       20220725_RH_Index.68 Analyte      Ibuprofen_0_bead_
#> # … with 140 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

Ready for pasting columns into MassLynx!

### Post-LC/MS examples

Recommended to place your output file in it’s own folder.

Run `SplitOutput.ML()` and select the output file.

## Planned features

### Plotting of graphs

``` r
# Plotting with ggplot2 here...
```
