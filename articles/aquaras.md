# Introduction to aquaras

``` r
library(aquaras)
```

## Overview

##### Pre-LC/MS run

Provides tools for working with runlists to use with MassLynx (Waters
LC/MS software). Calling
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md)
launches a Shiny app that lets you create the full data frame and a
runlist. Alternatively,
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md)
may be used directly.

##### Post-LC/MS run

Use
[`ras.SplitOutput()`](https://klorator.github.io/aquaras/reference/ras.SplitOutput.md)
to split the MassLynx complete summary output file into individual data
frames and write them to files. Unless `clean = FALSE`, this removes
blanks and splits the `Name` and `Sample Name` columns according to the
pre-LC/MS template. To only return the list of data frames and not write
to the file system, use `write = FALSE`.

##### Workflow summary

- Call
  [`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md)
- Input all the runlist info under “Well input”
  - Optionally: download/upload the tsv under “Full list” to save/resume
    a project
- Generate a runlist under “Runlist” and download the tsv
- Do experiments. Yeay!
- Call
  [`ras.SplitOutput()`](https://klorator.github.io/aquaras/reference/ras.SplitOutput.md)
  and select the MassLynx complete summary output file (the
  file-selection window might be hiding behind RStudio)
- Analyze the results for each compound
- Publish paper!
- Design new experiments (rinse and repeat)

## Example workflow

#### Pre-LC/MS

Here is a mock example of a populated runlist.

``` r
# str(Example_Runlist) # Run this in the console yourself
# View(Example_Runlist) # Run this in the console yourself
ras.Example_Runlist[c(1:2, 7:8, 13:14), c(1, 5, 8, 12, 14)]
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
Final_Runlist = ras.create.Runlist(ras.Example_Runlist)
Final_Runlist[c(1, 5, 8, 12, 14, 16)]
#> # A tibble: 150 × 6
#>    Index LC_Position Sample_name          LC_Well_Type Sample_text    Draw_Count
#>    <dbl> <chr>       <chr>                <chr>        <chr>               <dbl>
#>  1    13 3:B,1       20220725_RH_Index.13 blank        blank                   1
#>  2    13 3:B,1       20220725_RH_Index.13 blank        blank                   2
#>  3    13 3:B,1       20220725_RH_Index.13 blank        blank                   3
#>  4    62 3:F,2       20220725_RH_Index.62 Analyte      Ibuprofen_0_b…          1
#>  5    63 3:F,3       20220725_RH_Index.63 Analyte      Ibuprofen_0_b…          1
#>  6    64 3:F,4       20220725_RH_Index.64 Analyte      Ibuprofen_0_b…          1
#>  7    65 3:F,5       20220725_RH_Index.65 Analyte      Ibuprofen_0_b…          1
#>  8    66 3:F,6       20220725_RH_Index.66 Analyte      Ibuprofen_0_b…          1
#>  9    67 3:F,7       20220725_RH_Index.67 Analyte      Ibuprofen_0_b…          1
#> 10    68 3:F,8       20220725_RH_Index.68 Analyte      Ibuprofen_0_b…          1
#> # ℹ 140 more rows
```

Ready for pasting columns into MassLynx!

#### Post-LC/MS

Recommended to place your output file in it’s own folder.

Run
[`ras.SplitOutput()`](https://klorator.github.io/aquaras/reference/ras.SplitOutput.md)
and select the output file.

This splits the output into its constituent data frames and (unless
`write = FALSE`) writes them as individual .txt files in the same
directory as the source file. Naming convention used is “\[source file
name\] - \[compound name\]”. `clean = FALSE` may be used to not perform
any data manipulation at this stage. This is recommended if the source
file was not created using the {aquaras} standard format.
