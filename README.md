
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquaras

<!-- badges: start -->

[![R-CMD-check](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of aquaras is to make it easier to create runlists for use with
MassLynx (Waters LC/MS software) and facilitate some of the data
processing steps with the MassLynx complete summary output file.

## Installation

You can install the development version of aquaras from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Klorator/aquaras")
```

## Workflow summary

- Call `ras.RunlistGenerator()`
- Input all the runlist info under “Well input”
  - Optionally: download/upload the tsv under “Full list” to save/resume
    a project
- Generate a runlist under “Runlist” and download the tsv
- Do experiments. Yeay!
- Call `ras.SplitOutput()` and select the MassLynx complete summary
  output file (the file-selection window might be hiding behind RStudio)
- Analyze the results for each compound
- Publish paper!
- Design new experiments (rinse and repeat)

## Random group generator

Created the function `ras.ran_series()` to distribute a list of
observations/compounds into groups randomly.

## Planned features

##### Plotting of graphs

``` r
# Plotting with ggplot2 here...
```

##### Submitting to CRAN

This will probably never happen, but it would be cool.
