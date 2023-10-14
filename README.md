
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquaras

<!-- badges: start -->

[![R-CMD-check](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Klorator/aquaras/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

I created aquaras to solve some specific tasks and have added/will add
more functionality as the need arises. I would never have been able to
make things work without all the amazing people who create and maintain
the packages I’ve used. Thank you!

## Installation

You can install the development version of aquaras from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Klorator/aquaras")
```

## Runlists for MassLynx

Create runlists for use with MassLynx (Waters LC/MS software) and
facilitate some of the data processing steps with the MassLynx complete
summary output file.

### Workflow summary

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

Created the function `ras.Randomizer()` to distribute a list of
observations/compounds into groups randomly. Can create multiple series
and write to excel file (not default).

## TPA tools

Created the function `ras.TPA_calcFromIntensity()` which calculates TPA
from the intensity columns. It uses the raw output file from MaxQuant.

A few more functions, mainly `ras.TPAer` for calculating averages and
standard deviation of TPA columns.

## Fic workflow

Look at ras.Fic_workflow()

`df_Fic <- ras.Fic_workflow()`

…and ras.Fu_feces_workflow()

`df_FuFeces <- ras.Fic_workflow()`

See the help page for the arguments. You probably want to adapt some to
your data.

## Planned features

##### Plotting of graphs

``` r
# Plotting with ggplot2 here...
```

##### Submitting to CRAN

This will probably never happen, but it would be cool.
