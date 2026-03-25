# Random series of groups

Creates a series of groups with randomly sampled observations.
Observations are distributed such as that there are no repeated entries
between groups/within a series. Number of groups in the series is
calculated as `length(master_list)` divided by `group_size`, rounded up
to nearest integer. If an observation can not be found (for example
because the groups can not be filled evenly) an error message is printed
to the console and `N/A` inserted instead.

## Usage

``` r
ras.ran_series(master_list, group_size)
```

## Arguments

- master_list:

  A character vector with observations/compound names

- group_size:

  Integer of how many observations to attempt to place in each group

## Value

A list of character vectors with each random group (data frame?)

## Examples

``` r
# Example with 4 even groups
# Compound list
Compound_list = c(
"Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
"Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
"Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
"Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
"Comp_21", "Comp_22", "Comp_23", "Comp_24")

series_A = ras.ran_series(master_list = Compound_list, group_size = 6)
series_A
#>   Group_01 Group_02 Group_03 Group_04
#> 1   Comp_8  Comp_22   Comp_7   Comp_1
#> 2  Comp_13   Comp_4  Comp_18  Comp_23
#> 3  Comp_21  Comp_15  Comp_14  Comp_19
#> 4  Comp_12   Comp_6   Comp_2  Comp_17
#> 5  Comp_24  Comp_11   Comp_5  Comp_10
#> 6  Comp_20   Comp_9   Comp_3  Comp_16

# Example with 5 uneven groups
# Compound list
Compound_list = c(
"Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
"Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
"Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
"Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
"Comp_21", "Comp_22", "Comp_23", "Comp_24", "Comp_25")

series_B = ras.ran_series(Compound_list, group_size = 6)
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
series_B
#>   Group_01 Group_02 Group_03 Group_04 Group_05
#> 1   Comp_7  Comp_25  Comp_16  Comp_18  Comp_23
#> 2  Comp_17   Comp_2   Comp_5  Comp_24      N/A
#> 3  Comp_14   Comp_9   Comp_8   Comp_4      N/A
#> 4  Comp_11   Comp_1  Comp_13  Comp_20      N/A
#> 5  Comp_22  Comp_19  Comp_12  Comp_15      N/A
#> 6  Comp_10   Comp_3   Comp_6  Comp_21      N/A
```
