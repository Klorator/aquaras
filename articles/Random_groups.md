# Create random groups

``` r
library(aquaras)
```

`ras.Randomizer` creates multiple series of groups each with randomly
sampled observations. Observations are distributed such as that there
are no repeated entries between groups/within a series.

Number of groups in a series is calculated as `length(master_list)`
divided by `group_size`, rounded up to nearest integer. If an
observation can not be found (for example because the groups can not be
filled evenly) an error message is printed to the console and `N/A`
inserted instead.

``` r
# Example with 4 even groups
# Compound list
Compound_list = c(
  "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
  "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
  "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
  "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
  "Comp_21", "Comp_22", "Comp_23", "Comp_24")

series_A = ras.Randomizer(compound_list = Compound_list,
                          group_size = 6,
                          series = 2)
print("+++++ Series A +++++")
#> [1] "+++++ Series A +++++"
series_A
#> $Series_01
#>   Group_01 Group_02 Group_03 Group_04
#> 1  Comp_13   Comp_4  Comp_16  Comp_20
#> 2  Comp_23   Comp_9   Comp_3   Comp_1
#> 3  Comp_12  Comp_24  Comp_22  Comp_19
#> 4   Comp_5   Comp_2   Comp_7  Comp_14
#> 5   Comp_6  Comp_11  Comp_10  Comp_18
#> 6  Comp_15  Comp_21  Comp_17   Comp_8
#> 
#> $Series_02
#>   Group_01 Group_02 Group_03 Group_04
#> 1   Comp_1   Comp_2  Comp_22  Comp_13
#> 2  Comp_12  Comp_23   Comp_8  Comp_18
#> 3  Comp_19  Comp_10   Comp_9  Comp_24
#> 4  Comp_16   Comp_6  Comp_14  Comp_21
#> 5  Comp_11   Comp_4   Comp_5  Comp_20
#> 6   Comp_3   Comp_7  Comp_17  Comp_15

# Example with 5 uneven groups
# Compound list
Compound_list = c(
  "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
  "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
  "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
  "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
  "Comp_21", "Comp_22", "Comp_23", "Comp_24")

series_B = ras.Randomizer(Compound_list,
                          group_size = 5,
                          series = 2)
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
#> [1] "Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?"
print("+++++ Series B +++++")
#> [1] "+++++ Series B +++++"
series_B
#> $Series_01
#>   Group_01 Group_02 Group_03 Group_04 Group_05
#> 1  Comp_18   Comp_3   Comp_2  Comp_17   Comp_8
#> 2  Comp_12   Comp_4   Comp_5  Comp_23   Comp_7
#> 3  Comp_22  Comp_13  Comp_19   Comp_9  Comp_16
#> 4  Comp_21   Comp_6  Comp_24  Comp_15   Comp_1
#> 5  Comp_10  Comp_14  Comp_20  Comp_11      N/A
#> 
#> $Series_02
#>   Group_01 Group_02 Group_03 Group_04 Group_05
#> 1   Comp_8  Comp_15  Comp_20   Comp_7   Comp_2
#> 2  Comp_18  Comp_12  Comp_24  Comp_21  Comp_13
#> 3  Comp_22   Comp_6  Comp_16  Comp_10   Comp_4
#> 4  Comp_17   Comp_1  Comp_23  Comp_14  Comp_19
#> 5   Comp_5   Comp_9  Comp_11   Comp_3      N/A

# Example of writing to .xlsx file
  # Don't run
# Series_P = ras.Randomizer(Compound_list,
#                           group_size = 5,
#                           series = 2,
#                           write = TRUE,
#                           fileName = "Series_P Example for ras.Randomizer")
```
