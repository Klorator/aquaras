# Fu feces workflow wrapper

Fu feces workflow wrapper

## Usage

``` r
ras.Fu_feces_workflow(
  source = c("Sciex", "Waters"),
  ID = "Sample Name",
  Sample_type = "Sample Type",
  values = "Calculated Concentration (nM)",
  Buffer = "HBSS",
  Dilution_type = "[:digit:]+x",
  Dilution_extract = "[:digit:]+(?=x$)",
  Dilution_factor = 1,
  stab = "Stab",
  czero = "Czero",
  D = (1/((1/4.8) * 0.95)),
  MassBalance_2.5 = 1.75,
  .compound = "Analyte Peak Name",
  .checkValues = TRUE,
  .summarize = FALSE
)
```

## Arguments

- source:

  Data source (`"Sciex"` or `"Waters"`).

- ID:

  Column name for ID column after splitting.

- Sample_type:

  Name of the type column in raw data.

- values:

  Column name for numeric values.

- Buffer:

  Regex used to select buffer values.

- Dilution_type:

  Regex used to select dilution-factor values.

- Dilution_extract:

  Regex used to extract the dilution factor.

- Dilution_factor:

  Numeric value for dilution factor

- stab:

  Regex used to select stability values.

- czero:

  Regex used to select C-zero values.

- D:

  Numeric value for D.

- MassBalance_2.5:

  Factor for calculating Mass Balance 10.2.5.

- .compound:

  Column name for compounds to prefix Sample_ID, `NULL` =\> "Analyte
  Peak Name"

- .checkValues:

  Removes "\<" from the values column to make sure it can be coerced to
  numeric.

- .summarize:

  Summarize the data into unique rows in grouping column. Drops the
  original value columns.

## Value

Data frame with all variables used and calculated. Also writes the data
frame to `.csv` (EU) and `.xlsx` files.

## Examples

``` r
if (interactive()) {
  ras.Fu_feces_workflow(source = "Sciex")
}
```
