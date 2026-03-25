# Fic end-to-end workflow (!!!)

Currently not functional!

## Usage

``` r
ras.Fic_workflow(
  source = c("Waters", "Sciex"),
  ID = "Sample_ID",
  values = "Conc.",
  Buffer = "HBSS",
  Dilution_type = "[:digit:]x",
  Dilution_extract = "[:digit:]+(?=x)",
  stab = "Stab",
  czero = "Czero",
  prot_cell_value = "mg_Protein",
  prot_cell_type = "Cells",
  prot_hom_value = "Protein_conc._mg/mL",
  prot_hom_type = "hom",
  V.medium = 200,
  MassBalance_2.5 = 1.75,
  .compound = "Analyte Peak Name",
  .checkValues = TRUE,
  prot_.split = "Sample Text",
  .summarize = TRUE,
  .SD = TRUE
)
```

## Arguments

- source:

  Data source (`"Sciex"` or `"Waters"`).

- ID:

  Column name for ID column after splitting.

- values:

  Column name for numeric values.

- Buffer:

  Regex used to select buffer values.

- Dilution_type:

  Regex used to select dilution-factor values.

- Dilution_extract:

  Regex used to extract the dilution factor.

- stab:

  Regex used to select stability values.

- czero:

  Regex used to select C-zero values.

- prot_cell_value:

  Column name for values

- prot_cell_type:

  RegEx to filter by

- prot_hom_value:

  RegEx to filter by

- prot_hom_type:

  RegEx to filter by

- V.medium:

  RegEx to filter by

- MassBalance_2.5:

  Factor for calculating Mass Balance 10.2.5.

- .compound:

  Column name for compounds to prefix Sample_ID, `NULL` =\> "Analyte
  Peak Name"

- .checkValues:

  Removes "\<" from the values column to make sure it can be coerced to
  numeric.

- prot\_.split:

  Column name to split for samples in protein dataframe

- .summarize:

  Summarize the data into unique rows in grouping column. Drops the
  original value columns.

- .SD:

  Create column with Standard Deviation

## Value

Data frame with all variables used and calculated.

## Examples

``` r
if (interactive()) {
  ras.Fic_workflow(source = "Sciex")
}
```
