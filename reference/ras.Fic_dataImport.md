# Load data for Fic & Fu feces

Load data for Fic & Fu feces

## Usage

``` r
ras.Fic_dataImport(source, .compound)
```

## Arguments

- source:

  A string, either `"Sciex"` or `"Waters"`

- .compound:

  .compound as supplied to the workflow function

## Value

A list with the loaded data frame & appropriate value for .compound

## Examples

``` r
if (interactive()) {
  res <- ras.Fic_dataImport(source = "Sciex", .compound = "Analyte Peak Name")
  names(res)
}
```
