# Stack MassLynx output file

Stacks the MassLynx complete summary output file into a single
dataframe. Adds a column with what compound was used. Behaves like
[`ras.SplitOutput()`](https://klorator.github.io/aquaras/reference/ras.SplitOutput.md).

### write = TRUE

**!!! DEFAULT IS TO WRITE TO FILE SYSTEM !!!** Data frames written to
tsv files in the same directory as the source file. Use write = FALSE to
disable this.

## Usage

``` r
ras.StackOutput(
  sourceFiles = tcltk::tk_choose.files(caption = "Select MassLynx output file"),
  write = TRUE
)
```

## Arguments

- sourceFiles:

  A character vector with one or more file paths

- write:

  Defaults to TRUE for writing to file system

## Value

Writes a tsv file per compound to the same directory as the source file.

Also returns the nested list of data frames for each file

## See also

Other SplitOutput:
[`ras.SplitOutput()`](https://klorator.github.io/aquaras/reference/ras.SplitOutput.md)

## Examples

``` r
  if (FALSE) { # \dontrun{
  # See ras.SplitOutput() for now
  } # }
```
