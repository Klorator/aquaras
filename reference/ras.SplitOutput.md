# Split MassLynx output file

Splits the MassLynx complete summary output file into individual data
frames based on compound.

### Uninteresting output

Splitting the file generates a stream of "New names: • “ -\> `...1`"
output in the console that is not particularly interesting but lets you
know it's doing something.

### clean = TRUE

These data frames are cleaned by ras.cleanDF() (unless clean = FALSE).

### write = TRUE

**!!! DEFAULT IS TO WRITE TO FILE SYSTEM !!!** Data frames written to
tsv files in the same directory as the source file. Use write = FALSE to
disable this.

## Usage

``` r
ras.SplitOutput(
  sourceFiles = tcltk::tk_choose.files(),
  clean = FALSE,
  write = TRUE
)
```

## Arguments

- sourceFiles:

  A character vector with one or more file paths

- clean:

  Defaults to FALSE for data cleaning

- write:

  Defaults to TRUE for writing to file system

## Value

Writes a tsv file per compound to the same directory as the source file.

Also returns the nested list of data frames for each file

## See also

Other SplitOutput:
[`ras.StackOutput()`](https://klorator.github.io/aquaras/reference/ras.StackOutput.md)

## Examples

``` r
  if (FALSE) { # \dontrun{
ras.SplitOutput() # First thing the function does is ask the user for a file.
ras.SplitOutput(clean = FALSE) # If the summary output file was not based on the provided template.


# !!! DOES **NOT** WRITE TO FILE SYSTEM !!!
listDF = ras.SplitOutput(sourceFiles = system.file("extdata",
                                                  "Example_MLOutput.txt",
                                                  package = "aquaras",
                                                  mustWork = TRUE),
                         write = FALSE)
listDF
} # }
```
