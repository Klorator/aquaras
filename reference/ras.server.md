# Server for [`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md)

Fulfills the `server` argument in
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Usage

``` r
ras.server(input, output, session)
```

## Arguments

- input, output, session:

  As described under `server` in
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)

## See also

Other RunlistGenerator:
[`ras.RunlistGenerator()`](https://klorator.github.io/aquaras/reference/ras.RunlistGenerator.md),
[`ras.add.blank()`](https://klorator.github.io/aquaras/reference/ras.add.blank.md),
[`ras.add.compound()`](https://klorator.github.io/aquaras/reference/ras.add.compound.md),
[`ras.add.type()`](https://klorator.github.io/aquaras/reference/ras.add.type.md),
[`ras.create.Runlist()`](https://klorator.github.io/aquaras/reference/ras.create.Runlist.md),
[`ras.darkModeDF_Options()`](https://klorator.github.io/aquaras/reference/ras.darkModeDF_Options.md),
[`ras.dateSignAll.update()`](https://klorator.github.io/aquaras/reference/ras.dateSignAll.update.md),
[`ras.sanitizeInput()`](https://klorator.github.io/aquaras/reference/ras.sanitizeInput.md),
[`ras.well.update()`](https://klorator.github.io/aquaras/reference/ras.well.update.md)

## Examples

``` r
 if (FALSE) { # \dontrun{
shiny::shinyApp(ui = ras.ui, server = ras.server)
} # }
```
