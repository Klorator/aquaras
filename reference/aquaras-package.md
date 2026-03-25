# aquaras: Data processing related to MassLynx

Pre-LC/MS run: Provides tools for working with runlists to use with
MassLynx (Waters LC/MS software). Calling ras.RunlistGenerator()
launches a Shiny app that lets you create the full data frame and a
runlist. Alternatively, ras.create.Runlist() may be used directly.
Post-LC/MS run, Use ras.SplitOutput() to split the MassLynx complete
summary output file into individual data frames and write them to files.
Unless clean = FALSE, this removes blanks and splits the "Name" and
"Sample Name" columns according to the pre-LC/MS template.

## See also

Useful links:

- <https://github.com/Klorator/aquaras>

- <https://klorator.github.io/aquaras/>

- Report bugs at <https://github.com/Klorator/aquaras/issues>

## Author

**Maintainer**: Rasmus Hammar <Rasmus.Hammar@gmx.com>
