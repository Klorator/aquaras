% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randomGroups.R
\name{ras.Randomizer}
\alias{ras.Randomizer}
\title{Randomizer}
\usage{
ras.Randomizer(
  compound_list,
  group_size,
  series = 1,
  write = FALSE,
  fileName = NULL
)
}
\arguments{
\item{compound_list}{A character vector with observations/compound names}

\item{group_size}{Integer of how many observations to attempt to place in each group}

\item{series}{Number of series to make}

\item{write}{Default \code{FALSE}. If set to \code{TRUE} will write series to a .xlsx file}

\item{fileName}{Default \code{NULL}. Name of file. Automatically appended with \code{.xlsx}}
}
\value{
List of data frames
}
\description{
Uses \code{\link[=ras.ran_series]{ras.ran_series()}} to create a series of groups with randomly sampled observations.
Observations are distributed such as that there are no repeated entries between groups/within
a series. Number of groups in the series is calculated as \code{length(master_list)}
divided by \code{group_size}, rounded up to nearest integer. If an observation can
not be found (for example because the groups can not be filled evenly) an error
message is printed to the console and \code{N/A} inserted instead.
If \code{write = TRUE} will write the series to a comma delimited .csv file (excel?).
}
\examples{
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
series_A

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
print("+++++ Series B +++++")
series_B

  \dontrun{
# Example of writing to .xlsx file
 Series_P = ras.Randomizer(Compound_list,
                           group_size = 5,
                           series = 2,
                           write = TRUE,
                           fileName = "Series_P Example file of ras.Randomizer")
  }
}
