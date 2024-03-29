% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fic_data_prosessing.R
\name{generate_Fufeces_template}
\alias{generate_Fufeces_template}
\title{Write Fu feces template}
\usage{
generate_Fufeces_template(dir = tcltk::tk_choose.dir())
}
\arguments{
\item{dir}{Directory path, defaults to interactively choosing a folder}
}
\value{
Invisibly returns an openxlsx2 workbook object
}
\description{
Writes a runlist template .xlsx file to specified directory.
}
\examples{
  \dontrun{
  generate_Fufeces_template()
  }
}
