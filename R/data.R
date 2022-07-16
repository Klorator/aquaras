#' Default data frame for RunlistGenerator()
#'
#' Standard data frame populated with only blanks. Meant as a base to be filled in.
#'
#' @format A data frame with 672 rows and 14 variables:
#'  \describe{
#'      \item{Index}{Index to uniquely identify rows}
#'      \item{Plate}{Well plate number}
#'      \item{Row}{Row letter on well plate}
#'      \item{Col}{Column number on well plate}
#'      \item{LC_Position}{Composit coordinate to uniquely identify wells (Plate:Row,Col)}
#'      \item{Date}{Character string meant to hold a date}
#'      \item{Signature}{Signature}
#'      \item{Sample_name}{Composite description of Date and Signature, with underscore "_" as separator}
#'      \item{Compound}{Compound name}
#'      \item{Timepoint}{Timepoint}
#'      \item{Well_Type}{Well type ("Bead", "Medium", "Cell", "Standard", "Blank")}
#'      \item{LC_Well_Type}{Either Blank or Analyte}
#'      \item{Replicate}{Replicate}
#'      \item{Sample_text}{Composite description of Compound, Timepoint, and Well_Type, with underscore "_" as separator}
#' }
#'
#' @source Created in-house.
#' @examples
#' data(Runlist_default)
#' head(Runlist_default)
"Runlist_default"
