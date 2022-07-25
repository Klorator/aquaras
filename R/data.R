#' Example runlist data frame
#'
#' An example runlist for demonstration and test purposes.
#'
#' @format A data frame with 672 rows and 14 variables:
#'  \describe{
#'    \item{Index}{Unique row identifier}
#'    \item{Plate}{Well plate number, 3–9}
#'    \item{Row}{Row letter, A-H}
#'    \item{Col}{Column number, 1-12}
#'    \item{LC_Position}{Well coordinate, composite of "`Plate`:`Row`,`Col`"}
#'    \item{Date}{Date as character string}
#'    \item{Signature}{Signature to identify owner}
#'    \item{Sample_name}{Sample name, composite of "`Date_Signature`_Index.`Index`"}
#'    \item{Compound}{Name of compound being tested}
#'    \item{Timepoint}{}
#'    \item{Well_Type}{Type of well contents}
#'    \item{LC_Well_Type}{Either "blank" or "Analyte"}
#'    \item{Replicate}{Integer for numbering replicate experiments}
#'    \item{Sample_text}{Sample text, composite of "`Compound`_`Timepoint`_`Well_Type`_`Replicate`"}
#'  }
#' @source Created in house
"Example_Runlist"
