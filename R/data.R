#' Example runlist data frame
#'
#' An example runlist for demonstration and test purposes.
#'
#' @noMd
#'
#' @format A data frame with 672 rows and 14 variables:
#' \describe{
#'   \item{Index}{Unique row identifier}
#'   \item{Plate}{Well plate number, 3–9}
#'   \item{Row}{Row letter, A-H}
#'   \item{Col}{Column number, 1-12}
#'   \item{LC_Position}{Well coordinate, composite of "\code{Plate}:\code{Row},\code{Col}"}
#'
#'   \item{Date}{Date as character string}
#'   \item{Signature}{Signature to identify owner}
#'   \item{Sample_name}{Sample name, composite of "\code{Date}_\code{Signature}_Index.\code{Index}"}
#'   \item{Compound}{Name of compound being tested}
#'   \item{Timepoint}{Integer for plotting timeseries}
#'
#'   \item{Well_Type}{Type of well contents, one of "bead", "medium", "cell", "STD", or "blank"}
#'   \item{LC_Well_Type}{Either "blank" or "Analyte"}
#'   \item{Replicate}{Integer for numbering replicate experiments}
#'   \item{Sample_text}{Sample text, composite of "\code{Compound}_\code{Timepoint}_\code{Well_Type}_\code{Replicate}"}
#'  }
#' @source Created in house
"ras.Example_Runlist"
