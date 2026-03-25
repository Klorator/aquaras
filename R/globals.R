#' Global variable declarations for NSE checks
#'
#' @keywords internal
#' @noRd
#' @importFrom stats coef nls predict sd
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".data", ".df.cell", ".df.medium", ".p.cell", ".p.medium", ":=",
      "Date", "Dosing", "Fufeces_runlist_template", "ID", "Initials",
      "Occurence", "Sample Name", "Sample_ID", "Sample_origin",
      "Sample_type", "Timepoint", "aes", "cur_sam", "df_Cell.shiny",
      "df_Medium.shiny", "diff.sample_buffer", "diff.sample_buffer.sq",
      "directory", "extension", "gene_col_RegEx", "group_size",
      "master_list", "name", "sample_names", "sample_text", "samples_L",
      "series_check", "series_list", "y_hat", "y_lower", "y_upper"
    )
  )
}
