#' Cleaning for Fic calculation
#'
#' @param df A dataframe from the MassLynx complete summary output or similar
#'
#' @return Same dataframe but filtered and with Sample.Text separated into columns
#' @noRd
#'
ras.clean_for_Fic <- function(df) {
  df_clean <- dplyr::filter(df,
                            Type != "Blank",
                            !is.na(Sample.Text),
                            !str_detect(Sample.Text, "Blank"),
                            !str_detect(Sample.Text, "[:digit:]nM"))
  # Split Sample.Text into multiple columns
  df_clean <- tidyr::separate_wider_delim(df_clean,
                                          Sample.Text, delim = "_",
                                          names = c("Sample_ID",
                                                    "LiquidType",
                                                    "Timepoint",
                                                    "Replicate"),
                                          too_few = "align_end",
                                          cols_remove = F)
  return(df_clean)
}
