#' Cleaning for Fic calculation
#'
#' The Sample.Text needs to follow the pattern of Sample ID, Type of liquid,
#' Timepoint, Replicate; separated by underscores. E.g. "Hec42_Medium_30_1".
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
#' Extract the buffer concentration
#'
#' This feels too specific.
#'
#' @param df A dataframe from [ras.clean_for_Fic()].
#' @param values Name of the column to use for values
#' @param type String to filter the column `LiquidType` by
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`, & `Buffer_Conc.avg`
#' @noRd
ras.Fic_buffer <- function(df, values = "Conc.", type = "HBSS") {
  # Drop other columns and filter for HBSS
  df_buffer <- df %>%
    dplyr::select(Sample_ID,
                  LiquidType,
                  {{values}})
  df_buffer <- df_buffer %>%
    dplyr::filter(stringr::str_detect(LiquidType, {{type}}))
  # Group by Sample_ID and average
  df_buffer <- df_buffer %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::mutate(Buffer_Conc.avg = mean({{values}}),
                  .keep = "unused")
  # Remove duplicates to keep only one average value
  df_buffer <- df_buffer %>%
    dplyr::arrange(Sample_ID) %>%
    dplyr::filter(duplicated(Sample_ID) == FALSE)
  return(df_buffer)
}
