#' Fu feces workflow wrapper
#'
#' @param source What source the data file comes from
#' @param ID Column name for ID column after splitting
#' @param Sample_type Name for type column in raw data
#' @param values Column name for values in data
#' @param Buffer RegEx to filter by for buffer values
#' @param Dilution_type RegEx to filter by for dilution factor values
#' @param Dilution_extract RegEx for extracting the dilution factor
#' @param Dilution_factor Numeric value for dilution factor
#' @param stab RegEx to filter by for stability values
#' @param czero RegEx to filter by for C zero values values
#' @param D Numeric value for D
#' @param MassBalance_2.5 Factor for calulating Mass Balance 10.2.5
#' @param .compound Column name for compounds to prefix Sample_ID,
#' `NULL` => "Analyte Peak Name"
#' @param .checkValues Removes "<" from the values column to make sure it can
#' be coerced to numeric.
#' @param .summarize Summarize values into averages
#' @param .SD Create column with Standard Deviation
#'
#' @return Dataframe with all variables, used & calculated.
#' Also writes data frame to .csv (EU) & .xlsx files.
#' @export
#'
#' @examples
#'    \dontrun{
#'   # No example yet
#'   }
ras.Fu_feces_workflow <- function(
    source = c("Sciex", "Waters"),
    ID = "Sample Name",
    Sample_type = "Sample Type",
    values = "Calculated Concentration (nM)",
    Buffer = "HBSS",
    Dilution_type = "[:digit:]x",
    Dilution_extract = "[:digit:]+(?=x)",
    Dilution_factor = 1,
    stab = "Stab",
    czero = "Czero",
    D = ( 1 / ( (1/4.8) * 0.95) ),
    MassBalance_2.5 = 1.75,
    .compound = "Analyte Peak Name",
    .checkValues = TRUE,
    .summarize = TRUE,
    .SD = TRUE
) {
  # Set output directory
  output_dir <- tcltk::tk_choose.dir(caption = "Select output directory")


  # Load data ----
  bundle <- ras.Fic_dataImport(
    source = source[[1]],
    .compound = .compound
  )
  df <- bundle$df
  .compound <- bundle$.compound
  path.df <- bundle$path.df

  # Clean data ----
  df_clean <- df %>%
    ras.Fic_cleanup(
      .values = values,
      .type = Sample_type,
      .sample.text = ID,
      .split = ID,
      .compound = .compound,
      .checkValues = .checkValues
    )

  df_wide <- df_clean %>%
    tidyr::pivot_wider(
      names_from = Sample_type
    )


  # Extract values ----
  df_buffer <- df_clean %>%
    ras.Fic_extract_simple(
      values = values,
      type = Buffer,
      .summarize = FALSE,
      .SD = FALSE
    )
  name_buffer <- names(df_buffer[2])

  df_DiluteHom <- df_clean %>%
    ras.Fic_DiluteHom(
      values = values,
      type = Dilution_type,
      type_extract = Dilution_extract,
      .summarize = FALSE,
      .SD = FALSE
    )
  name_DiluteHom <- names(df_DiluteHom[2])
  name_dilution <- names(df_DiluteHom[3])

  df_DiluteHom_buffer <- ras.Fic_diff_sample_buffer(
    df_DiluteHom,
    df_buffer,
    Buffer_Conc.col = {{name_buffer}},
    Homogenate_Conc.col = {{name_DiluteHom}}
  )

  df_stab <- df_clean %>%
    ras.Fic_extract_simple(
      values = values,
      type = stab,
      .summarize = FALSE,
      .SD = FALSE
    )
  name_stab <- names(df_stab[2])

  df_czero <- df_clean %>%
    ras.Fic_extract_simple(
      values = values,
      type = czero,
      .summarize = FALSE,
      .SD = FALSE
    )
  name_czero <- names(df_czero[2])


  df_calc <- list(
    df_DiluteHom_buffer,
    df_stab,
    df_czero
  ) %>% purrr::reduce(dplyr::full_join)


  # Calculations ----
  df_calc <- df_calc %>%
    ras.Fic_Fu.hom(
      Buffer = {{name_buffer}},
      Homogenate = {{name_DiluteHom}},
      Dilution_factor = {{name_dilution}}
    )

  df_calc <- df_calc %>%
    ras.Fic_fu.feces(
      D = D,
      Fu.hom = "fuhom"
    )

  df_calc <- df_calc %>%
    ras.Fic_stability(
      Stab = {{name_stab}},
      C.zero = {{name_czero}}
    )

  df_calc <- df_calc %>%
    ras.Fu_feces_mass_balance_10.2.5(
      Homogenate = {{name_DiluteHom}},
      Dilution_factor = Dilution_factor,
      Buffer = {{name_buffer}},
      Stab = {{name_stab}},
      mass_factor = MassBalance_2.5
    )


  # Write df_calc to file ----
  fn <- basename(path.df)
  fn <- stringr::str_remove(fn, "\\.[:alnum:]{1,5}$")
  f <- paste0(Sys.Date()," Fu feces calculations - ", fn, ".csv")
  fp <- file.path(output_dir, f)
  readr::write_excel_csv2(
    df_calc,
    fp
  )

  fx <- paste0(Sys.Date()," Fu feces calculations - ", fn, ".xlsx")
  fxp <- file.path(output_dir, fx)
  openxlsx2::write_xlsx(
    df_calc,
    file = fxp,
    as_table = TRUE,
    overwrite = TRUE
  )

  return(df_calc)
}
