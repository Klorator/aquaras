# Workflow wrapper -------------------------------------------------------------

#' Fic end-to-end workflow (!!!)
#'
#' Currently not functional!
#'
#' @param source What source the data file comes from
#' @param ID Column name for ID column after splitting
#' @param values Column name for values in data
#' @param Buffer RegEx to filter by for buffer values
#' @param Dilution_type RegEx to filter by for dilution factor values
#' @param Dilution_extract RegEx for extracting the dilution factor
#' @param stab RegEx to filter by for stability values
#' @param czero RegEx to filter by for C zero values
#' @param prot_cell_value Column name for values
#' @param prot_cell_type RegEx to filter by
#' @param prot_hom_value RegEx to filter by
#' @param prot_hom_type RegEx to filter by
#' @param V.medium RegEx to filter by
#' @param MassBalance_2.5 Factor for calulating Mass Balance 10.2.5
#' @param .compound Column name for compounds to prefix Sample_ID,
#' `NULL` => "Analyte Peak Name"
#' @param .checkValues Removes "<" from the values column to make sure it can
#' be coerced to numeric.
#' @param prot_.split Column name to split for samples in protein dataframe
#' @param .summarize Summarize values into averages
#' @param .SD Create column with Standard Deviation
#'
#' @return Dataframe with all variables, used & calculated.
#' @export
#'
#' @examples
#'   \dontrun{
#'   # No example yet
#'   }
ras.Fic_workflow <- function(
    source = c("Waters","Sciex"),
    ID = "Sample_ID",
    values = "Conc.",
    Buffer = "HBSS",
    Dilution_type = "[:digit:]x",
    Dilution_extract = "[:digit:]+(?=x)",
    stab = "Stab",
    czero = "Czero",
    prot_cell_value = "mg_Protein",
    prot_cell_type = "Cells",
    prot_hom_value = "Protein_conc._mg/mL",
    prot_hom_type = "hom",
    V.medium = 200,
    MassBalance_2.5 = 1.75,
    .compound = "Analyte Peak Name",
    .checkValues = TRUE,
    prot_.split = "Sample Text",
    .summarize = TRUE,
    .SD = TRUE) {

  # Set output directory
  output_dir <- tcltk::tk_choose.dir(caption = "Select output directory")


  # Load data ----
  if (source[[1]] == "Waters") {
    path.df <- tcltk::tk_choose.files(
      caption = "Select MassLynx output file",
      multi = FALSE
    )
    list.df <- ras.StackOutput(sourceFiles = path.df)
    df <- list.df[[1]]
    .compound <- names(df[length(df)])
  }

  if (source[[1]] == "Sciex") {
    path.df <- tcltk::tk_choose.files(
      caption = "Select Sciex data",
      multi = FALSE
    )
    df <- readr::read_delim(path.df, delim = "\t")
  }

  path.prot <- tcltk::tk_choose.files(
    caption = "Select Protein data",
    multi = FALSE
  )
  df_protein <- readxl::read_excel(path = path.prot)


  # Clean data ----
  df_clean <- df %>% ras.Fic_cleanup(
    .compound = .compound,
    .checkValues = .checkValues
  )
  df_protein <- df_protein %>% ras.Fic_cleanup(
    .values = NA,
    .type = NA,
    .sample.text = NA,
    .checkValues = FALSE,
    .split = prot_.split
  )
test.clean <<- df_clean
test.prot <<- df_protein

  # Extract values ----
  df_buffer <- df_clean %>%
    ras.Fic_extract_simple(
      values = values,
      type = Buffer,
      .summarize = .summarize,
      .SD = .SD
    )
  name_buffer <- names(df_buffer[2])

  df_DiluteHom <- df_clean %>%
    ras.Fic_DiluteHom(
      values = values,
      type = Dilution_type,
      type_extract = Dilution_extract,
      .summarize = .summarize,
      .SD = .SD
    )
  name_DiluteHom <- names(df_DiluteHom[2])
  name_dilution <- names(df_DiluteHom[3])

  df_DiluteHom_buffer <- ras.Fic_diff_sample_buffer(
    df_DiluteHom,
    df_buffer,
    ID.col = ID,
    Buffer_Conc.col = {{name_buffer}},
    Homogenate_Conc.col = {{name_DiluteHom}}
  )

  time_list <- df_clean %>%
    ras.Fic_timepoint(
      values = values,
      .summarize = .summarize
    )
  df_cell <- time_list[[1]]
  df_medium <- time_list[[2]]

  name_cell <- names(df_cell[3])
  name_medium <- names(df_medium[3])

  df_stab <- df_clean %>%
    ras.Fic_extract_simple(
      values = values,
      type = stab,
      .summarize = .summarize,
      .SD = .SD
    )
  name_stab <- names(df_stab[2])

  df_czero <- df_clean %>%
    ras.Fic_extract_simple(values = values,
                           type = czero)
  name_czero <- names(df_czero[2])

  # df_czero <- df_protein %>%
  #   ras.Fic_extract_simple(
  #     values = prot_czero_value,
  #     type = prot_czero_type,
  #     .summarize = .summarize,
  #     .SD = .SD
  #   )
  # name_czero <- names(df_czero[2])

  df_protCell <- df_protein %>%
    ras.Fic_extract_simple(
      values = prot_cell_value,
      type = prot_cell_type,
      .summarize = .summarize,
      .SD = .SD
    )
  name_protCell <- names(df_protCell[2])
test.prot <<- df_protCell
  df_protHom <- df_protein %>%
    ras.Fic_extract_simple(
      values = prot_hom_value,
      type = prot_hom_type,
      .summarize = .summarize,
      .SD = .SD
    )
  name_protHom <- names(df_protHom[2])

  # Collect all variables in one dataframe ----
  if (source[[1]] == "Waters") {
    samples <- df_cell["Sample_ID"]
    df_protCell <- ras.Fic_expand(
      samples = samples,
      df = df_protCell,
      values = {{name_protCell}}
    )
    test.prot <<- df_protCell
    df_protHom <- ras.Fic_expand(
      samples = samples,
      df = df_protHom,
      values = {{name_protHom}}
    )
  }
  df_calc <- list(
    df_DiluteHom_buffer,
    df_cell,
    df_medium,
    df_stab,
    df_czero,
    df_protCell,
    df_protHom
  ) %>% ras.Fic_collect_variables()
test.calc <<- df_calc
  # Calculations ----
  df_calc <- df_calc %>%
    ras.Fic_Fu.hom(
      Buffer = {{name_buffer}},
      Homogenate = {{name_DiluteHom}},
      Dilution_factor = {{name_dilution}}
    )

  df_calc <- df_calc %>%
    ras.Fic_D.prot(Protein_col = {{name_protHom}})

  df_calc <- df_calc %>%
    ras.Fic_Fu.cell(
      D.prot = "D",
      Fu.hom = "fuhom"
    )

  df_calc <- df_calc %>%
    ras.Fic_stability(
      Stab = {{name_stab}},
      C.zero = {{name_czero}}
    )

  df_calc <- df_calc %>%
    ras.Fic_mass_balance_10.2.5(
      Homogenate = {{name_DiluteHom}},
      Dilution_factor = {{name_dilution}},
      Buffer = {{name_buffer}},
      Stab = {{name_stab}},
      mass_factor = MassBalance_2.5
    )

  df_calc <- df_calc %>%
    ras.Fic_A.cell(Cell = {{name_cell}})

  df_calc <- df_calc %>%
    ras.Fic_V.cell(Protein_volume = {{name_protCell}})

  df_calc <- df_calc %>%
    ras.Fic_Kp(
      A.cell = "Acell",
      V.cell = "Vcell",
      Medium = {{name_medium}}
    )

  df_calc <- df_calc %>%
    ras.Fic_mass_balance_10.3.4(
      A.cell = "Acell",
      Medium = {{name_medium}},
      V.medium = {{V.medium}},
      C.zero.Kp = {{name_czero}}
    )

  df_calc <- df_calc %>%
    ras.Fic_Fic(
      Fu.cell = "fucell",
      Kp = "Kp"
    )


  # Write df_calc to file ----
  fn <- basename(path.df)
  fn <- stringr::str_remove(fn, "\\.[:alnum:]{1,3}$")
  f <- paste0(Sys.Date(), " Fic calculations - ", fn, ".csv")
  fp <- file.path(output_dir, f)
  readr::write_excel_csv2(
    df_calc,
    fp
  )

  return(df_calc)
}
