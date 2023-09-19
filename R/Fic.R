# Data cleanup -----------------------------------------------------------------
#' Cleaning for Fic calculation
#'
#' The Sample.Text needs to follow the pattern of Sample ID, Type of liquid,
#' Timepoint, Replicate; separated by underscores. E.g. "Hec42_Medium_30_1",
#' "NA_HBSS_NA_1".
#'
#' @param df A dataframe from the MassLynx complete summary output or similar
#' @param .type Column name, filters away Blank
#' @param .sample.text Column name, filters away NA, Blank,
#' and digits ending in nM
#' @param .split Column name, separates by `_` into `Sample_ID`, `LiquidType`,
#' `Timepoint`, and `Replicate.` Left aligned.
#'
#' @return Same dataframe but filtered and with Sample.Text
#' separated into columns
#' @noRd
#'
ras.Fic_cleanup <- function(df,
                            .type = "Type",
                            .sample.text = "Sample.Text",
                            .split = "Sample.Text") {
  if (!is.null(.type)) {
    df <- df %>%
      dplyr::filter({{.type}} != "[Bb]lank")
  }
  if (!is.null(.sample.text)) {
    df <- df %>%
      dplyr::filter(
              !is.na({{.sample.text}}),
              !stringr::str_detect({{.sample.text}}, "[Bb]lank"),
              !stringr::str_detect({{.sample.text}}, "[:digit:]nM"))
  }
  # Split Sample.Text into multiple columns
  if (!is.null(.split)) {
    df <- df %>%
      tidyr::separate_wider_delim(
        {{.split}}, delim = "_",
        names = c("Sample_ID",
                  "LiquidType",
                  "Timepoint",
                  "Replicate"),
        too_few = "align_start",
        cols_remove = F)
  }
  return(df)
}
# Extraction of values ---------------------------------------------------------
#' Extract values: simple edition
#'
#' Get values that don't need further manipulation.
#' This feels too specific.
#'
#' @param df A dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values.
#' @param type String to filter the column `LiquidType` by. Used to name
#' the `*_Conc.avg` column.
#' @param new_name String to use as first part of new column
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`, &
#' `{{type}}_{{values}}_avg`
#' @noRd
ras.Fic_extract_simple <- function(df,
                                   values = "Conc.",
                                   type = "HBSS") {
  # Drop other columns and filter for {{type}}
  df_extract <- df %>%
    dplyr::select(Sample_ID, LiquidType, {{values}})
  df_extract <- df_extract %>%
    dplyr::filter(stringr::str_detect(LiquidType, {{type}})) %>%
    stats::na.omit({{values}})
  # Group by Sample_ID and average
  new_col <- paste0({{type}},"_",{{values}},"_avg")
  df_extract <- df_extract %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::mutate({{new_col}} := mean({{values}}), .keep = "unused")
  # Remove duplicates to keep only one average value
  df_extract <- df_extract %>%
    dplyr::arrange(Sample_ID) %>%
    dplyr::filter(duplicated(Sample_ID) == FALSE)
  return(df_extract)
}
#' Extract the dilution factor
#'
#' Very specific
#'
#' @param df A dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values
#' @param type Pattern to filter the column `LiquidType` by
#' @param type_extract Pattern to extract the dilution factor
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`,
#'  `Dilution_Conc.avg`, & `dilution`
#' @noRd
ras.Fic_DiluteHom <- function(df,
                             values = "Conc.",
                             type = "[:digit:]x",
                             type_extract = "[:digit:]+(?=x)") {
  # Drop other columns and filter for dilution factor
  df_DiluteHom <- df %>%
    dplyr::select(Sample_ID,
                  LiquidType,
                  {{values}})
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::filter(stringr::str_detect(LiquidType, {{type}}))
  # Group by Sample_ID & LiquidType and average
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::group_by(Sample_ID, LiquidType) %>%
    dplyr::mutate(Homogenate_Conc._avg = mean({{values}}))
  # Remove duplicates to keep only one average value
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::arrange(Sample_ID, LiquidType) %>%
    dplyr::filter(duplicated(Sample_ID) == FALSE &
                    duplicated(LiquidType) == FALSE)
  # Make dilution factor numeric
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::mutate(dilution = as.numeric(stringr::str_extract(LiquidType, {{type_extract}}) ) )
  return(df_DiluteHom)
}
#' Calculate smallest difference between dilution & buffer
#'
#' Calculate and filter for the dilution factor with smallest difference to buffer.
#' Very specific
#'
#' @param df_DiluteHom Dataframe from [ras.Fic_DiluteHom()]
#' @param df_bufferDataframe from [ras.Fic_buffer()]
#' @param ID.col String with name of ID column
#' @param Buffer_Conc.col String with name of buffer column
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`,
#'  `Dilution_Conc.avg`, `dilution`, & `Buffer_Conc._avg`
#' @noRd
ras.diff.sample_buffer <- function(df_DiluteHom, df_buffer,
                                   ID.col = "Sample_ID",
                                   Buffer_Conc.col = "Buffer_Conc._avg") {
  unique.Sample_ID <- unique(df_DiluteHom[[ID.col]])
  df_new <- tibble::tibble()
  for ( i in seq_along(unique.Sample_ID) ) {
    df_temp <- df_DiluteHom %>% dplyr::filter(.data[[ID.col]] == unique.Sample_ID[[i]]) %>%
      dplyr::mutate(Buffer_Conc._avg = df_buffer[df_buffer[ID.col] == unique.Sample_ID[[i]], Buffer_Conc.col][[1]])
    df_new <- dplyr::bind_rows(df_new, df_temp)
  }
  df_new <- df_new %>%
    dplyr::mutate(diff.sample_buffer.sq = (Buffer_Conc._avg-Hom_Conc.avg)^2 ) %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::slice(which.min(diff.sample_buffer.sq))
  df_new <- df_new %>%
    dplyr::select(-diff.sample_buffer.sq)
  return(df_new)
}
#' Extract values for `LiquidType`s with a `Timepoint`
#'
#' @param df A dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values
#' @param type String to filter the column `LiquidType` by. Used to name
#' the `*_Conc.avg` column.
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`, `Timepoint`,
#' `{{values}}`, & `{{type}}_{{values}}_avg`.
#' @noRd
ras.Fic_extract_timepoints <- function(df,
                                       values = "Conc.",
                                       type = "Cell") {
  col_avg <- paste0({{type}},"_",{{values}},"_avg")
  df_time <- df %>%
    dplyr::select(Sample_ID, LiquidType, Timepoint, {{values}}) %>%
    dplyr::filter(Sample_ID == {{type}})
  df_time <- df_time %>%
    dplyr::group_by(Sample_ID, Timepoint) %>%
    dplyr::mutate({{col_avg}} := mean({{values}})) %>%
    dplyr::filter(duplicated({{col_avg}} == FALSE))
  return(df_time)
}
#' Plot the timepoints
#'
#' @param df_time Dataframe from ras.Fic_extract_timepoints()
#' @param p_title String for plot title
#'
#' @return A ggplot2 object
#' @noRd
ras.Fic_plot_timepoints <- function(df_time,
                                    p_title = " ") {
  p <- ggplot2::ggplot(df_time) +
    ggplot2::aes(x = Timepoint,
                 y = .data[[5]],
                 group = Sample_ID,
                 color = Sample_ID) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line(linewidth = 1,
                       show.legend = TRUE) +
    ggplot2::labs(x = "Timepoint",
                  y = "Mean value",
                  title = {{p_title}}) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            color = "black"),
                   plot.background = ggplot2::element_rect(fill = "lightblue"))
  return(p)
}
#' App to select timepoints
#'
#' @param plots List of plots from [ras.Fic_plot_timepoints()]
#' to display (currently Cell & Medium)
#'
#' @return List of selected values same length as the list of plots
#' @noRd
ras.Fic_select_timepoints.app <- function(plots) {
  plot.cell_series <- plots[[1]]
  plot.medium_series <- plots[[2]]
  ui <- shiny::fluidPage(
    shiny::fluidRow( # Frozen header row
      shiny::actionButton("submit", "Submit", class = "btn-success btn-lg")
    ),
    shiny::sidebarLayout( # Cell info row
      shiny::sidebarPanel( # Inputs (create dynamic amount of inputs)
        shiny::uiOutput("boxes_Cell")
      ),
      shiny::mainPanel( # Display plot
        shiny::plotOutput("plot.Cell")
      )
    ),
    shiny::sidebarLayout( # Medium info row
      shiny::sidebarPanel(
        shiny::uiOutput("boxes_Medium")
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot.Medium")
      )
    ),
    shiny::fluidRow(shiny::tableOutput("check.table.c")),
    shiny::fluidRow(shiny::tableOutput("check.table.m"))
  )
  server <- function(input, output, session) {
    # Plots
    output$plot.Cell <- shiny::renderPlot(graphics::plot(plot.cell_series), res = 96)
    output$plot.Medium <- shiny::renderPlot(graphics::plot(plot.medium_series), res = 96)
    # Create sidebar UI elements
    id.c <- unique(df_cell$Sample_ID)
    output$boxes_Cell <- shiny::renderUI({
      lapply(1:length(id.c), function(i) {
        shiny::numericInput(inputId = paste0("Cell_", id.c[[i]]),
                     label = paste0("Cell ", id.c[[i]]),
                     value = 0)
      })
    })
    id.m <- unique(df_medium$Sample_ID)
    output$boxes_Medium <- shiny::renderUI({
      lapply(1:length(id.m), function(i) {
        shiny::numericInput(inputId = paste0("Medium_", id.m[[i]]),
                     label = paste0("Medium ", id.m[[i]]),
                     value = 0)
      })
    })
    # Store input values
    shiny::observeEvent(input$submit, {
      # Get values
      values.Cell <- sapply(1:length(id.c), function(i){
        as.numeric(input[[paste0("Cell_", id.c[[i]])]])
      })
      values.Medium <- sapply(1:length(id.m), function(i){
        as.numeric(input[[paste0("Medium_", id.m[[i]])]])
      })
      # Create dataframes
      data.Cell <- dplyr::tibble(Sample_ID = id.c, Time.Cell = values.Cell)
      data.Medium <- dplyr::tibble(Sample_ID = id.m, Time.Medium = values.Medium)
      # Assign to access downstream in script
      assign("df_Cell.shiny", data.Cell, envir = parent.env() )
      assign("df_Medium.shiny", data.Medium, envir = parent.env())
      # Display values for dev
      output$check.table.c <- shiny::renderTable(data.Cell)
      output$check.table.m <- shiny::renderTable(data.Medium)
    })
  }
  shiny::shinyApp(ui, server)
  time_list <- list(df_Cell.shiny,
                    df_Medium.shiny)
  return(time_list)
}
#' Extract values for Cell & Medium (values w/ timepoints)
#'
#' @param df Dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values.
#' @param types Vector with the types to select timepoints for. (Cell & Medium)
#'
#' @return List of dataframes with the selected timepoints
#' @export
#'
#' @examples
#'  \dontrun{
#'  # No example yet
#'  }
ras.Fic_timepoint <- function(df,
                              values = "Conc.",
                              types = c("Cell", "Medium")) {
  df_cell <- ras.Fic_extract_timepoints(df, values, types[[1]])
  df_medium <- ras.Fic_extract_timepoints(df, values, types[[2]])

  p.cell <- ras.Fic_plot_timepoints(df_cell,p_title = "Cell")
  p.medium <- ras.Fic_plot_timepoints(df_medium,p_title = "Medium")
  p.list <- list(p.cell, p.medium)

  correct_times <- ras.Fic_select_timepoints.app(p.list)

  df_cell <- df_cell %>% dplyr::filter(Timepoint == {{correct_times[[1]]}})
  df_medium <- df_medium %>% dplyr::filter(Timepoint == {{correct_times[[2]]}})

  timepoints_list <- list(df_cell, df_medium)
  return(timepoints_list)
}
#' Collect a list of variables in the same dataframe
#'
#' Selects columns `Sample_ID` & `*_avg`, then does a full join.
#'
#' @param df_list A list of dataframes to join
#'
#' @return A dataframe with `Sample_ID` & all the values
#' @noRd
ras.Fic_collect_variables <- function(df_list) {
  fun1 <- function(x) x <- x[c("Sample_ID",grep("_avg$|dilution",names(x)))]
  df_list <- df_list %>% lapply(fun1)
  df_calc <- purrr::reduce(df_list, dplyr::full_join)
  return(df_calc)
}
# Calculations -----------------------------------------------------------------
#' Calculate F u,Homogenate
#'
#'  Equation: f u,hom = Peak area(buffer sample) /
#'  ( Peak area(cell homogenate sample)*sample dilution factor )
#'
#' @param df_calc Dataframe with values
#' @param Buffer Name of column for buffer values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Name of column for dilution factor
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_Fu.hom <- function(df_calc,
                           Buffer = "Buffer_Con._avg",
                           Homogenate = "Hom_Conc._avg",
                           Dilution_factor = "dilution") {
  df_calc <- df_calc %>%
    dplyr::mutate(fuhom = ({{Buffer}})/
                    ({{Homogenate}} * {{Dilution_factor}}) )
  return(df_calc)
}
#' Calculate D protein (dilution factor of protein)
#'
#' Equation: D = 1000 /
#' ("Protein conc.(mg/ml)" * 6.5) )
#'
#' @param df_calc Dataframe with values
#' @param Protein_col Name of column for protein values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_D.prot <- function(df_calc,
                      Protein_col = "Protein_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(D = 1000 / ({{Protein_col}} * 6.5) )
  return(df_calc)
}
#' Calculate F u,cell
#'
#' Equation: fucell = 1 / (D.prot * (1/Fu.hom-1)+1)
#'
#' @param df_calc Dataframe with values
#' @param D.prot Name of column for protein dilution values
#' @param Fu.hom Name of column for F u,Homogenate values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_Fu.cell <- function(df_calc,
                            D.prot = "D",
                            Fu.hom = "fuhom") {
  df_calc <- df_calc %>%
    dplyr::mutate(fucell = 1 / ({{D.prot}} * (1/{{Fu.hom}}-1)+1) )
  return(df_calc)
}
#' Calculate Stability
#'
#' Equation: Stability = Stab_Conc.avg / Czero_Conc.avg
#'
#' @param df_calc Dataframe with values
#' @param Stab Name of column for stability values
#' @param C.zero Name of column for C zero values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_stability <- function(df_calc,
                              Stab = "Stab_Conc._avg",
                              C.zero = "Czero_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Stability = {{Stab}} / {{C.zero}})
  return(df_calc)
}
#' Calculate mass balance 10.2.5
#'
#' Calculate mass balance according to section 10.2.5 in the SOP.
#' Equation:Mass_balance_10.2.5 = (Hom_Conc.avg * dilution
#' + Buffer_Conc._avg * 1.75) / Stab_Conc.avg
#'
#' @param df_calc Dataframe with values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Name of column for dilution factor
#' @param Buffer Name of column for buffer values
#' @param Stab Name of column for stability values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_mass_balance_10.2.5 <- function(df_calc,
                                        Homogenate = "Hom_Conc._avg",
                                        Dilution_factor = "dilution",
                                        Buffer = "Buffer_Con._avg",
                                        Stab = "Stab_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
                    ({{Homogenate}}*{{Dilution_factor}} +{{Buffer}}*1.75)
                    / {{Stab}} )
  return(df_calc)
}
#' Calculate A cell
#'
#' Equation: Acell = Cell_Conc.avg(nM) * 200(micro l)
#'
#' @param df_calc Dataframe with values
#' @param Cell Name of column for cell values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_A.cell <- function(df_calc,
                           Cell = "Cell_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Acell = {{Cell}} * 200)
  return(df_calc)
}
#' Calculate Cell volume
#'
#' Equation: Vcell = "Protein amount/well" * 6.5
#'
#' @param df_calc Dataframe with values
#' @param Protein_volume Name of column for protein volume values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_V.cell <- function(df_calc,
                           Protein_volume = "V.Prot") {
  df_calc <- df_calc %>%
    dplyr::mutate(Vcell = {{Protein_volume}} * 6.5)
  return(df_calc)
}
#' Calculate Kp
#'
#' Equation: Kp = (Acell/Vcell) / (Medium_Conc.avg * 10
#'
#' @param df_calc Dataframe with values
#' @param A.cell Name of column for A cell values
#' @param V.cell Name of column for V cell values
#' @param Medium Name of column for medium values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_Kp <- function(df_calc,
                       A.cell = "Acell",
                       V.cell = "Vcell",
                       Medium = "Medium_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Kp = ({{A.cell}}/{{V.cell}}) / ({{Medium}} * 10) )
  return(df_calc)
}
#' Calculate mass balance 10.3.2
#'
#' Calculate mass balance according to section 10.3.2 in the SOP.
#' Equation: Mass_balance_10.3.5 =
#' ((Acell+Medium_Conc.avg*10*Vmedium)/(Vmedium))/Kp_Conc.avg
#'
#' @param df_calc Dataframe with values
#' @param A.cell Name of column for A cell values
#' @param Medium Name of column for medium values
#' @param V.medium Name of column for V medium values
#' @param C.zero.Kp Name of column for C zero Kp values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_mass_balance_10.3.4 <- function(df_calc,
                                        A.cell = "Acell",
                                        Medium = "Medium_Conc._avg",
                                        V.medium = 200,
                                        C.zero.Kp = "CzeroKp") {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.3.5 =
                    (({{A.cell}}+{{Medium}}*10*{{V.medium}})/({{V.medium}}))
                  /{{C.zero.Kp}} )
  return(df_calc)
}
#' Calculate F ic
#'
#' Equation: Fic = fucell * Kp
#'
#' @param df_calc Dataframe with values
#' @param Fu.cell Name of column for F u, cell values
#' @param Kp Name of column for Kp values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_Fic <- function(df_calc,
                        Fu.cell = "fucell",
                        Kp = "Kp") {
  df_calc <- df_calc %>%
    dplyr::mutate(Fic = {{Fu.cell}} * {{Kp}})
  return(df_calc)
}
# Workflow wrapper -------------------------------------------------------------
#' Fic end-to-end workflow
#'
#' @param source What source the data file comes from
#' @param ID Column name for ID column after splitting
#' @param values Column name for values in data
#' @param Buffer RegEx to filter by for buffer values
#' @param Dilution_type RegEx to filter by for dilution factor values
#' @param Dilution_extract RegEx for extracting the dilution factor
#' @param stab RegEx to filter by for stability values
#' @param czero RegEx to filter by for C zero values values
#' @param Kp RegEx to filter by for kp values
#' @param prot_cell_value Column name for values
#' @param prot_cell_type RegEx to filter by
#' @param prot_hom_value RegEx to filter by
#' @param prot_hom_type RegEx to filter by
#' @param V.medium RegEx to filter by
#'
#' @return Dataframe with all variables, used & calculated
#' @export
#'
#' @examples
#'   \dontrun{
#'   # No example yet
#'   }
ras.Fic_workflow <- function(source = c("Waters","Sciex"),
                             ID = "Sample_ID",
                             values = "Conc.",
                             Buffer = "HBSS",
                             Dilution_type = "[:digit:]x",
                             Dilution_extract = "[:digit:]+(?=x)",
                             stab = "Stab$",
                             czero = "Czero$",
                             Kp = "Kp$",
                             prot_cell_value = "mg_Protein",
                             prot_cell_type = "Cells",
                             prot_hom_value = "Protein_conc._mg/mL",
                             prot_hom_type = "hom",
                             V.medium = 200) {
  # Load data
  if (source[[1]] == "Waters") {
    list.df <- ras.StackOutput(sourceFiles =
      tcltk::tk_choose.files(caption = "Select MassLynx output file",
                             multi = FALSE))
    df <- list.df[[1]]
  }
  if (source[[1]] == "Sciex") {
    path.df <- tcltk::tk_choose.files(caption = "Select Sciex data",
                                 multi = FALSE)
    delim.df <- detect_delim(path = path.df) # EML::detect_delim()
    df <- readr::read_delim(path.df,
                            delim = delim.df)
  }
  path.prot <- tcltk::tk_choose.files(caption = "Select Protein data",
                                       multi = FALSE)
  delim.prot <-detect_delim(path = path.prot) # EML::detect_delim()
  df_protein <- readr::read_delim(path.prot,
                                  delim = delim.prot)
  # Clean data
  df_clean <- df %>% ras.Fic_cleanup()
  df_protein <- df_protein %>% ras.Fic_cleanup(.type = NULL)
  # Extract values
  df_buffer <- df_clean %>%
    ras.Fic_extract_simple(values = values,
                           type = Buffer)
  name_buffer <- names(df_buffer[3])

  df_DiluteHom <- df_clean %>%
    ras.Fic_DiluteHom(values = values,
                     type = Dilution_type,
                     type_extract = Dilution_extract)
  name_DiluteHom <- names(df_DiluteHom[[3]])
  name_dilution <- names(df_DiluteHom[[4]])

  df_DiluteHom_buffer <- ras.diff.sample_buffer(df_DiluteHom,
                                               df_buffer,
                                               ID.col = ID,
                                               Buffer_Conc.col = name_buffer)

  time_list <- df_clean %>%
    ras.Fic_timepoint(values = values)
  df_cell <- time_list[[1]]
  df_medium <- time_list[[2]]
  name_cell <- names(df_cell[3])
  name_medium <- names(df_medium[[3]])

  df_stab <- df_clean %>%
    ras.Fic_extract_simple(values = values,
                           type = stab)
  name_stab <- names(df_stab[3])

  df_czero <- df_clean %>%
    ras.Fic_extract_simple(values = values,
                           type = czero)
  name_czero <- names(df_czero[3])

  df_kp <- df_clean %>%
    ras.Fic_extract_simple(values = values,
                           type = Kp)
  name_kp <- names(df_kp[3])

  df_protCell <- df_protein %>%
    ras.Fic_extract_simple(values = prot_cell_value,
                           type = prot_cell_type)
  name_protCell <- names(df_protCell[3])

  df_protHom <- df_protein %>%
    ras.Fic_extract_simple(values = prot_hom_value,
                           type = prot_hom_type)
  name_protHom <- names(df_protHom[3])
  # Collect all variables in one dataframe
  df_calc <- list(
    df_DiluteHom_buffer,
    df_cell,
    df_medium,
    df_stab,
    df_czero,
    df_kp,
    df_protCell,
    df_protHom
    ) %>% ras.Fic_collect_variables()
  # Calculations
  df_calc <- df_calc %>%
    ras.Fic_Fu.hom(Buffer = name_buffer,
                   Homogenate = name_DiluteHom,
                   Dilution_factor = name_dilution)
  df_calc <- df_calc %>%
    ras.Fic_D.prot(Protein_col = name_protHom)
  df_calc <- df_calc %>%
    ras.Fic_Fu.cell(D.prot = "D",
                    Fu.hom = "fuhom")
  df_calc <- df_calc %>%
    ras.Fic_stability(Stab = name_stab,
                      C.zero = name_czero)
  df_calc <- df_calc %>%
    ras.Fic_mass_balance_10.2.5(Homogenate = name_DiluteHom,
                                Dilution_factor = name_dilution,
                                Buffer = name_buffer,
                                Stab = name_stab)
  df_calc <- df_calc %>%
    ras.Fic_A.cell(Cell = name_cell)
  df_calc <- df_calc %>%
    ras.Fic_V.cell(Protein_volume = name_protCell)
  df_calc <- df_calc %>%
    ras.Fic_Kp(A.cell = "Acell",
               V.cell = "Vcell",
               Medium = name_medium)
  df_calc <- df_calc %>%
    ras.Fic_mass_balance_10.3.4(A.cell = "Acell",
                                Medium = name_medium,
                                V.medium = V.medium,
                                C.zero.Kp = name_kp)
  df_calc <- df_calc %>%
    ras.Fic_Fic(Fu.cell = "fucell",
                Kp = "Kp")
  # Write df_calc to file
  f <- paste0(Sys.Date()," Fic calculations.xlsx")
  readr::write_excel_csv(df_calc,
                         f)
  return(df_calc)
}
