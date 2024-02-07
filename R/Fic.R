# Data cleanup -----------------------------------------------------------------

#' Load data for Fic & Fu feces
#'
#' @param source A string, either `"Sciex"` or `"Waters"`
#' @param .compound .compound as supplied to the workflow function
#'
#' @return A list with the loaded data frame & appropriate
#' value for .compound
#' @export
#'
#' @examples
#'   # No example
ras.Fic_dataImport <- function(source,
                               .compound) {
  if (source == "Waters") {
    path.df <- tcltk::tk_choose.files(caption = "Select MassLynx output file",
                                      multi = FALSE)
    list.df <- ras.StackOutput(sourceFiles = path.df)
    df <- list.df[[1]]
    .compound <- names(df[length(df)])
  }

  if (source == "Sciex") {
    path.df <- tcltk::tk_choose.files(caption = "Select Sciex data",
                                      multi = FALSE)
    df <- readxl::read_excel(path = path.df)
  }
  results <- list(
    df = df,
    .compound = .compound
  )
  return(results)
}

#' Cleaning for Fic calculation
#'
#' The Sample.Text needs to follow the pattern of Sample ID, Type of liquid,
#' Timepoint, Replicate; separated by underscores. E.g. "Hec42_Medium_30_1",
#' "NA_HBSS_NA_1".
#'
#' @param df A dataframe from the MassLynx complete summary output or similar
#' @param .type Column name, filters away Blank
#' @param .sample.text Column name, filters away NA, Blank,
#' and digits ending in nM.
#' @param .checkValues Removes "<" from the values column to make sure it can
#' be coerced to numeric.
#' @param .split Column name, separates by `_` into `Date`, `Initials`,
#' `Sample_origin`, `Dosing`, `Sample_type`, `Timepoint`, and `Replicate`. Left aligned.
#'
#' @return Same dataframe but filtered and with `.sample.text` separated into columns
#' @noRd
#'
ras.Fic_cleanup <- function(df,
                            .values = "Conc.",
                            .type = "Type",
                            .sample.text = "Sample.Text",
                            .checkValues = TRUE,
                            .split = "Sample.Text",
                            .compound = "Compound") {
  if (!is.na(.values)) { # Omit NAs from value column
    df <- df %>%
      dplyr::filter(!is.na(.data[[.values]]))
  }

  if (!is.na(.type)) { # Filter Type
    df <- df %>%
      dplyr::filter(!stringr::str_detect(df[[.type]], "[Bb]lank"))
    df <- df %>%
      dplyr::filter(!stringr::str_detect(df[[.type]], "Standard"))
  }

  if (!is.na(.sample.text)) { # Filter Sample.Text
    df <- df %>%
      dplyr::filter(
              !is.na({{.sample.text}}),
              !stringr::str_detect({{.sample.text}}, "[Bb]lank"),
              !stringr::str_detect({{.sample.text}}, "[:digit:]nM"),
              !stringr::str_detect({{.sample.text}}, "_STD_"))
  }


  if (.checkValues) { # Remove "<" from values to ensure conversion to numeric
    df <- df %>%
      dplyr::mutate({{.values}} := stringr::str_remove(.data[[{{.values}}]], "<"))
  }

  if (!is.na(.split)) { # Split Sample.Text into multiple columns
    df <- df %>%
      tidyr::separate_wider_delim(
        {{.split}}, delim = "_",
        names = c("Date",
                  "Initials",
                  "Sample_origin",
                  "Dosing",
                  "Sample_type",
                  "Timepoint",
                  "Replicate"),
        too_few = "align_start",
        too_many = "merge",
        cols_remove = F)
  }

  if (!is.na(.compound)) { # Combine compound with Date & Initials into Sample_ID
    df <- df %>%
      dplyr::mutate(
        Sample_ID = stringr::str_c(
          df[[.compound]], Date, Initials, Sample_origin, Dosing, Timepoint,
          sep = "_")
      )
  }

  # Re-evaluate column types
  print("Re-evaluate column types")
    df <- df %>%
      readr::type_convert()
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
#' @param type String to filter the column `Sample_type` by. Used to name
#' the `*_Conc.avg` column.
#' @param new_name String to use as first part of new column
#' @param .summarize Summarize values into averages
#' @param .SD Create column with Standard Deviation
#'
#' @return Dataframe with columns `Sample_ID`, `Sample_type`, &
#' `{{type}}_{{values}}_avg`
#' @noRd
ras.Fic_extract_simple <- function(df,
                                   values = "Conc.",
                                   type = "HBSS",
                                   .summarize = TRUE,
                                   .SD = TRUE) {
  # Drop other columns and filter for {{type}}
  df_extract <- df %>%
    dplyr::select(Sample_ID, Sample_type, {{values}})
  df_extract <- df_extract %>%
    dplyr::filter(stringr::str_detect(Sample_type, {{type}})) %>%
    stats::na.omit({{values}})

  # Group by Sample_ID and get SD
  sd_col <- paste0({{type}},"_",{{values}},"_SD")
  if (.SD) {
    df_extract <- df_extract %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::mutate({{sd_col}} := stats::sd(.data[[values]], na.rm = T)) %>%
      dplyr::ungroup()
  } else {
    df_extract <- df_extract %>%
      dplyr::mutate({{sd_col}} := NA)
  }

  # Group by Sample_ID and average
  if (.summarize) {
    value_col <- paste0(type,"_",values,"_avg")
    sd_col2 <- sd_col
    sd_col <- paste0(sd_col,"_avg")
    df_extract <- df_extract %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::summarise({{value_col}} := mean(.data[[values]], na.rm = T),
                       {{sd_col}} := mean(.data[[sd_col2]]), na.rm = T)
  } else {
    value_col <- paste0({{type}},"_",{{values}})
    df_extract <- df_extract %>%
      dplyr::mutate({{value_col}} := .data[[values]])
  }

  df_extract <- df_extract %>%
    dplyr::select(
      Sample_ID,
      {{value_col}},
      tidyselect::any_of({{sd_col}})
    )

  return(df_extract)
}
#' Extract the dilution factor
#'
#' Very specific
#'
#' @param df A dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values
#' @param type Pattern to filter the column `Sample_type` by
#' @param type_extract Pattern to extract the dilution factor
#' @param .summarize Summarize values into averages
#' @param .SD Create column with Standard Deviation
#'
#' @return Dataframe with columns `Sample_ID`, `Sample_type`,
#'  `Dilution_Conc.avg`, & `dilution`
#' @noRd
ras.Fic_DiluteHom <- function(df,
                             values = "Conc.",
                             type = "[:digit:]x",
                             type_extract = "[:digit:]+(?=x)",
                             .summarize = TRUE,
                             .SD = TRUE) {
  # Drop other columns and filter for dilution factor
  df_DiluteHom <- df %>%
    dplyr::select(Sample_ID,
                  Sample_type,
                  {{values}})
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::filter(stringr::str_detect(Sample_type, {{type}})) %>%
    stats::na.omit({{values}})

  # Get SD
  sd_col <- paste0("Homogenate","_",{{values}},"_SD")
  if (.SD) {
    df_DiluteHom <- df_DiluteHom %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::mutate({{sd_col}} := stats::sd(.data[[values]], na.rm = T)) %>%
      dplyr::ungroup()
  } else {
    df_DiluteHom <- df_DiluteHom %>%
      dplyr::mutate({{sd_col}} := NA)
  }

  # Group by Sample_ID & Sample_type and average
  if (.summarize) {
    hom_col <- paste0("Homogenate_Conc._avg")
    sd_col2 <- sd_col
    sd_col <- paste0(sd_col, "_avg")
    df_DiluteHom <- df_DiluteHom %>%
      dplyr::group_by(Sample_ID, Sample_type) %>%
      dplyr::summarise({{hom_col}} := mean(.data[[values]], na.rm = T),
                       {{sd_col}} := mean(.data[[sd_col2]], na.rm = T)) %>%
      dplyr::ungroup()
  } else {
    hom_col <- paste0("Homogenate_Conc.")
    df_DiluteHom <- df_DiluteHom %>%
      dplyr::mutate({{hom_col}} := .data[[values]])
  }

  # Make dilution factor numeric
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::mutate(dilution = stringr::str_extract(Sample_type, {{type_extract}}) )
  df_DiluteHom <- df_DiluteHom %>%
    dplyr::mutate(dilution = as.numeric(dilution) )

  df_DiluteHom <- df_DiluteHom %>%
    dplyr::select(
      Sample_ID,
      tidyselect::all_of({{hom_col}}),
      dilution,
      tidyselect::any_of({{sd_col}})
    )

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
#' @return Dataframe with columns `Sample_ID`, `Sample_type`,
#'  `Dilution_Conc.avg`, `dilution`, & `Buffer_Conc._avg`
#' @noRd
ras.Fic_diff_sample_buffer <- function(df_DiluteHom,
                                   df_buffer,
                                   ID.col = "Sample_ID",
                                   Buffer_Conc.col = "Buffer_Conc._avg",
                                   Homogenate_Conc.col = "Homogenate_Conc._avg") {
  df_combined <- dplyr::inner_join(
    df_DiluteHom,
    df_buffer,
    by = {{ID.col}}
  )

  hom_len <- length(df_DiluteHom[[ID.col]])
  buf_len <- length(df_buffer[[ID.col]])
  comb_len <- length(df_combined[[ID.col]])
  if (hom_len|buf_len > comb_len) {
    warning("Fewer samples after combining Homogenate & Buffer dataframes")
  }

  df_new <- df_combined %>%
    dplyr::mutate(diff.sample_buffer = .data[[Buffer_Conc.col]] - .data[[Homogenate_Conc.col]] ) %>%
    dplyr::mutate(diff.sample_buffer.sq = diff.sample_buffer^2)
  df_new <- df_new %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::slice(which.min(diff.sample_buffer.sq))
  df_new <- df_new %>%
    dplyr::select(-diff.sample_buffer,
                  -diff.sample_buffer.sq)
  return(df_new)
}
#' Extract values for `Sample_type`s with a `Timepoint`
#'
#' @param df A dataframe from [ras.Fic_cleanup()].
#' @param values Name of the column to use for values
#' @param type String to filter the column `Sample_type` by. Used to name
#' the `*_Conc.avg` column.
#' @param .summarize Summarize values into averages
#'
#' @return Dataframe with columns `Sample_ID`, `Sample_type`, `Timepoint`,
#' `{{values}}`, & `{{type}}_{{values}}_avg`.
#' @noRd
ras.Fic_extract_timepoints <- function(df,
                                       values = "Conc.",
                                       type = "Cell",
                                       .summarize = TRUE) {
  new_col <- paste0({{type}},"_",{{values}},"_avg")

  df_time <- df %>%
    dplyr::select(Sample_ID, Sample_type, Timepoint, {{values}}) %>%
    dplyr::filter(stringr::str_detect(Sample_type, {{type}})) %>%
    stats::na.omit({{values}})

  if (.summarize) {
    df_time <- df_time %>%
      dplyr::group_by(Sample_ID, Timepoint) %>%
      dplyr::summarise({{new_col}} := mean(.data[[values]], na.rm = T)) %>%
      dplyr::ungroup()
  } else {
    new_col <- paste0({{type}},"_",{{values}})
    df_time <- df_time %>%
      dplyr::mutate({{new_col}} := .data[[values]])
  }

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
                                    values.avg = "Cell_Conc._avg",
                                    p_title = " ") {
  p <- ggplot2::ggplot(df_time) +
    ggplot2::aes(x = Timepoint,
                 y = .data[[values.avg]],
                 group = Sample_ID,
                 color = Sample_ID) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line(linewidth = 1,
                       show.legend = TRUE) +
    ggplot2::scale_x_continuous(breaks = unique(df_time$Timepoint) ) +
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
ras.Fic_select_timepoints.app <- function(p.cell, p.medium, df.cell, df.medium) {

  # Make variables available for shiny & cleanup .GlobalEnv afterwards ----
  .GlobalEnv$.p.cell <- p.cell
  .GlobalEnv$.p.medium <- p.medium
  .GlobalEnv$.df.cell <- df.cell
  .GlobalEnv$.df.medium <- df.medium
  on.exit(rm(.p.cell, .df.cell,
             .p.medium, .df.medium,
             envir = .GlobalEnv))
  # Shiny UI ----
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fixedRow( # Frozen header row
      shiny::actionButton("submit", "Submit", class = "btn-success btn-lg"),
      shiny::actionButton("close", "Close", class = "btn-danger")
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
  # Shiny server
  server <- function(input, output, session) {
    # Setup: Plots
    output$plot.Cell <- shiny::renderPlot(graphics::plot(p.cell), res = 96)
    output$plot.Medium <- shiny::renderPlot(graphics::plot(p.medium), res = 96)
    # Setup: disable close button
    shinyjs::disable("close")
    # Create sidebar UI elements
    id.c <- unique(df.cell$Sample_ID)
    output$boxes_Cell <- shiny::renderUI({
      lapply(1:length(id.c), function(i) {
        shiny::numericInput(inputId = paste0("Cell_", id.c[[i]]),
                     label = paste0("Cell ", id.c[[i]]),
                     value = 60)
      })
    })
    id.m <- unique(df.medium$Sample_ID)
    output$boxes_Medium <- shiny::renderUI({
      lapply(1:length(id.m), function(i) {
        shiny::numericInput(inputId = paste0("Medium_", id.m[[i]]),
                     label = paste0("Medium ", id.m[[i]]),
                     value = 60)
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
      df_Cell.shiny <- dplyr::tibble(Sample_ID = id.c, Time.Cell = values.Cell)
      df_Medium.shiny <- dplyr::tibble(Sample_ID = id.m, Time.Medium = values.Medium)
      # Display values
      output$check.table.c <- shiny::renderTable(df_Cell.shiny)
      output$check.table.m <- shiny::renderTable(df_Medium.shiny)
      # Send dataframes up to parent environment
      df_Cell.shiny <<- df_Cell.shiny
      df_Medium.shiny <<- df_Medium.shiny
      # Enable close button
      shinyjs::enable("close")
    })
    shiny::observeEvent( input$close, shiny::stopApp() )
  }
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app)
  # Variables to send up ----
  df_Cell.shiny <<- df_Cell.shiny
  df_Medium.shiny <<- df_Medium.shiny
}
#' Extract values for Cell & Medium (values w/ timepoints)
#'
#' @param df Dataframe from ras.Fic_cleanup().
#' @param values Name of the column to use for values.
#' @param types Vector with the types to select timepoints for. (Cell & Medium)
#' @param .summarize Summarize values into averages
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
                              types = c("Cell", "Medium"),
                              .summarize = TRUE) {
  df_cell <- ras.Fic_extract_timepoints(df, values, types[[1]],
                                        .summarize = .summarize)
  df_medium <- ras.Fic_extract_timepoints(df, values, types[[2]],
                                          .summarize = .summarize)

  p.cell <- ras.Fic_plot_timepoints(df_cell,
                                    values.avg = names(df_cell[3]),
                                    p_title = "Cell")
  p.medium <- ras.Fic_plot_timepoints(df_medium,
                                      values.avg = names(df_medium[3]),
                                      p_title = "Medium")

  ras.Fic_select_timepoints.app(p.cell,
                                p.medium,
                                df_cell,
                                df_medium)
  # Variables received from timepoints app
    ## df_Cell.shiny
    ## df_Medium.shiny

  df_cell <- dplyr::inner_join(df_cell,
                               df_Cell.shiny,
                               c(Sample_ID = "Sample_ID",
                                 Timepoint = "Time.Cell"))
  df_medium <- dplyr::inner_join(df_medium,
                                 df_Medium.shiny,
                                 c(Sample_ID = "Sample_ID",
                                   Timepoint = "Time.Medium"))
  timepoints_list <- list(df_cell, df_medium)

  on.exit(rm(df_Cell.shiny, df_Medium.shiny,
             envir = .GlobalEnv))

  return(timepoints_list)
}
#' Expand a dataframe from "sample ID" to "compound_sample ID" format
#'
#' Takes a dataframe with one column of compound_sample and a dataframe that needs
#' to be expanded. Split out the sample ID and mutate new column with the
#' corresponding values.
#'
#' @param samples Dataframe with one column, sample names to expand to
#' @param df Dataframe with samples and values to expand
#' @param values Column name for values in `df`
#'
#' @return Dataframe with Sample_ID & values column, like from ras.Fic_extract_simple()
#' @noRd
ras.Fic_expand <- function(samples,
                           df,
                           values = "Cells_mg_Protein") {
  samples <- samples %>%
    dplyr::mutate(ID = stringr::str_extract(samples[[1]], "(?<=_)[:alnum:]+$"))

  df_expand <- dplyr::full_join(
    samples,
    df,
    by = dplyr::join_by(ID == Sample_ID)
  ) %>%
    dplyr::select(-ID)
  df_expand <- df_expand %>%
    dplyr::group_by(Sample_ID, .data[[values]]) %>%
    dplyr::filter(duplicated(values) == TRUE)

  return(df_expand)


# print(df)
#
#     short_ID <- df[[1]]
#     values_prot <- names(df[2])
#     samples <- samples %>%
#       dplyr::mutate(ID = stringr::str_extract(samples[[1]], "(?<=_)[:alnum:]+$"))
#
# print(short_ID)
# print(values_prot)
# print(samples)
#
#     df_new <- tibble::tibble()
#     for (i in seq_along(short_ID)) {
#       df_temp <- samples %>%
#         dplyr::filter(ID == short_ID[[i]]) %>%
#         dplyr::mutate({{values_prot}} := df[ df[values] == short_ID[[i]], {{values_prot}}])
#       df_new <- dplyr::bind_rows(df_new, df_temp)
#     }
#     df_new <- as.list(df_new)
#     df_new <- do.call(cbind, df_new)
#     df_new <- as.data.frame(df_new)
#
#     df_new <- df_new %>%
#       dplyr::select(-ID)
#
#     return(df_new)



}
#' Collect a list of variables in the same dataframe
#'
#' Obsolete?
#'
#' Selects columns `Sample_ID` & `*_avg`, then does a full join.
#'
#' @param df_list A list of dataframes to join
#'
#' @return A dataframe with `Sample_ID` & all the values
#' @noRd
ras.Fic_collect_variables <- function(df_list) {
  # fun1 <- function(x) x <- x[grep("Sample_ID|_avg$|dilution",names(x))]
  # df_list <- df_list %>% lapply(fun1)
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
    dplyr::mutate(fuhom = (.data[[Buffer]])/
                    (.data[[Homogenate]] * .data[[Dilution_factor]]) )
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
    dplyr::mutate(D = 1000 / (.data[[Protein_col]] * 6.5) )
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
    dplyr::mutate(fucell = 1 / (.data[[D.prot]] * (1/.data[[Fu.hom]]-1)+1) )
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
    dplyr::mutate(Stability = .data[[Stab]] / .data[[C.zero]])
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
                                        Stab = "Stab_Conc._avg",
                                        mass_factor = 1.75) {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
      (.data[[Homogenate]]*.data[[Dilution_factor]] +.data[[Buffer]]*mass_factor)
                                / .data[[Stab]] )
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
    dplyr::mutate(Acell = .data[[Cell]] * 200)
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
    dplyr::mutate(Vcell = .data[[Protein_volume]] * 6.5)
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
    dplyr::mutate(Kp = (.data[[A.cell]]/.data[[V.cell]]) / (.data[[Medium]] * 10) )
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
                    ((.data[[A.cell]]+.data[[Medium]]*10*{{V.medium}})/({{V.medium}}))
                  /.data[[C.zero.Kp]] )
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
    dplyr::mutate(Fic = .data[[Fu.cell]] * .data[[Kp]])
  return(df_calc)
}
#' Calculate Fu, feces
#'
#' Equation: Fufeces = 1 / (D.prot * (1/Fu.hom-1)+1)
#'
#' @param df_calc Dataframe with values
#' @param D Value for dilution factor
#' @param Fu.hom Name of column for F u,Homogenate values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_fu.feces <- function(df_calc,
                             D = 1 / ( (1/4.8) * 0.95),
                             Fu.hom = "fuhom") {
  df_calc <- df_calc %>%
    dplyr::mutate(fufeces = 1 / (D * (1/.data[[Fu.hom]]-1)+1) )
  return(df_calc)
}
#' Calculate mass balance 10.2.5 (Fu feces)
#'
#' Calculate mass balance according to section 10.2.5 in the SOP.
#' Equation:Mass_balance_10.2.5 = (Hom_Conc.avg * dilution
#' + Buffer_Conc._avg * 1.75) / Stab_Conc.avg
#'
#' @param df_calc Dataframe with values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Dilution factor value
#' @param Buffer Name of column for buffer values
#' @param Stab Name of column for stability values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fu_feces_mass_balance_10.2.5 <- function(df_calc,
                                        Homogenate = "Hom_Conc._avg",
                                        Dilution_factor = 4.8,
                                        Buffer = "Buffer_Con._avg",
                                        Stab = "Stab_Conc._avg",
                                        mass_factor = 1.75) {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
      (.data[[Homogenate]] * Dilution_factor + .data[[Buffer]] * mass_factor)
                            / .data[[Stab]] )
  return(df_calc)
}
