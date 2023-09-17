#' Cleaning for Fic calculation
#'
#' The Sample.Text needs to follow the pattern of Sample ID, Type of liquid,
#' Timepoint, Replicate; separated by underscores. E.g. "Hec42_Medium_30_1",
#' "NA_HBSS_NA_1".
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
#' Extract values: simple edition
#'
#' Get values that don't need further manipulation.
#' This feels too specific.
#'
#' @param df A dataframe from [ras.clean_for_Fic()].
#' @param values Name of the column to use for values.
#' @param type String to filter the column `LiquidType` by. Used to name
#' the `*_Conc.avg` column.
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`, &
#' `{{type}}_{{values}}_avg`
#' @noRd
ras.Fic_extract_simple <- function(df, values = "Conc.", type = "HBSS") {
  # Drop other columns and filter for {{type}}
  df_extract <- df %>%
    dplyr::select(Sample_ID, LiquidType, {{values}})
  df_extract <- df_extract %>%
    dplyr::filter(stringr::str_detect(LiquidType, {{type}})) %>%
    stats::na.omit({{values}})
  # Group by Sample_ID and average
  df_extract <- df_extract %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::mutate(paste0({{type}},"_",{{values}},"_avg") = mean({{values}}),
                  .keep = "unused")
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
#' @param df A dataframe from [ras.clean_for_Fic()].
#' @param values Name of the column to use for values
#' @param type Pattern to filter the column `LiquidType` by
#' @param type_extract Pattern to extract the dilution factor
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`,
#'  `Dilution_Conc.avg`, & `dilution`
#' @noRd
ras.Fic_dilution <- function(df,
                             values = "Conc.",
                             type = "[:digit:]x",
                             type_extract = "[:digit:]+(?=x)") {
  # Drop other columns and filter for dilution factor
  df_dilution <- df %>%
    dplyr::select(Sample_ID,
                  LiquidType,
                  {{values}})
  df_dilution <- df_dilution %>%
    dplyr::filter(stringr::str_detect(LiquidType, {{type}}))
  # Group by Sample_ID & LiquidType and average
  df_dilution <- df_dilution %>%
    dplyr::group_by(Sample_ID, LiquidType) %>%
    dplyr::mutate(Dilution_Conc.avg = mean({{values}}))
  # Remove duplicates to keep only one average value
  df_dilution <- df_dilution %>%
    dplyr::arrange(Sample_ID, LiquidType) %>%
    dplyr::filter(duplicated(Sample_ID) == FALSE &
                    duplicated(LiquidType) == FALSE)
  # Make dilution factor numeric
  df_dilution <- df_dilution %>%
    dplyr::mutate(dilution = as.numeric(stringr::str_extract(LiquidType, {{type_extract}}) ) )
  return(df_dilution)
}
#' Calculate smallest difference between dilution & buffer
#'
#' Calculate and filter for the dilution factor with smallest difference to buffer.
#' Very specific
#'
#' @param df_dilution Dataframe from [ras.Fic_dilution()]
#' @param df_bufferDataframe from [ras.Fic_buffer()]
#' @param ID.col String with name of ID column
#' @param Buffer_Conc.col String with name of buffer column
#'
#' @return Dataframe with columns `Sample_ID`, `LiquidType`,
#'  `Dilution_Conc.avg`, `dilution`, & `Buffer_Conc.avg`
#' @noRd
ras.diff.sample_buffer <- function(df_dilution, df_buffer,
                                   ID.col = "Sample_ID",
                                   Buffer_Conc.col = "Buffer_Conc.avg") {
  unique.Sample_ID <- unique(df_dilution[[ID.col]])
  df_new <- tibble::tibble()
  for ( i in seq_along(unique.Sample_ID) ) {
    df_temp <- df_dilution %>% dplyr::filter(.data[[ID.col]] == unique.Sample_ID[[i]]) %>%
      dplyr::mutate(Buffer_Conc.avg = df_buffer[df_buffer[ID.col] == unique.Sample_ID[[i]], Buffer_Conc.col][[1]])
    df_new <- dplyr::bind_rows(df_new, df_temp)
  }
  df_new <- df_new %>%
    dplyr::mutate(diff.sample_buffer.sq = (Buffer_Conc.avg-Hom_Conc.avg)^2 ) %>%
    dplyr::group_by(Sample_ID) %>%
    dplyr::slice(which.min(diff.sample_buffer.sq))
  df_new <- df_new %>%
    dplyr::select(-diff.sample_buffer.sq)
  return(df_new)
}
#' Extract values for `LiquidType`s with a `Timepoint`
#'
#' @param df A dataframe from [ras.clean_for_Fic()].
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
    dplyr::mutate({{col_avg}} = mean({{values}})) %>%
    dplyr::filter(duplicated({{col_avg}} == FALSE))
  return(df_time)
}
#' Plot the timepoints
#'
#' @param df_time
#' @param p_title
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
#' @param df Dataframe from [ras.clean_for_Fic()].
#' @param values Name of the column to use for values.
#' @param types Vector with the types to select timepoints for. (Cell & Medium)
#'
#' @return List of dataframes with the selected timepoints
#' @export
#'
#' @examples
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
#' Calculate F u,homogenous
#'
#'  Equation: f u,hom = Peak area(buffer sample) /
#'  ( Peak area(cell homogenate sample)*sample dilution factor )
#'
#' @param df_calc Dataframe with values
#' @param Buffer Name of column for buffer values
#' @param Homogenous Name of column for homogenous values
#' @param Dilution_factor Name of column for dilution factor
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_Fu.hom <- function(df_calc,
                           Buffer = "Buffer_Con._avg",
                           Homogenous = "Hom_Conc._avg",
                           Dilution_factor = "dilution") {
  df_calc <- df_calc %>%
    dplyr::mutate(fuhom = ({{Buffer}})/
                    ({{Homogenous}} * {{Dilution_factor}}) )
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
#' @param Fu.hom Name of column for F u,homogenous values
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
#' + Buffer_Conc.avg * 1.75) / Stab_Conc.avg
#'
#' @param df_calc Dataframe with values
#' @param Homogenous Name of column for homogenous values
#' @param Dilution_factor Name of column for dilution factor
#' @param Buffer Name of column for buffer values
#' @param Stab Name of column for stability values
#'
#' @return Same dataframe with added column from equation
#' @noRd
ras.Fic_mass_balance <- function(df_calc,
                                 Homogenous = "Hom_Conc._avg",
                                 Dilution_factor = "dilution",
                                 Buffer = "Buffer_Con._avg",
                                 Stab = "Stab_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
                    ({{Homogenous}}*{{Dilution_factor}} +{{Buffer}}*1.75)
                    / {{Stab}} )
  return(df_calc)
}
