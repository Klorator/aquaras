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
#' if (interactive()) {
#'   res <- ras.Fic_dataImport(source = "Sciex", .compound = "Analyte Peak Name")
#'   names(res)
#' }
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
    .compound = .compound,
    path.df = path.df
  )
  return(results)
}

#' Cleaning for Fic calculation
#'
#' `Sample.Text` needs to follow the pattern Sample ID, type of liquid,
#' timepoint, replicate; separated by underscores (e.g. "Hec42_Medium_30_1",
#' "NA_HBSS_NA_1".
#'
#' @param df A data frame from the MassLynx complete summary output or similar.
#' @param .type Column name, filters away Blank
#' @param .sample.text Column name, filters away NA, Blank,
#' and digits ending in nM.
#' @param .checkValues Removes "<" from the values column to make sure it can
#' be coerced to numeric.
#' @param .split Column name, separates by `_` into `Date`, `Initials`,
#' `Sample_origin`, `Dosing`, `Sample_type`, `Timepoint`, and `Replicate`. Left aligned.
#'
#' @return A filtered data frame with `.sample.text` separated into columns.
#' @noRd
#'
ras.Fic_cleanup <- function(df,
                            .values = "Conc.",
                            .type = "Type",
                            .sample.text = "Sample.Text",
                            .checkValues = TRUE,
                            .split = "Sample.Text",
                            .compound = "Compound") {
  df <- df %>%
    dplyr::select(
      {{.sample.text}},
      {{.type}},
      {{.compound}},
      {{.values}}
    )

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


    df <- df %>%
      dplyr::filter(
        !is.na(df$Sample_type)
      )

    df <- df %>%
      tidyr::pivot_wider(
        id_cols = -`Sample Name`,
        names_from = Sample_type,
        values_from = {{.values}}
      )

  return(df)
}
# Extraction of values ---------------------------------------------------------
#' Extract values: simple edition
#'
#' Get values that don't need further manipulation.
#' This feels too specific.
#'
#' @param df A data frame containing cleaned Fic values.
#' @param values Name of the column to use for values.
#' @param type String to filter the column `Sample_type` by. Used to name
#' the `*_Conc.avg` column.
#' @param .summarize Summarize values into averages
#' @param .SD Create column with Standard Deviation
#'
#' @return A data frame with columns `Sample_ID`, `Sample_type`, and
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
#' @inheritParams ras.Fic_extract_simple
#' @param type Pattern to filter the column names by
#' @param type_extract Pattern to extract the dilution factor
#'
#' @return Data frame with added dilution column
#' @noRd
ras.Fic_Dilution <- function(df,
                             type = "[:digit:]+x",
                             type_extract = "[:digit:]+(?=x)"
                            ) {
  col_names <- colnames(df)
  matches <- as.vector( stringr::str_match(col_names, type) )
  matches_not_na <- !is.na(matches)
  dil_factors <- sum(matches_not_na)

  if (dil_factors == 1) {
    column <- colnames(df[matches_not_na])
    dil <- stringr::str_extract(column, type_extract)
    df <- df %>%
      dplyr::mutate(
        dilution = as.numeric(dil)
      )
    df <- df %>%
      dplyr::rename(
        homogenate = {{column}}
      )
  }

  if (dil_factors > 1) { # To fix

  }

  return(df)
}
#' Calculate smallest difference between dilution & buffer
#'
#' Calculate and filter for the dilution factor with smallest difference to buffer.
#' Very specific
#'
#' @param df_DiluteHom Data frame from `ras.Fic_Dilution()`.
#' @param df_buffer Data frame with buffer values.
#' @param ID.col String with name of ID column
#' @param Buffer_Conc.col String with name of buffer column
#'
#' @return A data frame with columns `Sample_ID`, `Sample_type`,
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
#' @inheritParams ras.Fic_extract_simple
#'
#' @return A data frame with columns `Sample_ID`, `Sample_type`, `Timepoint`,
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
#' @param df_time Data frame from `ras.Fic_extract_timepoints()`.
#' @param values.avg Name of the y-value column to plot.
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
#' @param p.cell Plot from `ras.Fic_plot_timepoints()` for cell data.
#' @param p.medium Plot from `ras.Fic_plot_timepoints()` for medium data.
#' @param df.cell Data frame with cell timepoint values.
#' @param df.medium Data frame with medium timepoint values.
#'
#' @return Side effects only (launches a Shiny app and assigns selected values).
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
#' @param df Data frame from `ras.Fic_cleanup()`.
#' @param values Name of the column to use for values.
#' @param types Vector with the types to select timepoints for. (Cell & Medium)
#' @param .summarize Summarize values into averages.
#'
#' @return A list of data frames with the selected timepoints.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df_ex <- data.frame(
#'     Sample_ID = rep(c("A", "B"), each = 4),
#'     Sample_type = rep(c("Cell", "Cell", "Medium", "Medium"), times = 2),
#'     Timepoint = rep(c(30, 60, 30, 60), times = 2),
#'     `Conc.` = c(10, 15, 9, 11, 20, 24, 17, 19)
#'   )
#'
#'   ras.Fic_timepoint(df_ex)
#' }
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
#' Expand a data frame from "sample ID" to "compound_sample ID" format
#'
#' Takes a data frame with one column of compound_sample and a data frame that needs
#' to be expanded. Split out the sample ID and mutate new column with the
#' corresponding values.
#'
#' @param samples Data frame with one column, sample names to expand to
#' @param df Data frame with samples and values to expand
#' @param values Column name for values in `df`
#'
#' @return A data frame with `Sample_ID` and values, similar to
#'   `ras.Fic_extract_simple()` output.
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
}

#' Collect a list of variables in the same data frame
#'
#' Obsolete?
#'
#' Selects columns `Sample_ID` & `*_avg`, then does a full join.
#'
#' @param df_list A list of data frames to join
#'
#' @return A data frame with `Sample_ID` and all values.
#' @noRd
ras.Fic_collect_variables <- function(df_list) {
  df_calc <- purrr::reduce(df_list, dplyr::full_join)
  return(df_calc)
}

#' Write Fu feces template
#'
#' Writes a runlist template .xlsx file to specified directory.
#'
#' @param dir Directory path, defaults to interactively choosing a folder
#'
#' @return Invisibly returns an openxlsx2 workbook object
#' @export
#'
#' @examples
#'   \dontrun{
#'   generate_Fufeces_template()
#'   }
generate_Fufeces_template <- function(dir = tcltk::tk_choose.dir()) {
  openxlsx2::wb_save(
    Fufeces_runlist_template,
    file = file.path(dir,"Fu feces runlist template.xlsx"),
    overwrite = TRUE
  )
  return(invisible(Fufeces_runlist_template))
}
