#' TPA - Calculation
#'
#' Calculate TPA from intensity using the raw output file from MaxQuant.
#' The function cleans the column names and keeps only columns identified by the
#' `metaData_RegEx`pattern and containing "intensity". TPA is calculated for
#' each column while preserving the experiment identifier. If `dropIntensity`
#' = FALSE intensity columns are preserved.
#'
#' @param df_raw Data frame (from MaxQuant)
#' @param metaData_RegEx Character string for regular expression identifying
#' columns to keep (passed to [base::grep()]). Must include "mol_weight"!
#' @param dropIntensity If to drop intensity (default = TRUE)
#'
#' @return Data frame with meta data and TPA values
#' @noRd
#'
#' @examples
#'   \dontrun{
#' # my_TPA <- ras.TPA_calcFromIntensity(df_raw = my_data)
#'
#' # my_TPA2 <- ras.TPA_calcFromIntensity(df_raw = my_data,
#' #               metaData_RegEx = "protein_names|mol_weight")
#' }
#'
ras.TPA_calcFromIntensity <- function(df_raw,
                                      metaData_RegEx = "protein_names|gene_names|majority_protein|razor_unique_peptides$|reverse|potential_contaminant|mol_weight",
                                      dropIntensity = TRUE) {
  # Clean column names
  df_cleanNames <- janitor::clean_names(df_raw)
  # Select columns with regular expression
  data_RegEx <- "intensity"
  df_selected <- dplyr::select(df_cleanNames,
                               grep(metaData_RegEx, names(df_cleanNames)),
                               grep(data_RegEx, names(df_cleanNames)))
  ### Calc. TPA
  # Copy data frame
  df_TPA_pre <- df_selected
  # Get vector with all experiment intensity column names
  intensityColsRegEx <- "intensity_"
  intensityColsNames <- names(df_TPA_pre[grep(intensityColsRegEx, names(df_TPA_pre))])
  # Copy data frame
  df_TPA_value <- df_TPA_pre
  # Calculate TPA value per experiment intensity column
  for (i in intensityColsNames) {
    # Create new column name for experiment TPA values
    newTPAColName <- stringr::str_replace(i, "intensity_", "TPA_")
    # Sum of current intensity column for calculation
    currSumIntensity <- sum(df_TPA_pre[i])
    # Calculate TPA in new column
    # Formula: Intensity/(Mx * 1000 * \u03A3 Intensities)*10^9 :
      # .data[[i]]/(molecular weight * 1000 * \u03A3 Intensity column)*10^9
    df_TPA_newCol <- dplyr::mutate(df_TPA_pre,
      {{ newTPAColName }} := .data[[i]] / (df_TPA_pre$mol_weight_k_da * 1000 * currSumIntensity) * 10^9,
      .keep = "all")
    # Add new TPA column to data frame
    df_TPA_value <- dplyr::left_join(df_TPA_value,
                                     df_TPA_newCol)
  }
  # If for dropping the intensity columns (default)
  if ( dropIntensity == TRUE ) {
    # Drop all the intensity columns
    df_TPA_value <- dplyr::select(df_TPA_value,
                                 !grep(data_RegEx, names(df_TPA_value)))
  }
  return(df_TPA_value)
}


#' TPA - Sample names
#'
#' Extracts unique sample names and counts how many repeats it has.
#'
#' @param df Data frame with samples in columns
#'
#' @return List with two data frames, (sample_names) & (sample_groups). Each with two columns, (1) unique samples & (2) how many replicates.
#' @noRd
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_sample_names <- function(df) {
  # All column names
  sample_names <- colnames(df)
  # All sample columns
  sample_names <- stringr::str_extract(sample_names,
                                       "(?<=_)[:digit:]+[:alnum:]+(?=_[:digit:]+$)")
  # Omit NAs in vector from non-sample columns
  sample_names <- stats::na.omit(sample_names)
  # Convert to dataframe
  sample_names <- as.data.frame(sample_names)
  # Create column with sample group
  sample_groups <- stringr::str_extract(sample_names$sample_name,
                                        "[:alpha:][:alnum:]*$")
  # Count how many of each sample there are
  sample_names <- sample_names %>%
    dplyr::group_by_all() %>%
    dplyr::count()
  # Set column names
  colnames(sample_names) <- c("sample_name", "replicate_count")
  # Convert to dataframe
  sample_groups <- as.data.frame(sample_groups)
  # Count how many in each group
  sample_groups <- sample_groups %>%
    dplyr::group_by_all() %>%
    dplyr::count()
  # Set column names
  colnames(sample_groups) <- c("sample_group", "group_count")
  # Bundle in list for return variable
  samples_L <- mget(c("sample_names", "sample_groups"))
  return(samples_L)
}

#' TPA - Determine and calculate StDev or Range
#'
#' Internal function.
#'
#' Checks number of values in a row and calculates SD, range, or inputs NA.
#'
#' If `na.rm = TRUE` it drops NA values from the row first (doesn't affect the final data frame).
#'
#' @param df Data frame passed from [ras.TPA_avg_StDev_calc()]
#' @param na.rm If TRUE, drops NA values before determining calculation
#'
#' @return A vector to be inserted as a column in the original data frame
#' @noRd
#'
#' @examples
#'   # No example
ras.TPA_StDev_range_DeterminationForRows <- function(df, na.rm = FALSE) {
  if( na.rm == TRUE ) { df <- na.omit(df) }
  num_col <- length(df)
  # If there are more than 2 samples (StDev)
  if( num_col > 2 ) {
    # Calc. StDev by row for sample columns (assign to new col name)
    StDev_Range <- stats::sd(df)
  }
  # If there are only 2 samples (range)
  if( num_col == 2 ) {
    # Calc. range (diff/2)
    StDev_Range <- (sqrt((df[1] - df[2])^2))/2
  }
  # If fewer than 2 samples, return NA
  if( num_col < 2 ) {
    StDev_Range <- NA
  }
  return(StDev_Range)
}

#' TPA - Average & StDev/Range
#'
#' Adds columns for average and standard deviation (or range if there are only two sample cols).
#'
#' @param df Data frame to add columns to
#' @param sample_names List of samples from [ras.TPA_sample_names()]
#' @param na.rm If TRUE, NA values are dropped before determining calculation
#'
#' @return Same data frame with new columns
#' @noRd
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_avg_StDev_calc <- function(df, sample_names, na.rm = FALSE) {
  ### Calculate for sample names ###
  # For-loop construct iterating over sample names
  for( i in seq_len(nrow(sample_names[1])) ) {
    cur_sam <- sample_names[[i,1]]
    cur_sam_RegEx <- paste0("_", cur_sam, "_")
    # Current sample columns
    cur_cols <- df[grep(cur_sam_RegEx, names(df))]
    # Create new average column name and assign value
    new_Avg_col = paste0("_", cur_sam, "_avg")
    df[[new_Avg_col]] <- rowMeans(cur_cols, na.rm = na.rm)
    # Create new StDev.Range column name and empty column
    new_StDev.Range_col <- paste0("_", cur_sam, "_StDev.Range")
    df[[new_StDev.Range_col]] <- apply(cur_cols, 1,
                                       ras.TPA_StDev_range_DeterminationForRows, na.rm = na.rm)
  }
  return(df)
}

#' TPA - Barplot helper function
#'
#' Helper function that does the actual plotting and saving.
#'
#' @param row_v Named character vector row from apply
#' @param cur_sam Current sample
#' @param gene_col_RegEx RegEx for selecting the gene name column/value
#' @param save If `TRUE`, saves plot to directory
#' @param extension What extension to use for both file ending and [ggsave()] `device` argument
#' @param directory Directory path to write to
#'
#' @return One ggplot object
#' @noRd
#'
#' @examples
#'   # No example
ras.TPA_barplot_helper <- function(row_v, helper_args) {
  list2env(helper_args, envir = environment())
  # Split out information for plots
  row_v <- as.data.frame(setNames(as.list(as.vector(row_v)), names(row_v)))
  row_v[is.na(row_v)] <- 0
  row_v[grep("NaN", row_v)] <- 0
  cur_gene <-        row_v[grep(gene_col_RegEx, names(row_v))]
  cur_avg <-         as.numeric(row_v[grep("avg", names(row_v))])
  cur_StDev.Range <- as.numeric(row_v[grep("StDev.Range", names(row_v))])
  values_RegEx <-    paste0("_", cur_sam, "_[^(?:avg)(?:StDev)]")
  cur_values <-      row_v[grep(values_RegEx , names(row_v))]
  cur_values <-      tidyr::pivot_longer(cur_values, tidyselect::everything())
  # Create barplot
  bp <- ggplot2::ggplot() +
    ggplot2::aes(x = cur_values$name,
                 y = cur_values$value,
                 fill = cur_values$name) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(aes(ymax = (cur_avg + cur_StDev.Range),
                               ymin = (cur_avg - cur_StDev.Range) )) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(x = "Sample",
                  y = "Concentration [fmol/\u00B5 g]",
                  title = cur_gene[[1]]) +
    ggplot2::theme_minimal()
  # Write plot to directory
  if( save ) {
    file_name <- paste0(cur_gene[[1]], "_", cur_sam, ".", extension)
    ggplot2::ggsave(filename = file_name,
                    plot = bp,
                    device = extension,
                    path = directory,
                    bg = "white")
  }
  return(bp)
}

#' TPA - Barplot & save
#'
#' Create barplots and (if save = TRUE) save them to file system.
#'
#' @param df Data frame reshaped by `ras.TPA_reshape_filter`
#' @param save While `TRUE` writes files to system
#' @param extension File extension/type for exported images and [ggsave()] `device` argument
#' @param directory Path to destination folder
#'
#' @return Nested list of all plots generated
#' @noRd
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_barplot_save <- function(df,
                                 gene_col_RegEx = "[Gg]ene",
                                 sample_names = NULL,
                                 save = FALSE,
                                 extension = "png",
                                 directory = NA) {
  # If no directory supplied, choose one interactively
  if ( (save)&(is.na(directory)) ) { directory <- tcltk::tk_choose.dir() }
  # Initiate empty list to store plots in
  plot_list <- list()
  # If no vector of sample names supplied, grab all available
  if ( is.null(sample_names) ) {
    sample_L <- ras.TPA_sample_names(df = df)
    list2env(samples_L, envir = environment())
    sample_names <- sample_names[1]
  }
  # Grab vector with Gene names
  genes <- df[grep(gene_col_RegEx, names(df))]
  # Collect arguments for ras.TPA_barplot_helper()
  helper_args <- list(gene_col_RegEx = gene_col_RegEx,
                      sample_names = sample_names,
                      save = save,
                      extension = extension,
                      directory = directory)
  # Loop over sample names and use apply to generate plot
  for ( i in seq_len(nrow(sample_names)) ) {
    # Current sample name & columns
    cur_sam = as.character(sample_names[i,])
    helper_args[["cur_sam"]] <- cur_sam
    cur_sam_RegEx <- paste0("_", cur_sam, "_")
    cur_cols <- df[c(grep(gene_col_RegEx, names(df)),
                     grep(cur_sam_RegEx, names(df)))]
    # Apply to make barplot per row
    plot_list[[cur_sam]] <- apply(cur_cols, 1,
                                  FUN = ras.TPA_barplot_helper,
                                  helper_args)
    names(plot_list[[cur_sam]]) <- genes
  }
  return(plot_list)
}

#' TPA - Do all the TPA stuff
#'
#' Clean, restructure, calculate, plot, and export/save plots to file system.
#'
#' @param df Data frame with values
#' @param na.rm If `TRUE`, NA values are dropped before determining calculation
#' @param save If `TRUE`, writes files to system
#' @param file_type File extension/type for exported images
#' @param folder_path Path to destination folder
#'
#'
#' @return List containing (1) Data frame and (2) List of generated plots
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPAer <- function(df,
                      na.rm = FALSE,
                      save = FALSE,
                      file_type = "png",
                      folder_path = tcltk::tk_choose.dir()) {
  # Import the raw data
  # df_raw <- readxl::read_excel("D:/Dokument/Desktop backup/Bekkah/colon_2nd_run/202212_Colon_second_run_TPA.xlsx", sheet = "TPA_clean")


  # Clean column names
  df = janitor::clean_names(df)
  # Relocate everything that's not a sample col to beginning
  df <- dplyr::relocate(df,
                        !tidyselect::any_of(grep("_\\d+[A-Za-z0-9]+_\\d+$", names(df))),
                         tidyselect::everything())
  # Get sample names
  sample_L <- ras.TPA_sample_names(df = df)
  list2env(samples_L, envir = environment())
  # Calc. Avg & StDev & Range
  df <- ras.TPA_avg_StDev_calc(df = df,
                               sample_names = sample_names,
                               na.rm = na.rm)
  # Subset column
  # df <- dplyr::select(df,
  #                     tidyselect::any_of(grep("_\\d+[A-Za-z0-9]+_\\d+$", names(df))),
  #                     grep("_avg|_StDev.Range", names(df)))
  # Vars for plotting and saving files
  genes <- df["gene_names"] %>% # (genes == vector of gene names to plot)
    unique() %>%
    unlist()
  # Plot & save images
  plots_list <- ras.TPA_barplot_save(df = df,
                       extension = file_type,
                       directory = folder_path,
                       save = save)
  returnList <- list(df,
                     plots_list)
  return(returnList)
}
