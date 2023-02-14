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
#' @export
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
                                      metaData_RegEx = "protein_names|gene_names|
                                      majority_protein|razor_unique_peptides|
                                      reverse|potential_contaminant|mol_weight",
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
    # Formula: Intensity/(Mx*1000*ΣIntensities)*10^9 : .data[[i]]/(molecular weight*1000*Σ Intensity column)*10^9
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
#' @return Data frame with three columns, (1) unique samples, (2) how many replicates, and (3) the sample group
#' @export
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

#' TPA - Average & StDev/Range
#'
#' Adds columns for average and standard deviation (or range if there are only two sample cols).
#'
#' @param df Data frame to add columns to
#' @param sample_names List of samples from `ras.TPA_sample_names`
#'
#' @return Same data frame with new columns
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_avg_StDev_calc <- function(df, sample_names) {
  ### Calculate for sample names ###
  cols_used <- colnames(df[grep("_\\d+[A-Za-z0-9]+_\\d+$", names(df))])
  # For-loop construct adding averages
  for( i in seq_len(nrow(sample_names)) ) {
    print(paste0("+++ ", i, " +++")) # debug ++++++++++

    # Current sample vars
    cur_sam = sample_names[[i,1]]
    new_Avg_col = paste0(cur_sam, "_avg")
    # Create average column
    df[[new_Avg_col]] <- rowMeans(df[grep(cur_sam, names(df))])
    # If there are only 2 samples (range)
    if( sample_names[[i,2]] == 2 ) {
      # Full col names for current sample
      cur_range_col <- colnames(df[grep(cur_sam, cols_used)])
      cur_range_col <- cur_range_col[-length(cur_range_col)]
      # Calc. range (diff/2)
      range_col <- paste0(cur_sam, "_range")
      df[[range_col]] <- (sqrt((df[[cur_range_col[1]]] - df[[cur_range_col[2]]])^2))/2
    }
    # If there are more than 2 samples (StDev)
    if( sample_names[[i,2]] > 2 ) {
      # Full col names for the current sample
      cur_StDev_col <- colnames(df[grep(cur_sam, cols_used)])
      cur_StDev_col <- cur_StDev_col[-length(cur_StDev_col)]
      # Create temp df (x col required for creation)
      df_StDev <- data.frame(x = 1:nrow(df))
      # Loop to calc. distance to mean squared for each col
      for( j in seq_along(cur_StDev_col)) {
        square_col <- paste0("squared_", cur_StDev_col[j])
        df_StDev[[square_col]] <- (df[grep(cur_StDev_col[j],cols_used)] - df[[new_Avg_col]])^2
      }
      # Remove x col
      df_StDev <- df_StDev[,-1]
      # New col name for StDev
      new_StDev_col = paste0(cur_sam, "_StDev")
      # Calc. StDev
      df[[new_StDev_col]] <- sqrt(rowSums(df_StDev)/length(df_StDev))
    }
  }


  return(df)
}

ras.TPA_avg_StDev <- function(df, sample_L) {
  # Unpack the bundled sample list
  list2env(sample_L, envir = environment())

  r_df <- ras.TPA_avg_StDev_calc(df, sample_names) # Works

  g_df <- ras.TPA_avg_StDev_calc(df, sample_groups) # Doesn't work at row 8, last row
  # Error in rowMeans(df[grep(cur_sam, names(df))]) : 'x' must be numeric



  return()
}

#' TPA - Reshape & filter
#'
#' Pivots the data frame into a longer format creating the columns `sample` and `StDev_and_Range`
#'
#' @param df Data frame from `ras.TPA_avg_StDev`
#' @param gene_regex Regular expression for what genes to filter for
#'
#' @return Data frame with sample values in long format
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_reshape_filter <- function(df, gene_regex = "*") {
  # Subset rows/genes
  df_L <- df[grep(gene_regex, df$gene_names),]
  # Pivot longer
  # Averages
  df_L <- tidyr::pivot_longer(df_L,
                       tidyselect::all_of(grep("_avg|_StDev|_range", names(df_L))),
                       names_to = c("sample", "type"),
                       names_pattern = "(.*)_(.*)",
                       values_to = "value")
  # Split the type & value cols -> avg & StDev
  df_L <- tidyr::pivot_wider(df_L,
                      names_from = type,
                      values_from = value)
  # Add range as compound col & drop old ones
  df_L <- dplyr::mutate(df_L,
                 StDev_and_Range = dplyr::if_else(is.na(StDev), range, StDev),
                 .keep = "unused")
  return(df_L)
}

#' TPA - Barplot & save
#'
#' Create barplots and (if save = TRUE) save them to file system.
#'
#' @param df Data frame reshaped by `ras.TPA_reshape_filter`
#' @param genes Character vector of what genes to plot
#' @param extension File extension/type for exported images
#' @param directory Path to destination folder
#' @param save While `TRUE` (default) writes files to system
#'
#' @return List of all plots generated
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_barplot_save <- function(df,
                                 genes,
                                 extension = "png",
                                 directory = tcltk::tk_choose.dir(),
                                 save = TRUE) {
  plot_list <- list()
  # Plotting & saving
  for( i in seq_along(genes)) {

    # Barplot ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    p1 <- ggplot2::ggplot(dplyr::filter(df, gene_names == genes[[i]])) +
      ggplot2::aes(x = sample,
          y = avg,
          fill = sample) +
      ggplot2::geom_col() +
      ggplot2::geom_errorbar(aes(ymax = avg + StDev_and_Range,
                        ymin = avg - StDev_and_Range)) +
      ggplot2::guides(fill = "none",
             x = ggplot2::guide_axis(angle = 90)) +
      ggplot2::labs(x = "Sample",
           y = "Concentration [fmol/\\u00b5g]",
           title = genes[[i]]) +
      ggplot2::theme_minimal()
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Save plot to folder
    if( save == TRUE ) {
      file_name <- paste0(genes[[i]], ".", extension)
      ggplot2::ggsave(filename = file_name,
             plot = p1,
             device = extension,
             path = directory,
             bg = "white")
    }

    # Store plots in list for return
    plot_list[[i]] <- p1
  }
  return(plot_list)
}

#' TPA - Do all the TPA stuff
#'
#' Clean, restructure, pivot longer, subset, plot, and export/save plots to file system.
#'
#' @param df Data frame with values
#' @param gene_regex Regular expression for which genes to plot
#' @param file_type File extension/type for exported images
#' @param folder_path Path to destination folder
#' @param save While `TRUE` (default) writes files to system
#'
#' @return List containing (1) Data frame and (2) List of generated plots
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPAer <- function(df,
                      gene_regex = "*",
                      file_type = "png",
                      folder_path = tcltk::tk_choose.dir(),
                      save = TRUE) {
  # Import the raw data
  # df_raw <- readxl::read_excel("D:/Dokument/Desktop backup/Bekkah/colon_2nd_run/202212_Colon_second_run_TPA.xlsx", sheet = "TPA_clean")


  # Clean column names
  df = janitor::clean_names(df)
  # Relocate everything that's not a sample col to beginning
  df <- dplyr::relocate(df,
                        !tidyselect::any_of(grep("_\\d+[A-Za-z0-9]+_\\d+$", names(df))),
                        tidyselect::everything())
  # Get sample names
  sample_L <- aquaras::ras.TPA_sample_names(df = df)

  # Calc. Avg & StDev & Range
              ## Add _avg & _StDev for sample_group, maybe split the function ##
  df <- ras.TPA_avg_StDev(df = df,
                          sample_L = samples)

  ## In parts
  r_df <- ras.TPA_avg_StDev_replicate(df, sample_names)

  g_df <- ras.TPA_avg_StDev_group(df, sample_groups)


  # Subset column
  df <- dplyr::select(df,
                      tidyselect::any_of(grep("_\\d+[A-Za-z0-9]+_\\d+$", names(df))),
                      grep("_avg|_StDev|_range", names(df)))
  # Reshape
  df <- ras.TPA_reshape_filter(df = df,
                               gene_regex = gene_regex)
  # Vars for plotting and saving files
  genes <- df["gene_names"] %>% # (genes == vector of gene names to plot)
    unique() %>%
    unlist()
  # Plot & save images
  plots_list <- ras.TPA_barplot_save(df = df,
                       genes = genes,
                       extension = file_type,
                       directory = folder_path,
                       save = save)
  returnList <- list(df,
                     plots_list)
  return(returnList)
}
