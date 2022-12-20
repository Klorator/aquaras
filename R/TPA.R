#' TPA - Sample names
#'
#' Extracts unique sample names and counts how many repeats it has.
#'
#' @param df Data frame with samples in columns
#'
#' @return Data frame with two columns, (1) unique samples and (2) how many of each
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_sample_names <- function(df) {
  sample_names = df %>%
    colnames() %>%
    stringr::str_extract("_.*_.*$") %>%
    stats::na.omit() %>%
    stringr::str_extract("[:alnum:]{2,}") %>%
    as.data.frame() %>%
    dplyr::group_by_all() %>%
    dplyr::count()
  colnames(sample_names) <- c("sample_name", "count")
  return(sample_names)
}

#' TPA - Average & StDev/Range
#'
#' Adds columns for average and standard deviation (or range if there are only two sample cols).
#'
#' @param df Data frame to add columns to
#' @param sample_names Data frame from `ras.TPA_sample_names`
#'
#' @return Same data frame with new columns
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_avg_StDev <- function(df, sample_names) {
  # For-loop construct adding averages
  for( i in seq_len(nrow(sample_names)) ) {
    # Current sample vars
    cur_sam = sample_names[[i,1]]
    new_Avg_col = paste0(cur_sam, "_avg")
    # Create average column
    df[[new_Avg_col]] <- rowMeans(df[grep(cur_sam, names(df))])
    # If there are only 2 samples (range)
    if( sample_names[[i,2]] == 2 ) {
      # Full col names for current sample
      cur_range_col <- colnames(df[grep(cur_sam, names(df))])
      cur_range_col <- cur_range_col[-length(cur_range_col)]
      # Calc. range (diff/2)
      range_col <- paste0(cur_sam, "_range")
      df[[range_col]] <- (sqrt((df[[cur_range_col[1]]] - df[[cur_range_col[2]]])^2))/2
    }
    # If there are more than 2 samples (StDev)
    if( sample_names[[i,2]] > 2 ) {
      # Full col names for the current sample
      cur_StDev_col <- colnames(df[grep(cur_sam, names(df))])
      cur_StDev_col <- cur_StDev_col[-length(cur_StDev_col)]
      # Create temp df (x col required for creation)
      df_StDev <- data.frame(x = 1:nrow(df))
      # Loop to calc. distance to mean squared for each col
      for( j in seq_along(cur_StDev_col)) {
        square_col <- paste0("squared_", cur_StDev_col[j])
        df_StDev[[square_col]] <- (df[grep(cur_StDev_col[j],names(df))] - df[[new_Avg_col]])^2
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
                       tidyr::all_of(grep("_avg|_StDev|_range", names(df_L))),
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
#' @return `NULL`
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPA_barplot_save <- function(df,
                                 genes,
                                 extension = "png",
                                 directory = tcltk::tk_choose.dir(),
                                 save = TRUE) {
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
  }
  return(NULL)
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
#' @return Data frame
#' @export
#'
#' @examples
#' ### Very specific function; no example yet
ras.TPAer <- function(df,
                      gene_regex = "*",
                      file_type = "png",
                      folder_path = tcltk::tk_choose.dir(),
                      save = TRUE) {
  # Clean column names
  df = janitor::clean_names(df)
  df <- dplyr::relocate(df, 1:4, 71:74, dplyr::everything())
  # Pull list of all samples -Avg & -StDev
  sample_cols <- names(df[-1:-8])
  # Get sample names
  samples <- ras.TPA_sample_names(df = df)
  # Calc. Avg & StDev & Range
  df <- ras.TPA_avg_StDev(df = df, sample_names = samples)
  # Subset column
  df <- dplyr::select(df, 1:8, grep("_avg|_StDev|_range", names(df)))
  # Reshape
  df <- ras.TPA_reshape_filter(df = df,
                               gene_regex = gene_regex)
  # Vars for plotting and saving files
  genes <- df["gene_names"] %>% # (genes == vector of gene names to plot)
    unique() %>%
    unlist()
  # Plot & save images
  ras.TPA_barplot_save(df = df,
                       genes = genes,
                       extension = file_type,
                       directory = folder_path,
                       save = save)
  return(df)
}
