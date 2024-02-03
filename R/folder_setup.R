#' Function to set the output directory
#'
#' @param output_dir A directory path
#'
#' @return A directory path
#' @noRd
ras.set_output_directory <- function(output_dir) {
  if (is.null(output_dir)) {
    output_dir <- tcltk::tk_choose.dir(caption = "Select output directory")
  } else {
    output_dir <- output_dir
  }
  return(output_dir)
}

#' Function to create subfolders
#'
#' @param output_dir A directory path
#'
#' @return Nothing, creates folders
#' @noRd
ras.create_subfolders <- function(output_dir) {
  subfolders <- list(
    files      = "files",
    diff_limma = "plots/DiffExp/limma",
    diff_DEqMS = "plots/DiffExp/DEqMS",
    barplots   = "plots/Barplots",
    foldChange = "plots/Fold_Change",
    heatmaps   = "plots/Heatmaps",
    qc_VSN     = "plots/Quality_Control/VSN",
    qc_PCA     = "plots/Quality_Control/PCA",
    qc_UMAP    = "plots/Quality_Control/UMAP",
    qc_tSNE    = "plots/Quality_Control/t-SNE",
    qc_DEqMS   = "plots/Quality_Control/DEqMS"
  )
  for (i in seq_along(subfolders)) {
    subfolders[[i]] <- file.path(output_dir, subfolders[[i]])
  }
  folder_exists_warning_flag <- FALSE

  for (subfolder in subfolders) {
    dir_path <- file.path(subfolder)
    if (dir.exists(dir_path)) {
      folder_exists_warning_flag <- TRUE
    } else {
      dir.create(dir_path, recursive = TRUE)
    }
  }

  if (folder_exists_warning_flag) {
    warning("Some output subfolders already exist.
            Please Make Sure you are not overwriting existing data")
  }
  return(subfolders)
}


#' Setup directory & subfolders
#'
#' @param output_dir A directory path
#'
#' @return A directory path
#' @export
#'
#' @examples
#'  \dontrun{
#'  # No example
#'  }
ras.setup_dir_and_subfolders <- function(output_dir = NULL) {
  output_dir <- ras.set_output_directory(output_dir)
  subfolders <- ras.create_subfolders(output_dir)
  return(subfolders)
}
