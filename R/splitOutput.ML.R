# Script for splitting a MassLynx complete summary file into individual files per compound.

#' Load MassLynx complete summary file
#'
#' Asks the user for a source file and loads it with readr::read_lines().
#'
#' @returns A list of two variables:
#' @returns  values[1] is sourceFile; the source file chosen by the user
#' @returns  values[2] is dataLines: a list of all lines in source file with "END" appended as the last line
#' @export
#'
#' @examples load.list = loadFile.ML()
#' @examples   sourceFile = load.list[1]
#' @examples   dataLines = load.list[2]
loadFile.ML = function() {
  sourceFile = file.choose()                        # Choose source file
  dataLines =
    readr::read_lines(sourceFile, skip_empty_rows = T) %>% # Load file
    as.list()                                       # Coerce file into list
  dataLines[[length(dataLines)+1]] = "END"          # Add marker for end of file
  values = list(sourceFile, dataLines)              # Pack variables to return into a list
  return(values)
}

#' Split loaded file into data frames
#'
#' Split the file loaded with loadFile.ML() into dataframes by compound.
#'
#' @param dataLines List of vectors from readr::read_lines() in loadFile.ML()
#'
#' @return List of data frames
#' @export
#'
#' @examples listDF = splitDataLines.ML(dataLines)
splitDataLines.ML = function(dataLines) {
  # Create variables -----------------------------------------------------------
  fileSep = "\t"        # Delimiter in file
  tempDF = ""           # Stores temp df
  tempDF_name = ""      # Stores df name
  tempDF_header = ""    # Stores header
  tempDF_rows = list()  # Stores lines for temp df
  listDF = list()       # List to store compiled df:s in
  checkNewDF = FALSE    # Create Boolean check for 1st df
  # Split file by data frame ---------------------------------------------------
  for(i in 1:length(dataLines)) {                            # Iterate over data lines
    if( stringr::str_detect(dataLines[[i]], "^Compound") == TRUE ) {  # Check if line is Compound name
      if( checkNewDF == TRUE ) {                             # Check if df needs to be compiled
        tempDF = tempDF_rows %>%                             # Compile df
          purrr::map_dfr(function(x) {
            x = unlist(x)
            x = purrr::set_names(x, tempDF_header[[1]])
            x
          } ) %>%
          tibble::as_tibble()
        listDF[[tempDF_name]] = tempDF           # Save df in list
      }
      checkNewDF = TRUE                            # New df started
      tempDF_name = gsub(": ", "", dataLines[[i]]) # Save compound name & remove ": "
      tempDF_header = dataLines[[i+1]]             # Save header
      tempDF_rows = list()                         # Empty tempDF_rows
      next                                         # Skip to next iteration/line
    }
    if( dataLines[[i]] == tempDF_header ) {             # Check if line is header
      tempDF_header = stringr::str_split(tempDF_header, fileSep) # Split header into columns
      next                                              # Skip to next iteration/line
    }
    if( stringr::str_detect(dataLines[[i]], "^END") == TRUE ) {  # Check if line is end of file
      tempDF = tempDF_rows %>%                          # Compile df
        purrr::map_dfr(function(x) {
          x = unlist(x)
          x = purrr::set_names(x, tempDF_header[[1]])
          x
        } ) %>%
        tibble::as_tibble()
      listDF[[tempDF_name]] = tempDF                    # Save df in list
      tempDF_rows = list()                              # Empty tempDF_rows
      break                                             # End for loop
    }
    new_row = stringr::str_split(dataLines[[i]], fileSep)        # Split line by delimiter
    tempDF_rows[[length(tempDF_rows)+1]] = new_row      # Add new row to list
  }                                                     # Repeat for loop
  return(listDF) # Return list of data frames
}

#' Clean list of data frames
#'
#' Cleans each data frame by removing blanks and NAs. Also separates Name and
#' Sample Text into its composite columns.
#'
#' @param listDF List of data frames to clean
#'
#' @return List of data frames
#' @export
#'
#' @examples listDF = cleanDF.ML(listDF)
cleanDF.ML = function(listDF) {
  for(i in 1:length(listDF)) {
    listDF[[i]] = listDF[[i]] %>%
      dplyr::filter(`Sample Text` != "blank") %>%                   # Drop "blank"
      tidyr::separate(col = Name, into = c("Date", "Signature",
                                           "Index"), sep = "_") %>%
      tidyr::separate(col = `Sample Text`, into = c("Compound",     # Split "Sample text" into columns
                                                    "Timepoint", "Well_Type", "Replicate"), sep = "_")  # New columns and separator
  }
  return(listDF)
}

#' Write data frames to files
#'
#' Writes data frames to individual .txt files (tsv) in the same directory as source file.
#'
#' @param listDF List of data frames to write to files
#' @param sourceFile Source file for locating what directory to write to
#'
#' @export
#'
#' @examples writeFiles.ML(listDF, sourceFile)
writeFiles.ML = function(listDF, sourceFile) {
  old_directory = getwd()          # Save current working directory
  sourceFile %>%
    dirname() %>%                  # Get directory of source file
    setwd()                        # Set file directory as working directory
  sourceFile_name = sourceFile %>% basename() # Name of source file
  sourceFile_name = gsub("\\..{1, }$", " - ", sourceFile_name) # Replace file extension
  for(i in 1:length(listDF)) {
    newFileName = paste0(sourceFile_name, names(listDF[i]), ".txt") # Concatenate new file name
    listDF[[i]] %>% readr::write_tsv(file = newFileName)    # Write to file
  }
  setwd(old_directory)                                      # Go back to old working directory
}

#' Split MassLynx output file
#'
#' Splits the MassLynx complete summary output file into individual data frames
#' based on compound. These data frames are written to tsv files in the same
#' directory as the source file.
#'
#' @return Writes a tsv file per compound to the same directory as the source file.
#' @export
#'
#' @examples splitOutput.ML() # First thing the function does is ask the user for a file.
splitOutput.ML = function() {
  # Load file
  load.list = loadFile.ML()
  sourceFile = unlist(load.list[1])
  dataLines = unlist(load.list[2])
  # Split dataLines into data frames
  listDF = splitDataLines.ML(dataLines)
  # Clean data frames
  listDF = cleanDF.ML(listDF)
  # Write each data frame to a separate .txt file
  writeFiles.ML(listDF, sourceFile)
} # DONE! :)
