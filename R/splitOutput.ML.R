# Script for splitting a MassLynx complete summary file into individual files per compound.

#' Load MassLynx complete summary file
#'
#' Asks the user for a source file and loads it with [readr::read_lines()].
#'
#' @family SplitOutput
#'
#' @param sourceFile A file path
#'
#' @returns A list of all lines in source file with "END" appended as the last line
#'
#' @export
#'
#' @examples
#' dataLines = ras.loadFile(sourceFile = system.file("extdata",
#'                                                   "Example_MLOutput.txt",
#'                                                   package = "aquaras",
#'                                                   mustWork = TRUE))
#' head(dataLines)
ras.loadFile = function(sourceFile) {
  dataLines =
    readr::read_lines(sourceFile, skip_empty_rows = T) %>% # Load file
    as.list()                                       # Coerce file into list
  dataLines[[length(dataLines)+1]] = "END"          # Add marker for end of file
  return(dataLines)
}

#' Split loaded file into data frames
#'
#' Split the file loaded with [ras.loadFile()] into dataframes by compound.
#'
#' @family SplitOutput
#'
#' @param dataLines List of vectors from [readr::read_lines()] in [ras.loadFile()]
#'
#' @return List of data frames
#' @export
#'
#' @examples
#' # Setup
#' dataLines = ras.loadFile(sourceFile = system.file("extdata",
#'                                                   "Example_MLOutput.txt",
#'                                                   package = "aquaras",
#'                                                   mustWork = TRUE))
#' ### Ignore the "New names:" output.
#' listDF = ras.splitDataLines(dataLines)
#' listDF
ras.splitDataLines = function(dataLines) {
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
#' @family SplitOutput
#'
#' @param listDF List of data frames to clean
#'
#' @return List of data frames
#' @export
#'
#' @examples
#' # Setup
#' dataLines = ras.loadFile(sourceFile = system.file("extdata",
#'                                                   "Example_MLOutput.txt",
#'                                                   package = "aquaras",
#'                                                   mustWork = TRUE))
#' ### Ignore the "New names:" output.
#' listDF = ras.splitDataLines(dataLines)
#'
#' listDF.clean = ras.cleanDF(listDF)
#' listDF.clean
ras.cleanDF = function(listDF) {
  for(i in 1:length(listDF)) {
    listDF[[i]] = listDF[[i]] %>%
      dplyr::filter(`Sample Text` != "blank") %>% # Drop "blank"
      tidyr::separate(col = Name,
                      into = c("Date",
                               "Signature",
                               "Index",
                               "Internal row"),
                      sep = "_") %>%
      tidyr::separate(col = `Sample Text`,
                      into = c("Compound",
                               "Timepoint",       # Split "Sample text" into columns
                               "Well_Type",
                               "Replicate"),
                      sep = "_")                  # New columns and separator
  }
  return(listDF)
}

#' Write data frames to files
#'
#' Writes data frames to individual .txt files (tsv) in the same directory as source file.
#'
#' @family SplitOutput
#'
#' @param listDF List of data frames to write to files
#' @param sourceFile Source file for locating what directory to write to
#'
#' @export
#'
#' @examples
#'   \dontrun{
#' ras.writeFiles(listDF, sourceFile)
#' }
ras.writeFiles = function(listDF, sourceFile) {
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
#'  based on compound.
#' ## Uninteresting output
#' Spliting the file generates a stream of ```"New names: • `` -> `...1`"```
#'  output in the console that is not particularly interesting but lets you know
#'   it's doing something.
#' ## clean = TRUE
#' These data frames are cleaned by [ras.cleanDF()] (unless clean = FALSE).
#' ## write = TRUE
#' **!!! DEFAULT IS TO WRITE TO FILE SYSTEM !!!** Data frames written to tsv files
#'  in the same directory as the source file. Use write = FALSE to disable this.
#'
#' @family SplitOutput
#'
#' @param sourceFile A file path
#' @param clean Defaults to TRUE for data cleaning
#' @param write Defaults to TRUE for writing to file system
#'
#' @return Writes a tsv file per compound to the same directory as the source file.
#' @return Also returns the list of data frames
#' @export
#'
#' @examples
#'   \dontrun{
#' ras.SplitOutput() # First thing the function does is ask the user for a file.
#' ras.SplitOutput(clean = FALSE) # If the summary output file was not based on the provided template.
#' }
#'
#' # !!! DOES **NOT** WRITE TO FILE SYSTEM !!!
#' listDF = ras.SplitOutput(sourceFile = system.file("extdata",
#'                                                   "Example_MLOutput.txt",
#'                                                   package = "aquaras",
#'                                                   mustWork = TRUE),
#'                          write = FALSE)
#' listDF
#'
ras.SplitOutput = function(sourceFile = file.choose(), clean = TRUE, write = TRUE) {
  # Load file
  dataLines = ras.loadFile(sourceFile = sourceFile)
  # Split dataLines into data frames
  listDF = ras.splitDataLines(dataLines)
  # Clean data frames
  if ( clean == TRUE ) {listDF = ras.cleanDF(listDF)}
  # Write each data frame to a separate .txt file
  if ( write == TRUE ) {ras.writeFiles(listDF, sourceFile)}
  return(listDF)
} # DONE! :)