#' Random group
#'
#' Populate a group with random observations/compounds. Used inside [ras.ran_series()].
#'
#' @param masterList A character vector with observations/compound names
#' @param groupName Empty vector to fill with observations
#' @param seriesList Data frame used for control of how many times an observation occurs
#' @param seriesCheck Character vector that has the cumulative unique observations across the series
#' @param groupSize Integer of how many observations to attempt to place in each group
#'
#' @return List with 1. populated `groupName`, 2. updated `seriesList`, and 3. updated `seriesCheck`
#' @export
#'
ras.ran_group = function(masterList = master_list,
                         groupName = c(),
                         seriesList = series_list,
                         seriesCheck = series_check,
                         groupSize = group_size) {
  # For-loop to fill a group
  for( i in seq_len(groupSize) ) {
    # For breaking the while-loop
    loopControl = 0
    # Get random observation
    ran1 = sample(masterList, size = 1)
    # While-loop for checking observation
    while( ran1 %in% groupName == TRUE |
           ran1 %in% seriesCheck |
           ran1 %in% dplyr::filter(seriesList, Occurence > 3) ) {
      # Get random observation
      ran1 = sample(masterList, size = 1)
      # For breaking the while-loop
      loopControl = loopControl + 1
      if( loopControl >= 100 ) {
        print("Error: Tried 100 times but couldn't select a random observation that met the criteria. Are the groups unevenly sized?")
        ran1 = "N/A"
        break
      }
    }
    # Append observation to group
    groupName = append(groupName, ran1)
    # Append observation to seriesCheck
    seriesCheck = append(seriesCheck, ran1)
    # Increment series control list
    seriesList[seriesList[ , 1] == ran1 , 2] =
      seriesList[seriesList[ , 1] == ran1 , 2] +1
  }
  # Package return info (return variable)
  infoPackage = list(group_name = groupName,
                     series_list = seriesList,
                     series_check = seriesCheck)
  return(infoPackage)
}

#' Random series of groups
#'
#' Creates a series of groups with randomly sampled observations. Observations
#' are distributed such as that there are no repeated entries between groups/within
#' a series. Number of groups in the series is calculated as `length(master_list)`
#' divided by `group_size`, rounded up to nearest integer. If an observation can
#' not be found (for example because the groups can not be filled evenly) an error
#' message is printed to the console and `N/A` inserted instead.
#'
#' @param master_list A character vector with observations/compound names
#' @param group_size Integer of how many observations to attempt to place in each group
#'
#' @return A list of character vectors with each random group (data frame?)
#' @export
#'
#' @examples
#' # Example with 4 even groups
#' # Compound list
#' Compound_list = c(
#' "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
#' "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
#' "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
#' "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
#' "Comp_21", "Comp_22", "Comp_23", "Comp_24")
#'
#' series_A = ras.ran_series(master_list = Compound_list, group_size = 6)
#' series_A
#'
#' # Example with 5 uneven groups
#' # Compound list
#' Compound_list = c(
#' "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
#' "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
#' "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
#' "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
#' "Comp_21", "Comp_22", "Comp_23", "Comp_24", "Comp_25")
#'
#' series_B = ras.ran_series(Compound_list, group_size = 6)
#' series_B
#'
ras.ran_series = function(master_list,
                          group_size) {
  # Check for how many times an observation occurs
  series_control = data.frame(Compound = master_list, Occurence = c(0))
  # Check for unique observations in series
  series_check = c("Uniques in series")
  # List to fill with vectors
  group_list = list()
  # # Series indicator
  # series_name = "A" # Swap to variable if making a function that loops multiple series!
  # Number of groups
  series_size = ceiling(length(master_list) / group_size)
  for( i in seq_len(series_size) ) {
    # Populate a group
    infoPackage = ras.ran_group(masterList = master_list,
                                groupName = c(),
                                seriesList = series_control,
                                seriesCheck = series_check,
                                groupSize = group_size)
    # Unpack and re-integrate updated lists and checks
    group_name = infoPackage[[1]]
    series_control = infoPackage[[2]]
    series_check = infoPackage[[3]]
    # Store group in list (return variable)
    group_list = c(group_list, list(group_name))
  }
  group_df = as.data.frame(group_list)
  groupNames = sprintf("Group_%02d", 1:series_size)
  colnames(group_df) = groupNames
  return(group_df)
}


#' Randomizer
#'
#' Uses [ras.ran_series()] to create a series of groups with randomly sampled observations.
#' Observations are distributed such as that there are no repeated entries between groups/within
#' a series. Number of groups in the series is calculated as `length(master_list)`
#' divided by `group_size`, rounded up to nearest integer. If an observation can
#' not be found (for example because the groups can not be filled evenly) an error
#' message is printed to the console and `N/A` inserted instead.
#' If `write = TRUE` will write the series to a comma delimited .csv file (excel?).
#'
#' @param compound_list A character vector with observations/compound names
#' @param group_size Integer of how many observations to attempt to place in each group
#' @param series Number of series to make
#' @param write Default `FALSE`. If set to `TRUE` will write series to a .xlsx file
#' @param fileName Default `NULL`. Name of file. Automatically appended with `.xlsx`
#'
#' @return List of data frames
#' @export
#'
#' @examples
#' # Example with 4 even groups
#' # Compound list
#' Compound_list = c(
#'   "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
#'   "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
#'   "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
#'   "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
#'   "Comp_21", "Comp_22", "Comp_23", "Comp_24")
#'
#' series_A = ras.Randomizer(compound_list = Compound_list,
#'                           group_size = 6,
#'                           series = 2)
#' print("+++++ Series A +++++")
#' series_A
#'
#' # Example with 5 uneven groups
#' # Compound list
#' Compound_list = c(
#'   "Comp_1", "Comp_2", "Comp_3", "Comp_4", "Comp_5",
#'   "Comp_6", "Comp_7", "Comp_8", "Comp_9", "Comp_10",
#'   "Comp_11", "Comp_12", "Comp_13", "Comp_14", "Comp_15",
#'   "Comp_16", "Comp_17", "Comp_18", "Comp_19", "Comp_20",
#'   "Comp_21", "Comp_22", "Comp_23", "Comp_24")
#'
#' series_B = ras.Randomizer(Compound_list,
#'                           group_size = 5,
#'                           series = 2)
#' print("+++++ Series B +++++")
#' series_B
#'
#'   \dontrun{
#' # Example of writing to .xlsx file
#'  Series_P = ras.Randomizer(Compound_list,
#'                            group_size = 5,
#'                            series = 2,
#'                            write = TRUE,
#'                            fileName = "Series_P Example file of ras.Randomizer")
#'   }
ras.Randomizer = function(compound_list,
                          group_size,
                          series = 1,
                          write = FALSE,
                          fileName = NULL) {
  random_series_list = list()
  for( i in seq_len(series) ) {
    group_df = ras.ran_series(master_list = compound_list,
                              group_size = group_size)
    random_series_list = c(random_series_list, list(group_df))
  }
  seriesNames = sprintf("Series_%02d", 1:series)
  names(random_series_list) = seriesNames
  if( write == TRUE ) {
    fileName = ifelse(is.null(fileName),
                      paste0("Randomized series (aquaras) ", Sys.time(), ".xlsx"),
                      paste0(fileName, ".xlsx"))
    openxlsx::write.xlsx(random_series_list,
                         file = fileName)
  }
  return(random_series_list)
}
