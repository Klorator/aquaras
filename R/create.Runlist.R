# Functions for creating the final Runlist.
  # Input:
    # Full Runlist data frame.
    # How many blanks to start with.
    # How many blanks to end with.
    # How many blanks between compounds.
    # How many blanks between well-types.
    # How many max draws from the same blank.
  # Output:
    # Final Runlist data frame.
  # Sorting order:
    # Compounds in alphabetical order.
    # Well-type: Bead -> Medium -> Cell -> STD
################################################################################
# Functions ####################################################################
# Insert blank -----------------------------------------------------------------
#' Insert blank rows
#'
#' Function used within [ras.create.Runlist()] to add blanks.
#'
#' @family RunlistGenerator
#' @description Function used within [ras.create.Runlist()] to add blanks.
#'
#' @param Runlist A data frame to add blanks to.
#' @param df.blank A data frame consisting of only blanks to take from.
#' @param blank.insert An integer of how many blanks to add.
#'
#' @return Returns the same data frame that was supplied to the Runlist argument,
#' but with the specified number of appended blanks.
#' @export
#'
#' @examples
#' # Setup
#' full.list = ras.Example_Runlist
#' blank.max = 5
#' df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
#'   dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
#' Runlist = tibble::tibble(
#'   Index        = double(),
#'   Plate        = double(),
#'   Row          = character(),
#'   Col          = double(),
#'   LC_Position  = character(),
#'   Date         = character(),
#'   Signature    = character(),
#'   Sample_name  = character(),
#'   Compound     = character(),
#'   Timepoint    = character(),
#'   Well_Type    = character(),
#'   LC_Well_Type = character(),
#'   Replicate    = character(),
#'   Sample_text  = character(),
#'   Draw_Max     = double(),
#'   Draw_Count   = double())
#'
#' # Run function
#' Runlist1 = ras.add.blank(Runlist, df.blank, 3) # Adds 3 blanks to Runlist.
#' Runlist1
ras.add.blank = function(Runlist, df.blank, blank.insert) {

  for (i in 1:blank.insert) {
    current.plate = ifelse(length(Runlist$LC_Position) == 0,
                           3, Runlist$Plate[length(Runlist$LC_Position)])
    new.blank = dplyr::filter(df.blank, df.blank$Plate >= current.plate & df.blank$Draw_Count < df.blank$Draw_Max)[1,]
    Runlist = Runlist %>%
      tibble::add_row(
        Index = new.blank$Index,
        Plate = new.blank$Plate,
        Row = new.blank$Row,
        Col = new.blank$Col,
        LC_Position = new.blank$LC_Position,
        Date = new.blank$Date,
        Signature = new.blank$Signature,
        Sample_name = new.blank$Sample_name,
        Compound = new.blank$Compound,
        Timepoint = new.blank$Timepoint,
        Well_Type = new.blank$Well_Type,
        LC_Well_Type = new.blank$LC_Well_Type,
        Replicate = new.blank$Replicate,
        Sample_text = new.blank$Sample_text,
        Draw_Max = new.blank$Draw_Max,
        Draw_Count = new.blank$Draw_Count+1)
    df.blank[df.blank$LC_Position == new.blank$LC_Position, "Draw_Count"] =
      sum(df.blank$Draw_Count[df.blank$LC_Position == new.blank$LC_Position], 1)
  }
  return(Runlist)
}
# Insert analyte: well type segment --------------------------------------------
#' Insert analyte rows for a well type
#'
#' Function used within [ras.create.Runlist()] to add all analytes for a
#' specified compound and well type.
#'
#' @family RunlistGenerator
#'
#' @param Runlist A data frame to add blanks to.
#' @param df.analyte A data frame with analytes to take from.
#' @param compound A string with what compound to filter by.
#' @param wellType A string with What well type to filter by.
#'
#' @return Returns the same data frame that was supplied to the Runlist argument,
#' but with appended analytes.
#' @export
#'
#' @examples
#' # Setup
#' full.list = ras.Example_Runlist
#' blank.max = 5
#' df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
#' Runlist = tibble::tibble(
#'   Index        = double(),
#'   Plate        = double(),
#'   Row          = character(),
#'   Col          = double(),
#'   LC_Position  = character(),
#'   Date         = character(),
#'   Signature    = character(),
#'   Sample_name  = character(),
#'   Compound     = character(),
#'   Timepoint    = character(),
#'   Well_Type    = character(),
#'   LC_Well_Type = character(),
#'   Replicate    = character(),
#'   Sample_text  = character(),
#'   Draw_Max     = double(),
#'   Draw_Count   = double())
#'
#' # Run function
#' Runlist1 <- ras.add.type(Runlist, df.analyte, "Paracetamol", "cell")
#' Runlist1
ras.add.type = function(Runlist, df.analyte, compound, wellType) {
  new.segment = dplyr::filter(df.analyte, df.analyte$Compound == compound & df.analyte$Well_Type == wellType)
  Runlist = Runlist %>%
    tibble::add_row(
      Index = new.segment$Index,
      Plate = new.segment$Plate,
      Row = new.segment$Row,
      Col = new.segment$Col,
      LC_Position = new.segment$LC_Position,
      Date = new.segment$Date,
      Signature = new.segment$Signature,
      Sample_name = new.segment$Sample_name,
      Compound = new.segment$Compound,
      Timepoint = new.segment$Timepoint,
      Well_Type = new.segment$Well_Type,
      LC_Well_Type = new.segment$LC_Well_Type,
      Replicate = new.segment$Replicate,
      Sample_text = new.segment$Sample_text,
      Draw_Max = 1,
      Draw_Count = 1)
  return(Runlist)
}
# Insert analyte: Compound segment ---------------------------------------------
#' Insert all analyte and blank rows for a compound
#'
#' Function used within [ras.create.Runlist()] to add all analyte and
#' blank rows for a compound. Passes arguments to [ras.add.type()] and [ras.add.blank()].
#'
#' @family RunlistGenerator
#'
#' @param Runlist A data frame to add blanks to.
#' @param df.analyte A data frame with analytes to take from.
#' @param df.blank A data frame consisting of only blanks to take from.
#' @param compound A string with what compound to filter by.
#' @param wellType A vector with all well types to filter by. (string passed to
#' [ras.add.type()])
#' @param blank.type An integer of how many blanks to add between types.
#' (default = 1; passed to [ras.add.blank()])
#'
#' @return Returns the same data frame that was supplied to the Runlist argument,
#' but appended with all analyte and blank rows for a compound.
#' @export
#'
#' @examples
#' # Setup
#' full.list = ras.Example_Runlist
#' blank.max = 5
#' df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
#'   dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
#' df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
#' Runlist = tibble::tibble(
#'   Index        = double(),
#'   Plate        = double(),
#'   Row          = character(),
#'   Col          = double(),
#'   LC_Position  = character(),
#'   Date         = character(),
#'   Signature    = character(),
#'   Sample_name  = character(),
#'   Compound     = character(),
#'   Timepoint    = character(),
#'   Well_Type    = character(),
#'   LC_Well_Type = character(),
#'   Replicate    = character(),
#'   Sample_text  = character(),
#'   Draw_Max     = double(),
#'   Draw_Count   = double())
#'
#'
#' # Run function
#'   # Adds 1 blank between every type.
#' Runlist1 <- ras.add.compound(Runlist, df.analyte, df.blank, "Paracetamol",
#'   c("bead", "medium", "cell", "STD", "blank"))
#' Runlist1
#'
#'   # Adds 3 blanks between every type.
#' Runlist2 <- ras.add.compound(Runlist, df.analyte, df.blank, "Ibuprofen",
#'   c("bead", "medium", "cell", "STD", "blank"), 3)
#' Runlist2
ras.add.compound = function(Runlist, df.analyte, df.blank,
                            compound, wellType, blank.type = 1) {
  for (i in 1:length(wellType)) {
    Runlist = ras.add.type(Runlist, df.analyte, compound, wellType[i])
    if ( i == length(wellType) ) { break }
    Runlist = ras.add.blank(Runlist, df.blank, blank.insert = blank.type)
  }
  return(Runlist)
}
# Generate Runlist -------------------------------------------------------------
#' Create a Runlist from data frame
#'
#' Creates a Runlist from the standardized data frame supplied by/made
#' in the Shiny app [ras.RunlistGenerator()].
#'
#' @family RunlistGenerator
#'
#' @param full.list A full list, according to the package template, to be transformed
#' into a runlist.
#' @param blank.start Integer of blanks to start with.
#' @param blank.end Integer of blanks to end with.
#' @param blank.comp Integer of blanks to insert between compounds.
#' @param blank.type Integer of blanks to insert between well types.
#' @param blank.max Max number of times to draw from the same blank well.
#'
#' @return Returns a Runlist with blanks spaced as defined (with sensible defaults).
#' @export
#'
#' @examples
#' # Setup
#' full.list = ras.Example_Runlist
#' head(full.list)
#'
#' # Run function
#' Runlist1 <- ras.create.Runlist(full.list)
#' head(Runlist1)
#'
#' Runlist2 <- ras.create.Runlist(full.list,
#'                                blank.start = 4,
#'                                blank.end   = 3,
#'                                blank.comp  = 2,
#'                                blank.type  = 1,
#'                                blank.max   = 6)
#' head(Runlist2)
ras.create.Runlist = function(full.list, blank.start = 3, blank.end = 5,
                              blank.comp = 2, blank.type = 1, blank.max = 5) {
  df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
  df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
    dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
  analyte.compound = unique(df.analyte$Compound) %>% # List of compounds
    sort()
  analyte.wellType = c("bead", "medium", "cell", "STD") # List of well types
  Runlist = tibble::tibble(
    Index = double(),
    Plate = double(),
    Row = character(),
    Col = double(),
    LC_Position = character(),
    Date = character(),
    Signature = character(),
    Sample_name = character(),
    Compound = character(),
    Timepoint = character(),
    Well_Type = character(),
    LC_Well_Type = character(),
    Replicate = character(),
    Sample_text = character(),
    Draw_Max = double(),
    Draw_Count = double())
  Runlist = ras.add.blank(Runlist, df.blank, blank.start)
  for (i in 1:length(analyte.compound)) {
    Runlist = ras.add.compound(Runlist, df.analyte, df.blank,
                           analyte.compound[i], analyte.wellType, blank.type)
    if ( i == length(analyte.compound) ) { break }
    Runlist = ras.add.blank(Runlist, df.blank, blank.comp)
  }
  Runlist = ras.add.blank(Runlist, df.blank, blank.end)
  return(Runlist)
}
# END ##########################################################################
