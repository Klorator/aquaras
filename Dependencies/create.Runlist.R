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
# Library
library(dplyr)
# Functions ####################################################################
# Insert blank -----------------------------------------------------------------
add.blank = function(Runlist, df.blank, blank.insert) {
  # Add a number of blank rows to the Runlist data frame. Counts how many times 
  # a blank well has been used. Uses dplyr::filter and dplyr::add_row.
  # Arguments:
    # Runlist = A data frame to add blanks to.
    # df.blank = A data frame consisting of only blanks to take from.
    # blank.insert = An integer of how many blanks to add (default = 3).
  for (i in 1:blank.insert) {
    current.plate = ifelse(length(Runlist$LC_Position) == 0,
                           3, Runlist$Plate[length(Runlist$LC_Position)])
    new.blank = filter(df.blank, Plate >= current.plate & Draw_Count < Draw_Max)[1,]
    Runlist = Runlist %>% 
      add_row(Index = new.blank$Index,
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
add.type = function(Runlist, df.analyte, compound, wellType) {
  # Add a segment of analyte rows based on compound & well type.
  # Uses dplyr::filter and dplyr::add_row.
  # Arguments:
    # Runlist = A data frame to add blanks to.
    # df.analyte = A data frame with analytes to take from.
    # compound = A string with what compound to filter by.
    # wellType = A string with What well type to filter by.
  new.segment = filter(df.analyte, Compound == compound & Well_Type == wellType)
  Runlist = Runlist %>% 
    add_row(Index = new.segment$Index,
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
add.compound = function(Runlist, df.analyte, df.blank, 
                        compound, wellType, blank.type = 1) {
  # Add all type segments for a given compound. 
  # Uses functions add.type() & add.blank().
  # Arguments:
    # Runlist = A data frame to add blanks to.
    # df.analyte = A data frame with analytes to take from.
    # df.blank = A data frame consisting of only blanks to take from.
    # compound = A string with what compound to filter by.
    # wellType = A vector with all well types to filter by. (passed to add.type())
    # blank.type = An integer of how many blanks to add between types.
      # (default = 1; passed to add.blank())
  for (i in 1:length(wellType)) {
    Runlist = add.type(Runlist, df.analyte, compound, wellType[i])
    if ( i == length(wellType) ) { break }
    Runlist = add.blank(Runlist, df.blank, blank.insert = blank.type)
  }
  return(Runlist)
}
# Generate Runlist -------------------------------------------------------------
create.Runlist = function(full.list, blank.start = 3, blank.end = 5, 
                          blank.comp = 2, blank.type = 1, blank.max = 5) {
  # Full function for generating a runlist. Splits the data frame into analytes
  #   and blanks, which are passed on to add.blank() and add.compound().
  # Arguments:
    # full.list = A full list, according to the package template, to be 
      # transformed into a runlist.
    # blank.start = Integer of blanks to start with.
    # blank.end = Integer of blanks to end with.
    # blank.comp = Integer of blanks to insert between compounds.
    # blank.type = Integer of blanks to insert between well types.
    # blank.max = Max number of times to draw from the same blank well.
  df.analyte = filter(full.list, LC_Well_Type == "Analyte") # All sample rows
  df.blank = filter(full.list, LC_Well_Type == "blank") %>% # All blank rows
    mutate(., Draw_Max = blank.max, Draw_Count = 0)
  analyte.compound = unique(df.analyte$Compound) %>% # List of compounds
    sort()
  analyte.wellType = c("bead", "medium", "cell", "STD") # List of well types
  # Empty Runlist (col types: "ddcdcccccccccc#dd")
  Runlist = tibble(Index = double(),
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
  Runlist = add.blank(Runlist, df.blank, blank.start)
  for (i in 1:length(analyte.compound)) {
    Runlist = add.compound(Runlist, df.analyte, df.blank, 
                           analyte.compound[i], analyte.wellType, blank.type)
    if ( i == length(analyte.compound) ) { break }
    Runlist = add.blank(Runlist, df.blank, blank.comp)
  }
  Runlist = add.blank(Runlist, df.blank, blank.end)
  return(Runlist)
}
# END ##########################################################################