# Program to input info for 96-well-plates. Generates a runlist for the LC.
  # Select well with radiobuttons for coordinates.
    # Plate number (3-9)
    # Row (A-H)
    # Col (1-12)
  # Input:
    # Date
    # Signature (initials)
    # Compound
    # Timepoint
    # Type (Bead, Medium, Cell, Standard, Blank)
    # Replicate
  # Generate a runlist data frame using create.Runlist() function.
  # Output:
    # Download a tSV file of the runlist to open in excel.
################################################################################
# Library
library(shiny)
library(shinythemes)
library(DT)
library(reactable)
library(dplyr)
library(readr)
# Run app ----------------------------------------------------------------------
RunlistGenerator = function(...) {
  shinyApp(ui.aquaras, server.aquaras)
}
