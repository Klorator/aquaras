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
library(dplyr)
library(DT)
library(reactable)
library(readr)
library(shiny)
library(shinythemes)
# Run app ----------------------------------------------------------------------
#' App for making a runlist
#'
#' @description Program to input info for 96-well-plates. Generates a runlist for the Waters LC/MS software MassLynx.
#'
#' @param ... Takes no parameters. Opens a shiny app.
#'
#' @return Creates a Shiny GUI.
#' @export
#'
#' @examples RunlistGenerator() # Launches the Shiny GUI.
RunlistGenerator = function(...) {
  shiny::shinyApp(ui.aquaras, server.aquaras)
}
