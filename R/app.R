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
# Run app ----------------------------------------------------------------------
#' App for making a runlist
#'
#' Program to input info for 96-well-plates. Generates a runlist for the Waters LC/MS software MassLynx.
#'
#' @family RunlistGenerator
#'
#' @return Creates a Shiny GUI.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' ras.RunlistGenerator() # Launches the Shiny GUI.
#' }
ras.RunlistGenerator = function() {
  ras.darkModeDF_Options()
  shiny::shinyApp(ras.ui, ras.server)
}
