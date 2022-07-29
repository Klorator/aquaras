# UI for RunlistGenerator app.
# UI ---------------------------------------------------------------------------
aquaras.ui =
  shiny::fluidPage(
  theme = shinythemes::shinytheme("cyborg"),
  shiny::titlePanel("Data processing for LC/MS"),

  shiny::tabsetPanel(

    shiny::tabPanel("Well input", # Tab: Well input -----------------------------------
                    shiny::fluidRow( # top row: [Plate slider], [current well], [Update button]
                      htmltools::br(),
                      shiny::column(width = 5,
                                    shiny::radioButtons("well.plate", "Well plate", seq(from=3, to=9, by=1), inline = T),
                                    shiny::radioButtons("well.row", "Row",
            LETTERS[seq(from = 1, to = 8, by = 1)], inline = T)),
            shiny::column(width = 3,
                          shiny::textOutput("well.current.display"),
                          shiny::actionButton("well.update", "Update well",
            icon = shiny::icon("save"), class = "btn-success")),
            shiny::column(width = 4 # top row far right column
        ) # End column (top row far right column)
      ), # End fluidRow (top row)
      shiny::fluidRow(
        shiny::column(width = 8,
                      shiny::radioButtons("well.col", "Column", seq(1:12), inline = T))
      ),
      shiny::fluidRow( # Well info section
        shiny::column(width = 4,
                      shiny::uiOutput("ui.compound"),
                      shiny::uiOutput("ui.timepoint"),
                      shiny::uiOutput("ui.type")), # End column (Output: well position info)
        shiny::column(width = 4, # Input: sample data
                      shiny::uiOutput("ui.rep")), # End column (Input: sample data)
        shiny::column(width = 4, # Input: meta data
                      shiny::uiOutput("ui.date"),
                      shiny::uiOutput("ui.sign"),
                      shiny::actionButton("default.dateSign", "Set date & signature for all wells")) # End column (Input: meta data)
      ) # End fluidRow (Output: well info)
    ), # End tabPanel "Well input"

    shiny::tabPanel("Full list", # Tab: Full list -------------------------------------
                    shiny::fluidRow( # Upload/Download
                      shiny::column(width = 3,
                                    htmltools::br(),
          shiny::fileInput("up.file", "Upload csv or tsv file")),
          shiny::column(width = 3,
                        htmltools::br(),
                        htmltools::br(),
          shiny::downloadButton("down.txt", "Download list tsv")),
          shiny::column(width = 6,
                        htmltools::br(),
                        htmltools::br(),
          shiny::downloadButton("down.xlsx", "Download default .xlsx with formatting"))
      ), # End fluidRow (Upload/Download)
      shiny::fluidRow( # Full runlist data frame
        reactable::reactableOutput("Runlist.full")
      ) # End fluidRow (Full runlist data frame)
    ), # End tabPanel "Full list"

    shiny::tabPanel("Runlist", # Tab: Runlist -----------------------------------------
                    shiny::fluidRow(
                      shiny::column(width = 3,
                                    htmltools::br(),
          shiny::numericInput("blank.start", "Blanks at start", value = 3),
          shiny::numericInput("blank.end", "Blanks at end", value = 5)),
          shiny::column(width = 3,
                        htmltools::br(),
          shiny::numericInput("blank.comp", "Blanks between compounds", value = 2),
          shiny::numericInput("blank.type", "Blanks between well types", value = 1)),
          shiny::column(width = 3,
                        htmltools::br(),
          shiny::numericInput("blank.max", "Max draw from same blank", value = 5)),
          shiny::column(width = 3,
                        htmltools::br(),
                        htmltools::br(),
          shiny::actionButton("create.runlist", "Generate Runlist",
            class = "btn-success"),
          htmltools::br(),
          htmltools::br(),
          shiny::downloadButton("down.Runlist", "Download Runlist"))
      ), # End fluidRow (top row)
      shiny::fluidRow( # Display generated Runlist
        reactable::reactableOutput("Runlist.final")
      ) # End fluidRow (Display generated Runlist)
    ) # End tabPanel "Runlist"

  ) # End tabsetPanel

  ) # End fluidPage
# End UI
