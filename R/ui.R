# UI for RunlistGenerator app.
# UI ---------------------------------------------------------------------------
ui.aquaras = fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Data processing for LC/MS"),

  tabsetPanel(

    tabPanel("Well input", # Tab: Well input -------------------------------------
             fluidRow( # top row: [Plate slider], [current well], [Update button]
               br(),
               column(width = 5,
                      radioButtons("well.plate", "Well plate", seq(from=3, to=9, by=1), inline = T),
                      radioButtons("well.row", "Row",
                                   LETTERS[seq(from = 1, to = 8, by = 1)], inline = T)
               ),
               column(width = 3,
                      textOutput("well.current.display"),
                      actionButton("well.update", "Update well",
                                   icon = icon("save"), class = "btn-success")
               ),
               column(width = 4 # top row far right column
               ) # End column (top row far right column)
             ), # End fluidRow (top row)
             fluidRow(
               column(width = 8,
                      radioButtons("well.col", "Column", seq(1:12), inline = T)
               )
             ),
             fluidRow( # Well info section
               column(width = 4,
                      # textInput("well.compound", "Compound"),
                      uiOutput("ui.compound"),
                      uiOutput("ui.timepoint"),
                      uiOutput("ui.type")
               ), # End column (Output: well position info)
               column(width = 4, # Input: sample data
                      uiOutput("ui.rep")
               ), # End column (Input: sample data)
               column(width = 4, # Input: meta data
                      uiOutput("ui.date"),
                      actionButton("default.date", "Set date for all wells"),

                      uiOutput("ui.sign"),
                      actionButton("default.sign", "Set as default signature")
               ) # End column (Input: meta data)
             ) # End fluidRow (Output: well info)
    ), # End tabPanel "Well input"

    tabPanel("Full list", # Tab: Full list ---------------------------------------
             fluidRow( # Upload/Download
               column(width = 3,
                      br(),
                      # actionButton("upload.file", "Upload .txt or Excel file")
                      fileInput("up.file", "Upload csv or tsv file")
               ),
               column(width = 3,
                      br(),
                      br(),
                      # actionButton("download.txt", "Download list tsv")
                      downloadButton("down.txt", "Download list tsv")
               ),
               column(width = 6,
                      br(),
                      br(),
                      # actionButton("download.xlsx", "Download default .xlsx")
                      downloadButton("down.xlsx", "Download default .xlsx with formatting")
               )
             ), # End fluidRow (Upload/Download)
             fluidRow( # Full runlist data frame
               reactableOutput("Runlist.full")
               # DTOutput("Runlist.full")
             ) # End fluidRow (Full runlist data frame)
    ), # End tabPanel "Full list"

    tabPanel("Runlist", # Tab: Runlist -------------------------------------------
             fluidRow(
               column(width = 3,
                      br(),
                      numericInput("blank.start", "Blanks at start", value = 3),
                      numericInput("blank.end", "Blanks at end", value = 5)
               ),
               column(width = 3,
                      br(),
                      numericInput("blank.comp", "Blanks between compounds", value = 2),
                      numericInput("blank.type", "Blanks between well types", value = 1)
               ),
               column(width = 3,
                      br(),
                      numericInput("blank.max", "Max draw from same blank", value = 5)
               ),
               column(width = 3,
                      br(),
                      br(),
                      actionButton("create.runlist", "Generate Runlist", class = "btn-success"),
                      br(),
                      br(),
                      downloadButton("down.Runlist", "Download Runlist")
               )
             ), # End fluidRow (top row)
             fluidRow( # Display generated Runlist
               reactableOutput("Runlist.final")
             ) # End fluidRow (Display generated Runlist)
    ) # End tabPanel "Runlist"

  ) # End tabsetPanel

) # End fluidPage
# End UI
