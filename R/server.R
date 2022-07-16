# Server for Runlist app.
# Load data --------------------------------------------------------------------
Runlist.df.displayCol = c("LC_Position", "Compound", "Sample_name", "Sample_text")
sample.type.choices = c("Bead" = "bead", # Well Type options
                        "Medium" = "medium",
                        "Cell" = "cell",
                        "Standard" = "STD",
                        "Blank" = "blank")
# Set default table options for reactable (ref under Global theme in https://glin.github.io/reactable/articles/examples.html#theming)
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
)) # End reactableTheme
# Server -----------------------------------------------------------------------
server = function(input, output, session) {
  # source("Dependencies/create.Runlist.R", local = T) # Source functions for generating the final Runlist.
  df.col_types = "ddcdcccccccccc"
  Runlist.default = read_delim("Dependencies/Runlist_default.txt",
                               col_types = df.col_types)
  Runlist.final.empty = tibble(Index = double(),
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

  Runlist.full = reactiveValues(df = Runlist.default)
  observe({
    temp = read_delim(input$up.file$datapath,
                      col_types = df.col_types)
    Runlist.full$df = temp
  }) %>%
    bindEvent(., input$up.file)


  Runlist.final = reactiveValues(df = Runlist.final.empty)
  observe({
    generatedRunlist =
      create.Runlist(Runlist.full$df, input$blank.start, input$blank.end,
                     input$blank.comp, input$blank.type, input$blank.max)
    Runlist.final$df = generatedRunlist
  }) %>%
    bindEvent(., input$create.runlist)

  # Well info selection & input  -----------------------------------------------

  # Current well
  well.current = reactive({
    paste0(input$well.plate,":",input$well.row,",",input$well.col)
  })
  output$well.current.display = renderText({
    paste("Current well:", well.current() )
  }) # End (Current well)
  # Input/output for well compound
  output$ui.compound = renderUI({
    textInput("well.compound", "Compound",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Compound"] )
  }) # End (Input/output for well compound)
  # Input/output for well Timepoint
  output$ui.timepoint = renderUI({
    textInput("well.timepoint", "Timepoint",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Timepoint"] )
  }) # End (Input/output for well Timepoint)
  # Input/output for well Type
  output$ui.type = renderUI({
    selectInput("well.type", "Type", sample.type.choices,
                selected = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Well_Type"] )
  }) # End (Input/output for well Type)
  # Sample replicate
  output$ui.rep = renderUI({
    textInput("sample.rep", "Replicate",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Replicate"])
  }) # End Sample replicate
  # Input/output for sample date
  # observe({
  #   date.default = ifelse(is.null(Runlist$df[Runlist$df["LC_Position"] == well.current(), "Date"]),
  #                         NULL, Runlist$df[Runlist$df["LC_Position"] == well.current(), "Date"])
  # }) %>%
  #   bindEvent(., well.current())

  output$ui.date = renderUI({
    textInput("sample.date", "Date",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Date"])
  }) # End (Input/output for sample date)
  # Input/output for sample signature
  output$ui.sign = renderUI({
    textInput("sample.sign", "Signature (Initials)",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Signature"] )
  }) # End (Input/output for sample signature)

  # End Well info selection & input





  # Update buttons -------------------------------------------------------------

  observe({
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Compound"] = input$well.compound
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Timepoint"] = input$well.timepoint
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Type"] = input$well.type
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Replicate"] = input$sample.rep
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Date"] = input$sample.date
    Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Signature"] = input$sample.sign
  }) %>%
    bindEvent(., input$well.update)


  observe({
    Runlist.full$df["Date"] = input$sample.date
  }) %>%
    bindEvent(., input$default.date)


  observe({
    Runlist.full$df["Signature"] = input$sample.sign
  }) %>%
    bindEvent(., input$default.sign)

  # End (Update buttons)

  # Runlist blank parameters


  # Output Runlist.full --------------------------------------------------------

  proxy.Runlist.full = dataTableProxy("Runlist.full")

  observe({
    replaceData(proxy.Runlist.full, Runlist.full$df)
  })

  output$Runlist.full = renderReactable({
    reactable(

      Runlist.full$df,
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = T)
      ), # End defaultColDef
      showPageSizeOptions = T,
      pageSizeOptions = seq(from = 96, to = 672, by = 96),
      defaultPageSize = 96,
      paginationType = "jump",
      height = 500,
      filterable = T,
      striped = T,
      highlight = T,
      compact = T

    ) # End reactable (Runlist.full)
  }) # End renderReactable (Runlist.full)

  # Output Runlist.final -----------------------------------------------------
  proxy.Runlist.final = dataTableProxy("Runlist.final")

  observe({
    replaceData(proxy.Runlist.final, Runlist.final$df)
  })

  output$Runlist.final = renderReactable({
    reactable(
      Runlist.final$df,
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = T)
      ),
      showPageSizeOptions = T,
      pageSizeOptions = seq(from = 96, to = 672, by = 96),
      defaultPageSize = 96,
      paginationType = "jump",
      height = 500,
      filterable = T,
      striped = T,
      highlight = T,
      compact = T
    )
  })

  # Download handling ##########################################################
  output$down.txt = downloadHandler( # Download raw Runlist tsv
    filename = function() {
      paste0(Sys.Date(), ".Runlist_raw.txt")
    },
    content = function(file) {
      write_tsv(Runlist.full$df, file)
    }
  ) # End (Download raw Runlist tsv)

  output$down.xlsx = downloadHandler( # Download default .xlsx file with formatting
    filename = function() {
      paste0(Sys.Date(), ".Runlist_default.xlsx")
    },
    content = function(file) {
      file.copy("Runlist_default.xlsx", file)
    }
  ) # End (Download default .xlsx file with formatting)

  output$down.Runlist = downloadHandler( # Download final Runlist tsv
    filename = function() {
      paste0(Sys.Date(), ".Runlist_final.txt")
    },
    content = function(file) {
      write_tsv(Runlist.final$df, file)
    }
  )


}
# End Server
