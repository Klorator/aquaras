# Server for RunlistGenerator app.
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
server.aquaras = function(input, output, session) {
  # Load data --------------------------------------------------------------------
  Runlist.df.displayCol = c("LC_Position", "Compound", "Sample_name", "Sample_text") # Columns to display in reactable
  sample.type.choices = c("Bead" = "bead", # Well Type options
                          "Medium" = "medium",
                          "Cell" = "cell",
                          "Standard" = "STD",
                          "Blank" = "blank")
  Runlist.default = Runlist_default # Load default list to start with
  Runlist.final.empty = Runlist_final_empty # Load df with zero rows

  Runlist.full = reactiveValues(df = Runlist.default) # Reactive version of df to make it editable and display changes
  observe({ # Load df uploaded by user
    temp = read_delim(input$up.file$datapath,
                      col_types = df.col_types)
    Runlist.full$df = temp
  }) %>%
    bindEvent(., input$up.file)
  Runlist.final = reactiveValues(df = Runlist.final.empty) # Reactive version of final df to make it editable and display changes
  observe({
    generatedRunlist =
      create.Runlist(Runlist.full$df, input$blank.start, input$blank.end,
                     input$blank.comp, input$blank.type, input$blank.max)
    Runlist.final$df = generatedRunlist
  }) %>%
    bindEvent(., input$create.runlist)
  # Well info selection & input  -----------------------------------------------
  # Current well
  well.current = reactive({ # Create a current well string == LC_Position
    paste0(input$well.plate,":",input$well.row,",",input$well.col)
  })
  output$well.current.display = renderText({ # Display of current well
    paste("Current well:", well.current() )
  }) # End (Current well)
  output$ui.compound = renderUI({ # Input/output for well compound
    textInput("well.compound", "Compound",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Compound"] )
  }) # End (Input/output for well compound)
  output$ui.timepoint = renderUI({ # Input/output for well Timepoint
    textInput("well.timepoint", "Timepoint",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Timepoint"] )
  }) # End (Input/output for well Timepoint)
  output$ui.type = renderUI({ # Input/output for well Type
    selectInput("well.type", "Type", sample.type.choices,
                selected = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Well_Type"] )
  }) # End (Input/output for well Type)
  output$ui.rep = renderUI({ # Sample replicate
    textInput("sample.rep", "Replicate",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Replicate"])
  }) # End Sample replicate
  output$ui.date = renderUI({ # Date string
    textInput("sample.date", "Date",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Date"])
  }) # End (Input/output for sample date)
  output$ui.sign = renderUI({ # Input/output for sample signature
    textInput("sample.sign", "Signature (Initials)",
              value = Runlist.full$df[Runlist.full$df["LC_Position"] == well.current(), "Signature"] )
  }) # End (Input/output for sample signature)
  # End Well info selection & input
  # Update buttons -------------------------------------------------------------
  observe({
    Runlist.full$df = update.well(Runlist.full$df, well.current(),
                                  input$sample.date, input$sample.sign,
                                  input$well.compound, input$well.timepoint,
                                  input$well.type, input$sample.rep)
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
  # Output Runlist.full --------------------------------------------------------
  proxy.Runlist.full = dataTableProxy("Runlist.full") # Proxy df of runlist
  observe({
    replaceData(proxy.Runlist.full, Runlist.full$df) # Update parts of df to avoid reloading the entire thing
  })
  output$Runlist.full = renderReactable({ # Display runlist
    reactable(
      Runlist.full$df,
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = T)), # End defaultColDef
      columns = list(
        Index = colDef(align = "center"),
        Plate = colDef(align = "center"),
        Row = colDef(align = "center"),
        Col = colDef(align = "center"),
        LC_Position = colDef(align = "center")),
      showPageSizeOptions = T,
      pageSizeOptions = seq(from = 96, to = 672, by = 96),
      defaultPageSize = 96,
      paginationType = "jump",
      height = 600,
      wrap = F,
      filterable = T,
      striped = T,
      highlight = T,
      compact = T
    ) # End reactable (Runlist.full)
  }) # End renderReactable (Runlist.full)
  # Output Runlist.final -----------------------------------------------------
  proxy.Runlist.final = dataTableProxy("Runlist.final") # Proxy df of final runlist
  observe({
    replaceData(proxy.Runlist.final, Runlist.final$df) # Update parts of the final df to avoid reloading the entire thing
  })
  output$Runlist.final = renderReactable({ # Display final runlist
    reactable(
      Runlist.final$df,
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = T)),
      columns = list(
        Index = colDef(align = "center"),
        Plate = colDef(align = "center"),
        Row = colDef(align = "center"),
        Col = colDef(align = "center"),
        LC_Position = colDef(align = "center")),
      showPageSizeOptions = T,
      pageSizeOptions = seq(from = 96, to = 672, by = 96),
      defaultPageSize = 96,
      paginationType = "jump",
      height = 600,
      wrap = F,
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
  ) # End (Download final Runlist tsv)
}
# End Server
