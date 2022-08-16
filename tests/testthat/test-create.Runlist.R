test_that("ras.add.blank() snapshot looks as intended", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    blank.max = 5
    df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
      dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
    Runlist = tibble::tibble(
      Index        = double(),
      Plate        = double(),
      Row          = character(),
      Col          = double(),
      LC_Position  = character(),
      Date         = character(),
      Signature    = character(),
      Sample_name  = character(),
      Compound     = character(),
      Timepoint    = character(),
      Well_Type    = character(),
      LC_Well_Type = character(),
      Replicate    = character(),
      Sample_text  = character(),
      Draw_Max     = double(),
      Draw_Count   = double())
    # Run function
    ras.add.blank(Runlist, df.blank, 3) # Adds 3 blanks to Runlist.
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.add.type() snapshot", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    blank.max = 5
    df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
    Runlist = tibble::tibble(
      Index        = double(),
      Plate        = double(),
      Row          = character(),
      Col          = double(),
      LC_Position  = character(),
      Date         = character(),
      Signature    = character(),
      Sample_name  = character(),
      Compound     = character(),
      Timepoint    = character(),
      Well_Type    = character(),
      LC_Well_Type = character(),
      Replicate    = character(),
      Sample_text  = character(),
      Draw_Max     = double(),
      Draw_Count   = double())
    # Run function
    ras.add.type(Runlist, df.analyte, "Paracetamol", "cell")
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.add.compound() snapshot (1 blank, default)", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    blank.max = 5
    df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
      dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
    df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
    Runlist = tibble::tibble(
      Index        = double(),
      Plate        = double(),
      Row          = character(),
      Col          = double(),
      LC_Position  = character(),
      Date         = character(),
      Signature    = character(),
      Sample_name  = character(),
      Compound     = character(),
      Timepoint    = character(),
      Well_Type    = character(),
      LC_Well_Type = character(),
      Replicate    = character(),
      Sample_text  = character(),
      Draw_Max     = double(),
      Draw_Count   = double())
    # Run function
      # Adds 1 blank between every type.
    ras.add.compound(Runlist, df.analyte, df.blank, "Paracetamol",
      c("bead", "medium", "cell", "STD", "blank"))
  })
})

test_that("ras.add.compound() snapshot (3 blanks)", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    blank.max = 5
    df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>% # All blank rows
      dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
    df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte") # All sample rows
    Runlist = tibble::tibble(
      Index        = double(),
      Plate        = double(),
      Row          = character(),
      Col          = double(),
      LC_Position  = character(),
      Date         = character(),
      Signature    = character(),
      Sample_name  = character(),
      Compound     = character(),
      Timepoint    = character(),
      Well_Type    = character(),
      LC_Well_Type = character(),
      Replicate    = character(),
      Sample_text  = character(),
      Draw_Max     = double(),
      Draw_Count   = double())
    # Run function
      # Adds 3 blanks between every type.
    ras.add.compound(Runlist, df.analyte, df.blank, "Ibuprofen",
      c("bead", "medium", "cell", "STD", "blank"), 3)
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.create.Runlist() snapshot (default)", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    # Run function
    ras.create.Runlist(full.list)
  })
})

test_that("ras.create.Runlist() snapshot", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    # Run function
    ras.create.Runlist(full.list,
                       blank.start = 4,
                       blank.end   = 2,
                       blank.comp  = 3,
                       blank.type  = 2,
                       blank.max   = 3)
  })
})
