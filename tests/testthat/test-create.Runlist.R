test_that("add.blank() snapshot looks as intended", {
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
    add.blank(Runlist, df.blank, 3) # Adds 3 blanks to Runlist.
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("add.type() snapshot", {
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
    add.type(Runlist, df.analyte, "Paracetamol", "cell")
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("add.compound() snapshot (1 blank, default)", {
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
    add.compound(Runlist, df.analyte, df.blank, "Paracetamol",
      c("bead", "medium", "cell", "STD", "blank")) # Adds 1 blank between every type.
  })
})

test_that("add.compound() snapshot (3 blanks)", {
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
    add.compound(Runlist, df.analyte, df.blank, "Ibuprofen",
      c("bead", "medium", "cell", "STD", "blank"), 3) # Adds 3 blanks between every type.
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("create.Runlist() snapshot (default)", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    # Run function
    create.Runlist(full.list)
  })
})

test_that("create.Runlist() snapshot", {
  expect_snapshot({
    # Setup
    full.list = Example_Runlist
    # Run function
    create.Runlist(full.list,
                   blank.start = 4,
                   blank.end   = 2,
                   blank.comp  = 3,
                   blank.type  = 2,
                   blank.max   = 3)
  })
})
