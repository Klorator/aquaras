test_that("loadFile.ML() loads dataLines", {
  expect_snapshot(
    loadFile.ML(sourceFile = system.file("extdata",
                                         "Example_MLOutput.txt",
                                         package = "aquaras",
                                         mustWork = TRUE))
    )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("splitDataLines.ML() splits into data frame list", {
  expect_snapshot({
    # Setup
    dataLines = loadFile.ML(sourceFile = system.file("extdata",
                                                     "Example_MLOutput.txt",
                                                     package = "aquaras",
                                                     mustWork = TRUE))
    ### Ignore the "New names:" output.
    splitDataLines.ML(dataLines)
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("cleanDF.ML() snapshot looks as intended", {
  expect_snapshot({
    # Setup
    dataLines = loadFile.ML(sourceFile = system.file("extdata",
                                                     "Example_MLOutput.txt",
                                                     package = "aquaras",
                                                     mustWork = TRUE))
    ### Ignore the "New names:" output.
    listDF = splitDataLines.ML(dataLines)

    cleanDF.ML(listDF)
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# writeFiles.ML() writes to file system

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("SplitOutput.ML() snapshot looks as intended", {
  expect_snapshot({
    # !!! DOES **NOT** WRITE TO FILE SYSTEM !!!
    SplitOutput.ML(sourceFile = system.file("extdata",
                                            "Example_MLOutput.txt",
                                            package = "aquaras",
                                            mustWork = TRUE),
                   write = FALSE)
  })
})
