test_that("ras.loadFile() loads dataLines", {
  expect_snapshot(
    ras.loadFile(sourceFile = system.file("extdata",
                                          "Example_MLOutput.txt",
                                          package = "aquaras",
                                          mustWork = TRUE))
    )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.splitDataLines() splits into data frame list", {
  expect_snapshot({
    # Setup
    dataLines = ras.loadFile(sourceFile = system.file("extdata",
                                                      "Example_MLOutput.txt",
                                                      package = "aquaras",
                                                      mustWork = TRUE))
    ### Ignore the "New names:" output.
    ras.splitDataLines(dataLines)
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.cleanDF() snapshot looks as intended", {
  expect_snapshot({
    # Setup
    dataLines = ras.loadFile(sourceFile = system.file("extdata",
                                                      "Example_MLOutput.txt",
                                                      package = "aquaras",
                                                      mustWork = TRUE))
    ### Ignore the "New names:" output.
    listDF = ras.splitDataLines(dataLines)

    ras.cleanDF(listDF)
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# writeFiles.ML() writes to file system

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.SplitOutput() snapshot looks as intended", {
  expect_snapshot({
    # !!! DOES **NOT** WRITE TO FILE SYSTEM !!!
    ras.SplitOutput(sourceFile = system.file("extdata",
                                             "Example_MLOutput.txt",
                                             package = "aquaras",
                                             mustWork = TRUE),
                    write = FALSE)
  })
})
