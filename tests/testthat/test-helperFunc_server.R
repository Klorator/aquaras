test_that("ras.well.update() snapshot test (warning)", {
  expect_snapshot({
    New_list = ras.well.update(df = Example_Runlist,
                               well.current = "3:A,1",
                               date         = "20220730",
                               signature    = "RH",
                               compound     = "Warfarin",
                               timepoint    = "42",
                               type         = "bead",
                               replicate    = "6")
    New_list[1,]
  })
})
# The test for res.well.update() shows a warning, but only in the snapshot.
#
# Warning <lifecycle_warning_deprecated>
#   The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
#   Convert to a vector.
#

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.dateSignAll.update() snapshot", {
  expect_snapshot({
    ras.dateSignAll.update(df = Example_Runlist,
                           date      = "20220730",
                           signature = "RH")
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("ras.sanitizeInput() snapshot", {
  expect_snapshot({
    ras.sanitizeInput("Spaces etc. / -- but_no_underscore!")
    })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

