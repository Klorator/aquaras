test_that("well.update() snapshot test", {
  expect_snapshot({
    New_list = well.update(df = Example_Runlist,
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("dateSignAll.update() snapshot", {
  expect_snapshot({
    dateSignAll.update(df = Example_Runlist,
                       date      = "20220730",
                       signature = "RH")
  })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("sanitizeInput() snapshot", {
  expect_snapshot({
    sanitizeInput("Spaces etc. / -- but_no_underscore!")
    })
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

