test_that("ras.Fu_feces_meanSD adds grouped mean and sd columns", {
  df_ex <- data.frame(
    Sample_ID = c("S1", "S1", "S2", "S2"),
    `Sample Type` = c("hom", "hom", "hom", "hom"),
    `Analyte Peak Name` = c("cmpd", "cmpd", "cmpd", "cmpd"),
    value = c(10, 12, 20, 22)
  )

  out <- ras.Fu_feces_meanSD(df_ex)

  expect_true(all(c("value_mean", "value_sd") %in% names(out)))
  expect_equal(unique(out$value_mean[out$Sample_ID == "S1"]), 11)
  expect_equal(unique(out$value_mean[out$Sample_ID == "S2"]), 21)
})
