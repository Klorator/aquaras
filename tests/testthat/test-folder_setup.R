test_that("ras.setup_dir_and_subfolders creates expected directory tree", {
  out_dir <- file.path(tempdir(), paste0("aquaras_test_", as.integer(Sys.time())))

  paths <- ras.setup_dir_and_subfolders(out_dir)

  expect_type(paths, "list")
  expect_true(length(paths) >= 5)
  expect_true(all(vapply(paths, dir.exists, logical(1))))

  unlink(out_dir, recursive = TRUE, force = TRUE)
})
