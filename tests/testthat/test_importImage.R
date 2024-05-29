library(testthat)
library(biopixR)

test_that("importImage", {
  tempdir()
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, "beads.png")
  save.image(beads, file_path)

  expect_equal(load.image(file_path), importImage(file_path))

  unlink(temp_dir, recursive = TRUE)
})
