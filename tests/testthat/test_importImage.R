library(testthat)
library(biopixR)

test_that("importImage", {
  path2img <- system.file("images/beads.png", package = 'biopixR')

  expect_equal(load.image(path2img), importImage(path2img))

})
