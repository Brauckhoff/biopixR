library(testthat)
library(biopixR)

test_that("interactive_objectDetection", {
  img <- beads
  iniv <- c(1, 2, 1)
  names(iniv) <- c("alpha", "sigma", "scale")
  expect_equal(iniv, interactive_objectDetection(img, return_param = TRUE))
})
