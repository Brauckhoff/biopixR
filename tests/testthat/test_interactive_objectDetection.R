library(testthat)
library(biopixR)

test_that("interactive_objectDetection", {
  img <- beads
  iniv <- c(1, 2, 1, "edge")
  names(iniv) <- c("alpha", "sigma", "scale", "method")
  expect_equal(iniv, interactive_objectDetection(img, return_param = TRUE))
})
