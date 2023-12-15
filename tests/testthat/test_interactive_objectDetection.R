library(testthat)
library(biopixR)

test_that("interactive_objectDetection", {
  img <- beads
  iniv <- c(0.7, 0.1)
  names(iniv) <- c("alpha", "sigma")
  expect_equal(iniv, interactive_objectDetection(img, return_param = TRUE))
})
