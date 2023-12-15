library(testthat)
library(biopixR)

test_that("interpolatePixels", {
  mat <- matrix(0, 5, 5)
  mat[1, 1] <- 1
  mat[3, 5] <- 1

  expect_equal(max(abs(3 - 1), abs(5 - 1)) + 1, 5)
  expect_length(round(seq(1, 3, length.out = 5)), 5)
  expect_equal(nrow(interpolatePixels(1, 1, 3, 5)), 5)
  expect_equal(class(interpolatePixels(1, 1, 3, 5)), c("matrix", "array"))
})
