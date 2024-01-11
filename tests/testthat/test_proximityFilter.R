library(testthat)
library(biopixR)

test_that("proximityFilter", {
  img <- beads
  res_objectDetection <-
    objectDetection(img, alpha = 0.75, sigma = 0.1)
  res_proximityFilter <- proximityFilter(
    res_objectDetection$centers,
    res_objectDetection$coordinates,
    radius = "auto"
  )

  expect_lte(
    length(res_proximityFilter$centers$value),
    length(res_objectDetection$centers$value)
  )
  expect_lte(
    length(res_proximityFilter$coordinates$value),
    length(res_objectDetection$coordinates$value)
  )
  expect_error(proximityFilter(img))

  expect_type(res_proximityFilter, "list")
  expect_length(res_proximityFilter, 3)
  expect_equal(class(res_proximityFilter$coordinates), "data.frame")

  expect_equal(
    length(res_proximityFilter$centers$value),
    length(res_proximityFilter$size)
  )
  expect_equal(
    unique(res_proximityFilter$coordinates$value),
    res_proximityFilter$centers$value
  )
})
