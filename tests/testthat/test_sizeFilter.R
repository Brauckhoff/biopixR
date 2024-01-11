library(testthat)
library(biopixR)

test_that("sizeFilter", {
  img <- beads
  res_objectDetection <- objectDetection(img, alpha = 0.75, sigma = 0.1)
  res_sizeFilter <- sizeFilter(
    centers = res_objectDetection$centers,
    coordinates = res_objectDetection$coordinates,
    lowerlimit = 50,
    upperlimit = 100
  )

  expect_equal(
    class(res_sizeFilter$coordinates),
    c("data.frame")
  )
  expect_lte(
    nrow(res_sizeFilter$coordinates),
    nrow(res_objectDetection$coordinates)
  )
  expect_gt(
    nrow(res_sizeFilter$coordinates),
    length(res_sizeFilter$size)
  )

  expect_equal(
    length(which(res_sizeFilter$coordinates$value == 1)),
    unlist(res_sizeFilter$size[1])
  )
  expect_equal(
    max(res_sizeFilter$coordinates$value),
    length(res_sizeFilter$size)
  )

  expect_type(res_sizeFilter, "list")
  expect_length(res_sizeFilter, 3)
  expect_equal(length(res_sizeFilter$size), nrow(res_objectDetection$centers))
  expect_equal(
    length(unlist(res_sizeFilter$size)),
    length(unique(res_sizeFilter$coordinates$value))
  )

  expect_error(sizeFilter(img))
  expect_error(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = "auto",
      upperlimit = 150
    ),
    regexp = "both limits must be set to 'auto' or selected individually"
  )
  expect_error(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = 50,
      upperlimit = "auto"
    ),
    regexp = "both limits must be set to 'auto' or selected individually"
  )
  expect_error(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = "auto",
      upperlimit = "auto"
    ),
    regexp = "detected number of objects is to small for automated detection"
  )
})
