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
    res_sizeFilter$centers$size[1]
  )
  expect_equal(
    unique(res_sizeFilter$coordinates$value),
    res_sizeFilter$centers$value
  )

  expect_type(res_sizeFilter, "list")
  expect_length(res_sizeFilter, 2)
  expect_equal(
    length(res_sizeFilter$centers$size),
    length(unique(res_sizeFilter$coordinates$value))
  )

  expect_error(sizeFilter(img))
  expect_error(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = "auto",
      upperlimit = 150
    ),
    regexp = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " Both limits must be set to 'auto' or selected individually")
  )
  expect_error(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = 50,
      upperlimit = "auto"
    ),
    regexp = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " Both limits must be set to 'auto' or selected individually")
  )
  expect_warning(
    sizeFilter(res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = "auto",
      upperlimit = "auto"
    ),
    regexp = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " Number of detected objects should be >50 for automated detection")
  )
})
