library(testthat)
library(biopixR)

test_that("resultAnalytics", {
  img <- beads
  res_objectDetection <-
    objectDetection(img, alpha = 0.75, sigma = 0.1)
  res_sizeFilter <-
    sizeFilter(
      res_objectDetection$centers,
      res_objectDetection$coordinates,
      lowerlimit = 50,
      upperlimit = 150
    )
  res_resultAnalytics <-
    resultAnalytics(
      unfiltered = res_objectDetection$coordinates,
      coordinates = res_sizeFilter$coordinates,
      res_sizeFilter$size,
      img = beads
    )

  expect_equal(
    sum(res_resultAnalytics$detailed$size),
    nrow(res_sizeFilter$coordinates)
  )
  expect_equal(
    length(res_resultAnalytics$detailed$beadnumber),
    length(unique(res_sizeFilter$coordinates$value))
  )

  expect_type(res_resultAnalytics, "list")
  expect_length(res_resultAnalytics, 2)
  expect_s3_class(res_resultAnalytics$summary, "data.frame")
  expect_s3_class(res_resultAnalytics$detailed, "data.frame")
  expect_equal(
    res_resultAnalytics$summary$number_of_beads,
    length(unlist(res_sizeFilter$size))
  )
  expect_equal(
    res_resultAnalytics$summary$number_of_beads,
    nrow(res_resultAnalytics$detailed)
  )
  expect_equal(
    mean(res_resultAnalytics$detailed$size),
    res_resultAnalytics$summary$mean_size
  )
})
