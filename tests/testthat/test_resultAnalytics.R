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
      img = beads
    )

  expect_equal(
    sum(res_resultAnalytics$detailed$size),
    nrow(res_sizeFilter$coordinates)
  )
  expect_equal(
    length(res_resultAnalytics$detailed$objectnumber),
    length(unique(res_sizeFilter$coordinates$value))
  )

  expect_type(res_resultAnalytics, "list")
  expect_length(res_resultAnalytics, 2)
  expect_s3_class(res_resultAnalytics$summary, "data.frame")
  expect_s3_class(res_resultAnalytics$detailed, "data.frame")
  expect_equal(
    res_resultAnalytics$summary$number_of_objects,
    length(unlist(res_sizeFilter$centers$size))
  )
  expect_equal(
    res_resultAnalytics$summary$number_of_objects,
    nrow(res_resultAnalytics$detailed)
  )
  expect_equal(
    mean(res_resultAnalytics$detailed$size),
    res_resultAnalytics$summary$mean_size
  )
})
