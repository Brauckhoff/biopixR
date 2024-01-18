library(testthat)
library(biopixR)

test_that("imgPipe", {
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

  res <- imgPipe(
    img,
    alpha = 0.75,
    sigma = 0.1,
    sizeFilter = TRUE,
    lowerlimit = 50,
    upperlimit = 150,
    proximityFilter = FALSE
  )

  expect_equal(res, res_resultAnalytics)
  expect_error(imgPipe(img1 = beads, img2 = droplet_beads))

  expect_type(res, "list")
  expect_length(res, 2)
  expect_s3_class(res$summary, "data.frame")
  expect_s3_class(res$detailed, "data.frame")
  expect_equal(
    res$summary$number_of_beads,
    length(unlist(res_sizeFilter$size))
  )
  expect_equal(
    res$summary$number_of_beads,
    nrow(res_resultAnalytics$detailed)
  )
  expect_equal(
    mean(res$detailed$size),
    res$summary$mean_size
  )

  res_objectDetection <-
    objectDetection(droplet_beads, alpha = 1, sigma = 0.1)
  res_resultAnalytics <- resultAnalytics(
    unfiltered = res_objectDetection$coordinates,
    coordinates = res_objectDetection$coordinates,
    size = res_objectDetection$size,
    img = grayscale(droplet_beads)
  )
  res <-
    imgPipe(
      droplet_beads,
      alpha = 1,
      sigma = 0.1,
      proximityFilter = FALSE,
      sizeFilter = FALSE
    )

  expect_equal(res, res_resultAnalytics)
})
