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
      img = beads,
      coordinates = res_sizeFilter$coordinates,
      unfiltered = res_objectDetection$coordinates
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
  expect_error(imgPipe(img1 = beads, img2 = grayscale(droplet_beads)))

  expect_type(res, "list")
  expect_length(res, 2)
  expect_s3_class(res$summary, "data.frame")
  expect_s3_class(res$detailed, "data.frame")
  expect_equal(
    res$summary$number_of_objects,
    length(unlist(res_sizeFilter$centers$size))
  )
  expect_equal(
    res$summary$number_of_objects,
    nrow(res_resultAnalytics$detailed)
  )
  expect_equal(
    mean(res$detailed$size),
    res$summary$mean_size
  )

  res_objectDetection <-
    objectDetection(grayscale(droplet_beads), alpha = 1, sigma = 0.1)
  res_resultAnalytics <-
    resultAnalytics(
      img = grayscale(droplet_beads),
      coordinates = res_objectDetection$coordinates,
      unfiltered = res_objectDetection$coordinates
    )
  res <-
    imgPipe(
      grayscale(droplet_beads),
      alpha = 1,
      sigma = 0.1,
      proximityFilter = FALSE,
      sizeFilter = FALSE
    )

  expect_equal(res, res_resultAnalytics)
})
