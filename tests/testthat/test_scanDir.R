library(testthat)
library(biopixR)


test_that("scanDir", {
  imgs_path <- system.file("images", package = 'biopixR')

  result <- scanDir(imgs_path,
                    method = "edge")

  beads_img <- importImage("inst/images/beads.png")
  beads_large1_img <- importImage("inst/images/beads_large1.bmp")
  pre_reference1 <- objectDetection(beads_img, method = "edge")
  reference1 <-
    resultAnalytics(beads_img, pre_reference1$coordinates)
  rownames(reference1$summary) <-
    tools::file_path_sans_ext(basename("inst/images/beads.png"))

  pre_reference2 <-
    objectDetection(beads_large1_img, method = "edge")
  reference2 <-
    resultAnalytics(beads_large1_img, pre_reference2$coordinates)
  rownames(reference2$summary) <-
    tools::file_path_sans_ext(basename("inst/images/beads_large1.bmp"))

  expect_equal(result[4, 3:9], reference1$summary)
  expect_equal(result[1, 3:9], reference2$summary)

  expect_error(scanDir(beads_img))
  expect_s3_class(result, "data.frame")
  expect_equal(result$estimated_rejected, c(0, 0, 0, 0, 0))

  expect_no_error(scanDir(imgs_path,
                          method = "threshold"))
  expect_no_error(scanDir(imgs_path,
                          method = "threshold",
                          parallel = TRUE))
  expect_no_error(scanDir(imgs_path,
                          method = "gaussian",
                          parallel = TRUE))
  expect_equal(
    scanDir(
      imgs_path,
      method = "edge",
      alpha = 1,
      sigma = 0,
      parallel = TRUE
    ),
    scanDir(
      imgs_path,
      method = "edge",
      alpha = 1,
      sigma = 0,
      parallel = FALSE
    )
  )
})
