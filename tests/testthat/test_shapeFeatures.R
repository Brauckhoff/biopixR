library(testthat)
library(biopixR)

test_that("shapeFeatures", {
  expect_warning(shapeFeatures(droplet_beads),
                 regexp = "Running edge detector on luminance channel")

  test1 <- shapeFeatures(beads,
                     alpha = 1,
                     sigma = 0,
                     SOM = FALSE)

  res_objectDetection <- objectDetection(beads, sigma = 0)
  res <- resultAnalytics(res_objectDetection$coordinates,
                         img = beads)

  expect_equal(test1[,1:6], res$detailed)
  expect_length(test1, 12)
  expect_null(test1$class)

  test2 <- shapeFeatures(beads,
                         alpha = 1,
                         sigma = 0,
                         SOM = TRUE,
                         visualize = FALSE)

  expect_error(recordPlot())
  expect_no_error(test2$class)
  expect_length(test2, 13)
  expect_equal(nrow(test1), nrow(test2))
  expect_gt(ncol(test2), ncol(test1))

  test3 <- shapeFeatures(beads,
                         alpha = 1,
                         sigma = 0,
                         SOM = TRUE,
                         visualize = TRUE)
  current_plot <- recordPlot()

  expect_equal(class(current_plot), "recordedplot")
  expect_length(test3, 13)
  expect_equal(nrow(test1), nrow(test3))
  expect_gt(ncol(test3), ncol(test1))

  })
