library(testthat)
library(biopixR)

test_that("edgeDetection", {
  img <- beads

  expect_equal(edgeDetection(img), cannyEdges(img))
  expect_equal(
    edgeDetection(img, alpha = 0.75, sigma = 0.1),
    cannyEdges(img, alpha = 0.75, sigma = 0.1)
  )


  img2 <- grayscale(droplet_beads)

  expect_equal(edgeDetection(img2), cannyEdges(img2))
  expect_equal(
    edgeDetection(img2, alpha = 0.75, sigma = 0.1),
    cannyEdges(img2, alpha = 0.75, sigma = 0.1)
  )


  img3 <- droplet_beads

  expect_warning(edgeDetection(img3),
    regexp = "Running edge detector on luminance channel"
  )
})
