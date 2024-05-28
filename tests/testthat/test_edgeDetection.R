library(testthat)
library(biopixR)

test_that("edgeDetection", {
  img <- beads

  expect_equal(class(edgeDetection(img))[1], 'pixset')
  expect_error(edgeDetection(img, alpha = 10),
               regexp = "The parameters cannot be increased any further since no edges could be detected.")


  img3 <- droplet_beads

  expect_warning(edgeDetection(img3),
    regexp = "Running edge detector on luminance channel"
  )
})
