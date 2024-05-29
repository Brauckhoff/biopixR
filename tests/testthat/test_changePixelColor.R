library(testthat)
library(biopixR)

test_that("changePixelColor", {
  mat <- matrix(0, 4, 4)
  mat[2:3, 2:3] <- 1
  img <- as.cimg(mat)
  coordinates <- data.frame(x = c(1, 3),
                            y = c(1, 3))

  expect_error(changePixelColor(mat, coordinates),
               regexp = "image must be of class 'cimg'")
  expect_no_error(changePixelColor(img, coordinates))

  expect_equal(dim(img)[4], 1)
  expect_equal(dim(add.colour(img))[4], 3)

  expect_equal(as.vector(col2rgb("red") / 255), as.vector(c(1, 0, 0)))
  expect_equal(as.vector(col2rgb("green") / 255), as.vector(c(0, 1, 0)))
  expect_equal(as.vector(col2rgb("blue") / 255), as.vector(c(0, 0, 1)))

  expect_equal(img[1], as.array(img)[1])

  test <- changePixelColor(img, coordinates, color = "blue")
  expect_equal(test[1, 1, ,], as.vector(c(0, 0, 1)))
  expect_equal(test[2, 2, ,], as.vector(c(1, 1, 1)))
  expect_equal(test[1, 2, ,], as.vector(c(0, 0, 0)))
})
