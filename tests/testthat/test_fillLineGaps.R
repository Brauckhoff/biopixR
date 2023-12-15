library(testthat)
library(biopixR)

test_that("fillLineGaps", {
  closed_gaps <- fillLineGaps(droplets)

  expect_equal(class(closed_gaps)[1], "cimg")
  expect_equal(dim(droplets), dim(closed_gaps))

  lab_o <- label(droplets)
  df_lab_o <- as.data.frame(lab_o) |>
    subset(value > 0)
  lab <- label(closed_gaps)
  df_lab <- as.data.frame(lab) |>
    subset(value > 0)
  expect_gt(
    length(unique(df_lab_o$value)),
    length(unique(df_lab$value))
  )

  mat <- matrix(1, 8, 8)
  mat[3, 1:2] <- 0
  mat[4, 3] <- 0
  mat[7:8, 3] <- 0
  mat[5, 6:8] <- 0
  mat_cimg <- as.cimg(mat)
  filled_mat <- fillLineGaps(mat_cimg, threshold = "1%")
  expect_true(unique(mat_cimg %in% filled_mat))
  expect_error(fillLineGaps(mat))
  expect_equal(filled_mat[5:6, 3], c(1, 1))
  expect_equal(filled_mat[5, 4], 0)
  expect_equal(filled_mat[6, 3:5], c(1, 1, 1)) # 1 = foreground
  expect_equal(mat_cimg[6, 3:5], c(1, 1, 1)) # 1 = background
})
