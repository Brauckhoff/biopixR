library(testthat)
library(biopixR)

test_that("adaptiveInterpolation", {
  # creating example to test
  ## creating artifical image
  mat <- matrix(0, 8, 8)
  mat[3, 1:2] <- 1
  mat[4, 3] <- 1
  mat[7:8, 3] <- 1
  mat[5, 6:8] <- 1
  mat_cimg <- as.cimg(mat)

  ## preprocessing / LineEnd detection / labeling
  mat_cimg_m <- mirror(mat_cimg, axis = "x")
  mat_magick <- cimg2magick(mat_cimg)
  lineends <-
    image_morphology(mat_magick, "HitAndMiss", "LineEnds")
  diagonalends <-
    image_morphology(mat_magick, "HitAndMiss", "LineEnds:2>")
  lineends_cimg <- magick2cimg(lineends)
  diagonalends_cimg <- magick2cimg(diagonalends)
  end_points <-
    which(lineends_cimg == TRUE, arr.ind = TRUE)
  end_points_df <- as.data.frame(end_points)
  colnames(end_points_df) <- c("x", "y", "dim3", "dim4")
  diagonal_edges <-
    which(diagonalends_cimg == TRUE, arr.ind = TRUE)
  diagonal_edges_df <- as.data.frame(diagonal_edges)
  colnames(diagonal_edges_df) <-
    c("x", "y", "dim3", "dim4")
  lab <- label(mat_cimg_m)
  df_lab <- as.data.frame(lab) |> subset(value > 0)
  alt_x <- list()
  alt_y <- list()
  alt_value <- list()
  for (g in seq_len(nrow(df_lab))) {
    if (mat_cimg_m[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
      alt_x[g] <- df_lab$x[g]
      alt_y[g] <- df_lab$y[g]
      alt_value[g] <- df_lab$value[g]
    }
  }
  clean_lab_df <- data.frame(
    x = unlist(alt_x),
    y = unlist(alt_y),
    value = unlist(alt_value)
  )

  # actual function
  overlay <- adaptiveInterpolation(end_points_df,
                                   diagonal_edges_df,
                                   clean_lab_df,
                                   mat_cimg)

  # begin tests
  expect_equal(nrow(end_points_df), 3)
  expect_equal(nrow(clean_lab_df), 8)
  expect_equal(nrow(diagonal_edges_df), 1)

  expect_equal(dim(overlay$overlay)[1:2], dim(mat_cimg_m)[1:2])
  expect_equal(
    which(
      clean_lab_df$x > (2 - 4) &
        clean_lab_df$x < (2 + 4) &
        clean_lab_df$y > (3 - 4) &
        clean_lab_df$y < (3 + 4)
    ),
    3:6
  )
  expect_equal(which(clean_lab_df$x == 2 & clean_lab_df$y == 3), 4)
  expect_equal(which(clean_lab_df[3:6,]$value !=
                       clean_lab_df[4,]$value), 3:4)
  expect_equal(
    which(
      clean_lab_df$x > (2 - 4) &
        clean_lab_df$x < (2 + 4) &
        clean_lab_df$y > (3 - 4) &
        clean_lab_df$y < (3 + 4)
    )[3:4],
    5:6
  )
  expect_true(isTRUE(nrow(
    merge(end_points_df[2,], diagonal_edges_df)
  ) > 0))
  expect_false(isTRUE(nrow(
    merge(end_points_df[1,], diagonal_edges_df)
  ) > 0))


  expect_length(overlay, 1)
  expect_equal(class(overlay$overlay), c("matrix", "array"))
})
