library(testthat)
library(biopixR)

test_that("haralickCluster", {
  # with not existing directory
  expect_error(haralickCluster(temp_dir))

  # test1
  df <- data.frame(file = rep(NA, 4),
                   md5_sum = rep(NA, 4),
                   cluster = rep(NA, 4))
  # creating a directory to apply function to
  tempdir()
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, "beads.png")
  save.image(beads, file_path)
  file_path <- file.path(temp_dir, "droplet_beads.png")
  save.image(grayscale(droplet_beads), file_path)
  file_path <- file.path(temp_dir, "beads_large1.png")
  save.image(beads_large1, file_path)
  file_path <- file.path(temp_dir, "beads_large2.png")
  save.image(grayscale(beads_large2), file_path)

  calculatemd5 <- function(y) {
    md5_hash <- tools::md5sum(y)
    return(md5_hash)
  }

  # applying calculating function to all files in directory
  md5sums <- function(x) {
    sapply(x, calculatemd5)
  }

  all_items <- list.files(path = temp_dir, full.names = TRUE, recursive = FALSE)
  files_only <- all_items[!file.info(all_items)$isdir]
  df[, 1] <- files_only
  df[, 2] <- md5sums(files_only)
  df[, 3] <- c(1,2,1,2)
  df[, 3] <- as.integer(df$cluster)
  rownames(df) <- basename(files_only)

  expect_no_error(haralickCluster(temp_dir))

  result <- haralickCluster(temp_dir)
  expect_equal(result, df)

  # delete temporary directory containing test files
  unlink(temp_dir, recursive = TRUE)

  # test2
  # creating a directory to apply function to
  tempdir()
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, "beads.png")
  save.image(beads, file_path)
  file_path <- file.path(temp_dir, "droplet_beads.png")
  save.image(grayscale(droplet_beads), file_path)
  file_path <- file.path(temp_dir, "beads_large1.png")
  save.image(beads, file_path)
  file_path <- file.path(temp_dir, "beads_large2.png")
  save.image(grayscale(beads_large2), file_path)

  # create md5 error
  expect_error(haralickCluster(temp_dir))

  # delete temporary directory containing test files
  unlink(temp_dir, recursive = TRUE)

  # wrong input
  expect_error(haralickCluster(beads))
  input <- list(beads, droplet_beads, beads_large1, beads_large2)
  expect_error(haralickCluster(input))
})
