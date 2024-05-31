library(testthat)
library(biopixR)

test_that("haralickCluster", {
  # with not existing directory
  expect_error(haralickCluster(path2dir))

  # test1
  df <- data.frame(file = rep(NA, 4),
                   md5_sum = rep(NA, 4),
                   cluster = rep(NA, 4))
  # creating a directory to apply function to
  path2dir <- system.file("images", package = 'biopixR')

  calculatemd5 <- function(y) {
    md5_hash <- tools::md5sum(y)
    return(md5_hash)
  }

  # applying calculating function to all files in directory
  md5sums <- function(x) {
    sapply(x, calculatemd5)
  }

  all_items <- list.files(path = path2dir, full.names = TRUE, recursive = FALSE)
  files_only <- all_items[!file.info(all_items)$isdir]
  df[, 1] <- files_only
  df[, 2] <- md5sums(files_only)
  df[, 3] <- c(1,2,1,2)
  df[, 3] <- as.integer(df$cluster)
  rownames(df) <- basename(files_only)

  expect_no_error(haralickCluster(path2dir))

  result <- haralickCluster(path2dir)
  expect_equal(result, df)

  # wrong input
  expect_error(haralickCluster(beads))
  input <- list(beads, droplet_beads, beads_large1, beads_large2)
  expect_error(haralickCluster(input))
})
