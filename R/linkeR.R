#' Reconnecting discontinous lines
#'
#'
#' @param bead.img
#' @param droplet.img description
#' @param threshold (from \code{\link[imager]{threshold}})
#' @param alpha (from \code{\link[imager]{cannyEdges}})
#' @param sigma (from \code{\link[imager]{cannyEdges}})
#' @returns image with continuous edges (closed gaps)
#' @import imager
#' @import magick
#' @import datatable
#' @examples
#' # example code
#'
#'
#' @export
linkeR <-
  function(bead.img,
           droplet.img,
           threshold = "13%",
           alpha = 0.75,
           sigma = 0.1) {
    # assign imports
    beads_to_del <- bead.img
    droplets <- droplet.img

    # first remove beads from droplet image (important for the linking of
    # discontinuous edges, as otherwise they may connect with the beads)
    # preprocessing: threshold, negate and mirroring
    bead_coords <- detecteR(beads_to_del, alpha, sigma)
    thresh <- threshold(droplets, threshold)
    thresh_cimg <- as.cimg(thresh)
    thresh_magick <- cimg2magick(thresh_cimg)
    neg_thresh <- image_negate(thresh_magick)
    neg_thresh_cimg <- magick2cimg(neg_thresh)
    neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")

    # transform binary image to array in order to modify individual values
    thresh_array <- as.array(neg_thresh_m)
    for (i in 1:nrow(bead_coords$coordinates)) {
      thresh_array[bead_coords$coordinates[i, 1],
                   bead_coords$coordinates[i, 2], 1, 1] <- 0
    }

    # successfully removed beads from droplets and retransformation to cimg
    thresh_clean_cimg <- as.cimg(thresh_array)

    # getting coordinates of all line ends and only diagonal line ends
    thresh_clean_magick <- cimg2magick(thresh_clean_cimg)
    mo1_lineends <- image_morphology(thresh_clean_magick,
                                     "HitAndMiss", "LineEnds")
    mo2_diagonalends <- image_morphology(thresh_clean_magick,
                                         "HitAndMiss", "LineEnds:2>")

    # transform extracted coordinates into data frames
    lineends_cimg <- magick2cimg(mo1_lineends)
    diagonalends_cimg <- magick2cimg(mo2_diagonalends)

    end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
    end_points_df <- as.data.frame(end_points)
    colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

    diagonal_edges <- which(diagonalends_cimg == TRUE, arr.ind = TRUE)
    diagonal_edges_df <- as.data.frame(diagonal_edges)
    colnames(diagonal_edges_df) <- c("x", "y", "dim3", "dim4")

    # labeling lines and transforming into data frame
    lab <- label(thresh_clean_cimg)
    df_lab <- as.data.frame(lab) |>
      subset(value > 0)

    # make sure that only lines are labeled and closed circles are not included
    # in the data frame, as line ends could connect with them
    alt_x <- list()
    alt_y <- list()
    alt_value <- list()
    for (g in 1:nrow(df_lab)) {
      if (droplets[df_lab$x[g], df_lab$y[g]] > 0.5) {
        alt_x <- df_lab$x[g]
        alt_y <- df_lab$y[g]
        alt_value <- df_lab$value[g]
      }
    }
    clean_lab_df <- data.frame(
      x = unlist(alt_x),
      y = unlist(alt_y),
      value = unlist(alt_value)
    )

    #
    first_overlay <-
      scanneR(end_points_df,
              diagonal_edges_df,
              clean_lab_df,
              lineends_cimg)

    first_connect <-
      parmax(list(thresh_clean_cimg, as.cimg(first_overlay$overlay)))


  }
