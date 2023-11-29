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
#' @details
#' Additional details...
#'
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
           sigma = 0.1,
           visualize = TRUE) {
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

    diagonal_edges <-
      which(diagonalends_cimg == TRUE, arr.ind = TRUE)
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

    # scan the surroundings of line ends and connect with nearest cluster
    # creates a binary matrix that can be displayed as image
    # (0 = background/black & 1 = white/foreground)
    first_overlay <-
      scanneR(end_points_df,
              diagonal_edges_df,
              clean_lab_df,
              lineends_cimg)

    # combine matrix with starting image
    first_connect <-
      parmax(list(thresh_clean_cimg, as.cimg(first_overlay$overlay)))

    # repeat above process:
    # line end and diagonal end calculation
    connect_magick <- cimg2magick(first_connect)
    thin_connect <-
      image_morphology(connect_magick, "thinning", "skeleton")
    mo3_lineends <-
      image_morphology(thin_connect, "HitAndMiss", "LineEnds")
    mo4_diagonalends <-
      image_morphology(thin_connect, "HitAndMiss", "LineEnds:2>")

    thin_cimg <- magick2cimg(thin_connect)
    sec_lineends_cimg <- magick2cimg(mo3_lineends)
    sec_diagonalends_cimg <- magick2cimg(mo4_diagonalends)

    # label image
    sec_lab <- label(thin_cimg)
    df_sec_lab <- as.data.frame(sec_lab) |>
      subset(value > 0)

    # transform extracted points into data.frames
    sec_lineends <- which(sec_lineends_cimg == TRUE, arr.ind = TRUE)
    sec_lineends_df <- as.data.frame(sec_lineends)
    colnames(sec_lineends_df) <- c("x", "y", "dim3", "dim4")

    sec_diagonal_edges <-
      which(sec_diagonalends_cimg == TRUE, arr.ind = TRUE)
    sec_diagonal_edges_df <- as.data.frame(sec_diagonal_edges)
    colnames(sec_diagonal_edges_df) <- c("x", "y", "dim3", "dim4")

    # more important to exclude closed circles then in the first run, as now
    # more partitions have continuous edges
    lab2_clean_x <- list()
    ĺab2_clean_y <- list()
    lab2_clean_value <- list()

    for (e in 1:nrow(df_sec_lab)) {
      if (thin_cimg[df_sec_lab[e, 1], df_sec_lab[e, 2]] == 1) {
        lab2_clean_x[e] <- df_sec_lab[e, ]$x
        ĺab2_clean_y[e] <- df_sec_lab[e, ]$y
        lab2_clean_value[e] <- df_sec_lab[e, ]$value
      }
    }

    df_sec_lab_clean <- data.frame(
      x = unlist(lab2_clean_x),
      y = unlist(ĺab2_clean_y),
      value = unlist(lab2_clean_value)
    )

    # second time filling up gaps in lines
    sec_overlay <-
      scanneR(sec_lineends_df,
              sec_diagonal_edges_df,
              df_sec_lab_clean,
              thin_cimg)

    # combine overlay matrix with former image
    result <- parmax(list(thin_cimg, sec_overlay$overlay))

    # final thin edges
    result_magick <- cimg2magick(result)
    result_thin <-
      image_morphology(result_magick, "Thinning", "Skeleton")
    result_cimg <- magick2cimg(result_thin)

    if (visualize = TRUE) {
      # creating an cimg in which there are 0, 1 and 2
      # 0 - background
      # 1 - filled by algorithm
      # 2 - original line
      vis <- add(list(result_cimg, neg_thresh_m))
      vis_col <- add.color(vis)

      # get coordinates from new / filled pixels
      to_color <- which(vis_col == 1, arr.ind = TRUE)

      # colorize and plot specific coordinates with vis_linkeR function
      vis_linkeR(neg_thresh_m, to_color)
    }

    out <- list(result_cimg)
    out
  }
