#' Reconnecting discontinuous lines
#'
#' The function attempts to fill in edge discontinuities in order to enable
#' normal labeling and edge detection.
#' @param droplet.img image that contains discontinuous lines like edges or
#' contours
#' @param bead.img image that contains objects that should be removed before
#' before applying the fill algorithm
#' @param threshold "in %" (from \code{\link[imager]{threshold}})
#' @param alpha threshold adjustment factor for edge detection
#' (from \code{\link[imager]{cannyEdges}})
#' @param sigma smoothing (from \code{\link[imager]{cannyEdges}})
#' @param radius maximal radius that should be scanned for another cluster
#' @param iterations how many times the algorithm should find line ends and
#' reconnect them to their closest neighbor
#' @param visualize if TRUE (default) a plot is displayed highlighting the
#' added pixels in the original image
#' @returns image with continuous edges (closed gaps)
#' @import imager
#' @import magick
#' @import data.table
#' @details
#' The function pre-processes the image to enable the application of
#' adaptiveInterpolation. Pre-processing involves thresholding, optional
#' object removal, LineEnd and diagonal LineEnd detection, and labeling. The
#' threshold should be set to allow for some remaining "bridge" pixels between
#' gaps to facilitate reconnection. For more details about reconnection,
#' please consult adaptiveInterpolation.
#' Post-processing involves thinning the lines. When removing objects from an
#' image, their coordinates are collected using the objectDetection function.
#' Next, the pixels of the detected objects are nullified in the original
#' image, allowing the algorithm to proceed without the objects.
#' @examples
#' fillLineGaps(droplets)
#' @export
fillLineGaps <-
  function(droplet.img,
           bead.img = NULL,
           threshold = "13%",
           alpha = 0.75,
           sigma = 0.1,
           radius = 5,
           iterations = 2,
           visualize = TRUE) {
    # assign import
    droplets <- droplet.img

    # preprocessing: threshold, negate and mirroring
    thresh <- threshold(droplets, threshold)
    thresh_cimg <- as.cimg(thresh)
    thresh_magick <- cimg2magick(thresh_cimg)
    neg_thresh <- image_negate(thresh_magick)
    neg_thresh_cimg <- magick2cimg(neg_thresh)
    neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")

    # first remove beads from droplet image (important for the linking of
    # discontinuous edges, as otherwise they may connect with the beads)
    # removes objects to prevent reconnecting with labeled regions that
    # are not lines/edges
    if (!is.null(bead.img)) {
      beads_to_del <- bead.img
      bead_coords <- objectDetection(beads_to_del, alpha, sigma)

      # transform binary image to array in order to modify individual values
      thresh_array <- as.array(neg_thresh_m)
      for (i in 1:nrow(bead_coords$coordinates)) {
        thresh_array[
          bead_coords$coordinates[i, 1],
          bead_coords$coordinates[i, 2], 1, 1
        ] <- 0
      }
      # removed beads from droplets and retransformation to cimg
      thresh_clean_cimg <- as.cimg(thresh_array)
    } else {
      thresh_clean_cimg <- neg_thresh_m
    }

    # same orientation for 'cimg' and 'magick-image'
    thresh_clean_m <- mirror(thresh_clean_cimg, axis = "x")
    thresh_clean_magick <- cimg2magick(thresh_clean_cimg)

    for (i in 1:iterations) {
      # getting coordinates of all line ends and only diagonal line ends
      mo1_lineends <- image_morphology(
        thresh_clean_magick,
        "HitAndMiss", "LineEnds"
      )
      mo2_diagonalends <- image_morphology(
        thresh_clean_magick,
        "HitAndMiss", "LineEnds:2>"
      )

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
      if (i == 1) {
        lab <- label(thresh_clean_m)
        df_lab <- as.data.frame(lab) |>
          subset(value > 0)
      } else {
        lab <- label(out_cimg)
        df_lab <- as.data.frame(lab) |>
          subset(value > 0)
      }

      # make sure that only lines are labeled and closed circles are not included
      # in the data frame, as line ends could connect with them
      alt_x <- list()
      alt_y <- list()
      alt_value <- list()

      if (i == 1) {
        for (g in 1:nrow(df_lab)) {
          # droplets_array <- as.array(droplets)
          if (thresh_clean_m[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
            alt_x[g] <- df_lab$x[g]
            alt_y[g] <- df_lab$y[g]
            alt_value[g] <- df_lab$value[g]
          }
        }
      } else {
        for (g in 1:nrow(df_lab)) {
          # droplets_array <- as.array(droplets)
          if (out_cimg[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
            alt_x[g] <- df_lab$x[g]
            alt_y[g] <- df_lab$y[g]
            alt_value[g] <- df_lab$value[g]
          }
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
        adaptiveInterpolation(end_points_df,
          diagonal_edges_df,
          clean_lab_df,
          lineends_cimg,
          radius = radius
        )

      # combine matrix with starting image
      if (i == 1) {
        first_connect <-
          parmax(list(thresh_clean_m, as.cimg(first_overlay$overlay)))
      } else {
        first_connect <-
          parmax(list(out_cimg, as.cimg(first_overlay$overlay)))
      }

      first_connect_m <- mirror(first_connect, axis = "x")

      # repeat above process:
      # line end and diagonal end calculation
      connect_magick <- cimg2magick(first_connect_m)
      thresh_clean_magick <-
        image_morphology(connect_magick, "thinning", "skeleton")

      out_cimg <- magick2cimg(thresh_clean_magick)
    }

    # resulting image with continous edges
    result_cimg <- magick2cimg(thresh_clean_magick)
    result_m <- mirror(result_cimg, axis = "x")

    if (visualize == TRUE) {
      # creating an cimg in which there are 0, 1 and 2
      # 0 - background
      # 1 - filled by algorithm
      # 2 - original line
      vis <- add(list(result_m, thresh_clean_cimg))
      vis_col <- add.color(vis)

      # get coordinates from new / filled pixels
      to_color <- which(vis_col == 1, arr.ind = TRUE)

      # colorize and plot specific coordinates with changePixelColor function
      changePixelColor(neg_thresh_m, to_color)
    }

    out <- result_m
    out
  }
