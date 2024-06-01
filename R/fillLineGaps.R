#' Reconnecting discontinuous lines
#'
#' The function attempts to fill in edge discontinuities in order to enable
#' normal labeling and edge detection.
#' @param contours image that contains discontinuous lines like edges or
#' contours
#' @param objects image that contains objects that should be removed before
#' applying the fill algorithm
#' @param threshold "in %" (from \code{\link[imager]{threshold}})
#' @param alpha threshold adjustment factor for edge detection
#' (from \code{\link[biopixR]{edgeDetection}})
#' @param sigma smoothing (from \code{\link[biopixR]{edgeDetection}})
#' @param radius maximal radius that should be scanned for another cluster
#' @param iterations how many times the algorithm should find line ends and
#' reconnect them to their closest neighbor
#' @param visualize if TRUE (default) a plot is displayed highlighting the
#' added pixels in the original image
#' @returns Image with continuous edges (closed gaps).
#' @details
#' The function pre-processes the image in order to enable the implementation
#' of the \code{\link[biopixR]{adaptiveInterpolation}} function. The
#' pre-processing stage encompasses a number of operations, including
#' thresholding, the optional removal of objects, the detection of line ends
#' and diagonal line ends, and the labeling of pixels. The threshold should be
#' set to allow for the retention of some "bridge" pixels between gaps, thus
#' facilitating the subsequent process of reconnection. For further details
#' regarding the process of reconnection, please refer to the documentation on
#' \code{\link[biopixR]{adaptiveInterpolation}}. The subsequent post-processing
#' stage entails the reduction of line thickness in the image. With regard to
#' the possibility of object removal, the coordinates associated with these
#' objects are collected using the \code{\link[biopixR]{objectDetection}}
#' function. Subsequently, the pixels of the detected objects are set to null
#' in the original image, thus allowing the algorithm to proceed without the
#' objects.
#' @import imager
#' @import magick
#' @import data.table
#' @examples
#' fillLineGaps(droplets)
#' @export
fillLineGaps <-
  function(contours,
           objects = NULL,
           threshold = "13%",
           alpha = 1,
           sigma = 2,
           radius = 5,
           iterations = 2,
           visualize = TRUE) {
    # Apply threshold to contours and convert to different image format for
    # processing
    thresh <- threshold(contours, threshold)
    thresh_cimg <- as.cimg(thresh)
    thresh_magick <- cimg2magick(thresh_cimg)
    neg_thresh <- image_negate(thresh_magick)
    neg_thresh_cimg <- magick2cimg(neg_thresh)
    neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")

    # Optionally remove unwanted objects (like micro particles) detected in the
    # image to improve edge linking
    if (!is.null(objects)) {
      objects_to_del <- objects
      object_coords <-
        objectDetection(objects_to_del, alpha = alpha, sigma = sigma)
      thresh_array <- as.array(neg_thresh_m)

      # Erase objects from the image by setting their coordinates to zero
      for (i in seq_len(nrow(object_coords$coordinates))) {
        thresh_array[object_coords$coordinates[i, 1],
                     object_coords$coordinates[i, 2], 1, 1] <- 0
      }
      thresh_clean_cimg <- as.cimg(thresh_array)
    } else {
      thresh_clean_cimg <- neg_thresh_m
    }

    # Mirror the cleaned image for consistent orientation across different
    # formats
    thresh_clean_m <- mirror(thresh_clean_cimg, axis = "x")
    thresh_clean_magick <- cimg2magick(thresh_clean_cimg)

    # Perform image morphology operations across specified number of iterations
    for (i in 1:iterations) {
      # Detect both general and diagonal line ends in the image
      mo1_lineends <- image_morphology(thresh_clean_magick,
                                       "HitAndMiss", "LineEnds")
      mo2_diagonalends <- image_morphology(thresh_clean_magick,
                                           "HitAndMiss", "LineEnds:2>")

      # Convert image of line ends into a usable data frame format
      lineends_cimg <- magick2cimg(mo1_lineends)
      diagonalends_cimg <- magick2cimg(mo2_diagonalends)
      end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
      end_points_df <- as.data.frame(end_points)
      colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

      diagonal_edges <-
        which(diagonalends_cimg == TRUE, arr.ind = TRUE)
      diagonal_edges_df <- as.data.frame(diagonal_edges)
      colnames(diagonal_edges_df) <- c("x", "y", "dim3", "dim4")

      # Label and process line data for the iteration
      if (i == 1) {
        lab <- label(thresh_clean_m)
        df_lab <- as.data.frame(lab) |>
          subset(value > 0)
      } else {
        lab <- label(out_cimg)
        df_lab <- as.data.frame(lab) |>
          subset(value > 0)
      }


      # Filter for lines, avoiding connections to non-line features
      alt_x <- list()
      alt_y <- list()
      alt_value <- list()
      if (i == 1) {
        for (g in seq_len(nrow(df_lab))) {
          # droplets_array <- as.array(droplets)
          if (thresh_clean_m[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
            alt_x[g] <- df_lab$x[g]
            alt_y[g] <- df_lab$y[g]
            alt_value[g] <- df_lab$value[g]
          }
        }
      } else {
        for (g in seq_len(nrow(df_lab))) {
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

      # Connect isolated line ends, producing a combined output image
      first_overlay <-
        adaptiveInterpolation(end_points_df,
                              diagonal_edges_df,
                              clean_lab_df,
                              lineends_cimg,
                              radius = radius)
      if (i == 1) {
        first_connect <-
          parmax(list(thresh_clean_m, as.cimg(first_overlay$overlay)))
      } else {
        first_connect <-
          parmax(list(out_cimg, as.cimg(first_overlay$overlay)))
      }

      # Mirror the resulting connected image for the next iteration
      first_connect_m <- mirror(first_connect, axis = "x")
      connect_magick <- cimg2magick(first_connect_m)
      thresh_clean_magick <-
        image_morphology(connect_magick, "thinning", "skeleton")

      out_cimg <- magick2cimg(thresh_clean_magick)
    }

    # Prepare final output image
    result_cimg <- magick2cimg(thresh_clean_magick)
    result_m <- mirror(result_cimg, axis = "x")

    # Visualize the result if requested
    if (visualize == TRUE) {
      # creating an 'cimg' in which there are 0, 1 and 2
      # 0 - background
      # 1 - filled by algorithm
      # 2 - original line
      vis <- add(list(result_m, thresh_clean_cimg))
      vis_col <- add.color(vis)
      to_color <- which(vis_col == 1, arr.ind = TRUE)
      changePixelColor(neg_thresh_m, to_color, visualize = TRUE)
    }

    # Return the final processed image
    out <- result_m
  }
