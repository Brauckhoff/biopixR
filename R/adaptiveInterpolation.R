#' Connects LineEnd with the nearest labeled region
#'
#' Designed to be part of the fillLineGaps function. Function scans an increasing
#' radius around a line end and connects it with the nearest labeled region,
#' which in the fillLineGaps function is the closest edge.
#' @param end_points_df Data of type 'data.frame'.
#' @param diagonal_edges_df Data of type 'data.frame'.
#' @param clean_lab_df Data of type 'data.frame'.
#' @param lineends_cimg Image with dimensions of the image with discontinuous
#' edges. Just for giving the dimensions of the output matrix.
#' @return Binary matrix that can be applied as overlay for example with
#' \code{\link[imager]{imager.combine}} to fill the gaps between line ends.
#' @details
#' Additional details...
#'
#'
#' @examples
#' # example code
#'
#' @export
adaptiveInterpolation <- function(end_points_df,
                    diagonal_edges_df,
                    clean_lab_df,
                    lineends_cimg) {
  # creating an matrix with only 0 = background in the dimensions of the
  # image to be modified
  connected_components <- matrix(0,
    nrow = nrow(lineends_cimg),
    ncol = ncol(lineends_cimg)
  )

  # scan trough end points / line ends
  for (a in 1:nrow(end_points_df)) {
    x <- end_points_df$x[a]
    y <- end_points_df$y[a]
    # setting the radius of the surroundings that should be searched for
    # another labeled region
    for (b in 2:5) {
      # creating a square around the line end
      one_for_all <- which(
        clean_lab_df$x > (x - b) &
          clean_lab_df$x < (x + b) &
          clean_lab_df$y > (y - b) &
          clean_lab_df$y < (y + b)
      )
      # if conditions are met find position of the current line end in the
      # data frame containing all the labeled lines (to get the cluster of the
      # current line end later on)
      d <- which(clean_lab_df$x == x & clean_lab_df$y == y)

      # special condition for diagonal line ends:
      # as the label function does not recognize single pixel diagonal lines as
      # one line (and labels every pixel in that line as a separate cluster)
      # these type of line ends are difficult to connect to another cluster.
      # to at least get some of these line ends reconnected additionally to
      # ignoring the own cluster, the first neighboring cluster will also be
      # ignored and not taken into account as reconnection partner
      if (isTRUE(nrow(merge(end_points_df[a, ], diagonal_edges_df)) >
        0) == TRUE) {
        if (b == 2) {
          clus_ign <- which(one_for_all != d)
          dia_ignore <- one_for_all[clus_ign]
        }
        connector_pos <- which(
          clean_lab_df[one_for_all, ]$value !=
            clean_lab_df[d, ]$value &
            clean_lab_df[one_for_all, ]$value !=
              clean_lab_df[dia_ignore, ]$value
        )
      } else {
        connector_pos <- which(clean_lab_df[one_for_all, ]$value !=
          clean_lab_df[d, ]$value)
      }
      # get the line end surrounding cluster(s) that are not equal to the
      # cluster of the line end
      connector <- one_for_all[connector_pos]

      # use the interpolatePixels function to connect two coordinates with each other
      # the coordinates obtained by interpolatePixels are used for changing the value
      # of th coordinates in the matrix
      if (length(connector) != 0) {
        for (c in connector) {
          interpolated_pixels <- interpolatePixels(
            x, y,
            clean_lab_df[c, ]$x,
            clean_lab_df[c, ]$y
          )
          connected_components[interpolated_pixels] <- 1
        }
        break
      }
    }
  }
  # output: list containing the matrix with connected points
  out <- list(overlay = connected_components)
  out
}
