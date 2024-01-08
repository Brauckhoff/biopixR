#' Connects LineEnd with the nearest labeled region
#'
#' Function scans an increasing radius around a line end and connects it with
#' the nearest labeled region.
#' @param end_points_df data frame with the coordinates of all line ends. can
#' be obtained with \code{\link[magick]{image_morphology}}.
#' @param diagonal_edges_df data frame with coordinates of diagonal line ends.
#' can also be obtained by \code{\link[magick]{image_morphology}}.
#' @param clean_lab_df data of type 'data.frame', containing the x, y and value
#' information of every labeled region in an image. (only the edges should be
#' labeled)
#' @param lineends_cimg image with dimensions of the image with discontinuous
#' edges. just for giving the dimensions of the output matrix.
#' @param radius maximal radius that should be scanned for another cluster
#' @return binary matrix that can be applied as an overlay, for example with
#' \code{\link[imager]{imager.combine}} to fill the gaps between line ends.
#' @details
#' This function is intended to be part of the fillLineGaps function, which
#' does the thresholding and line end detection preprocessing. The
#' adaptiveInterpolation creates a matrix in the dimensions of the original
#' image. At the beginning there are only background values (0) = black image.
#' The function then searches for LineEnds and looks for a given radius around
#' this line end for the nearest labeled region. The own cluster of the line
#' end is of course not considered as nearest neighbor.If another cluster is
#' found, the interpolatePixels function is used to connect the line end to the
#' found cluster. This means that specified pixels of the matrix are
#' transformed to a foreground value of (1). The diagonal line ends get a
#' special treatment, because for the labeling function,7 the diagonal pixels
#' are always treated as a separate cluster, which makes them difficult to
#' reconnect. To deal with this problem, diagonal line ends ignore not only
#' their cluster, but also the cluster of the direct neighbor. Thereafter,
#' the same procedure as before is repeated, where pixel values are changed
#' according to the interpolatePixel function.
#' @examples
#' # creating an artificial binary image
#' mat <- matrix(0, 8, 8)
#' mat[3, 1:2] <- 1
#' mat[4, 3] <- 1
#' mat[7:8, 3] <- 1
#' mat[5, 6:8] <- 1
#' mat_cimg <- as.cimg(mat)
#'
#' # preprocessing / LineEnd detection / labeling  (done in fillLineGaps)
#' mat_cimg_m <- mirror(mat_cimg, axis = "x")
#' mat_magick <- cimg2magick(mat_cimg)
#' lineends <- image_morphology(mat_magick, "HitAndMiss", "LineEnds")
#' diagonalends <- image_morphology(mat_magick, "HitAndMiss", "LineEnds:2>")
#' lineends_cimg <- magick2cimg(lineends)
#' diagonalends_cimg <- magick2cimg(diagonalends)
#' end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
#' end_points_df <- as.data.frame(end_points)
#' colnames(end_points_df) <- c("x", "y", "dim3", "dim4")
#' diagonal_edges <- which(diagonalends_cimg == TRUE, arr.ind = TRUE)
#' diagonal_edges_df <- as.data.frame(diagonal_edges)
#' colnames(diagonal_edges_df) <- c("x", "y", "dim3", "dim4")
#' lab <- label(mat_cimg_m)
#' df_lab <- as.data.frame(lab) |> subset(value > 0)
#' alt_x <- list()
#' alt_y <- list()
#' alt_value <- list()
#' for (g in 1:nrow(df_lab)) {
#'   if (mat_cimg_m[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
#'     alt_x[g] <- df_lab$x[g]
#'     alt_y[g] <- df_lab$y[g]
#'     alt_value[g] <- df_lab$value[g]
#'   }
#' }
#' clean_lab_df <- data.frame(
#'   x = unlist(alt_x),
#'   y = unlist(alt_y),
#'   value = unlist(alt_value)
#' )
#'
#' # actual function
#' overlay <- adaptiveInterpolation(
#'   end_points_df,
#'   diagonal_edges_df,
#'   clean_lab_df,
#'   mat_cimg
#' )
#' parmax(list(mat_cimg_m, as.cimg(overlay$overlay))) |> plot()
#' @export
adaptiveInterpolation <- function(end_points_df,
                                  diagonal_edges_df,
                                  clean_lab_df,
                                  lineends_cimg,
                                  radius = 5) {
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
    for (b in 2:radius) {
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
      xy_pos <- which(clean_lab_df$x == x & clean_lab_df$y == y)

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
          clus_ign <- which(one_for_all != xy_pos)
          dia_ignore <- one_for_all[clus_ign]
        }
        connector_pos <- which(
          clean_lab_df[one_for_all, ]$value !=
            clean_lab_df[xy_pos, ]$value &
            clean_lab_df[one_for_all, ]$value !=
              clean_lab_df[dia_ignore, ]$value
        )
      } else {
        connector_pos <- which(clean_lab_df[one_for_all, ]$value !=
          clean_lab_df[xy_pos, ]$value)
      }
      # get the line end surrounding cluster(s) that are not equal to the
      # cluster of the line end
      connector <- one_for_all[connector_pos]

      # use the interpolatePixels function to connect two coordinates with each
      # other the coordinates obtained by interpolatePixels are used for
      # changing the value of th coordinates in the matrix
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
