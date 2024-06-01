#' Connects Line Ends with the nearest labeled region
#'
#' The function scans an increasing radius around a line end and connects it
#' with the nearest labeled region.
#' @param end_points_df \code{data.frame} with the coordinates of all line ends
#' (can be obtained by using \code{\link[magick]{image_morphology}})
#' @param diagonal_edges_df \code{data.frame} with coordinates of diagonal line ends
#' (can also be obtained by using \code{\link[magick]{image_morphology}})
#' @param clean_lab_df data of type \code{data.frame}, containing the x, y and value
#' information of every labeled region in an image (only the edges should be
#' labeled)
#' @param img image providing the dimensions of the output matrix
#' (import by \code{\link[biopixR]{importImage}})
#' @param radius maximal radius that should be scanned for another cluster
#' @return Binary matrix that can be applied as an overlay, for example with
#' \code{\link[imager]{imager.combine}} to fill the gaps between line ends.
#' @details
#' This function is designed to be part of the
#' \code{\link[biopixR]{fillLineGaps}} function, which performs the thresholding
#' and line end detection preprocessing. The
#' \code{\link[biopixR]{adaptiveInterpolation}} generates a matrix with
#' dimensions matching those of the original image. Initially, the matrix
#' contains only background values (0) corresponding to a black image. The
#' function then searches for line ends and identifies the nearest labeled
#' region within a given radius of the line end. It should be noted that the
#' cluster of the line end in question is not considered a nearest neighbor. In
#' the event that another cluster is identified, the
#' \code{\link[biopixR]{interpolatePixels}} function is employed to connect the
#' line end to the aforementioned cluster. This entails transforming the
#' specified pixels of the matrix to a foreground value of (1).
#' It is important to highlight that diagonal line ends receive a special
#' treatment, as they are always treated as a separate cluster by the labeling
#' function. This makes it challenging to reconnect them. To address this issue,
#' diagonal line ends not only ignore their own cluster but also that of their
#' direct neighbor. Thereafter, the same procedure is repeated, with pixel
#' values being changed according to the
#' \code{\link[biopixR]{interpolatePixels}} function.
#' @examples
#' # Creating an artificial binary image
#' mat <- matrix(0, 8, 8)
#' mat[3, 1:2] <- 1
#' mat[4, 3] <- 1
#' mat[7:8, 3] <- 1
#' mat[5, 6:8] <- 1
#' mat_cimg <- as.cimg(mat)
#' plot(mat_cimg)
#'
#' # Preprocessing / LineEnd detection / labeling (done in fillLineGaps())
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
#' for (g in seq_len(nrow(df_lab))) {
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
#' # Actual function
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
                                  img,
                                  radius = 5) {
  # Initialize a matrix to represent the image, filled with zeros (background)
  connected_components <- matrix(0,
                                 nrow = nrow(img),
                                 ncol = ncol(img))

  # Iterate over all end points specified in the end_points_df data frame
  for (a in seq_len(nrow(end_points_df))) {
    x <- end_points_df$x[a]
    y <- end_points_df$y[a]

    # Iterate from a radius of 2 to the specified radius around each end point
    for (b in 2:radius) {
      # Find indices of points within a square (radius 'b') around the current
      # end point
      one_for_all <- which(
        clean_lab_df$x > (x - b) &
          clean_lab_df$x < (x + b) &
          clean_lab_df$y > (y - b) &
          clean_lab_df$y < (y + b)
      )

      # Locate the current end point in the clean_lab_df
      xy_pos <- which(clean_lab_df$x == x & clean_lab_df$y == y)

      # Check for diagonal line ends by merging current end point with
      # diagonal_edges_df
      if (isTRUE(nrow(merge(end_points_df[a, ], diagonal_edges_df)) >
                 0) == TRUE) {
        if (b == 2) {
          # Ignore the cluster of the current end point and the first
          # neighboring cluster
          clus_ign <- which(one_for_all != xy_pos)
          dia_ignore <- one_for_all[clus_ign]
        }
        # Identify connecting positions excluding the ignored clusters
        connector_pos <- which(
          clean_lab_df[one_for_all, ]$value !=
            clean_lab_df[xy_pos, ]$value &
            clean_lab_df[one_for_all, ]$value !=
            clean_lab_df[dia_ignore, ]$value
        )
      } else {
        # Identify connecting positions excluding the cluster of the current
        # end point
        connector_pos <- which(clean_lab_df[one_for_all, ]$value !=
                                 clean_lab_df[xy_pos, ]$value)
      }

      # Get indices of clusters that can connect to the current end point
      connector <- one_for_all[connector_pos]

      # Connect the current end point to each found connector using
      # interpolatePixels function
      if (length(connector) != 0) {
        for (c in connector) {
          interpolated_pixels <- interpolatePixels(x, y,
                                                   clean_lab_df[c, ]$x,
                                                   clean_lab_df[c, ]$y)
          # Update the connected_components matrix at the interpolated positions
          connected_components[interpolated_pixels] <- 1
        }
        break
      }
    }
  }
  # Return a list containing the matrix with connected components
  out <- list(overlay = connected_components)
  out
}
