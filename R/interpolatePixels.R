#' Pixel Interpolation
#'
#' Connects two points in a matrix, array, or an image.
#' @param row1 first row: together with col1 coordinate for the first point
#' @param col1 first column: together with row1 coordinate for the first point
#' @param row2 second row: together with col2 coordinate for the second point
#' @param col2 second column: together with row2 coordinate for the second point
#' @returns
#' matrix containing the coordinates to connect the two input points
#' @examples
#' test <- matrix(0, 4, 4)
#' test[1, 1] <- 1
#' test[3, 4] <- 1
#' link <- interpolatePixels(1, 1, 3, 4)
#' test[link] <- 1
#' @export
interpolatePixels <- function(row1, col1, row2, col2) {
  # calculate the number of points needed for interpolation
  num_points <- max(abs(row2 - row1), abs(col2 - col1)) + 1

  # generate linearly spaced coordinates
  rows <- round(seq(row1, row2, length.out = num_points))
  cols <- round(seq(col1, col2, length.out = num_points))

  # combine row and column coordinates into a matrix
  interpolated_pixels <- cbind(rows, cols)

  return(interpolated_pixels)
}
