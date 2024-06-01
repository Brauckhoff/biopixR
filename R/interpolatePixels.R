#' Pixel Interpolation
#'
#' Connects two points in a matrix, array, or an image.
#' @param row1 row index for the first point
#' @param col1 column index for the first point
#' @param row2 row index for the second point
#' @param col2 column index for the second point
#' @returns
#' Matrix containing the coordinates to connect the two input points.
#' @examples
#' # Simulate two points in a matrix
#' test <- matrix(0, 4, 4)
#' test[1, 1] <- 1
#' test[3, 4] <- 1
#' as.cimg(test) |> plot()
#'
#' # Connect them with each other
#' link <- interpolatePixels(1, 1, 3, 4)
#' test[link] <- 1
#' as.cimg(test) |> plot()
#' @export
interpolatePixels <- function(row1, col1, row2, col2) {
  # Calculate the maximum number of points required for interpolation between
  # two points
  num_points <- max(abs(row2 - row1), abs(col2 - col1)) + 1

  # Generate linearly spaced row coordinates from row1 to row2, with length
  # equal to num_points
  rows <- round(seq(row1, row2, length.out = num_points))

  # Similarly, generate linearly spaced column coordinates from col1 to col2
  cols <- round(seq(col1, col2, length.out = num_points))

  # Combine the row and column coordinates to form a 2-column matrix, where the
  # first column is row indices and the second column is column indices. Each
  # row in this matrix represents a pixel coordinate on a linear path between
  # the start and end points.
  interpolated_pixels <- cbind(rows, cols)

  # Return the matrix of interpolated pixel coordinates.
  return(interpolated_pixels)
}
