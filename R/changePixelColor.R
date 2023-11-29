#' Visualization for the fillLineGaps function
#'
#' Can be generally used to change color of specified pixels in an image. In
#' this Package it is meant to visualize and highlight the pixels that where
#' added by the fillLineGaps function.
#' @param image image
#' @param coord Coordinates specifying which pixels to be colored (should
#' be a X|Y Data frame)
#' @param color Color with which to replace specified pixels. Can be either a
#' an RGB triplet or one of the colors listed by \code{\link[grDevices]{colors}}.
#' @returns
#' plot displaying the image with highlighted pixels at desired coordinates
#' @examples
#' # to do
#' @references https://CRAN.R-project.org/package=countcolors
#' @export
changePixelColor <- function(image, coords, color = "green") {
  # transform image to array so that transformation of pixels becomes available
  img_array <- as.array(image)

  # Make sure target color is or can be coerced to an RGB triplet
  if (is.character(color)) {
    color <- as.vector(col2rgb(color) / 255)
  }

  # check that vector is of appropriate length and has a 0-1 (not 0-255) range
  if (length(color) != 3) {
    stop(
      "'color' must be a numeric vector of length 3 with
             values between 0 and 1 or one of the colors listed by colors()"
    )
  } else if (range(color)[2] > 1) {
    color <- color / 255
  }

  # change specified pixels to target color
  for (i in 1:nrow(coords)) {
    img_array[coords[i, 1], coords[i, 2], 1, 1:3] <- color
  }

  # display result
  as.cimg(img_array) |> plot()
}
