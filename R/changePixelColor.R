#' Change the color of pixels
#'
#' Can be used to change the color of specified pixels in an image. The coordinates
#' of the pixels are needed to colorize them.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param coords coordinates specifying which pixels to be colored (should
#' be a X|Y Data frame (first column: X; second column: Y)).
#' @param color color with which to replace specified pixels. can be either a
#' an RGB triplet or one of the colors listed by \code{\link[grDevices]{colors}}.
#' @param visualize if TRUE the resulting image gets plotted
#' @returns
#' cimg with changed colors at desired positions and plot of the cimg
#' @importFrom grDevices col2rgb
#' @examples
#' coordinates <- objectDetection(beads)
#' changePixelColor(beads, coordinates$coordinates)
#' @references https://CRAN.R-project.org/package=countcolors
#' @export
changePixelColor <- function(img, coords, color = "purple", visualize = FALSE) {
  # this code is partialy based on the changePixelColor function from the
  # countcolors package (v0.9.1)
  # check class of import
  if (class(img)[1] != "cimg") {
    stop(
      "image must be of class 'cimg'"
    )
  }

  # check if color channels are present and if not add them
  img_dim <- dim(img)
  if (img_dim[4] != 3) {
    img <- add.colour(img)
  }

  # transform image to array so that transformation of pixels becomes available
  img_array <- as.array(img)

  # make sure target color is or can be coerced to an RGB triplet
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

  if (visualize == TRUE) {
    # display result
    as.cimg(img_array) |> plot()
  }

  out <- as.cimg(img_array)
}
