#' Change the color of pixels
#'
#' Can be used to change the color of specified pixels in an image. The
#' coordinates of the pixels are needed to colorize them.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param coordinates specifying which pixels to be colored (should
#' be a X|Y data frame (first column: X; second column: Y)).
#' @param color color with which to replace specified pixels. can be either an
#' RGB triplet or one of the colors listed by \code{\link[grDevices]{colors}}.
#' @param visualize if TRUE the resulting image gets plotted
#' @returns
#' 'cimg' with changed colors at desired positions and a plot of the 'cimg'
#' @importFrom grDevices col2rgb
#' @examples
#' coordinates <- objectDetection(beads)
#' changePixelColor(beads, coordinates$coordinates)
#' @references https://CRAN.R-project.org/package=countcolors
#' @export
changePixelColor <-
  function(img,
           coordinates,
           color = "purple",
           visualize = FALSE) {
    # This code is partially based on the changePixelColor function from the 'countcolors' package (v0.9.1)

    # Ensure the image is of type 'cimg'
    if (class(img)[1] != "cimg") {
      stop("image must be of class 'cimg'")
    }

    # Check if the image has the expected three color channels (RGB)
    img_dim <- dim(img)
    if (img_dim[4] != 3) {
      img <- add.colour(img) # Function to add color channels if missing
    }

    # Convert the image to an array format for pixel manipulation
    img_array <- as.array(img)

    # Convert color input to an RGB numeric vector, normalizing if necessary
    if (is.character(color)) {
      color <-
        as.vector(col2rgb(color) / 255) # Convert color name to RGB
    }

    # Check that the color vector is correctly formatted
    if (length(color) != 3) {
      stop(
        "'color' must be a numeric vector of length 3 with
             values between 0 and 1 or one of the colors listed by colors()"
      )
    } else if (range(color)[2] > 1) {
      color <- color / 255 # Normalize if values exceed 1
    }

    # Apply the new color to the specified pixels
    for (i in 1:nrow(coordinates)) {
      img_array[coordinates[i, 1], coordinates[i, 2], 1, 1:3] <- color
    }

    # Optionally display the modified image
    if (visualize == TRUE) {
      as.cimg(img_array) |> plot() # Convert array back to 'cimg' and plot
    }

    # Return the modified image as a 'cimg' object
    out <- as.cimg(img_array)
  }
