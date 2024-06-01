#' Change the color of pixels
#'
#' The function allows the user to alter the color of a specified set of pixels
#' within an image. In order to achieve this, the coordinates of the pixels in
#' question must be provided.
#' @param img image (import by \code{\link[biopixR]{importImage}})
#' @param coordinates specifying which pixels to be colored (should
#' be a x|y data frame).
#' @param color color to be applied to specified pixels:
#' \itemize{
#'   \item color from the list of colors defined by \code{\link[grDevices]{colors}}
#'   \item object of class factor
#' }
#' @param visualize if TRUE the resulting image gets plotted
#' @returns Object of class 'cimg' with changed colors at desired positions.
#' @import imager
#' @importFrom grDevices col2rgb rainbow
#' @importFrom stats na.omit setNames
#' @examples
#' coordinates <-
#'   objectDetection(beads,
#'                   method = 'edge',
#'                   alpha = 1,
#'                   sigma = 0)
#' changePixelColor(
#'   beads,
#'   coordinates$coordinates,
#'   color = factor(coordinates$coordinates$value),
#'   visualize = TRUE
#' )
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

    # Convert color input to an RGB numeric vector
    if (is.character(color)) {
      # Convert color name to RGB and normalize values
      color <-
        as.vector(col2rgb(color) / 255) # Convert color name to RGB


      # Check that the color vector is correctly formatted
      if (length(color) != 3) {
        stop(
          "'color' must be a numeric vector of length 3 with
             values between 0 and 1 or one of the colors listed by colors()"
        )
      } else if (range(color)[2] > 1) {
        color <- color / 255 # Normalize if values exceed 1
      }
    }

    # Convert the color factor to character and then map to the provided color palette
    if (is.factor(color)) {
      # Generate a color palette dynamically based on the number of levels in color
      num_levels <- length(levels(color))
      color_palette <- sample(rainbow(num_levels)) # Use sample() to randomize the rainbow palette

      # Map factor levels to colors
      color_map <- setNames(color_palette, levels(color))
      colors_rgb <-
        sapply(color_map, function(col)
          as.vector(col2rgb(col) / 255), simplify = FALSE)
      color_factor <- color
    }

    # Apply the new color to the specified pixels
    for (i in seq_len(nrow(coordinates))) {
      if (exists("color_factor")) {
        color <- colors_rgb[[as.character(color_factor[i])]]
      }

      # Assign the new color to the specified pixel
      img_array[coordinates[i, 1], coordinates[i, 2], 1, 1:3] <-
        color
    }

    # Optionally display the modified image
    if (visualize == TRUE) {
      as.cimg(img_array) |> plot() # Convert array back to 'cimg' and plot
    }

    # Return the modified image as a 'cimg' object
    out <- as.cimg(img_array)
  }
