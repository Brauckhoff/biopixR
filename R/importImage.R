#' Import an Image File
#'
#' This function is a wrapper to the \code{\link[imager]{load.image}} and
#' \code{\link[magick]{image_read}} functions, and imports an image file and
#' returns the image as a 'cimg' object. The following file formats are
#' supported: TIFF, PNG, JPG/JPEG, and BMP. In the event that the image in
#' question contains an alpha channel, that channel is omitted.
#' @param path2file path to file
#' @returns An image of class 'cimg'.
#' @import magick
#' @import imager
#' @importFrom tools file_ext
#' @examples
#' path2img <- system.file("images/beads_large1.bmp", package = 'biopixR')
#' img <- importImage(path2img)
#' img |> plot()
#'
#' path2img <- system.file("images/beads_large2.png", package = 'biopixR')
#' img <- importImage(path2img)
#' img |> plot()
#' @export
importImage <- function(path2file) {
  # Extract the file extension and convert it to lowercase
  file_ext <- tolower(tools::file_ext(path2file))

  # Check the file extension and process the image accordingly
  if (file_ext %in% c("tif", "tiff")) {
    # For TIFF files, read the image using magick and convert to cimg
    img <- image_read(path2file)
    img <- magick2cimg(img)
  } else if (file_ext == "png") {
    # For PNG files, load the image using imager
    img <- load.image(path2file)
  } else if (file_ext == "jpg" | file_ext == "jpeg") {
    # For JPG/JPEG files, load the image using imager
    img <- load.image(path2file)
  } else if (file_ext == "bmp") {
    # For BMP files, load the image using imager
    img <- load.image(path2file)
  } else {
    # If the file extension is not supported, throw an error
    stop("Unsupported file format: ",
         file_ext,
         ". Please use TIFF, PNG, JPG or BMP.")
  }

  # Check if the image has an alpha channel (4th dimension length greater than 3)
  if (dim(img)[4] > 3) {
    # Discard the alpha channel by subsetting the image to only include the first three channels (R, G, B)
    img <- img[1:dim(img)[1], 1:dim(img)[2], 1, 1:3]

    # Ensure the array has the correct dimensions expected by the cimg object
    img <- array(img, dim = c(dim(img)[1], dim(img)[2], 1, 3))

    # Convert the modified array back to a cimg object
    img <- as.cimg(img)
  }

  # Return the image
  return(img)
}
