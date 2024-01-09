#' Object detection
#'
#' This function identifies objects in an image using edge detection and
#' labeling, gathering the coordinates and centers of the identified objects.
#' Detected objects are then highlighted with colored circles for easy
#' recognition.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. image were beads are marked by a purple circle
#' @import data.table
#' @import imager
#' @examples
#' objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' @export
objectDetection <- function(img,
                            alpha = 0.75,
                            sigma = 0.1) {
  # assign import
  object_img <- img

  # check class of import
  if (class(object_img)[1] != "cimg") {
    stop(
      "image must be of class 'cimg'"
    )
  }

  # in case the image is from a luminescence channel transform to gray scale
  if (dim(object_img)[4] != 1) {
    object_img <- grayscale(object_img)
  }

  # edge detection with default: alpha = 0.75, sigma = 0.1
  edge_img <- edgeDetection(object_img, alpha = alpha, sigma = sigma)

  # fill detected edges and label areas
  first_lab <- label(edge_img)
  fill_hulls <- which(first_lab != 0)
  filled_img <- edge_img
  filled_img[fill_hulls] <- TRUE
  labeled_img <- label(filled_img)

  # create data frame without background
  df_lab_img <- as.data.frame(labeled_img) |>
    subset(value > 0)
  DT <- data.table(df_lab_img)

  # summarize by cluster and calculate center
  grouped_lab_img <-
    DT[, list(mx = mean(x), my = mean(y)), by = value]

  # visualization by highlighting the edges of detected beads
  edge_coords <- which(edge_img == TRUE, arr.ind = TRUE)
  colored_edge <- changePixelColor(object_img, edge_coords, color = "purple")

  out <- list(
    centers = grouped_lab_img,
    coordinates = df_lab_img,
    marked_beads = colored_edge
  )
}
