#' Bead Detection
#'
#' Detects centers of beads
#' @param image image
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @returns list of 3 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. original image
#' @import imager
#' @import EBImage
#' @import data.table
#' @examples
#' detecteR(beads, alpha = 0.75, sigma = 0.1)
#' @export
detecteR <- function(image, alpha = 0.75, sigma = 0.1) {
  # first section: detect all beads
  img <- image

  # edge detection with default: alpha = 0.75, sigma = 0.1
  edge_img <- cannyEdges(img, alpha = alpha, sigma = sigma)
  # fill detected edges and label areas
  filled_img <- fillHull(edge_img)
  labeled_img <- label(filled_img)

  # create data frame without background
  df_lab_img <- as.data.frame(labeled_img) |>
    subset(value > 0)
  DT <- data.table(df_lab_img)

  # summarize cluster and calculate center
  grouped_lab_img <- DT[ , .(mxx = mean(x), myy = mean(y)), by = value]
  out <- list(centers = grouped_lab_img,
              coordinates = df_lab_img,
              image = img)

  out
}
