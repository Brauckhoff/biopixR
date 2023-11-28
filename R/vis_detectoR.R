#' Visualization of Bead Detection
#'
#' Visualizes the centers of detected beads
#' @param image image
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @returns list of 1 objects:
#' 1. image were beads are marked by a purple circle
#' @import imager
#' @import data.table
#' @examples
#' vis_detectoR(beads, alpha = 0.75, sigma = 0.1)
#' @export
vis_detectoR <- function(image,
                         alpha = 0.75,
                         sigma = 0.1) {
  # assigning import
  img <- image

  # edge detection followed by filling of circular objects,
  # resulting in binary image
  edge_img <- cannyEdges(img, alpha = alpha, sigma = sigma)
  first_lab <- label(edge_img)
  fill_hulls <- which(first_lab != 0)
  filled_img <- edge_img
  filled_img[fill_hulls] <- TRUE

  # labeling the filled objects (beads) and calculate their center coordinates
  labeled_img <- label(filled_img)
  df_lab_img <- as.data.frame(labeled_img) |>
    subset(value > 0)
  DT <- data.table(df_lab_img)
  grouped_lab_img <-
    DT[, .(mxx = mean(x), myy = mean(y)), by = value]

  # visualization by drawing colored circles around the center of detected beads
  img_rgb <- add.color(img, simple = TRUE)
  circ_img <- draw_circle(
    img_rgb,
    grouped_lab_img$mxx,
    grouped_lab_img$myy,
    radius = 2,
    filled = TRUE,
    color = "purple"
  )
  out <- list(marked_beads = circ_img)
  out
}
