#' Object detection
#'
#' This function identifies objects in an image using edge detection and
#' labeling, gathering the coordinates and centers of the identified objects.
#' The edges of detected objects are then highlighted for easy recognition.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. size of labeled objects
#' 4. image were object edges (purple) and detected centers (green) are colored
#' @import data.table
#' @import imager
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_objectDetection$marked_beads |> plot()
#' @export
objectDetection <- function(img,
                            alpha = 1,
                            sigma = 2) {
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
    warning(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Image is from a luminescence channel and was converted into grayscale"
    )
  }

  # edge detection with default: alpha = 1, sigma = 2
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

  # size calculation, which is needed to calculate the radius
  cluster_size <- list()
  for (c in grouped_lab_img$value) {
    for (e in df_lab_img$value) {
      if (c == e) {
        clus_pxl <- which(df_lab_img$value == c)
        size <- length(clus_pxl)
        if (is.null(size) != TRUE) {
          cluster_size[c] <- c(size)
        }
      }
    }
  }

  # visualization by highlighting the edges of detected beads
  edge_coords <- which(edge_img == TRUE, arr.ind = TRUE)
  colored_edge <- changePixelColor(object_img, edge_coords, color = "purple")
  colored_edge <- draw_circle(colored_edge,
                              grouped_lab_img$mx,
                              grouped_lab_img$my,
                              radius = (sqrt(mean(unlist(cluster_size)) / pi) / 2),
                              color = "darkgreen")

  out <- list(
    centers = grouped_lab_img,
    coordinates = df_lab_img,
    size = unlist(cluster_size),
    marked_beads = colored_edge
  )
}
