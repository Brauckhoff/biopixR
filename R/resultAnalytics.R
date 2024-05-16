#' Result Extraction and Summary
#'
#' This function summarizes the data obtained by previous functions:
#' \code{\link[biopixR]{objectDetection}}, \code{\link[biopixR]{proximityFilter}}
#' or \code{\link[biopixR]{sizeFilter}}. Extracts information like amount,
#' intensity, size and density.
#' @param unfiltered all coordinates from every object before applying filter functions
#' @param coordinates all filtered coordinates of the objects (x|y|value data frame)
#' @param size size of the objects
#' @param img image (import by \code{\link[imager]{load.image}})
#' @returns list of 2 objects:
#' 1. summary of all the objects in the image
#' 2. detailed information about every single object
#' @import data.table
#' @seealso [objectDetection()], [sizeFilter()], [proximityFilter()]
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_sizeFilter <- sizeFilter(
#'   res_objectDetection$centers,
#'   res_objectDetection$coordinates,
#'   lowerlimit = 50, upperlimit = 150
#'   )
#' res_proximityFilter <- proximityFilter(
#'   res_sizeFilter$centers,
#'   res_objectDetection$coordinates,
#'   radius = "auto"
#'   )
#' res_resultAnalytics <- resultAnalytics(
#'   unfiltered = res_objectDetection$coordinates,
#'   coordinates = res_proximityFilter$coordinates,
#'   size = res_proximityFilter$size,
#'   img = beads
#'   )
#' plot(beads)
#' with(
#'   res_objectDetection$centers,
#'   points(
#'     res_objectDetection$centers$mx,
#'     res_objectDetection$centers$my,
#'     col = "red",
#'     pch = 19
#'     )
#'   )
#' with(
#'   res_resultAnalytics$detailed,
#'   points(
#'     res_resultAnalytics$detailed$x,
#'     res_resultAnalytics$detailed$y,
#'     col = "darkgreen",
#'     pch = 19
#'     )
#'   )
#' @export
resultAnalytics <- function(unfiltered,
                            coordinates,
                            size,
                            img) {
  # Binding for global variables
  intensity <- cluster <- NULL

  # Assign input arguments to local variables
  all_coords <- unfiltered
  xy_coords <- coordinates
  cluster_size <- size
  pic <- img

  # Include intensity values of pixels from remaining clusters in a data frame
  for (h in 1:nrow(xy_coords)) {
    x <- xy_coords$x[h]
    y <- xy_coords$y[h]
    int <- as.array(pic)[x, y, ,]
    xy_coords$intensity[h] <- c(int)
  }

  # Group data frame by cluster and calculate mean coordinates and intensity
  DT_intense <- data.table(xy_coords)
  intense <- DT_intense[, list(
    x = mean(x),
    y = mean(y),
    intensity = mean(intensity),
    sd_intensity = sd(intensity)
  ),
  by = value]

  # Adapt size according to remaining coordinates if necessary
  if (length(cluster_size) != nrow(intense)) {
    cluster_size <- unlist(cluster_size[intense$value])
  }

  # Approximate the number of discarded pixels
  # Calculate the number of true coordinates
  amount_true <- nrow(all_coords)
  dis_count <-
    round(amount_true / mean(unlist(size)) - nrow(intense))

  # Summary for every passing objects
  res_df_long <- data.frame(
    objectnumber = intense$value,
    size = unlist(cluster_size),
    intensity = intense$intensity,
    sd_intensity = intense$sd_intensity,
    x = intense$x,
    y = intense$y
  )

  # Summary of res_df_long / whole image
  result <- data.frame(
    number_of_objects = nrow(intense),
    mean_size = mean(unlist(cluster_size)),
    sd_size = sd(unlist(cluster_size)),
    mean_intensity = mean(xy_coords$intensity),
    sd_intensity = sd(xy_coords$intensity),
    estimated_rejected = dis_count,
    coverage = sum(res_df_long$size) / (width(img) * height(img))
  )

  # Return the summary and detailed results as a list
  out <- list(summary = result,
              detailed = res_df_long)
}
