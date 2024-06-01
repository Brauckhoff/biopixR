#' Result Calculation and Summary
#'
#' This function summarizes the data obtained by previous functions:
#' \code{\link[biopixR]{objectDetection}}, \code{\link[biopixR]{proximityFilter}}
#' or \code{\link[biopixR]{sizeFilter}}. Extracts information like amount,
#' intensity, size and density of the objects present in the image.
#' @param img image (import by \code{\link[biopixR]{importImage}})
#' @param coordinates all filtered coordinates of the objects (x|y|value data frame)
#' @param unfiltered all coordinates from every object before applying filter functions
#' @returns list of 2 objects:
#' \itemize{
#'   \item \code{summary}: A summary of all the objects in the image.
#'   \item \code{detailed}: Detailed information about every single object.
#' }
#' @details
#' The \code{\link[biopixR]{resultAnalytics}} function provides comprehensive
#' summary of objects detected in an image:
#' \enumerate{
#'   \item \strong{Summary}
#'   \itemize{
#'     \item Generates a summary of all detected objects, including the total number of objects, their mean size, size standard deviation, mean intensity, intensity standard deviation, estimated rejected objects, and coverage.
#'   }
#'   \item \strong{Detailed Object Information}
#'   \itemize{
#'     \item Provides detailed information for each object, including size, mean intensity, intensity standard deviation, and coordinates.
#'   }
#' }
#' @import data.table
#' @seealso [objectDetection()], [sizeFilter()], [proximityFilter()]
#' @examples
#' res_objectDetection <- objectDetection(beads,
#'                                        alpha = 1,
#'                                        sigma = 0)
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
#'   coordinates = res_proximityFilter$coordinates,
#'   unfiltered = res_objectDetection$coordinates,
#'   img = beads
#'   )
#' print(res_resultAnalytics$summary)
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
resultAnalytics <- function(img,
                            coordinates,
                            unfiltered = NULL) {
  # Binding for global variables
  intensity <- NULL

  # Assign input arguments to local variables
  xy_coords <- coordinates
  all_coords <- unfiltered
  object_img <- img

  # Include intensity values of pixels from remaining clusters in a data frame
  for (h in seq_len(nrow(xy_coords))) {
    x <- xy_coords$x[h]
    y <- xy_coords$y[h]
    int <- as.array(object_img)[x, y, , ]
    xy_coords$intensity[h] <- c(int)
  }

  # Group data frame by cluster and calculate mean coordinates and intensity
  DT_intense <- data.table(xy_coords)
  intense <- DT_intense[, list(
    x = mean(x),
    y = mean(y),
    intensity = mean(intensity),
    sd_intensity = sd(intensity),
    size = length(x)
  ),
  by = value]

  # Approximate the number of discarded pixels
  # Calculate the number of true coordinates
  amount_true <- nrow(all_coords)
  dis_count <-
    round(amount_true / mean(intense$size) - nrow(intense))

  # Summary for every passing objects
  res_df_long <- data.frame(
    objectnumber = intense$value,
    size = intense$size,
    intensity = intense$intensity,
    sd_intensity = intense$sd_intensity,
    x = intense$x,
    y = intense$y
  )

  # Summary of res_df_long / whole image
  result <- data.frame(
    number_of_objects = nrow(intense),
    mean_size = mean(intense$size),
    sd_size = sd(intense$size),
    mean_intensity = mean(xy_coords$intensity),
    sd_intensity = sd(xy_coords$intensity),
    estimated_rejected = ifelse(length(dis_count) == 0, 0, dis_count),
    coverage = sum(res_df_long$size) / (width(img) * height(img))
  )

  # Return the summary and detailed results as a list
  out <- list(summary = result,
              detailed = res_df_long)
}
