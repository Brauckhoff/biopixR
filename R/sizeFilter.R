#' Size-based exclusion
#'
#' Calculates the size of the objects in an image and discards objects based
#' on a lower and an upper size limit.
#' (Input can be obtained by \code{\link[biopixR]{objectDetection}} function)
#' @param centers center coordinates of objects (needs to include 'value'
#' representing the center number)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param lowerlimit smallest accepted object size (numeric / 'auto')
#' @param upperlimit highest accepted object size (numeric / 'auto')
#' @returns list of 3 objects:
#' 1. remaining centers after discarding according to size
#' 2. remaining coordinates after discarding according to size
#' 3. size of remaining objects
#' @details
#' The `sizeFilter()` function is designed to filter detected objects based on
#' their size, either through automated detection or user-defined limits. The
#' automated detection of size limits uses the 1.5*IQR method to identify and
#' remove outliers. This approach is most effective when dealing with a large
#' number of objects, (typically more than 50), and when the sizes of the
#' objects are relatively uniform. For smaller samples or when the sizes of
#' the objects vary significantly, the automated detection may not be as
#' accurate, and manual limit setting is recommended.
#' @importFrom stats sd
#' @importFrom stats quantile
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_sizeFilter <- sizeFilter(
#'   centers = res_objectDetection$centers,
#'   coordinates = res_objectDetection$coordinates,
#'   lowerlimit = 50, upperlimit = 150
#'   )
#' changePixelColor(
#'   beads,
#'   res_sizeFilter$coordinates,
#'   color = "darkgreen",
#'   visualize = TRUE
#'   )
#' @export
sizeFilter <- function(centers,
                       coordinates,
                       lowerlimit = "auto",
                       upperlimit = "auto") {
  # Assign input arguments to local variables
  center_df <- centers
  xy_coords <- coordinates

  # Error handling: Both limits must be set to 'auto' or selected individually
  if (lowerlimit == "auto" & upperlimit != "auto") {
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }
  if (lowerlimit != "auto" & upperlimit == "auto") {
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }

  # Automated limit calculation
  if (lowerlimit == "auto" & upperlimit == "auto") {
    # Calculate the size of the detected objects
    cluster_size <- list()
    for (c in center_df$value) {
      for (e in xy_coords$value) {
        if (c == e) {
          clus_pxl <- which(xy_coords$value == c)
          size <- length(clus_pxl)
          if (is.null(size) != TRUE) {
            cluster_size[c] <- c(size)
          }
        }
      }
    }

    data <- unlist(cluster_size)

    # Calculate quartiles
    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)

    # Error handling: Warn if the number of detected objects is less than 50
    if (nrow(center_df) < 50) {
      data |> plot(ylab = "size in px")
      warning(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Number of detected objects should be >50 for automated detection"
      )
    }

    # Calculate IQR
    iqr <- q3 - q1

    # Identify non-outliers using the 1.5*IQR rule
    no_outliers <-
      which(data > (q1 - 1.5 * iqr) & data < (q3 + 1.5 * iqr))

    # Filter results to include only non-outliers
    cluster_size <- cluster_size[no_outliers]
    res_centers <- center_df[no_outliers]

    # Extract remaining coordinates
    cluster <- list()
    x_coord <- list()
    y_coord <- list()
    for (f in seq_along(res_centers$value)) {
      remaining_pos <- which(xy_coords$value == f)
      cluster[remaining_pos] <- c(f)
      x_coord[remaining_pos] <- xy_coords$x[remaining_pos]
      y_coord[remaining_pos] <- xy_coords$y[remaining_pos]
    }
    res_xy_coords <- data.frame(
      x = unlist(x_coord),
      y = unlist(y_coord),
      value = unlist(cluster)
    )
  }

  # Manual limit calculation
  if (lowerlimit != "auto" &
      upperlimit != "auto") {
    # Initialize a list to store cluster sizes
    # Extract all coordinates of the clusters within the size limits
    cluster_size <- vector("list", length = nrow(center_df))
    for (c in center_df$value) {
      for (e in xy_coords$value) {
        if (c == e) {
          clus_pxl <- which(xy_coords$value == c)
          size <- length(clus_pxl)
          if (is.null(size) != TRUE &
              size < upperlimit & size > lowerlimit) {
            cluster_size[c] <- c(size)
          }
        }
      }
    }

    # Get cluster numbers of remaining clusters after exclusion
    clus_num <- list()
    for (f in center_df$value) {
      if (is.null(cluster_size[[f]]) != TRUE) {
        clus_num[f] <- c(f)
      }
    }

    # Create a new data frame that contains objects that pass the size-based exclusion
    cluster <- list()
    x_coord <- list()
    y_coord <- list()
    for (g in unlist(clus_num)) {
      remaining_pos <- which(xy_coords$value == g)
      cluster[remaining_pos] <- c(g)
      x_coord[remaining_pos] <- xy_coords$x[remaining_pos]
      y_coord[remaining_pos] <- xy_coords$y[remaining_pos]
    }

    res_xy_coords <- data.frame(
      x = unlist(x_coord),
      y = unlist(y_coord),
      value = unlist(cluster)
    )

    # Create a data table with remaining center coordinates
    DT <- data.table(res_xy_coords)

    # Summarize by cluster and calculate center
    res_centers <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  # Return the filtered centers, coordinates, and sizes as a list
  out <- list(centers = res_centers,
              coordinates = res_xy_coords,
              size = cluster_size)
}
