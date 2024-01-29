#' Size-based exclusion
#'
#' Calculates the size of the objects in an image and discards objects based
#' on a lower and an upper size limit. (Input can be obtained by objectDetection function)
#' @param centers center coordinates of objects (needs to include 'value'
#' representing the center number)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param lowerlimit smallest accepted object size (when 'auto' both limits are
#' calculated by using the mean and the standard deviation)
#' @param upperlimit highest accepted object size
#' @returns list of 3 objects:
#' 1. remaining centers after discarding according to size
#' 2. remaining coordinates after discarding according to size
#' 3. size of remaining objects
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
  # assign imports
  center_df <- centers
  xy_coords <- coordinates

  # errors
  if (lowerlimit == "auto" & upperlimit != "auto") {
    stop(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }
  if (lowerlimit != "auto" & upperlimit == "auto") {
    stop(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }

  # automated limit calculation
  # calculating the size of the detected objects
  if (lowerlimit == "auto" & upperlimit == "auto") {
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

    # calculating quartiles
    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)

    # error with small n
    # plots distribution of size in order to simplify manual selection of limits
    if (nrow(center_df) < 50) {
      data |> plot(ylab = "size in px")
      warning(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Number of detected objects should be >50 for automated detection"
      )
    }

    # Calculate IQR
    iqr <- q3 - q1

    # Identify non outliers
    no_outliers <- which(data > (q1 - 1.5 * iqr) & data < (q3 + 1.5 * iqr))

    # results: size & remaining centers
    cluster_size <- cluster_size[no_outliers]
    res_centers <- center_df[no_outliers]

    # result: remaining coordinates
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

  if (lowerlimit != "auto" &
    upperlimit != "auto") {
    # aim: extract all coordinates (pixels) of the clusters
    # how many coordinates per cluster & cluster number that is in the list of
    # the remaining clusters (center_df) -> count = size of cluster
    # size has an upper and lower limit
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

    # getting cluster numbers of remaining clusters after exclusion due to
    # cluster size
    clus_num <- list()
    for (f in center_df$value) {
      if (is.null(cluster_size[[f]]) != TRUE) {
        clus_num[f] <- c(f)
      }
    }

    # creating new data frame that contains objects that pass the size-based exclusion
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

    # creating data frame with remaining center coordinates
    DT <- data.table(res_xy_coords)

    # summarize by cluster and calculate center
    res_centers <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  out <- list(
    centers = res_centers,
    coordinates = res_xy_coords,
    size = cluster_size
  )
}
