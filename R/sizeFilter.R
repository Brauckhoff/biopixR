#' Size-based exclusion
#'
#' Calculates the size of the objects in an image and discards objects based
#' on a lower and an upper size limit. (Input can be obtained by objectDetection function)
#' @param centers center coordinates of objects (needs to include 'value'
#' representing the cluster number)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param lowerlimit smallest accepted object size (when 'auto' both limits are
#' calculated by using the IQR)
#' @param upperlimit highest accepted object size
#' @returns list of 2 objects:
#' 1. remaining coordinates after discarding according to size
#' 2. size of a labeled region
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' sizeFilter(centers = res_objectDetection$centers,
#' coordinates = res_objectDetection$coordinates,
#' lowerlimit = 50, upperlimit = 150)
#' @export
sizeFilter <- function(centers,
                       coordinates,
                       lowerlimit = 'auto',
                       upperlimit = 'auto') {
  # assign imports
  cluster_df <- centers
  xy_coords <- coordinates

  # errors
  if (lowerlimit == 'auto' & upperlimit != 'auto') {
    stop(
      "both limits needs to be 'auto' or indiviually be choosen"
    )
  }
  if (lowerlimit != 'auto' & upperlimit == 'auto') {
    stop(
      "both limits needs to be 'auto' or indiviually be choosen"
    )
  }

  # automated limit calculation
  if(lowerlimit == 'auto' & upperlimit == 'auto') {
    cluster_size <- list()
    for (c in cluster_df$value) {
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
    sd_value <- sd(data)

    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)
    iqr <- q3 - q1

    if(length(cluster_df) < 100) {
      data |> plot()
      stop(
        "detected number of objects is to small for automated detection"
      )
    }

    lower_bound <- q1 - sd_value * iqr
    upper_bound <- q3 + sd_value * iqr

    tro <- which(abs(data) > lower_bound & abs(data) < upper_bound)

    cluster_size <- cluster_size[tro]
    res_centers <- cluster_df[tro]

    for (f in seq_along(res_centers$value)) {
      remaining_pos <- which(xy_coords$value == f)
      cluster[remaining_pos] <- c(f)
      x_coord[remaining_pos] <- xy_coords$x[remaining_pos]
      y_coord[remaining_pos] <- xy_coords$y[remaining_pos]
    }
    res_xy_coords <- data.frame(
      x = unlist(x_coord),
      y = unlist(y_coord),
      value = unlist(cluster),
    )
  }

  if(lowerlimit != 'auto' & upperlimit != 'auto') {
  # aim: extract all coordinates (pixels) of the clusters
  # how many coordinates per cluster & cluster number that is in the list of
  # the remaining clusters (cluster_df) -> count = size of cluster
  # size has an upper and lower limit
  cluster_size <- list()
  for (c in cluster_df$value) {
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
  for (f in cluster_df$value) {
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
