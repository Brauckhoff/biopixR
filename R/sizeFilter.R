#' Size-based exclusion
#'
#' Calculates the size of the objects in an image and discards objects based
#' on a lower and an upper size limit. (Input can be obtained by proximityFilter
#' function)
#' @param res_proximityFilter list of 2 objects:
#' 1. named 'remaining.centers' - containing data frame with center coordinates;
#' three columns 'value' - cluster number, 'mxx' - x coordinates &
#' 'myy' - y coordinates
#' 2. named 'remaining.coordinates' - containing date frame with all
#' coordinates of the clusters; three columns 'x' - x coordinates,
#' 'y' - y coordinates & 'value' - cluster number
#' @param lowerlimit smallest accepted object size
#' @param upperlimit highest accepted object size
#' @returns list of 3 objects:
#' 1. remaining beads after discarding according to size
#' 2. size of a labeled region
#' 3. original image
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)
#' sizeFilter(res_proximityFilter, lowerlimit = 50, upperlimit = 150)
#' @export
sizeFilter <- function(res_proximityFilter,
                  lowerlimit = 50,
                  upperlimit = 150) {
  # assign imports
  remaining_cluster_df <- res_proximityFilter$remaining.centers
  xy_cords_clus <- res_proximityFilter$remaining.coordinates

  # aim: extract all coordinates (pixels) of the clusters
  # how many coordinates per cluster & cluster number that is in the list of
  # the remaining clusters (remaining_cluster_df) -> count = size of cluster
  # size has an upper and lower limit
  cluster_size <- list()
  for (c in remaining_cluster_df$value) {
    for (e in xy_cords_clus$value) {
      if (c == e) {
        clus_pxl <- which(xy_cords_clus$value == c)
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
  for (f in remaining_cluster_df$value) {
    if (is.null(cluster_size[[f]]) != TRUE) {
      clus_num[f] <- c(f)
    }
  }

  # creating new data frame that contains cluster that pass both exclusions
  cluster <- list()
  x_coord <- list()
  y_coord <- list()
  for (g in unlist(clus_num)) {
    remaining_pos <- which(xy_cords_clus$value == g)
    cluster[remaining_pos] <- c(g)
    x_coord[remaining_pos] <- xy_cords_clus$x[remaining_pos]
    y_coord[remaining_pos] <- xy_cords_clus$y[remaining_pos]
  }

  res_xy_clus <- data.frame(
    x = unlist(x_coord),
    y = unlist(y_coord),
    cluster = unlist(cluster),
    intensity = rep(NA, length(unlist(x_coord)))
  )

  out <- list(
    remaining.coordinates.s = res_xy_clus,
    size = cluster_size,
    image = res_proximityFilter$image
  )
}
