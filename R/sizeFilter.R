#' Size based exclusion
#'
#' Calculates the size of the objects in an image and discards objects based
#' on a lower and a upper size limit.
#' @param res_proximityFilter list obtained by the proximityFilter function
#' @param lowerlimit smallest accepted object size
#' @param upperlimit highest accepted object size
#' @returns list of 4 objects:
#' 1. remaining beads after discarding according to distance
#' 2. all coordinates that are in labeled regions
#' 3. size of a labeled region
#' 4. original image
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)
#' sizeFilter(res_proximityFilter, lowerlimit = 50, upperlimit = 150)
#' @export
sizeFilter <- function(res_proximityFilter,
                  lowerlimit = 50,
                  upperlimit = 150) {
  # assign imports
  grouped_lab_img <- res_proximityFilter$centers
  df_lab_img <- res_proximityFilter$coordinates
  distance_discard_df <- res_proximityFilter$discard

  # first: discard data points that did not pass the proximityFilter from
  # original data
  # then get position of remaining clusters in original labeled image
  remaining_cluster <- which(is.na(distance_discard_df$mx) == TRUE)
  remaining_cluster_df <- grouped_lab_img[remaining_cluster, ]

  # extracting all coordinates from the original labeled image with cluster
  # that pass the criteria
  pos_clus_img <- list()
  for (b in remaining_cluster_df$value) {
    clus_pos <- which(df_lab_img$value == b)
    pos_clus_img[clus_pos] <- c(clus_pos)
  }

  clean_pos_clus <- unlist(pos_clus_img)
  xy_cords_clus <- df_lab_img[clean_pos_clus, ]

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
  out <- list(
    cluster = remaining_cluster_df,
    coordinates = xy_cords_clus,
    size = cluster_size,
    image = res_proximityFilter$image
  )
}
