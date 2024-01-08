#' Bead Image Summary
#'
#' Extracts all important information of the remaining microbeads. This function
#' summarizes the data obtained by previous functions: objectDetection,
#' proximityFilter and sizeFilter. Provides information like amount, intensity,
#' size and density.
#' @param res_sizeFilter list obtained by the sizeFilter function
#' @returns list of 2 objects:
#' 1. summary of all the microbeads in the image
#' 2. detailed information about every single bead
#' @import data.table
#' @importFrom stats na.omit
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)
#' res_sizeFilter <- sizeFilter(res_proximityFilter, lowerlimit = 50, upperlimit = 150)
#' resultAnalytics(res_sizeFilter)
#' @export
resultAnalytics <- function(res_sizeFilter) {
  # binding for global variables
  intensity <- cluster <- NULL

  # assign imports
  res_xy_clus <- res_sizeFilter$remaining.coordinates.s
  cluster_size <- res_sizeFilter$size
  pic <- res_sizeFilter$image

  # including intensity values of pixels from remaining clusters in a data frame
  for (h in 1:nrow(res_xy_clus)) {
    xx <- res_xy_clus$x[h]
    yy <- res_xy_clus$y[h]
    int <- as.array(pic)[xx, yy, , ]
    res_xy_clus[h, 4] <- c(int)
  }

  # group data frame by cluster
  DT_intense <- data.table(res_xy_clus)
  intense <- DT_intense[, list(
    x = mean(x),
    y = mean(y),
    intensity = mean(intensity)
  ),
  by = cluster
  ]

  # approximate amount of discarded pixels
  # calculate amount of true coordinates
  amount_true <- length(which(threshold(pic)) == TRUE)
  dis_count <- round((amount_true / mean(unlist(cluster_size))) - nrow(intense))

  # summary for every passing bead
  res_df_long <- data.frame(
    beadnumber = intense$cluster,
    size = unlist(cluster_size),
    intensity = intense$intensity,
    x = intense$x,
    y = intense$y
  )

  # summary of res_df_long / whole image
  result <- data.frame(
    number_of_beads = nrow(intense),
    mean_size = mean(unlist(cluster_size)),
    mean_intensity = mean(res_xy_clus$intensity),
    bead_density = (nrow(intense) *
                      mean(unlist(cluster_size))) /
      length(pic),
    estimated_rejected = dis_count
  )

  # calculate the relative distance between all beadcenters based on Euclidean
  # distance
  DT <- data.table(res_sizeFilter$remaining.coordinates.s)
  res_center <- DT[, list(x = mean(x), y = mean(y)), by = cluster]

  # formula for the Euclidean distance
  euclidean_distance <- function(point1, point2) {
    sqrt((point2$x - point1$x)^2 + (point2$y - point1$y)^2)
  }

  num_points <- nrow(res_center)
  relative_distances <- matrix(NA, nrow = num_points, ncol = num_points)

  for (i in 1:num_points) {
    for (j in 1:num_points) {
      if (i == j) {
        next
      } else {
        relative_distances[i, j] <- euclidean_distance(res_center[i, ], res_center[j, ])
      }
    }
  }
  as.matrix(relative_distances)

  # add relative distance to the detailed result
  for (a in 1:num_points) {
    res_df_long$relative_distance[a] <- mean(na.omit(relative_distances[a, ]))
  }

  # add mean distance to the summarized result
  result$mean_distance <- mean(res_df_long$relative_distance)

  out <- list(
    summary = result,
    detailed = res_df_long
  )
}
