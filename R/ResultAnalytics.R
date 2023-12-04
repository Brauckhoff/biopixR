#' Bead Image Summary
#'
#' Extracts all important information of the remaining beads. This function
#' summarizes the data obtained by previous functions: objectDetection,
#' proximityFilter and sizeFilter. Gives information like amount, intensity,
#' size and density.
#' @param res_sizeFilter list obtained by the sizeFilter function
#' @returns list of 2 objects:
#' 1. summary of whole all beads in the image
#' 2. detailed information about every single bead
#' @import data.table
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)
#' res_sizeFilter <- sizeFilter(res_proximityFilter, lowerlimit = 50, upperlimit = 150)
#' ResultAnalytics(res_sizeFilter)
#' @export
ResultAnalytics <- function(res_sizeFilter) {
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
  intense <- DT_intense[, .(
    x = mean(x),
    y = mean(y),
    intensity = mean(intensity)
  ),
  by = Cluster
  ]

  # summary for every passing bead
  res_df_long <- data.frame(
    Beadnumber = intense$Cluster,
    Size = unlist(cluster_size),
    Intensity = intense$intensity,
    x = intense$x,
    y = intense$y
  )

  # approximation of the amount of discarded pixels
  # calculate amount of true coordinates
  amount_true <- length(which(threshold(pic)) == TRUE)
  dis_count <- round((amount_true / mean(unlist(cluster_size))) - nrow(intense))

  # summary of res_df_long / whole image
  Result <- data.frame(
    Number_of_Beads = nrow(intense),
    Mean_Size = mean(unlist(cluster_size)),
    Mean_intensity = mean(res_xy_clus$intensity),
    Bead_density = (nrow(intense) *
      mean(unlist(cluster_size))) /
      length(pic),
    Estimated_rejected = dis_count
  )

  out <- list(
    Summary = Result,
    detailed = res_df_long
  )
  out
}
