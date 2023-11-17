#' Bead Image Summary
#'
#' Extracts all important information of the remaining beads
#' @param res_sizeR list obtained by the sizeR function
#' @returns list of 2 objects:
#' 1. summary of whole all beads in the image
#' 2. detailed information about every single bead
#' @import data.table
#' @examples
#' res_detecteR <- detecteR(beads, alpha = 0.75, sigma = 0.1)
#' res_distanceR <- distanceR(res_detecteR, radius = 10)
#' res_sizeR <- sizeR(res_distanceR, lowerlimit = 50, upperlimit = 150)
#' visualizeR(res_sizeR)
#' @export
visualizeR <- function(res_sizeR) {
  # fourth section: post-processing and visualization
  remaining_cluster_df <- res_sizeR$cluster
  xy_cords_clus <- res_sizeR$coordinates
  cluster_size <- res_sizeR$size
  pic <- res_sizeR$image

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

  res_xy_clus <- data.frame(x = unlist(x_coord),
                            y = unlist(y_coord),
                            intensity = rep(NA, length(unlist(x_coord))),
                            Cluster = unlist(cluster))

  # including intensity values of pixels from remaining clusters in df
  for(h in 1:nrow(res_xy_clus)) {
    xx <- res_xy_clus$x[h]
    yy <- res_xy_clus$y[h]
    int <- as.array(pic)[xx, yy, ,]
    res_xy_clus[h, 3] <- c(int)
  }

  # group data frame by cluster
  DT_intense <- data.table(res_xy_clus)
  intense <- DT_intense[ , .(x = mean(x),
                             y = mean(y),
                             intensity = mean(intensity)),
                         by = Cluster]

  # summary for every passing bead
  res_df_long <- data.frame(Beadnumber = unlist(clus_num),
                            Size = unlist(cluster_size),
                            Intensity = intense$intensity,
                            x = intense$x,
                            y = intense$y)

  Result <- data.frame(Number_of_Beads = length(unlist(clus_num)),
                       Mean_Size = mean(unlist(cluster_size)),
                       Mean_intensity = mean(res_xy_clus$intensity),
                       Bead_density = (length(unlist(clus_num))*
                                         mean(unlist(cluster_size)))/
                         length(pic))

  out <- list(Summary = Result,
              detailed = res_df_long)
  out
}
