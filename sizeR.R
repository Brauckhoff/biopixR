# third section: size exclusion

# first: discard data points that did not pass the second section from original 
# data
# then get position of remaining clusters in original labeled image 
# aim: extract all coordinates (pixels) of the clusters
# how many coordinates per cluster & cluster number that is in the list of 
# the remaining clusters (remaining_cluster_df) -> count = size of cluster
# size has an upper and lower limit to discard duplets(&multis) and noise
sizeR <- function(distance_discard_df, df_lab_img, lowerlimit = 50, 
                  upperlimit = 150) {
  source("distanceR.R")
  remaining_cluster <- which(is.na(distance_discard_df$mx) == TRUE)
  remaining_cluster_df <- grouped_lab_img[remaining_cluster, ]
  
  pos_clus_img <- list()
  for (b in remaining_cluster_df$value) {
    clus_pos <- which(df_lab_img$value == b)
    pos_clus_img[clus_pos] <- c(clus_pos)
  }
  
  clean_pos_clus <- unlist(pos_clus_img)
  xy_cords_clus <- df_lab_img[clean_pos_clus, ]
  
  cluster_size <- list()
  for (c in remaining_cluster_df$value) {
    for (e in xy_cords_clus$value) {
      if (c == e) {
        clus_pxl <- which(xy_cords_clus$value == c)
        size <- length(clus_pxl)
        if (is.null(size) != TRUE & size < upperlimit & size > lowerlimit) {
          cluster_size[c] <- c(size)
        }
      }
    }
  }
  cluster_size
}



