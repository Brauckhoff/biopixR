library(imager)
library(EBImage)
library(data.table)

# note do.call

tic("test")
# image import
img <- load.image("2.bmp")

#source("edgeR.R")

# first section: detect all beads

# edge detection with default: alpha = 0.75, sigma = 0.1
# fill detected edges and label areas
# create data frame without background
# summarize cluster and calculate center -> display & check
edge_img <- cannyEdges(img, alpha = 0.75, sigma = 0.1)
filled_img <- fillHull(edge_img)
labeled_img <- label(filled_img)
df_lab_img <- as.data.frame(labeled_img) |> 
  subset(value > 0)
DT <- data.table(df_lab_img) 
grouped_lab_img <- DT[ , .(mxx = mean(x), myy = mean(y)), by = value]

edgeR(image = img)
 
plot(img)
with(grouped_lab_img, points(mxx, myy, col = "yellow"))


# section two: distance based exclusion

# looking at every center of an labeled object
# first: check the surrounding x pixels from the current center, if there is
# another center (looking only at x coordinates) -> rectangle over hole y axis,
# with width of 2x
# second (if first true): check if the found x coordinates are in a range of x 
# pixels from the current center regarding the y coordinates -> creates second
# rectangle over hole x axis, with width of 2x
# third: only if a coordinate is in both rectangles (the x^2 around the current 
# center) it is viewed as too close and therefore discarded
distanced_excl_list <- list()
for(i in seq_along(grouped_lab_img$value)) {
  x <- grouped_lab_img[i, ]$mxx
  y <- grouped_lab_img[i, ]$myy
  for(d in grouped_lab_img$mxx) {
    if(x == d) {
      next
    } else {
      if(x - 10 < d & x + 10 > d) {
        clus <- which(grouped_lab_img$mxx == d)
        #print(g)
        y_clus <- grouped_lab_img[clus, ]$myy
        #print(h)
        for(v in y_clus) {
          if(y - 10 < v & y + 10 > v) {
            too_close <- which(grouped_lab_img$myy == v & grouped_lab_img$mxx == d)
            distanced_excl_list[too_close] <- c(too_close)
          }
        }
      }
    }
  }
}

clean_distanced_excl <- unlist(distanced_excl_list)

distance_discard_df <- data.frame(mx = rep(NA, length(clean_distanced_excl)), 
                   my = rep(NA, length(clean_distanced_excl)))

for (a in clean_distanced_excl) {
  x <- grouped_lab_img[a, ]$mxx
  y <- grouped_lab_img[a, ]$myy
  distance_discard_df[a, 1] <- x
  distance_discard_df[a, 2] <- y
}

plot(img)
with(grouped_lab_img, points(mxx, myy, col="green"))
with(distance_discard_df, points(mx, my, col = "red"))


# third section: size exclusion

# first: discard data points that did not pass the second section from original 
# data
remaining_cluster <- which(is.na(distance_discard_df$mx) == TRUE)
remaining_cluster_df <- grouped_lab_img[remaining_cluster, ]

# get position of remaining clusters in original labeled image 
# aim: extract all coordinates (pixels) of the clusters
pos_clus_img <- list()
for (b in remaining_cluster_df$value) {
  clus_pos <- which(df_lab_img$value == b)
  pos_clus_img[clus_pos] <- c(clus_pos)
}

clean_pos_clus <- unlist(pos_clus_img)
xy_cords_clus <- df_lab_img[clean_pos_clus, ]


# determination of cluster size
# how many coordinates per cluster & cluster number that is in the list of 
# the remaining clusters (remaining_cluster_df) -> count = size of cluster
# size has an upper and lower limit to discard duplets(&multis) and noise
cluster_size <- list()
for (c in remaining_cluster_df$value) {
  for (e in xy_cords_clus$value) {
    if (c == e) {
      clus_pxl <- which(xy_cords_clus$value == c)
      size <- length(clus_pxl)
      if (is.null(size) != TRUE & size < 150 & size > 50) {
        cluster_size[c] <- c(size)
      }
    }
  }
}


# fourth section: post-processing and visualization

# getting cluster numbers after exclusion due to cluster size
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
for (h in 1:nrow(res_xy_clus)) {
  xx <- res_xy_clus$x[h]
  yy <- res_xy_clus$y[h]
  int <- img[xx, yy]
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
                      Intesity = intense[, 3],
                      x = intense[, 1],
                      y = intense[, 2])

plot(img)
with(grouped_lab_img, points(mxx, myy, col="orange"))
with(distance_discard_df, points(mx, my, col = "red"))
with(res_df_long, points(x, y, col = "green"))

# end result
Result <- data.frame(Number_of_Beads = length(unlist(clus_num)), 
                     Mean_Size = mean(unlist(cluster_size)), 
                     Mean_intensity = mean(res_xy_clus$intensity),
                     Bead_density = (length(unlist(clus_num))*
                                       mean(unlist(cluster_size)))/length(img))

toc()