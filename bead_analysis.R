library(imager)
library(EBImage)
library(data.table)

# note do.call


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

# get position of remaining clusters in original labeled image -> 
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
# the remaining clusters (remaining_cluster_df) -> number = size of cluster
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


res6 <- list()

for (ok in remaining_cluster_df$value) {
    if (is.null(cluster_size[[ok]]) != TRUE) {
      res6[ok] <- c(ok)
  }
}



#sum(unlist(cluster_size))
# xy_cords_clus enthält noch Werte welche durch größe ausgeschlossen wurden daher entstehen NAs es wird empfohlen vor der Speicherung im Datensatz in 
# listen zu speichern und zu unlisten dann in dataframe :D

res7 <- list()

res8 <- list()

res9 <- list()



for (kk in unlist(res6)) {
  pp <- which(xy_cords_clus$value == kk)
  res7[pp] <- c(kk)
  res8[pp] <- xy_cords_clus$x[pp]
  res9[pp] <- xy_cords_clus$y[pp]
}

data2 <- data.frame(x = unlist(res8),
                    y = unlist(res9),
                    intensity = rep(NA, length(unlist(res8))),
                    Cluster = unlist(res7))

for (jj in 1:nrow(data2)) {
  xx <- data2$x[jj]
  yy <- data2$y[jj]
  int <- img[xx, yy]
  data2[jj, 3] <- c(int)
}

intense <- dplyr::group_by(data2[, c(3:4)], Cluster) %>% 
  dplyr::summarise(intensity = mean(intensity))

amazing <- data.frame(Beadnumber = unlist(res6), 
                      Size = unlist(cluster_size), 
                      Intesity = intense[,2])

data3 <- dplyr::group_by(data2[, c(1, 2, 4)], Cluster) %>%
  dplyr::summarise(xc = mean(x), yc = mean(y))

plot(img)
with(grouped_lab_img, points(mxx, myy, col="red"))
with(data3, points(xc, yc, col = "green"))

Result <- data.frame(Number_of_Beads = length(unlist(res6)), 
                     Mean_Size = mean(unlist(cluster_size)), 
                     Mean_intensity = mean(data2$intensity))

