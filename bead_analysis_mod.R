library(imager)
library(EBImage)
library(data.table)

img <- load.image("2.bmp")

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

# output section one:
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
dis_excl_res <- lapply(seq_along(grouped_lab_img$value), function(clus) {
  x <- grouped_lab_img[clus, ]$mxx
  y <- grouped_lab_img[clus, ]$myy
  lapply(as.list(grouped_lab_img$mxx), function(x_cord) {
    if(x != x_cord) {
      if(x - 10 < x_cord & x + 10 > x_cord) {
        cluster <- which(grouped_lab_img$mxx == x_cord)
        y_coordinates <- grouped_lab_img[cluster, ]$myy
        lapply(as.list(y_coordinates), function(y_cord) {
          if(y - 10 < y_cord & y + 10 > y_cord) {
            distanced_clus <- which(grouped_lab_img$myy == y_cord & 
                         grouped_lab_img$mxx == x_cord)
            distanced_clus
          }
        })
      }
    }
  })
})

distanced_list <- sort(unique(unlist(dis_excl_res)))


# output section two:
x <- lapply(distanced_list, function(dis_clus) {
  x <- grouped_lab_img[dis_clus, ]$mxx
  x
})
y <- lapply(distanced_list, function(dis_clus) {
  y <- grouped_lab_img[dis_clus, ]$myy
  y
})
distanced_df <- data.frame(mx = rep(NA, length(distanced_list)), 
                           my = rep(NA, length(distanced_list)))
distanced_df[, 1] <- unlist(x)
distanced_df[, 2] <- unlist(y)
plot(img)
with(grouped_lab_img, points(mxx, myy, col = "green"))
with(distanced_df, points(mx, my, col = "red"))


# third section: size exclusion

# first: discard data points that did not pass the second section from original 
# data
cluster_green <- lapply(seq_along(dis_excl_res), function(rv) {
  if(is.null(unlist(dis_excl_res[[rv]])) == TRUE){
    rv
  }
})
cluster_nr_green <- sort(unlist(cluster_green))
df_dis_excl <- grouped_lab_img[cluster_nr_green, ]

value_green <- lapply(df_dis_excl$value, function(value) {
  which(df_lab_img$value == value)
})

un_value_green <- sort(unlist(value_green))
green_lab_img <- df_lab_img[c(un_value_green), ]

bead_size <- lapply(df_dis_excl$value, function(lab_value) {
  lapply(green_lab_img$value, function(img_value) {
    if(lab_value == img_value) {
      pos_lab_img <- which(green_lab_img$value == lab_value)
      cluster_size <- length(unlist(pos_lab_img))
      if(is.null(cluster_size) != TRUE & 
                 cluster_size < 150 & 
                 cluster_size > 50) {
        cluster_size
      }
    }
  })
})

bead_size_list <- lapply(seq_along(bead_size), function(bead_pos) {
  list <- unique(bead_size[[bead_pos]])
  lapply(list, function(be_si) {
    if(is.null(be_si) != TRUE) {
      be_si
    }
  })
})
cluster_pos <- lapply(seq_along(bead_size), function(bead_pos) {
  list <- unique(bead_size[[bead_pos]])
  lapply(list, function(be_si) {
    if(is.null(be_si) != TRUE) {
      bead_pos
    }
  })
})

bead_size_clean <- unlist(bead_size_list)
cluster_pos_clean <- cluster_nr_green[unlist(cluster_pos)]


# fourth section: intensity of the remaining labeled regions

# takes very long exchange against for loop
x_coordinates <- lapply(1:nrow(df_lab_img), function(cl1) {
  lapply(seq_along(cluster_pos_clean), function(cl2) {
    if(df_lab_img$value[cl1] == cluster_pos_clean[cl2]) {
      m_x <- df_lab_img$x[cl1]
      m_x
    }
  })
})
un_x_coordinates <- unlist(x_coordinates)
y_coordinates <- lapply(1:nrow(df_lab_img), function(cl1) {
  lapply(seq_along(cluster_pos_clean), function(cl2) {
    if(df_lab_img$value[cl1] == cluster_pos_clean[cl2]) {
      m_y <- df_lab_img$y[cl1]
      m_y
    }
  })
})
un_y_coordinates <- unlist(y_coordinates)
cluster_coordinates <- lapply(1:nrow(df_lab_img), function(cl1) {
  lapply(seq_along(cluster_pos_clean), function(cl2) {
    if(df_lab_img$value[cl1] == cluster_pos_clean[cl2]) {
      cluster_pos_clean[cl2]
    }
  })
})
un_cluster_coordinates <- unlist(cluster_coordinates)


# not lapply does not work including the img
intensity_list <- list()
for (k in seq_along(un_cluster_coordinates)) {
  int <- img[un_x_coordinates[k], un_y_coordinates[k]]
  intensity_list[k] <- int
}

un_int_list <- unlist(intensity_list)


final_df <- data.frame(x_f = rep(NA, length(un_x_coordinates)), 
                       y_f = rep(NA, length(un_y_coordinates)),
                       c_f = rep(NA, length(un_cluster_coordinates)),
                       intensity = rep(NA, length(intensity_list)))
final_df[, 1] <- un_x_coordinates
final_df[, 2] <- un_y_coordinates
final_df[, 3] <- un_cluster_coordinates
final_df[, 4] <- un_int_list

DT <- data.table(final_df) 
final_dt <- DT[ , .(xc = mean(x_f), 
                    yc = mean(y_f), 
                int = mean(intensity)), by = c_f]

final_dt$size <- bead_size_clean

plot(img)
with(grouped_lab_img, points(mxx, myy, col="red"))
with(final_dt, points(xc, yc, col = "green"))



result <- data.frame(number_of_beads = nrow(final_dt),
                     mean_size = mean(final_dt$size),
                     mean_intensity = mean(final_dt$int)
                     )
result
