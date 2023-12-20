## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, warning = FALSE, message = FALSE,
  cache = FALSE, comment = NA, verbose = TRUE,
  fig.width = 5, fig.height = 5, dev = "jpeg",
  dev.args = list(quality = 25)
)
options(digits = 3)

## ----beads--------------------------------------------------------------------
library(biopixR)
plot(beads)

## ----class--------------------------------------------------------------------
class(beads)

## ----objectDetection----------------------------------------------------------
res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)

## ----visualization_1----------------------------------------------------------
plot(beads)
with(
  res_objectDetection$centers,
  points(res_objectDetection$centers$mxx,
    res_objectDetection$centers$myy,
    col = factor(res_objectDetection$centers$value),
    pch = 19
  )
)

## ----visualization_2----------------------------------------------------------
changePixelColor(beads, res_objectDetection$coordinates, color = "purple")

## ----visualization_3----------------------------------------------------------
res_objectDetection$marked_beads |> plot()

## -----------------------------------------------------------------------------
res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)

## -----------------------------------------------------------------------------
plot(beads)
with(
  res_proximityFilter$remaining.centers,
  points(res_proximityFilter$remaining.centers$mxx,
    res_proximityFilter$remaining.centers$myy,
    col = "darkgreen",
    pch = 19
  )
)
with(
  res_proximityFilter$discard,
  points(res_proximityFilter$discard$mx,
    res_proximityFilter$discard$my,
    col = "darkred",
    pch = 19
  )
)

## -----------------------------------------------------------------------------
small_experiment <- proximityFilter(res_objectDetection, radius = 13)

plot(beads)
with(
  small_experiment$remaining.centers,
  points(small_experiment$remaining.centers$mxx,
    small_experiment$remaining.centers$myy,
    col = "darkgreen",
    pch = 19
  )
)
with(
  small_experiment$discard,
  points(small_experiment$discard$mx,
    small_experiment$discard$my,
    col = "darkred",
    pch = 19
  )
)

## -----------------------------------------------------------------------------
res_sizeFilter <- sizeFilter(res_proximityFilter,
  lowerlimit = 0,
  upperlimit = 150
)

## -----------------------------------------------------------------------------
DT <- data.table(res_sizeFilter$remaining.coordinates.s)
res_center <- DT[, .(x = mean(x), y = mean(y)), by = cluster]

changePixelColor(beads,
                 res_sizeFilter$remaining.coordinates.s,
                 color = "darkgreen")
text(res_center$x,
     res_center$y,
     res_center$cluster,
     col = "grey")

## -----------------------------------------------------------------------------
result <- resultAnalytics(res_sizeFilter)
result$detailed

## -----------------------------------------------------------------------------
result$Summary

## -----------------------------------------------------------------------------
DT <- data.table(res_sizeFilter$remaining.coordinates.s)
res_center <- DT[, .(x = mean(x), y = mean(y)), by = cluster]

## -----------------------------------------------------------------------------
euclidean_distance <- function(point1, point2) {
  sqrt((point2$x - point1$x)^2 + (point2$y - point1$y)^2)
}

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
for (a in 1:num_points) {
  result$detailed$relative_distance[a] <- mean(na.omit(relative_distances[a, ]))
}

result$detailed

## -----------------------------------------------------------------------------
result$Summary$mean_distance <- mean(result$detailed$relative_distance)
result$Summary

## ----fig.show='hold', out.width="49%"-----------------------------------------
plot(droplets)
plot(droplet_beads)

## ----fig.show='hold', out.width="49%"-----------------------------------------
# preprocessing: threshold, negate and mirroring
thresh <- threshold(droplets, "13%")
thresh_cimg <- as.cimg(thresh)
thresh_magick <- cimg2magick(thresh_cimg)
neg_thresh <- image_negate(thresh_magick)
neg_thresh_cimg <- magick2cimg(neg_thresh)
neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")

# first remove microbeads from droplet image (important for the linking of
# discontinuous edges, as otherwise they may connect with the microbeads)
# removes objects to prevent reconnecting with labeled regions that
# are not lines/edges
beads_to_del <- droplet_beads
bead_coords <-
  objectDetection(beads_to_del, alpha = 1, sigma = 0.1)

# transform binary image to array to modify individual values
thresh_array <- as.array(neg_thresh_m)
for (i in 1:nrow(bead_coords$coordinates)) {
  thresh_array[bead_coords$coordinates[i, 1],
               bead_coords$coordinates[i, 2], 1, 1] <- 0
}
# removed microbeads from droplets and retransformation to cimg
thresh_clean_cimg <- as.cimg(thresh_array)

# displaying problem of discontinous edges
plot(thresh_cimg)
# displaying removed microbeads
plot(thresh_clean_cimg)

## ----fig.align='center'-------------------------------------------------------
# same orientation for 'cimg' and 'magick-image'
thresh_clean_m <- mirror(thresh_clean_cimg, axis = "x")
thresh_clean_magick <- cimg2magick(thresh_clean_m)

# getting coordinates of all line ends
mo1_lineends <- image_morphology(thresh_clean_magick,
                                 "HitAndMiss", "LineEnds")

# transform extracted coordinates into data frame
lineends_cimg <- magick2cimg(mo1_lineends)

end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
end_points_df <- as.data.frame(end_points)
colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

# highlighted line ends
vis_lineend <-
  changePixelColor(thresh_clean_cimg, end_points_df, color = "green")

## ----fig.show='hold', out.width="49%"-----------------------------------------
closed_gaps <- fillLineGaps(droplets,
  droplet_beads,
  threshold = "13%",
  alpha = 1,
  sigma = 0.1,
  radius = 5,
  iterations = 3,
  visualize = TRUE
)

closed_gaps |> plot()

## ----fig.align='center'-------------------------------------------------------
first_img <- vis_lineend
second_img <- closed_gaps

first_m <- mirror(first_img, axis = "x")
second_m <- mirror(second_img, axis = "x")

first_magick <- cimg2magick(first_m)
second_magick <- cimg2magick(second_m)

img <- c(first_magick, second_magick)

image_animate(image_scale(img, "500x635"), fps = 1, dispose = "previous")

## -----------------------------------------------------------------------------
# label resulting image with filled gaps
lab_partitions <- label(closed_gaps)
df_lab_part <- as.data.frame(lab_partitions) |>
  subset(value > 0)

## ----fig.align='center'-------------------------------------------------------
# removing the edges as labeled regions
x <- list()
y <- list()
value <- list()

for (g in 1:nrow(df_lab_part)) {
  # droplets_array <- as.array(droplets)
  if (closed_gaps[df_lab_part$x[g], df_lab_part$y[g], 1, 1] == 0) {
    x[g] <- df_lab_part$x[g]
    y[g] <- df_lab_part$y[g]
    value[g] <- df_lab_part$value[g]
  }
}

# data frame of labeled partitions (without contours)
clean_lab_df <- data.frame(x = unlist(x),
                           y = unlist(y),
                           value = unlist(value))

changePixelColor(closed_gaps, clean_lab_df, color = "purple")

## ----fig.align='center', out.width="100%"-------------------------------------
# summarizing data frame by cluster (value)
DT_droplet <- data.table(clean_lab_df)

grouped_droplets <-
  DT_droplet[, .(mx = mean(x), my = mean(y)), by = value]

plot(closed_gaps)
text(
  grouped_droplets$mx,
  grouped_droplets$my,
  grouped_droplets$value,
  col = "gray",
  cex = 0.5
)

## -----------------------------------------------------------------------------
# checking in which partition the center coordinates of the microbeads are present
bead_partition <- list()
for (c in 1:nrow(bead_coords$centers)) {
  partition_pos <- which(
    clean_lab_df$x == round(bead_coords$centers$mxx[c]) &
      clean_lab_df$y == round(bead_coords$centers$myy[c])
  )
  bead_partition[c] <- clean_lab_df$value[partition_pos]
}
res_part <- table(as.character(bead_partition))
res_part

## -----------------------------------------------------------------------------
size_list <- list()
for (d in unique(clean_lab_df$value)) {
  pxl_number <- which(clean_lab_df$value == d)
  size <- length(pxl_number)
  size_list[d] <- size
}

size_df <- data.frame(partition = unique(clean_lab_df$value),
                      size = unlist(size_list))

hist(
  size_df$size,
  main = "Histogramm of partition size",
  xlab = "size",
  # labels = TRUE,
  breaks = "Scott",
  ylim = c(0, 120),
  xlim = c(0, 1400)
)

## -----------------------------------------------------------------------------
small <- which(size_df$size == 1)
number_small <- length(small)

## -----------------------------------------------------------------------------
data.frame(
  partitions = nrow(grouped_droplets) - number_small,
  empty_partitions = nrow(grouped_droplets) - length(unique(bead_partition)) - number_small,
  bead_partitions = length(unique(bead_partition)),
  single_bead = length(which(res_part == 1)),
  muliple_beads = length(which(res_part > 1))
)

## ----fig.show='hold', out.width="49%"-----------------------------------------
plot(droplets)
plot(closed_gaps)

