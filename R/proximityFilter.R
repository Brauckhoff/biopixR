#' Proximity-based exclusion
#'
#' To detect objects within a defined range of one another, it is necessary to
#' calculate their centers to determine proximity. Pairs that are too close
#' will be discarded. (Input can be obtained by objectDetection function)
#' @param res_objectDetection list of objects:
#' 1. named 'centers' - containing data frame with center coordinates of
#' objects; three columns 'mxx' - x coordinates, 'myy' - y coordinates &
#' 'value' - cluster number
#' 2. named 'coordinates' - containing date frame with all labeled coordinates;
#' three columns 'x' - x coordinates, 'y' - y coordinates & 'value' - cluster
#' number (optional)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels)
#' @returns list of 4 objects:
#' 1. center coordinates of discarded objects
#' 2. center coordinates of remaining objects
#' 3. all coordinates of remaining objects
#' 4. original image
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' proximityFilter(res_objectDetection, radius = 10)
#'
#' # without usage of objectDetection (only centers)
#' mat <- matrix(0, 8, 8)
#' mat[3, 5] <- 1
#' mat[5, 2] <- 1
#' mat[6, 7] <- 1
#' mat[7, 3] <- 1
#' sim_img <- as.cimg(mat)
#' centers <- data.frame(
#'   mxx = c(3, 5, 6, 7),
#'   myy = c(5, 2, 7, 3),
#'   value = c(1:4)
#' )
#' objects <- list(centers = centers)
#' proximityFilter(objects, radius = 3)
#'
#' # Visualization
#' res_l <- proximityFilter(res_objectDetection, radius = 10)
#' changePixelColor(beads, res_l$remaining.coordinates)
#'
#' # without objectDetection
#' res_m <- proximityFilter(objects, radius = 3)
#' changePixelColor(sim_img, res_m$remaining.centers)
#' @export
proximityFilter <- function(res_objectDetection, radius = 10) {
  # assign imports
  grouped_lab_img <- res_objectDetection$centers
  df_lab_img <- res_objectDetection$coordinates

  # transfer to sizeFilter and change of order
  if (radius == 'auto') {
    # automated detection of radius
    # taking a part from the sizeFilter function to calculate the size
    # knowing the size we can calculate the radius (requirement: circular shape)
    cluster_size <- list()
    for (c in grouped_lab_img$value) {
      for (e in df_lab_img$value) {
        if (c == e) {
          clus_pxl <- which(df_lab_img$value == c)
          size <- length(clus_pxl)
          if (is.null(size) != TRUE) {
            cluster_size[c] <- c(size)
          }
        }
      }
    }

    data <- unlist(cluster_size)
    sd_value <- sd(data)

    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - sd_value * iqr
    upper_bound <- q3 + sd_value * iqr

    tro <- which(abs(data) > lower_bound & abs(data) < upper_bound)

    # calculate radius from given approximate size
    #radius <- sqrt(mean_size$mean_size[app_size] / pi)

    # at least half a bead space between beads
    # radius <- round(radius) * 3
  }

  # looking at every center of an labeled object
  distanced_excl_list <- list()
  for (i in seq_along(grouped_lab_img$value)) {
    x <- grouped_lab_img[i,]$mxx
    y <- grouped_lab_img[i,]$myy
    for (d in grouped_lab_img$mxx) {
      if (x == d) {
        next
      } else {
        # first: check the surrounding pixels from the current center, if there
        # is another center -> rectangle over hole y axis, with width of
        # 2*radius
        if (x - radius < d & x + radius > d) {
          clus <- which(grouped_lab_img$mxx == d)
          y_clus <- grouped_lab_img[clus,]$myy

          # second (if first true): check if the found x coordinates are in a
          # range of 'radius' pixels from the current center regarding the y
          # coordinates -> creates second rectangle over hole x axis, with
          # width of 2*radius
          for (v in y_clus) {
            if (y - radius < v & y + radius > v) {
              # third: only if a coordinate is in both rectangles (the radius^2
              # around the current center) it is viewed as too close and
              # therefore discarded
              too_close <- which(grouped_lab_img$myy == v &
                                   grouped_lab_img$mxx == d)
              distanced_excl_list[too_close] <- c(too_close)
            }
          }
        }
      }
    }
  }

  clean_distanced_excl <- unlist(distanced_excl_list)

  # create data frame with the center coordinates of the discarded clusters
  distance_discard_df <- data.frame(mx = rep(NA, nrow(grouped_lab_img)),
                                    my = rep(NA, nrow(grouped_lab_img)))

  for (a in clean_distanced_excl) {
    x <- grouped_lab_img[a,]$mxx
    y <- grouped_lab_img[a,]$myy
    distance_discard_df[a, 1] <- x
    distance_discard_df[a, 2] <- y
  }

  # discard data points that did not pass the proximityFilter from
  # original data
  # then get position of remaining clusters in original labeled image
  remaining_cluster <- which(is.na(distance_discard_df$mx) == TRUE)
  remaining_cluster_df <- grouped_lab_img[remaining_cluster,]

  # extracting all coordinates from the original labeled image with cluster
  # that pass the criteria
  pos_clus_img <- list()
  for (b in remaining_cluster_df$value) {
    clus_pos <- which(df_lab_img$value == b)
    pos_clus_img[clus_pos] <- c(clus_pos)
  }
  clean_pos_clus <- unlist(pos_clus_img)
  xy_cords_clus <- df_lab_img[clean_pos_clus,]

  out <- list(
    discard = distance_discard_df,
    remaining.centers = remaining_cluster_df,
    remaining.coordinates = xy_cords_clus,
    image = res_objectDetection$image
  )
}
