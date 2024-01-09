#' Proximity-based exclusion
#'
#' To detect objects within a defined range of one another, it is necessary to
#' calculate their centers to determine proximity. Pairs that are too close
#' will be discarded. (Input can be obtained by objectDetection function)
#' @param centers center coordinates of objects (mx|my|value data frame)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param size size of the objects (list with length equal to centers)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels)
#' @returns list of 2 objects:
#' 1. center coordinates of remaining objects
#' 2. all coordinates of remaining objects
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' res_sizeFilter <- sizeFilter(res_objectDetection$centers, res_objectDetection$coordinates, lowerlimit = 50, upperlimit = 150)
#' proximityFilter(res_sizeFilter$centers, res_objectDetection$coordinates,
#' size = res_sizeFilter$size, radius = 'auto')
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
proximityFilter <- function(centers,
                            coordinates,
                            size = NULL,
                            radius = 10) {
  # assign imports
  center_df <- centers
  xy_coords <- coordinates

  # transfer to sizeFilter and change of order
  if(radius == 'auto') {
    if(is.null(size) == TRUE) {
      stop(
        "size is required for calculation of the radius"
      )
    }

    if(radius == 'auto') {
      mean_size <- mean(unlist(size))
      # calculate radius from given size
      radius <- sqrt(mean_size / pi)

      # at least half a bead space between objects
      radius <- round(radius) * 2

      # making proximity filter more reliable
      xy_edit <- xy_coords
      for (j in 1:nrow(center_df)) {
        x <- center_df$mx[j]
        y <- center_df$my[j]
        center_sur <- which(
          xy_coords$x > (x - radius) &
            xy_coords$x < (x + radius) &
            xy_coords$y > (y - radius) &
            xy_coords$y < (y + radius)
        )

        if (is.null(center_sur) == FALSE &
            length(unique(xy_coords$value[center_sur])) > 1) {
          too_close <- unique(xy_coords$value[center_sur])

          for (k in too_close) {
            xy_pos <- which(xy_coords$value == k)
            xy_edit[xy_pos, ] <- NA
          }
        } else {
          next
        }
      }
    }

    xy_cords_clus <- na.omit(xy_edit)

    DT <- data.table(xy_cords_clus)

    # summarize by center and calculate center
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  if (radius != 'auto') {
    # making proximity filter more reliable
    xy_edit <- xy_coords
    for (j in 1:nrow(center_df)) {
      x <- center_df$mx[j]
      y <- center_df$my[j]
      center_sur <- which(
        xy_coords$x > (x - radius) &
          xy_coords$x < (x + radius) &
          xy_coords$y > (y - radius) &
          xy_coords$y < (y + radius)
      )

      if (is.null(center_sur) == FALSE &
          length(unique(xy_coords$value[center_sur])) > 1) {
        too_close <- unique(xy_coords$value[center_sur])

        for (k in too_close) {
          xy_pos <- which(xy_coords$value == k)
          xy_edit[xy_pos, ] <- NA
        }
      } else {
        next
      }
    }

    xy_cords_clus <- na.omit(xy_edit)

    DT <- data.table(xy_cords_clus)

    # summarize by center and calculate center
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]

  }

  out <- list(
    centers = remaining_center_df,
    coordinates = xy_cords_clus
  )
}
