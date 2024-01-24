#' Proximity-based exclusion
#'
#' To detect objects within a defined range of one another, it is necessary to
#' calculate their centers to determine proximity. Pairs that are too close
#' will be discarded. (Input can be obtained by objectDetection function)
#' @param centers center coordinates of objects (mx|my|value data frame)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels)
#' @returns list of 3 objects:
#' 1. center coordinates of remaining objects
#' 2. all coordinates of remaining objects
#' 3. size of remaining objects
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_proximityFilter <- proximityFilter(
#'   res_objectDetection$centers,
#'   res_objectDetection$coordinates,
#'   radius = "auto"
#'   )
#' changePixelColor(
#'   beads,
#'   res_proximityFilter$coordinates,
#'   color = "darkgreen",
#'   visualize = TRUE
#'   )
#' @export
proximityFilter <- function(centers,
                            coordinates,
                            radius = "auto") {
  # assign imports
  center_df <- centers
  xy_coords <- coordinates

  # size calculation, which is needed to calculate the radius
  cluster_size <- list()
  for (c in center_df$value) {
    for (e in xy_coords$value) {
      if (c == e) {
        clus_pxl <- which(xy_coords$value == c)
        size <- length(clus_pxl)
        if (is.null(size) != TRUE) {
          cluster_size[c] <- c(size)
        }
      }
    }
  }

  size <- unlist(cluster_size)

  if (radius != "auto") {
    # proximity calculation starting from every center and scanning for any TRUE
    # pixel in the given radius
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

      # checking that the detected pixels are not part of their own object
      if (is.null(center_sur) == FALSE &
        length(unique(xy_coords$value[center_sur])) > 1) {
        too_close <- unique(xy_coords$value[center_sur])

        # removing them from the xy coordinate data set
        for (k in too_close) {
          xy_pos <- which(xy_coords$value == k)
          xy_edit[xy_pos, ] <- NA
        }
      } else {
        next
      }
    }

    # removing NAs
    pre_coords <- na.omit(xy_edit)

    # comparing to input centers (only keeping xy coordinates that pass the
    # proximityFilter and are included in centers)
    update <- list()
    for (l in center_df$value) {
      to_keep <- which(pre_coords$value == l)
      if (length(to_keep) != 0) {
        update[to_keep] <- to_keep
      }
    }

    # result: xy coordinates
    xy_coords_clus <- pre_coords[unlist(update), ]

    # result: remaining centers
    DT <- data.table(xy_coords_clus)

    # summarize by center and calculate center
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  # error
  # automated radius calculation
  if (radius == "auto") {
    # calculate radius from given size (for circular shaped objects)
    mean_size <- mean(size)
    radius <- sqrt(mean_size / pi)

    # radius times two, as starting from the center one radius covers the
    # current object
    radius <- round(radius) * 2

    # proximity calculation starting from every center and scanning for any TRUE
    # pixel in the given radius
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

      # checking that the detected pixels are not part of their own object
      if (is.null(center_sur) == FALSE &
        length(unique(xy_coords$value[center_sur])) > 1) {
        too_close <- unique(xy_coords$value[center_sur])

        # removing them from the xy coordinate data set
        for (k in too_close) {
          xy_pos <- which(xy_coords$value == k)
          xy_edit[xy_pos, ] <- NA
        }
      } else {
        next
      }
    }

    # removing NAs
    pre_coords <- na.omit(xy_edit)

    # comparing to input centers (only keeping xy coordinates that pass the
    # proximityFilter and are included in centers)
    update <- list()
    for (l in center_df$value) {
      to_keep <- which(pre_coords$value == l)
      if (length(to_keep) != 0) {
        update[to_keep] <- to_keep
      }
    }

    # result: xy coordinates
    xy_coords_clus <- pre_coords[unlist(update), ]

    # result: remaining centers
    DT <- data.table(xy_coords_clus)

    # summarize by center and calculate center
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  out <- list(
    centers = remaining_center_df,
    coordinates = xy_coords_clus,
    size = cluster_size[remaining_center_df$value]
  )
}
