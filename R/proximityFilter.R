#' Proximity-based exclusion
#'
#' In order to identify objects within a specified proximity, it is essential to
#' calculate their respective centers, which serve to determine their proximity.
#' Pairs that are in close proximity will be discarded.
#' (Input can be obtained by \code{\link[biopixR]{objectDetection}} function)
#' @param centers center coordinates of objects (mx|my|value data frame)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels) (numeric / 'auto')
#' @param elongation factor by which the radius should be multiplied to create
#' the area of exclusion (default 2)
#' @returns list of 3 objects:
#' 1. center coordinates of remaining objects
#' 2. all coordinates of remaining objects
#' 3. size of remaining objects
#' @details
#' The automated radius calculation in the `proximityFilter()` function is based
#' on the presumption of circular-shaped objects. The radius is calculated using
#' the formula `sqrt(A/pi)`, where A is the area of the detected objects. The
#' function will exclude objects that are too close by extending the calculated
#' radius by one radius length beyond the assumed circle, effectively doubling
#' the radius to create an exclusion zone. Therefore the elongation factor is
#' set to 2 by default, with one radius covering the object and an additional
#' radius creating the area of exclusion.
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
                            radius = "auto",
                            elongation = 2) {
  # Assign input arguments to local variables
  center_df <- centers
  xy_coords <- coordinates

  # Calculate the size of each detected object
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
    # Proximity calculation for a given radius
    xy_edit <- xy_coords
    for (j in 1:nrow(center_df)) {
      x <- center_df$mx[j]
      y <- center_df$my[j]
      # Find pixels within the specified radius of the current center
      center_sur <- which(
        xy_coords$x > (x - radius) &
          xy_coords$x < (x + radius) &
          xy_coords$y > (y - radius) &
          xy_coords$y < (y + radius)
      )

      # Check that the detected pixels are not part of their own object
      if (is.null(center_sur) == FALSE &
          length(unique(xy_coords$value[center_sur])) > 1) {
        too_close <- unique(xy_coords$value[center_sur])

        # Remove the close objects from the xy coordinate data set
        for (k in too_close) {
          xy_pos <- which(xy_coords$value == k)
          xy_edit[xy_pos, ] <- NA
        }
      } else {
        next
      }
    }

    # Remove NAs from the edited coordinates
    pre_coords <- na.omit(xy_edit)

    # Keep only xy coordinates that pass the proximity filter and are included
    # in centers
    update <- list()
    for (l in center_df$value) {
      to_keep <- which(pre_coords$value == l)
      if (length(to_keep) != 0) {
        update[to_keep] <- to_keep
      }
    }

    # Result: xy coordinates that pass the filter
    xy_coords_clus <- pre_coords[unlist(update), ]

    # Result: remaining centers
    DT <- data.table(xy_coords_clus)

    # Summarize by center and calculate mean coordinates
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  # Automated radius calculation if radius is set to "auto"
  if (radius == "auto") {
    # Calculate radius from the average size (assuming circular objects)
    mean_size <- mean(size)
    radius <- sqrt(mean_size / pi)

    # Radius times two to cover the entire object starting from the center
    radius <- round(radius) * elongation

    # Proximity calculation for the auto-calculated radius
    xy_edit <- xy_coords
    for (j in 1:nrow(center_df)) {
      x <- center_df$mx[j]
      y <- center_df$my[j]
      # Find pixels within the auto-calculated radius of the current center
      center_sur <- which(
        xy_coords$x > (x - radius) &
          xy_coords$x < (x + radius) &
          xy_coords$y > (y - radius) &
          xy_coords$y < (y + radius)
      )

      # Check that the detected pixels are not part of their own object
      if (is.null(center_sur) == FALSE &
          length(unique(xy_coords$value[center_sur])) > 1) {
        too_close <- unique(xy_coords$value[center_sur])

        # Remove the close objects from the xy coordinate data set
        for (k in too_close) {
          xy_pos <- which(xy_coords$value == k)
          xy_edit[xy_pos, ] <- NA
        }
      } else {
        next
      }
    }

    # Remove NAs from the edited coordinates
    pre_coords <- na.omit(xy_edit)

    # Keep only xy coordinates that pass the proximity filter and are included
    # in centers
    update <- list()
    for (l in center_df$value) {
      to_keep <- which(pre_coords$value == l)
      if (length(to_keep) != 0) {
        update[to_keep] <- to_keep
      }
    }

    # Result: xy coordinates that pass the filter
    xy_coords_clus <- pre_coords[unlist(update), ]

    # Result: remaining centers
    DT <- data.table(xy_coords_clus)

    # Summarize by center and calculate mean coordinates
    remaining_center_df <-
      DT[, list(mx = mean(x), my = mean(y)), by = value]
  }

  # Return the filtered centers, coordinates, and sizes as a list
  out <- list(centers = remaining_center_df,
              coordinates = xy_coords_clus,
              size = cluster_size[remaining_center_df$value])
}
