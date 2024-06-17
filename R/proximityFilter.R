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
#' @returns list of 2 objects:
#' \itemize{
#'   \item Center coordinates of remaining objects.
#'   \item All coordinates of remaining objects.
#' }
#' @details
#' The automated radius calculation in the \code{\link[biopixR]{proximityFilter}}
#' function is based on the presumption of circular-shaped objects. The radius
#' is calculated using the following formula:
#' \deqn{\sqrt{\frac{A}{\pi}}}{sqrt(A/pi)}
#' where A is the area of the detected objects. The function will exclude
#' objects that are too close by extending the calculated radius by one radius
#' length beyond the assumed circle, effectively doubling the radius to create
#' an exclusion zone. Therefore the elongation factor is set to 2 by default,
#' with one radius covering the object and an additional radius creating the
#' area of exclusion.
#' @examples
#' res_objectDetection <- objectDetection(beads,
#'                                        alpha = 1,
#'                                        sigma = 0)
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
  size <- center_df$size

  if (radius != "auto") {
    # Proximity calculation for a given radius
    # Function to calculate distances and filter too-close objects
    proximityCalc <- function(xy_coords, center_df, radius) {
      xy_edit <- xy_coords
      for (j in seq_len(nrow(center_df))) {
        x <- center_df$mx[j]
        y <- center_df$my[j]

        # Define the bounding box for the radius
        x_min <- x - radius
        x_max <- x + radius
        y_min <- y - radius
        y_max <- y + radius

        # Find pixels within the specified radius of the current center
        in_radius <- (xy_coords$x > x_min) & (xy_coords$x < x_max) &
          (xy_coords$y > y_min) & (xy_coords$y < y_max)

        # Check that the detected pixels are not part of their own object
        if (any(in_radius)) {
          center_sur <- xy_coords[in_radius, , drop = FALSE]
          if (length(unique(center_sur$value)) > 1) {
            too_close <- unique(center_sur$value)

            # Remove the close objects from the xy coordinate data set
            for (k in too_close) {
              xy_edit[xy_edit$value == k,] <- NA
              xy_edit <-
                na.omit(xy_edit)   # Remove NAs from the edited coordinates
            }
          }
        }
      }
      return(xy_edit)
    }

    # Usage
    xy_edit <- proximityCalc(xy_coords, center_df, radius)

    # Result: xy coordinates that pass the filter
    xy_coords_clus <- xy_edit[xy_edit$value %in% center_df$value, ]

    # Result: remaining centers
    DT <- data.table(xy_coords_clus)

    # Summarize by center and calculate mean coordinates
    remaining_center_df <-
      DT[, list(mx = mean(x),
                my = mean(y),
                size = length(x)), by = value]
  }

  # Automated radius calculation if radius is set to "auto"
  if (radius == "auto") {
    # Calculate radius from the average size (assuming circular objects)
    mean_size <- mean(size)
    radius <- sqrt(mean_size / pi)

    # Radius times two to cover the entire object starting from the center
    radius <- round(radius) * elongation

    # Function to calculate distances and filter too-close objects
    proximityCalc <- function(xy_coords, center_df, radius) {
      xy_edit <- xy_coords
      for (j in seq_len(nrow(center_df))) {
        x <- center_df$mx[j]
        y <- center_df$my[j]

        # Define the bounding box for the radius
        x_min <- x - radius
        x_max <- x + radius
        y_min <- y - radius
        y_max <- y + radius

        # Find pixels within the specified radius of the current center
        in_radius <- (xy_coords$x > x_min) & (xy_coords$x < x_max) &
          (xy_coords$y > y_min) & (xy_coords$y < y_max)

        # Check that the detected pixels are not part of their own object
        if (any(in_radius)) {
          center_sur <- xy_coords[in_radius, , drop = FALSE]
          if (length(unique(center_sur$value)) > 1) {
            too_close <- unique(center_sur$value)

            # Remove the close objects from the xy coordinate data set
            for (k in too_close) {
              xy_edit[xy_edit$value == k,] <- NA
              xy_edit <-
                na.omit(xy_edit)   # Remove NAs from the edited coordinates
            }
          }
        }
      }
      return(xy_edit)
    }

    # Usage
    xy_edit <- proximityCalc(xy_coords, center_df, radius)

    # Result: xy coordinates that pass the filter
    xy_coords_clus <- xy_edit[xy_edit$value %in% center_df$value, ]

    # Result: remaining centers
    DT <- data.table(xy_coords_clus)

    # Summarize by center and calculate mean coordinates
    remaining_center_df <-
      DT[, list(mx = mean(x),
                my = mean(y),
                size = length(x)), by = value]
  }

  # Return the filtered centers, coordinates, and sizes as a list
  out <- list(centers = remaining_center_df,
              coordinates = xy_coords_clus)
}
