#' Image Summary
#'
#' Extracts all important information of the remaining microbeads. This function
#' summarizes the data obtained by previous functions: objectDetection,
#' proximityFilter and sizeFilter. Provides information like amount, intensity,
#' size and density.
#' @param unfiltered all coordinates from every object before applying filter functions
#' @param coordinates all filtered coordinates of the objects (x|y|value data frame)
#' @param size size of the objects
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param parallel if TRUE uses multiple cores (75 %) to process results
#' @returns list of 2 objects:
#' 1. summary of all the microbeads in the image
#' 2. detailed information about every single bead
#' @import data.table
#' @import parallel
#' @importFrom stats na.omit
#' @import foreach
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_sizeFilter <- sizeFilter(
#'   res_objectDetection$centers,
#'   res_objectDetection$coordinates,
#'   lowerlimit = 50, upperlimit = 150
#'   )
#' res_proximityFilter <- proximityFilter(
#'   res_sizeFilter$centers,
#'   res_objectDetection$coordinates,
#'   radius = "auto"
#'   )
#' res_resultAnalytics <- resultAnalytics(
#'   unfiltered = res_objectDetection$coordinates,
#'   coordinates = res_proximityFilter$coordinates,
#'   size = res_proximityFilter$size,
#'   img = beads
#'   )
#' plot(beads)
#' with(
#'   res_objectDetection$centers,
#'   points(
#'     res_objectDetection$centers$mx,
#'     res_objectDetection$centers$my,
#'     col = "red",
#'     pch = 19
#'     )
#'   )
#' with(
#'   res_resultAnalytics$detailed,
#'   points(
#'     res_resultAnalytics$detailed$x,
#'     res_resultAnalytics$detailed$y,
#'     col = "darkgreen",
#'     pch = 19
#'     )
#'   )
#' @export
resultAnalytics <- function(unfiltered,
                            coordinates,
                            size,
                            img,
                            parallel = FALSE) {
  # binding for global variables
  intensity <- cluster <- NULL

  # assign imports
  all_coords <- unfiltered
  xy_coords <- coordinates
  cluster_size <- size
  pic <- img

  # including intensity values of pixels from remaining clusters in a data frame
  for (h in 1:nrow(xy_coords)) {
    x <- xy_coords$x[h]
    y <- xy_coords$y[h]
    int <- as.array(pic)[x, y, , ]
    xy_coords$intensity[h] <- c(int)
  }

  # group data frame by cluster
  DT_intense <- data.table(xy_coords)
  intense <- DT_intense[, list(
    x = mean(x),
    y = mean(y),
    intensity = mean(intensity)
  ),
  by = value
  ]

  if (length(cluster_size) != nrow(intense)) {
    # adapting size according to remaining coordinates
    cluster_size <- unlist(cluster_size[intense$value])
  }

  # approximate amount of discarded pixels
  # calculate amount of true coordinates
  amount_true <- nrow(all_coords)
  dis_count <- round(amount_true / mean(unlist(size)) - nrow(intense))

  # summary for every passing bead
  res_df_long <- data.frame(
    beadnumber = intense$value,
    size = unlist(cluster_size),
    intensity = intense$intensity,
    x = intense$x,
    y = intense$y
  )

  # summary of res_df_long / whole image
  result <- data.frame(
    number_of_beads = nrow(intense),
    mean_size = mean(unlist(cluster_size)),
    mean_intensity = mean(xy_coords$intensity),
    bead_density = amount_true / length(pic),
    estimated_rejected = dis_count
  )

  # calculate the relative distance between all object centers based on
  # Euclidean distance
  DT <- data.table(coordinates)
  res_center <- DT[, list(x = mean(x), y = mean(y)), by = value]

  # formula for the Euclidean distance
  euclidean_distance <- function(point1, point2) {
    sqrt((point2$x - point1$x)^2 + (point2$y - point1$y)^2)
  }

  num_points <- nrow(res_center)
  relative_distances <- matrix(NA, nrow = num_points, ncol = num_points)

  # using parallel processing to compare the distance between all objects
  if (parallel == TRUE) {
    if (requireNamespace("doParallel", quietly = TRUE)) {
      # Code that uses functions from the suggested packages

      n_cores <- round(detectCores() * 0.75)
      my_cluster <- makeCluster(n_cores,
                                type = "PSOCK")
      doParallel::registerDoParallel(cl = my_cluster)

      relative_distances <-
        foreach(i = 1:num_points, .combine = 'cbind') %dopar% {
          distances <- rep(NA, num_points)
          for (j in 1:num_points) {
            if (i != j) {
              distances[j] <- euclidean_distance(res_center[i, ], res_center[j, ])
            }
          }
          distances
        }

      stopCluster(cl = my_cluster)
    } else {
      # Handle the case when any of the suggested packages is not available
      warning("Please install the Package 'doParallel' for parallel processing \n (install.package('doparallel')")
    }
  }


  # calculating distance without parallel processing
  if (parallel == FALSE) {
    for (i in 1:num_points) {
      for (j in 1:num_points) {
        if (i == j) {
          next
        } else {
          relative_distances[i, j] <-
            euclidean_distance(res_center[i,], res_center[j,])
        }
      }
    }
    as.matrix(relative_distances)
  }

  # add relative distance to the detailed result
  for (a in 1:num_points) {
    res_df_long$relative_distance[a] <- mean(na.omit(relative_distances[a, ]))
  }

  # add mean distance to the summarized result
  result$mean_distance <- mean(res_df_long$relative_distance)

  out <- list(
    summary = result,
    detailed = res_df_long
  )
}
