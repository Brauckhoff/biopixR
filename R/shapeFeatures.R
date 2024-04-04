#' Extraction of shape features
#'
#' This function analyzes the objects detected in an image and calculates
#' distinct shape characteristics for each object, such as circularity,
#' eccentricity, radius, and perimeter. The resulting shape attributes can then
#' be grouped using a Self-Organizing Map (SOM) from the Kohonen package.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @param SOM if TRUE runs SOM algorithms on extracted shape features, grouping
#' the detected objects
#' @param xdim x-dimension for the SOM-grid (grid = hexagonal)
#' @param ydim y-dimension for the SOM-grid (xdim * ydim = number of neurons)
#' @param parallel if TRUE uses multiple cores (75 %) to process results
#' @param visualize visualizes the groups computed by SOM
#' @returns
#' data frame containing detailed information about every single bead
#' @import data.table
#' @examples
#' shapeFeatures(
#'   beads,
#'   alpha = 1,
#'   sigma = 2,
#'   SOM = TRUE,
#'   parallel = FALSE,
#'   visualize = TRUE
#' )
#' @export
shapeFeatures <-
  function(img,
           alpha = 1,
           sigma = 2,
           xdim = 2,
           ydim = 1,
           SOM = FALSE,
           parallel = FALSE,
           visualize = FALSE) {
    # convert to grayscale
    if (dim(img)[4] != 1) {
      warning("Running edge detector on luminance channel")
      img <- grayscale(img)
    }

    # detection of objects with interal function
    res_objectDetection <-
      objectDetection(img, alpha = alpha, sigma = sigma)
    res_resultAnalytics <-
      resultAnalytics(
        res_objectDetection$coordinates,
        res_objectDetection$coordinates,
        res_objectDetection$size,
        img,
        parallel = parallel
      )

    # extract feature: area
    area <- res_resultAnalytics$detailed$size

    # extract feature: perimeter
    edge_img <- edgeDetection(img, alpha = alpha, sigma = sigma)

    unique_labels <- unique(res_objectDetection$coordinates$value)
    # coordinates of labeled object
    object_coords <- res_objectDetection$coordinates

    # assign each coordinate a TRUE / FALSE value whether the pixel is part of
    # the edge or not
    for (i in 1:nrow(res_objectDetection$coordinates)) {
      x <- res_objectDetection$coordinates$x[i]
      y <- res_objectDetection$coordinates$y[i]
      t_edge <- edge_img[x, y, 1, 1]

      res_objectDetection$coordinates$edge[i] <- t_edge
    }

    # collect all pixels that are part of the edge and create a data frame
    pos_true <- which(res_objectDetection$coordinates$edge == TRUE)
    df_edges <- res_objectDetection$coordinates[pos_true,]

    # calculate perimeter by grouping by each value and count number of pixels in
    # each of the objects contours
    DT <- as.data.table(df_edges)
    DT_peri <- DT[, list(perimeter = length(x)), by = value]
    perimeter <- DT_peri$perimeter

    # extract feature: radius
    # get center coordinates
    center <-
      cbind(res_resultAnalytics$detailed$x,
            res_resultAnalytics$detailed$y)

    distances <- data.frame(matrix(ncol = 2, nrow = nrow(center)))
    for (i in 1:nrow(center)) {
      pos_edge <- which(df_edges$value == i)

      # calculate the distance from each edge pixel towards the center coordinate
      # formula for the Euclidean distance
      distance <-
        sqrt((df_edges$x[pos_edge] - center[i, 1]) ^ 2 +
               (df_edges$y[pos_edge] - center[i, 2]) ^ 2)
      min_radius <- min(distance)
      max_radius <- max(distance)
      mean_radius <- mean(c(min_radius, max_radius))
      sd_radius <- sd(c(min_radius, max_radius))

      distances[i, 1] <- mean_radius
      distances[i, 2] <- sd_radius
      distances[i, 3] <- max_radius
      distances[i, 4] <- min_radius
    }

    # extract feature: eccentricity
    eccentricity <-
      (distances[, 3] - distances[, 4]) / (distances[, 3] + distances[, 4])

    # extract feature: circularity
    circularity <- (4 * pi * area) / perimeter ^ 2

    # assign results
    mean_radius <- distances$X1
    sd_radius <- distances$X2
    intensity <- res_resultAnalytics$detailed$intensity

    # Combine features into a data frame
    features <-
      data.frame(intensity,
                 area,
                 perimeter,
                 circularity,
                 eccentricity,
                 mean_radius,
                 sd_radius)

    if (SOM == FALSE) {
      res_resultAnalytics$detailed <-
        cbind(res_resultAnalytics$detailed, features[, 3:7])
      return(res_resultAnalytics$detailed)
    } else {
      if (requireNamespace("kohonen", quietly = TRUE)) {
        data <- scale(features)
        grid_size <-
          kohonen::somgrid(xdim = xdim,
                           ydim = ydim,
                           topo = "hexagonal")
        som_model <-
          kohonen::som(
            data,
            grid = grid_size,
            rlen = 10000,
            alpha = c(0.05, 0.01),
            keep.data = TRUE
          )
        res_resultAnalytics$detailed$class <- som_model$unit.classif

        res_resultAnalytics$detailed <-
          cbind(res_resultAnalytics$detailed, features[, 3:7])

        if (visualize == TRUE) {
          img |> plot()
          with(
            res_resultAnalytics$detailed,
            points(
              res_resultAnalytics$detailed$x,
              res_resultAnalytics$detailed$y,
              col = factor(res_resultAnalytics$detailed$class),
              pch = 19
            )
          )

        }
        return(res_resultAnalytics$detailed)
      } else {
        cat("Please install the Package 'kohonen' for SOM \n (install.package('kohonen')")
      }
    }
  }
