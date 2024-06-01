#' Extraction of Shape Features
#'
#' This function analyzes the objects detected in an image and calculates
#' distinct shape characteristics for each object, such as circularity,
#' eccentricity, radius, and perimeter. The resulting shape attributes can then
#' be grouped using a Self-Organizing Map (SOM) from the 'Kohonen' package.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param SOM if TRUE runs SOM algorithm on extracted shape features, grouping
#' the detected objects
#' @param xdim x-dimension for the SOM-grid (grid = hexagonal)
#' @param ydim y-dimension for the SOM-grid (xdim * ydim = number of neurons)
#' @param visualize visualizes the groups computed by SOM
#' @returns \code{data.frame} containing detailed information about every single object.
#' @import data.table
#' @seealso [objectDetection()], [resultAnalytics()], \code{\link[kohonen]{som}}
#' @examples
#' shapeFeatures(
#'   beads,
#'   alpha = 1,
#'   sigma = 0,
#'   SOM = TRUE,
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
           visualize = FALSE) {
    # Binding for global variables
    min_radius <- max_radius <- NULL

    # Assign input image to a variable
    object_img <- img

    # Convert to grayscale if the image is not already in grayscale
    if (dim(object_img)[4] != 1) {
      warning("Running edge detector on luminance channel")
      object_img <- grayscale(object_img)
    }

    # Detect objects in the image using the internal objectDetection function
    res_objectDetection <-
      objectDetection(object_img, alpha = alpha, sigma = sigma)

    # Analyze the results of object detection
    res_resultAnalytics <-
      resultAnalytics(res_objectDetection$coordinates,
                      img = object_img)

    # Extract feature: area
    area <- res_resultAnalytics$detailed$size

    # Extract feature: perimeter
    edge_img <-
      edgeDetection(object_img, alpha = alpha, sigma = sigma)
    unique_labels <- unique(res_objectDetection$coordinates$value)
    object_coords <- res_objectDetection$coordinates

    # Vectorized operation to assign TRUE/FALSE values indicating whether the pixel is part of the edge
    object_coords$edge <-
      edge_img[cbind(object_coords$x, object_coords$y, 1, 1)]

    # Ensure the result is logical (TRUE/FALSE)
    object_coords$edge <- object_coords$edge == 1

    # Collect all pixels that are part of the edge and create a data frame
    df_edges <- object_coords[object_coords$edge == TRUE, ]

    # Calculate perimeter by grouping by each value and counting the number of
    # pixels in each object's contour
    DT <- as.data.table(df_edges)
    DT_peri <- DT[, list(perimeter = length(x)), by = value]

    perimeter <- DT_peri$perimeter

    # Extract feature: radius
    # Get center coordinates
    center <-
      as.data.table(res_resultAnalytics$detailed[, c("x", "y")])

    # Convert df_edges to data.table for efficiency
    setDT(df_edges)

    # Initialize a data.table to store the radius metrics
    distances <- data.table(
      mean_radius = numeric(nrow(center)),
      sd_radius = numeric(nrow(center)),
      max_radius = numeric(nrow(center)),
      min_radius = numeric(nrow(center))
    )

    # Calculate distances using vectorized operations
    for (i in seq_len(nrow(center))) {
      pos_edge <- df_edges[value == i, list(x, y)]

      # Calculate the Euclidean distances from the center to each edge pixel
      distances_i <-
        sqrt((pos_edge$x - center$x[i]) ^ 2 + (pos_edge$y - center$y[i]) ^ 2)

      distances[i, mean_radius := mean(distances_i)]
      distances[i, sd_radius := sd(distances_i)]
      distances[i, max_radius := max(distances_i)]
      distances[i, min_radius := min(distances_i)]
    }

    # Extract feature: eccentricity
    eccentricity <-
      (distances$max_radius - distances$min_radius) / (distances$max_radius + distances$min_radius)

    # Extract feature: circularity
    circularity <- (4 * pi * area) / perimeter ^ 2

    # Extract feature: aspect ratio
    aspect_ratio <-
      (2 * distances$max_radius) / (2 * distances$min_radius)

    # Assign results to variables
    mean_radius <- distances$mean_radius
    sd_radius <- distances$sd_radius
    intensity <- res_resultAnalytics$detailed$intensity

    # Combine features into a data frame
    features <-
      data.frame(
        intensity,
        area,
        perimeter,
        circularity,
        eccentricity,
        mean_radius,
        sd_radius,
        aspect_ratio
      )

    # If SOM is not used, combine features with the detailed results and return
    if (SOM == FALSE) {
      res_resultAnalytics$detailed <-
        cbind(res_resultAnalytics$detailed, features[, 3:8])
      return(res_resultAnalytics$detailed)
    } else {
      # If SOM is used, perform self-organizing map clustering
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

        # Add identified class to resulting data frame
        res_resultAnalytics$detailed$class <- som_model$unit.classif

        # Combine resulting data frame with shape features
        res_resultAnalytics$detailed <-
          cbind(res_resultAnalytics$detailed, features[, 3:8])

        # If visualize is TRUE, plot the image and the detected points
        if (visualize == TRUE) {
          object_img |> plot()
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
