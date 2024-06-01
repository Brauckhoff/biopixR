#' Object detection
#'
#' This function identifies objects in an image using either edge detection or
#' thresholding methods. It gathers the coordinates and centers of the
#' identified objects, highlighting the edges or overall coordinates for easy
#' recognition.
#' @param img image (import by \code{\link[biopixR]{importImage}})
#' @param method choose method for object detection ('edge' / 'threshold')
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian') (only needed for 'edge')
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian') (only needed for 'edge')
#' @param vis creates image were object edges/coordinates (purple) and detected centers (green) are highlighted (TRUE | FALSE)
#' @returns list of 3 objects:
#' \itemize{
#'   \item \code{data.frame} of labeled regions with the central coordinates (including size information).
#'   \item All coordinates that are in labeled regions.
#'   \item Image where object edges/coordinates (purple) and detected centers (green) are colored.
#' }
#' @details
#' The \code{\link[biopixR]{objectDetection}} function provides several methods
#' for calculating the alpha and sigma parameters, which are critical for edge
#' detection:
#' \enumerate{
#'   \item \strong{Input of a Numeric Value:}
#'   \itemize{
#'     \item Users can directly input numeric values for alpha and sigma, allowing for precise control over the edge detection parameters.
#'   }
#'   \item \strong{Static Scanning:}
#'   \itemize{
#'     \item When both alpha and sigma are set to "static", the function systematically tests all possible combinations of these parameters within the range (alpha: 0.1 - 1.5, sigma: 0 - 2). This exhaustive search helps identify the optimal parameter values for the given image. (Note: takes a lot of time)
#'   }
#'   \item \strong{Interactive Selection:}
#'   \itemize{
#'     \item Setting the alpha and sigma values to "interactive" initiates a Tcl/Tk graphical user interface (GUI). This interface allows users to adjust the parameters interactively, based on visual feedback. To achieve optimal results, the user must input the necessary adjustments to align the parameters with the specific requirements of the image. The user can also switch between the methods through the interface.
#'   }
#'   \item \strong{Multi-Objective Optimization:}
#'   \itemize{
#'     \item For advanced parameter optimization, the function \code{\link[GPareto]{easyGParetoptim}} will be utilized for multi-objective optimization using Gaussian process models. This method leverages the 'GPareto' package to perform the optimization. It involves building Gaussian Process models for each objective and running the optimization to find the best parameter values.
#'   }
#' }
#' @import data.table
#' @import imager
#' @importFrom stats complete.cases
#' @examples
#' res_objectDetection <- objectDetection(beads,
#'                                        method = 'edge',
#'                                        alpha = 1,
#'                                        sigma = 0)
#' res_objectDetection$marked_objects |> plot()
#'
#' res_objectDetection <- objectDetection(beads,
#'                                        method = 'threshold')
#' res_objectDetection$marked_objects |> plot()
#' @export
objectDetection <- function(img,
                            method = 'edge',
                            alpha = 1,
                            sigma = 2,
                            vis = TRUE) {
  # Assign import
  object_img <- img

  # Ensure the image is of type 'cimg', if not, stop the function
  if (class(object_img)[1] != "cimg") {
    stop("image must be of class 'cimg'")
  }

  # Convert luminescence channel images to grayscale
  if (dim(object_img)[4] != 1) {
    object_img <- grayscale(object_img)
    warning(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Image is from a luminescence channel and was converted into grayscale"
    )
  }

  if (method == 'edge') {
    # Handle numeric input for alpha and sigma
    if (is.numeric(alpha) & is.numeric(sigma)) {
      alpha_x <- alpha
      sigma_x <- sigma
    }

    # If set to 'static', optimization is attempted - testing all combinations
    if (alpha == "static" &
        sigma == "static") {
      # Fitness function to evaluate parameter sets
      staticHayflick <- function(object_img, alpha_a, sigma_a) {
        # Compute shape features based on the image and given parameters
        property <- shapeFeatures(object_img, alpha_a, sigma_a)

        # Filter rows without missing values
        df_complete <- property[complete.cases(property), ]
        # Apply a thresholding function to the image
        t <- threshold(object_img)

        # Calculate a variety of statistics based on image features
        combine <- data.frame(
          area = abs(length(which(t == TRUE)) / sum(property$size) - 1),
          pixels = length(which(
            df_complete$size < (mean(df_complete$size) - 0.9 * mean(df_complete$size))
          )) / mean(df_complete$size),
          sd_area = sd(df_complete$size) / mean(df_complete$size),
          sd_perimeter = sd(df_complete$perimeter) / mean(df_complete$perimeter),
          mean_circularity = mean(abs(df_complete$circularity - 1)),
          mean_eccentricity = mean(df_complete$eccentricity),
          sd_radius = sd(df_complete$mean_radius) / mean(df_complete$mean_radius)
        )

        # Calculate overall quality score as average of combined statistics
        quality <- sum(combine) / ncol(combine)

        # Return both quality and total size as a list
        quality_size <- list(quality, sum(property$size))
        return(quality_size)
      }

      # Generate parameter ranges for alpha and sigma
      alpha_range <- seq(0.1, 1.5, by = 0.1)
      sigma_range <- seq(0, 2, by = 0.1)

      # Create a grid of parameter combinations for alpha and sigma
      param_grid <-
        expand.grid(alpha = alpha_range, sigma = sigma_range)
      n <- nrow(param_grid)  # Number of parameter combinations

      # Initialize an empty data frame to store results
      results_df <-
        data.frame()

      # Loop through each row in the parameter grid
      for (b in 1:n) {
        row <- param_grid[b, ]
        # Call the hayflick function and handle errors
        res_main <- tryCatch({
          staticHayflick(object_img, row$alpha, row$sigma)
        }, error = function(error_condition) {
          return(list(NA, NA))  # Ensure the list has two NAs to match expected structure
        })
        # Create a data frame row from the results and bind it to the results data frame
        temp_df <- data.frame(quality = unlist(res_main[1]),
                              size = unlist(res_main[2]))
        results_df <-
          rbind(results_df, temp_df)  # Combine results row-wise
      }

      # Define the reverse min-max normalization function
      normalizeReverseMinmax <- function(x) {
        return(1 - (x - min(x)) / (max(x) - min(x)))
      }

      # Filter rows without missing values
      results_df <- results_df[complete.cases(results_df),]

      # Normalize and aggregate results for fitness calculation
      if (length(unique(results_df$size)) != 1) {
        results_df$size <- normalizeReverseMinmax(results_df$size) / 7
        fitness <- rowSums(results_df) / 2
      } else {
        fitness <- results_df$quality
      }

      # Store unique fitness scores and sort them
      u_fitness <- unique(fitness)
      sorted_res <- sort(u_fitness, decreasing = FALSE)
      sorted_res <- sorted_res[1]

      # Gather best results based on sorted fitness scores
      mylist <- list()
      for (b in 1:length(sorted_res)) {
        index <- which(fitness == sorted_res[b])
        mylist[[b]] <- index
      }

      # Retrieve the best parameter set from the parameter grid
      result_1 <- param_grid[unlist(mylist),]
      result <- result_1
      result <- result[1,]
      alpha_x <- result$alpha
      sigma_x <- result$sigma
    }

    # If parameters are set to 'interactive', call the interactive detection function
    if (alpha == "interactive" & sigma == "interactive") {
      parameter <- interactive_objectDetection(object_img)
      alpha_x <- as.numeric(parameter[1])
      sigma_x <- as.numeric(parameter[2])
      method <- parameter[4]
    }

    # If parameters are set to 'gaussian', perform Gaussian process optimization
    if (alpha == "gaussian" &
        sigma == "gaussian") {
      if (requireNamespace(c("GPareto"), quietly = TRUE)) {
        gaussianHayflick <- function(x) {
          alpha_a <- x[1]
          sigma_a <- x[2]

          property <- tryCatch({
            # Compute shape features based on the image and given parameters
            property <-
              shapeFeatures(object_img, alpha = alpha_a, sigma = sigma_a)
            property
          }, error = function(e) {
            return(NULL)
          })

          if (is.null(property)) {
            quality <- 2
            property <- list(size = 0)
          } else {
            # Filter rows without missing values
            df_complete <- property[complete.cases(property), ]
            # Apply a thresholding function to the image
            t <- threshold(object_img)

            # Calculate a variety of statistics based on image features
            combine <- data.frame(
              area = abs(length(which(
                t == TRUE
              )) / sum(property$size) - 1),
              pixels = length(which(
                df_complete$size < (
                  mean(df_complete$size) - 0.9 * mean(df_complete$size)
                )
              )) / mean(df_complete$size),
              sd_area = sd(df_complete$size) / mean(df_complete$size),
              sd_perimeter = sd(df_complete$perimeter) / mean(df_complete$perimeter),
              mean_circularity = mean(abs(df_complete$circularity - 1)),
              mean_eccentricity = mean(df_complete$eccentricity),
              sd_radius = sd(df_complete$mean_radius) / mean(df_complete$mean_radius)
            )

            # Calculate overall quality score as average of combined statistics
            quality <-
              sum(na.omit(t(combine))) / nrow(na.omit(t(combine)))
          }

          # Return both quality and total size as a list
          return(c(quality,-sum(property$size)))
        }

        # Define bounds for the parameters
        lower_bounds <- c(alpha = 0.1, sigma = 0)
        upper_bounds <- c(alpha = 1.5, sigma = 2)

        # Perform multi-objective optimization using Gaussian Process models
        results <-
          GPareto::easyGParetoptim(
            fn = gaussianHayflick,
            budget = 20,
            lower = lower_bounds,
            upper = upper_bounds
          )

        # Extract the Pareto-optimal solutions
        pareto_set <- results$par
        pareto_front <- results$value

        # Function for Min-Max Normalization
        min_max_normalize <- function(x) {
          if (!is.numeric(x))
            stop("Input must be numeric")
          if (length(unique(x)) == 1)
            return(rep(0, length(x))) # Avoid division by zero when all values are identical

          min_val <- min(x)
          max_val <- max(x)

          normalized_x <- (x - min_val) / (max_val - min_val)
          return(normalized_x)
        }

        pareto_front_n <- pareto_front
        pareto_front_n[, 2] <- min_max_normalize(pareto_front[, 2])

        # Calculate the Euclidean distance from the ideal point
        ideal_point <- c(0, 0)
        distances <-
          apply(pareto_front_n, 1, function(x)
            sqrt(sum((
              x - ideal_point
            ) ^ 2)))

        # Find the index of the knee point
        knee_point_index <- which.min(distances)

        # Extract the knee point parameters
        knee_point_parameters <- pareto_set[knee_point_index, ]

        # Extract optimized parameters
        alpha_x <- knee_point_parameters[1]
        sigma_x <- knee_point_parameters[2]

      } else {
        stop(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " Please install the Package 'GPareto' for multi-objective optimization. \n (install.package('GPareto')"
        )
      }
    }

    # Apply edge detection to the image using specified alpha and sigma parameters
    edge_img <-
      edgeDetection(object_img, alpha = alpha_x, sigma = sigma_x)

    # Label the edges detected in the image to identify distinct regions
    first_lab <- label(edge_img)
    # Identify indices where labels are non-zero (indicating detected edges)
    fill_hulls <- which(first_lab != 0)
    filled_img <- edge_img
    # Fill these detected edges in the image
    filled_img[fill_hulls] <- TRUE
    # Label the filled image to separate distinct objects
    labeled_img <- label(filled_img)

    # Convert the labeled image to a data frame, excluding background (label 0)
    df_lab_img <- as.data.frame(labeled_img) |>
      subset(value > 0)
    # Convert to data table for efficient data manipulation
    DT <- data.table(df_lab_img)

    # Aggregate data to calculate the mean coordinates for each unique
    # label (cluster)
    grouped_lab_img <-
      DT[, list(mx = mean(x),
                my = mean(y),
                size = length(x)), by = value]
  }

  if (method == 'threshold') {
    if (requireNamespace(c("imagerExtra"), quietly = TRUE)) {
      # Solving Screened Poisson Equation (SPE)
      spe_img <- imagerExtra::SPE(object_img, lamda = 0.1)
      thresh_spe <- threshold(spe_img)
      coords_spe <- as.data.frame(thresh_spe)

      # If too many coordinates are detected, reduce lambda
      if (nrow(coords_spe) > (dim(object_img)[1] * dim(object_img)[1] *
                              0.2)) {
        spe_img <- imagerExtra::SPE(object_img, lamda = 0.05)
        thresh_spe <- threshold(spe_img)
        coords_spe <- as.data.frame(thresh_spe)
      }

      if (nrow(coords_spe) > (dim(object_img)[1] * dim(object_img)[1] *
                              0.2)) {
        spe_img <- imagerExtra::SPE(object_img, lamda = 0.01)
        thresh_spe <- threshold(spe_img)
        coords_spe <- as.data.frame(thresh_spe)
      }

      thresh_ori <- threshold(object_img)

      combined_img <- add(list(thresh_ori, thresh_spe))
      combined_img[which(combined_img > 0)] <- 1

      labeled_components <-
        label(combined_img, high_connectivity = TRUE)

      df_lab <-
        as.data.frame(labeled_components) |> subset(value > 0)

      common_rows <-
        merge(coords_spe[, 1:2], df_lab[, 1:3], by = c("x", "y"))

      # Convert to data table for efficient data manipulation
      DT <- data.table(common_rows)

      # Aggregate data to calculate the mean coordinates for each unique
      # label (cluster)
      grouped_lab_img <-
        DT[, list(mx = mean(x),
                  my = mean(y),
                  size = length(x)), by = value]

      small_regions <-
        grouped_lab_img[grouped_lab_img$size < (0.1 * mean(grouped_lab_img$size))]
      large_regions <-
        grouped_lab_img[grouped_lab_img$size >= (0.1 * mean(grouped_lab_img$size)), ]

      # Function to calculate Euclidean distance
      euclidean_distance <- function(x1, y1, x2, y2) {
        sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
      }

      # Distance threshold for merging small regions into large regions
      distance_threshold <-
        1.5 * sqrt(mean(grouped_lab_img$size) / pi)  # Distance threshold for merging

      # Merge small regions into nearest large regions if within distance threshold
      if (nrow(small_regions) != 0) {
        for (i in seq_len(nrow(small_regions))) {
          small_region <- small_regions[i,]
          distances <-
            mapply(
              euclidean_distance,
              small_region$mx,
              small_region$my,
              large_regions$mx,
              large_regions$my
            )

          # Find the nearest large region
          closest_idx <- which.min(distances)
          closest_distance <- distances[closest_idx]

          if (closest_distance < distance_threshold) {
            closest_cluster <- large_regions[closest_idx,]

            # Merge small region into large region
            common_rows$value[common_rows$value == small_regions$value[i]] <-
              closest_cluster$value

          }
        }
      }

      # Aggregate merged data
      DT1 <- data.table(common_rows)
      grouped_lab_img <-
        DT1[, list(mx = mean(x),
                   my = mean(y),
                   size = length(x)), by = value]

      # Discard clusters that are very likely noise (small size clusters)
      grouped_lab_img <-
        grouped_lab_img[grouped_lab_img$size >= (0.1 * mean(grouped_lab_img$size))]

      # Get all pixels that are part of significant clusters
      all_pixels <-
        common_rows[common_rows$value %in% grouped_lab_img$value,]

      # Assign output variables
      df_lab_img <- all_pixels
      edge_coords <- all_pixels

    } else {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Please install the Package 'imagerExtra' for method 'threshold'. \n (install.package('imagerExtra')"
      )
    }
  }

  if (vis == TRUE) {
    if (method == 'edge') {
      # Get coordinates of detected edges
      edge_coords <- which(edge_img == TRUE, arr.ind = TRUE)
    }
    # Change the color of the detected edges to purple for visualization
    colored_edge <-
      changePixelColor(object_img, edge_coords, color = "purple")
    # Draw circles around the detected clusters using their mean coordinates
    colored_edge <- draw_circle(
      colored_edge,
      grouped_lab_img$mx,
      grouped_lab_img$my,
      radius = (sqrt(mean(
        grouped_lab_img$size
      ) / pi) / 2),
      # Compute radius from the area assuming clusters are roughly circular
      color = "darkgreen"   # Circle color
    )
  } else {
    colored_edge <- NULL
  }

  # Compile all useful information into a single output list
  out <- list(
    centers = grouped_lab_img,      # Centers of clusters
    coordinates = df_lab_img,       # Coordinates of all labeled pixels
    marked_objects = colored_edge   # Visualized image with marked edges and circles
  )
}
