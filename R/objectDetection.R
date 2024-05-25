#' Object detection
#'
#' This function identifies objects in an image using edge detection and
#' labeling, gathering the coordinates and centers of the identified objects.
#' The edges of detected objects are then highlighted for easy recognition.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian')
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian')
#' @param lowContrast
#'
#' @param vis creates image were object edges (purple) and detected centers (green) are highlighted (TRUE | FALSE)
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. size of labeled objects
#' 4. image were object edges (purple) and detected centers (green) are colored
#' @details
#' The `objectDetection()` function provides several methods for calculating
#' the alpha and sigma parameters, which are critical for edge detection:
#' 1. Input of a Numeric Value:
#' - Users can directly input numeric values for alpha and sigma, allowing for precise control over the edge detection parameters.
#' 2. Static Scanning:
#' - When both alpha and sigma are set to "static", the function systematically tests all possible combinations of these parameters within the range (alpha: 0.1 - 1.4, sigma: 0 - 1.4). This exhaustive search helps identify the optimal parameter values for the given image. (Note: takes a lot of time)
#' 3. Interactive Selection:
#' - By setting alpha and sigma to "interactive", a Tcl/Tk graphical user interface (GUI) is opened. This allows users to select the parameters interactively based on visual feedback. This method requires user input for fine-tuning the parameters according to the specific requirements of the image.
#' 4. Multi-Objective Optimization:
#' - For advanced parameter optimization, the function \code{\link[GPareto]{GParetoptim}} will be utilize for multi-objective optimization using Gaussian process models. This method leverages the 'GPareto' package to perform the optimization. It involves building Gaussian Process models for each objective and running the optimization to find the best parameter values.
#' @import data.table
#' @import imager
#' @import GPareto
#' @importFrom imagerExtra SPE
#' @importFrom stats complete.cases
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_objectDetection$marked_beads |> plot()
#' @export
objectDetection <- function(img,
                            alpha = 1,
                            sigma = 2,
                            lowContrast = FALSE,
                            vis = TRUE) {
  # Assign import
  object_img <- img
  #alpha_i <- alpha
  #sigma_i <- sigma

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

  # Initialize edge detection parameters
  # If set to 'static', optimization is attempted - testing all combinations
  if (alpha == "static" &
      sigma == "static") {
    # Fitness function
    hayflick <- function(img, alpha, sigma) {
      # Compute shape features based on the image and given parameters
      property <- shapeFeatures(img, alpha, sigma)

      # Filter rows without missing values
      df_complete <- property[complete.cases(property),]
      # Apply a thresholding function to the image
      t <- threshold(img)

      # Calculate a variety of statistics based on image features
      combine <- data.frame(
        area = abs(length(which(t == TRUE)) / sum(property$size) - 1),
        pixels = length(which(
          df_complete$size < (mean(df_complete$size) - 0.9 * mean(df_complete$size))
        )) / mean(df_complete$size),
        sd_area = sd(df_complete$size) / mean(df_complete$size),
        sd_perimeter = sd(df_complete$perimeter) / mean(df_complete$perimeter),
        mean_circularity = mean(abs(df_complete$circularity - 1)),
        #sd_circularity = sd(df_complete$circularity) / mean(df_complete$circularity),
        mean_eccentricity = mean(df_complete$eccentricity),
        #sd_eccentricity = sd(df_complete$eccentricity),
        sd_radius = sd(df_complete$mean_radius) / mean(df_complete$mean_radius)
      )

      # Calculate overall quality score as average of combined statistics
      quality <- sum(combine) / ncol(combine)

      # Return both quality and total size as a list
      quality_size <- list(quality, sum(property$size))
      return(quality_size)
    }

    # Generate parameter ranges for alpha and sigma
    alpha_range <- seq(0.1, 1.4, by = 0.1)
    sigma_range <- seq(0, 1.4, by = 0.1)

    # Create a grid of parameter combinations for alpha and sigma
    param_grid <-
      expand.grid(alpha = alpha_range, sigma = sigma_range)

    # Assuming param_grid is already defined and is a data.frame
    n <- nrow(param_grid)
    # Initialize an empty data frame to store results
    results_df <-
      data.frame()

    # Loop through each row in the parameter grid
    for (b in 1:n) {
      row <- param_grid[b,]
      # Call the hayflick function and handle errors
      res_main <- tryCatch({
        hayflick(object_img, row$alpha, row$sigma)
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
    results_df <- results_df[complete.cases(results_df), ]

    # Normalize and aggregate results for fitness calculation
    if (length(unique(results_df$size)) != 1) {
      results_df$size <- normalizeReverseMinmax(results_df$size) / 7
      fitness <- rowSums(results_df) / 2
    } else {
      fitness <- results_df$quality
    }

    # Store unique fitness scores
    u_fitness <- unique(fitness)

    # Sort fitness scores in ascending order
    sorted_res <- sort(u_fitness, decreasing = FALSE)
    sorted_res <- sorted_res[1]

    # Gather best results based on sorted fitness scores
    mylist <- list()
    for (b in 1:length(sorted_res)) {
      index <- which(fitness == sorted_res[b])
      mylist[[b]] <- index
    }

    # Retrieve the best parameter set from the parameter grid
    result_1 <- param_grid[unlist(mylist), ]
    result <- result_1
    result <- result[1, ]
    alpha <- result$alpha
    sigma <- result$sigma
  }

  # If parameters are set to 'interactive', call the interactive detection function
  if (alpha == "interactive" & sigma == "interactive") {
    parameter <- interactive_objectDetection(object_img)
    alpha <- as.numeric(parameter[1])
    sigma <- as.numeric(parameter[2])
  }

  # If parameters are set to 'gaussian', perform Gaussian process optimization
  if (alpha == "gaussian" &
      sigma == "gaussian") {
    if (requireNamespace(c("GPareto"), quietly = TRUE)) {
      hayflick <- function(x) {
        alpha_h <- x[1]
        sigma_h <- x[2]

        property <- tryCatch({
          # Compute shape features based on the image and given parameters
          property <-
            shapeFeatures(object_img, alpha = alpha_h, sigma = sigma_h)
          property
        }, error = function(e) {
          return(NULL)
        })

        if(is.null(property)) {
          quality <- 2
          property <- list(size = 0)
        } else {
          # Filter rows without missing values
          df_complete <- property[complete.cases(property),]
          # Apply a thresholding function to the image
          t <- threshold(img)

          # Calculate a variety of statistics based on image features
          combine <- data.frame(
            area = abs(length(which(t == TRUE)) / sum(property$size) - 1),
            pixels = length(which(
              df_complete$size < (mean(df_complete$size) - 0.9 * mean(df_complete$size))
            )) / mean(df_complete$size),
            sd_area = sd(df_complete$size) / mean(df_complete$size),
            sd_perimeter = sd(df_complete$perimeter) / mean(df_complete$perimeter),
            mean_circularity = mean(abs(df_complete$circularity - 1)),
            #sd_circularity = sd(df_complete$circularity) / mean(df_complete$circularity),
            mean_eccentricity = mean(df_complete$eccentricity),
            #sd_eccentricity = sd(df_complete$eccentricity),
            sd_radius = sd(df_complete$mean_radius) / mean(df_complete$mean_radius)
          )

          # Calculate overall quality score as average of combined statistics
          quality <- sum(na.omit(t(combine))) / nrow(na.omit(t(combine)))
        }

        # Return both quality and total size as a list
        return(c(quality, -sum(property$size)))
      }

      # Function to find the lower boundary
      findLowerBound <- function(min_value, max_value, step_size) {
        parameter <- min_value
        while (parameter <= max_value) {
          result <- tryCatch({
            setTimeLimit(elapsed = timeout, transient = FALSE)
            edgeDetection(object_img, alpha = parameter, sigma = 0)
            return(parameter)
          }, error = function(e) {
            return(NULL)
          }, finally = {
            setTimeLimit(elapsed = Inf)   # Disable the timeout
          })
          if (!is.null(result)) {
            return(result)
          } else {
            parameter <- parameter + step_size
          }
        }
        return(NA)  # In case no valid parameter is found within the range
      }

      # Function to find the upper boundary
      findUpperBound <- function(max_value, min_value, step_size) {
        parameter <- max_value
        while (parameter >= min_value) {
          result <- tryCatch({
            setTimeLimit(elapsed = timeout, transient = FALSE)
            edgeDetection(object_img, alpha = parameter, sigma = 2)
            return(parameter)
          }, error = function(e) {
            return(NULL)
          }, finally = {
            setTimeLimit(elapsed = Inf)
          })
          if (!is.null(result)) {
            return(result)
          } else {
            parameter <- parameter - step_size
          }
        }
        return(NA)  # In case no valid parameter is found within the range
      }

      # Define the range and step size
      #min_value <- 0.1  # Starting point for lower boundary search
      #max_value <- 1.4   # Starting point for upper boundary search
      #step_size <- 0.1  # Increment/Decrement step size
      #timeout <- 10

      # Find the lower and upper boundaries
      #lower_bound <- findLowerBound(min_value, max_value, step_size)
      #upper_bound <- findUpperBound(max_value, min_value, step_size)

      # Define bounds for the parameters
      lower_bounds <- c(alpha = 0.1, sigma = 0)
      upper_bounds <- c(alpha = 1.5, sigma = 2)

      if (lowContrast == TRUE) {
        if (requireNamespace(c("imagerExtra"), quietly = TRUE)) {
          object_img <- imagerExtra::SPE(object_img, lamda = 0.1)
          object_img <- as.cimg(threshold(object_img))
        } else {
          stop(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " Please install the Package 'imagerExtra' for contrast optimization. \n (install.package('imagerExtra')"
          )
        }
      }

      # Perform multi-objective optimization using Gaussian Process models
      results <-
        GPareto::easyGParetoptim(
          fn = hayflick,
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
      # Assuming a minimization problem, the ideal point is (0, 0)
      ideal_point <- c(0, 0)
      distances <-
        apply(pareto_front_n, 1, function(x)
          sqrt(sum((x - ideal_point) ^ 2)))

      # Find the index of the knee point
      knee_point_index <- which.min(distances)

      # Extract the knee point parameters
      knee_point_parameters <- pareto_set[knee_point_index,]

      # Extract optimized parameters
      alpha <- knee_point_parameters[1]
      sigma <- knee_point_parameters[2]

    } else {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Please install the Package 'GPareto' for multi-objective optimization. \n (install.package('GPareto')"
      )
    }
  }

  # Apply edge detection to the image using specified alpha and sigma parameters
  edge_img <-
    edgeDetection(object_img, alpha = alpha, sigma = sigma)

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
    DT[, list(mx = mean(x), my = mean(y)), by = value]

  # Calculate the size of each cluster, which is used to compute radii
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

  if (lowContrast == TRUE) {
    object_img <- img
  }

  if (vis == TRUE) {
    # Visualization: Highlight the edges of detected objects
    edge_coords <- which(edge_img == TRUE, arr.ind = TRUE)
    # Change the color of the detected edges to purple for visualization
    colored_edge <-
      changePixelColor(object_img, edge_coords, color = "purple")
    # Draw circles around the detected clusters using their mean coordinates
    colored_edge <- draw_circle(
      colored_edge,
      grouped_lab_img$mx,
      grouped_lab_img$my,
      radius = (sqrt(mean(
        unlist(cluster_size)
      ) / pi) / 2),
      # Compute radius from the area assuming clusters are roughly circular
      color = "darkgreen"   # Circle color
    )
  } else {
    colored_edge <- NULL
  }

  # Compile all useful information into a single output list
  out <- list(
    centers = grouped_lab_img,
    # Centers of clusters
    coordinates = df_lab_img,
    # Coordinates of all labeled pixels
    size = unlist(cluster_size),
    # Sizes of each cluster
    marked_beads = colored_edge   # Visualized image with marked edges and circles
  )
}
