#' Object detection
#'
#' This function identifies objects in an image using edge detection and
#' labeling, gathering the coordinates and centers of the identified objects.
#' The edges of detected objects are then highlighted for easy recognition.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor (numeric / 'auto')
#' @param sigma smoothing (numeric / 'auto')
#' @param parallel (TRUE | FALSE) when alpha & sigma = 'auto', calculation
#' of parameters will be done using 'foreach'
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. size of labeled objects
#' 4. image were object edges (purple) and detected centers (green) are colored
#' @import data.table
#' @import imager
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
#' res_objectDetection$marked_beads |> plot()
#' @export
objectDetection <- function(img,
                            alpha = 1,
                            sigma = 2,
                            parallel = FALSE) {
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

  # Initialize edge detection parameters
  # If set to 'auto', optimization is attempted
  if (alpha == "auto" &
      sigma == "auto") {
    # Fitness function for optimizing alpha and sigma via image analysis
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

    # Placeholder values for future developments
    perc = 1
    iterations = 1
    #population = 5,
    #window = 0.1) {

    # Generate parameter ranges for alpha and sigma
    alpha_range <- seq(0.1, 1, by = 0.1)
    sigma_range <- seq(0, 2, by = 0.1)

    # Create a grid of parameter combinations for alpha and sigma
    param_grid <-
      expand.grid(alpha = alpha_range, sigma = sigma_range)

    # Modify parameter grid based on a Gaussian distribution, if perc != 1
    # <under development: start>
    if (perc != 1) {
      # parameter for gauss curve
      grid_size <- length(sigma_range)
      center_mean <- grid_size / 2
      std_dev <- grid_size / 3

      gaussian_probs <-
        dnorm(1:grid_size, mean = center_mean, sd = std_dev)
      gaussian_probs <- gaussian_probs / sum(gaussian_probs)

      # weighted approach for sigma highest probability of good values between 0.6 and 1.4
      random_indices <-
        sample(
          1:grid_size,
          size = (perc * nrow(param_grid)),
          replace = TRUE,
          prob = gaussian_probs
        )

      alpha_start <-
        sample(alpha_range,
               size = (perc * nrow(param_grid)),
               replace = TRUE)
      sigma_start <- sigma_range[random_indices]

      # starting grid
      param_grid <- data.frame(alpha = alpha_start,
                               sigma = sigma_start) |> unique()
    }

    # Iterate over the number of iterations specified
    for (i in 1:iterations) {
      # creating new adaptive grid
      if (i > 1) {
        lower_a <- result$alpha - window
        upper_a <- result$alpha + window
        lower_s <- result$sigma - window
        upper_s <- result$sigma + window

        for (a in 1:length(lower_a)) {
          alpha_range <- seq(lower_a[a], upper_a[a], by = 0.1)
          sigma_range <- seq(lower_s[a], upper_s[a], by = 0.1)

          # create a grid of parameter combinations
          param_grid <-
            expand.grid(alpha = alpha_range, sigma = sigma_range)
          if (a == 1) {
            new_grid <- param_grid
          }

          if (a > 1) {
            new_grid <- rbind(new_grid, param_grid)
          }
        }

        param_grid <- as.data.frame(new_grid) |> unique()
      }
      # <under development: end>

      # If parallel processing is enabled, setup and perform parallel
      # computations
      if (parallel == TRUE) {
        if (requireNamespace("doParallel", quietly = TRUE)) {
          # Initialize parallel computing cluster with a specified number of cores
          n <- detectCores()
          num_cores <- round(n * 0.75)
          cl <- makeCluster(num_cores,
                            type = "PSOCK")

          doParallel::registerDoParallel(cl)

          # Setup environment for parallel computation
          env <- new.env()
          env$object_img <- object_img
          env$hayflick <- function(img, alpha, sigma) {
            property <- shapeFeatures(img, alpha, sigma)

            df_complete <- property[complete.cases(property), ]
            t <- threshold(img)

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
              #sd_circularity = sd(df_complete$circularity) / mean(df_complete$circularity),
              mean_eccentricity = mean(df_complete$eccentricity),
              #sd_eccentricity = sd(df_complete$eccentricity),
              sd_radius = sd(df_complete$mean_radius) / mean(df_complete$mean_radius)
            )

            quality <-
              sum(combine) / ncol(combine)

            quality_size <- list(quality, sum(property$size))

            return(quality_size)
          }
          env$param_grid <-
            param_grid  # Assume param_grid is previously created

          varlist <- c("object_img",
                       "hayflick",
                       "param_grid")
          # Export necessary functions and data to parallel workers
          clusterExport(cl, varlist, envir = env)

          # Execute parallel computation using foreach loop
          #tictoc::tic()
          results_df <-
            foreach(i = 1:nrow(param_grid),
                    .combine = 'rbind') %dopar% {
                      row <- param_grid[i, ]
                      res_main <- tryCatch({
                        hayflick(object_img, row$alpha, row$sigma)
                      },
                      error = function(error_condition) {
                        return(NA)
                      })
                      data.frame(quality = unlist(res_main[1]),
                                 size = unlist(res_main[2]))
                      #result_list[[i]] <- res_main

                    }
          #tictoc::toc()
          stopCluster(cl)
        } else {
          stop(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " Please install the Package 'doParallel' for parallel processing \n (install.package('doparallel')"
          )
        }
      }


      if (parallel == FALSE) {
        # Assuming param_grid is already defined and is a data.frame
        n <- nrow(param_grid)
        results_df <-
          data.frame()  # Initialize an empty data frame to store results

        for (b in 1:n) {
          row <- param_grid[b, ]
          res_main <- tryCatch({
            hayflick(object_img, row$alpha, row$sigma)  # Function call as in your foreach loop
          }, error = function(error_condition) {
            return(list(NA, NA))  # Ensure the list has two NAs to match expected structure
          })
          # Create a data frame row from the results and bind it to the results data frame
          temp_df <- data.frame(quality = unlist(res_main[1]),
                                size = unlist(res_main[2]))
          results_df <-
            rbind(results_df, temp_df)  # Combine results row-wise
        }
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

      # Store unique fitness scores
      u_fitness <- unique(fitness)

      if (i != iterations) {
        sorted_res <- sort(u_fitness, decreasing = FALSE)
        sorted_res <- sorted_res[1:(population - (i - 1))]
      } else {
        sorted_res <- sort(u_fitness, decreasing = FALSE)
        sorted_res <- sorted_res[1]
      }

      # Gather best results based on sorted fitness scores
      mylist <- list()
      for (b in 1:length(sorted_res)) {
        index <- which(fitness == sorted_res[b])
        mylist[[b]] <- index
      }

      result_1 <- param_grid[unlist(mylist),]

      if (i != iterations) {
        result <- unique(result_1)[1:(population - (i - 1)), ]
      } else {
        result <- result_1
      }

    }
    result <- result[1,]
    alpha <- result$alpha
    sigma <- result$sigma
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
    ) / pi) / 2),   # Compute radius from the area assuming clusters are roughly circular
    color = "darkgreen"   # Circle color
  )

  # Compile all useful information into a single output list
  out <- list(
    centers = grouped_lab_img,    # Centers of clusters
    coordinates = df_lab_img,     # Coordinates of all labeled pixels
    size = unlist(cluster_size),  # Sizes of each cluster
    marked_beads = colored_edge   # Visualized image with marked edges and circles
  )
}
