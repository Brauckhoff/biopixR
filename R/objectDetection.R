#' Object detection
#'
#' This function identifies objects in an image using edge detection and
#' labeling, gathering the coordinates and centers of the identified objects.
#' The edges of detected objects are then highlighted for easy recognition.
#' @param img image (import by \code{\link[imager]{load.image}})
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
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
                            sigma = 2) {
  # assign import
  object_img <- img

  # check class of import
  if (class(object_img)[1] != "cimg") {
    stop("image must be of class 'cimg'")
  }

  # in case the image is from a luminescence channel transform to gray scale
  if (dim(object_img)[4] != 1) {
    object_img <- grayscale(object_img)
    warning(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Image is from a luminescence channel and was converted into grayscale"
    )
  }

  # Check if both alpha and sigma parameters are set to "auto"
  if (alpha == "auto" &
      sigma == "auto") {
    # Define the fitness function hayflick for image analysis
    hayflick <- function(img, alpha, sigma) {
      # Compute shape features based on the image and given parameters
      property <- shapeFeatures(img, alpha, sigma)

      # Filter rows without missing values
      df_complete <- property[complete.cases(property), ]
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
        foreach(i = 1:nrow(param_grid), .combine = 'rbind') %dopar% {
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

      # Define the reverse min-max normalization function
      normalize_reverse_minmax <- function(x) {
        return(1 - (x - min(x)) / (max(x) - min(x)))
      }

      # Filter rows without missing values
      results_df <- results_df[complete.cases(results_df), ]

      # Normalize and aggregate results for fitness calculation
      if (length(unique(results_df$size)) != 1) {
        results_df$size <- normalize_reverse_minmax(results_df$size) / 7
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

      result_1 <- param_grid[unlist(mylist), ]

      if (i != iterations) {
        result <- unique(result_1)[1:(population - (i - 1)),]
      } else {
        result <- result_1
      }

    }
    result <- result[1, ]
    alpha <- result$alpha
    sigma <- result$sigma
  }

  # edge detection with default: alpha = 1, sigma = 2
  edge_img <-
    edgeDetection(object_img, alpha = alpha, sigma = sigma)

  # fill detected edges and label areas
  first_lab <- label(edge_img)
  fill_hulls <- which(first_lab != 0)
  filled_img <- edge_img
  filled_img[fill_hulls] <- TRUE
  labeled_img <- label(filled_img)

  # create data frame without background
  df_lab_img <- as.data.frame(labeled_img) |>
    subset(value > 0)
  DT <- data.table(df_lab_img)

  # summarize by cluster and calculate center
  grouped_lab_img <-
    DT[, list(mx = mean(x), my = mean(y)), by = value]

  # size calculation, which is needed to calculate the radius
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

  # visualization by highlighting the edges of detected beads
  edge_coords <- which(edge_img == TRUE, arr.ind = TRUE)
  colored_edge <-
    changePixelColor(object_img, edge_coords, color = "purple")
  colored_edge <- draw_circle(
    colored_edge,
    grouped_lab_img$mx,
    grouped_lab_img$my,
    radius = (sqrt(mean(
      unlist(cluster_size)
    ) / pi) / 2),
    color = "darkgreen"
  )

  out <- list(
    centers = grouped_lab_img,
    coordinates = df_lab_img,
    size = unlist(cluster_size),
    marked_beads = colored_edge
  )
}
