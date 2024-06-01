# Declaring global variables to avoid R CMD check notes
globalVariables(c('path', 'objectnumber', 'size', 'new_script_path'))

# Function to print a message with a timestamp
printWithTimestamp <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, msg))
}

# Another function to print with timestamp (but directly into log file)
logIt <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(timestamp, msg, "\n",
      file = new_script_path,
      append = TRUE)
}

#' Image analysis pipeline
#'
#' This function serves as a pipeline that integrates tools for complete
#' start-to-finish image analysis. It enables the handling of images from
#' different channels, for example the analysis of dual-color micro particles.
#' This approach simplifies the workflow, providing a straightforward method to
#' analyze complex image data.
#' @param img1 image (import by \code{\link[biopixR]{importImage}})
#' @param color1 name of color in img1
#' @param img2 image (import by \code{\link[biopixR]{importImage}})
#' @param color2 name of color in img2
#' @param img3 image (import by \code{\link[biopixR]{importImage}})
#' @param color3 name of color in img3
#' @param method choose method for object detection ('edge' / 'threshold')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param sizeFilter applying \code{\link[biopixR]{sizeFilter}} function (default - FALSE)
#' @param upperlimit highest accepted object size (numeric / 'auto')
#' (only needed if sizeFilter = TRUE)
#' @param lowerlimit smallest accepted object size (numeric / 'auto')
#' (only needed if sizeFilter = TRUE)
#' @param proximityFilter applying \code{\link[biopixR]{proximityFilter}} function (default - FALSE)
#' @param radius distance from one object in which no other centers
#' are allowed (in pixels) (only needed if proximityFilter = TRUE)
#' @returns list of 2 to 3 objects:
#' \itemize{
#'   \item Summary of all the objects in the image.
#'   \item Detailed information about every single object.
#'   \item (optional) Result for every individual color.
#' }
#' @import data.table
#' @seealso [objectDetection()], [sizeFilter()], [proximityFilter()], [resultAnalytics()]
#' @examples
#' result <- imgPipe(
#'   beads,
#'   alpha = 1,
#'   sigma = 2,
#'   sizeFilter = TRUE,
#'   upperlimit = 150,
#'   lowerlimit = 50
#'   )
#'
#' # Highlight remaining microparticles
#' plot(beads)
#' with(
#'   result$detailed,
#'   points(
#'     result$detailed$x,
#'     result$detailed$y,
#'     col = "darkgreen",
#'     pch = 19
#'     )
#'   )
#' @export
imgPipe <- function(img1 = img,
                    color1 = "color1",
                    img2 = NULL,
                    color2 = "color2",
                    img3 = NULL,
                    color3 = "color3",
                    method = 'edge',
                    alpha = 1,
                    sigma = 2,
                    sizeFilter = FALSE,
                    upperlimit = "auto",
                    lowerlimit = "auto",
                    proximityFilter = FALSE,
                    radius = "auto") {
  # Initial message indicating the start of the object detection process
  printWithTimestamp("Starting object detection")

  # Check if the variable 'path' exists and log file exists within the specified path
  if (exists("path") == TRUE) {
    if (file.exists(file.path(path, "log_file.Rmd")) == TRUE) {
      new_script_path <- file.path(path, "log_file.Rmd")
      logIt("Starting object detection   ")
    }
  }

  # Binding for global variables
  img <- intensity <- color <- NULL

  # Object detection for the first image channel
  col1_detect <-
    objectDetection(img1,
                    method = method,
                    alpha = alpha,
                    sigma = sigma)

  # Object detection for the second image channel if provided
  if (!is.null(img2)) {
    printWithTimestamp("Starting object detection II & checking dimensions")
    col2_detect <-
      objectDetection(img2,
                      method = method,
                      alpha = alpha,
                      sigma = sigma)

    # Error handling: Check if the dimensions of img1 and img2 match
    if (unique(dim(img1)[1:2] != dim(img2)[1:2])) {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " image1 and image2 must have the same dimensions (width & height)"
      )
    }
  }

  # Object detection for the third image channel if provided
  if (!is.null(img3)) {
    printWithTimestamp("Starting object detection III & checking dimensions")
    col3_detect <-
      objectDetection(img3,
                      method = method,
                      alpha = alpha,
                      sigma = sigma)

    # Error handling: Check if the dimensions of all images match
    if (unique(dim(img1)[1:2] != dim(img2)[1:2]) &
        unique(dim(img1)[1:2] != dim(img3)[1:2])) {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " all images must have the same dimensions (width & height)"
      )
    }
  }

  # Assign input for the next function when only one image is used
  if (is.null(img2) & is.null(img3)) {
    centers <- col1_detect$centers
    coordinates <- col1_detect$coordinates
  }

  # Combine results obtained from different images into one data frame
  if (!is.null(img2)) {
    printWithTimestamp("Creating new dataframe for all objects")
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    centers <- rbind(col1_detect$centers, col2_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates, col2_detect$coordinates)

    # Adaptation for objects of different sizes
    if (lowerlimit == "auto" &
        upperlimit == "auto") {
      # Calculate quartiles and IQR for col1
      q1_col1 <- quantile(col1_detect$centers$size, 0.25)
      q3_col1 <- quantile(col1_detect$centers$size, 0.75)
      iqr_col1 <- q3_col1 - q1_col1

      lower_col1 <- q1_col1 - 1.5 * iqr_col1
      upper_col1 <- q3_col1 + 1.5 * iqr_col1

      # Calculate quartiles and IQR for col2
      q1_col2 <- quantile(col2_detect$centers$size, 0.25)
      q3_col2 <- quantile(col2_detect$centers$size, 0.75)
      iqr_col2 <- q3_col2 - q1_col2

      lower_col2 <- q1_col2 - 1.5 * iqr_col2
      upper_col2 <- q3_col2 + 1.5 * iqr_col2

      # Set global upper and lower limits
      upperlimit <-
        max(upper_col1, upper_col2)
      lowerlimit <-
        min(lower_col1, lower_col2)
    }
  }

  # Combine results if a third image is provided
  if (!is.null(img3)) {
    printWithTimestamp("Creating new dataframe for all objects")
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    col3_detect$centers$value <-
      col3_detect$centers$value + max(col2_detect$centers$value)
    col3_detect$coordinates$value <-
      col3_detect$coordinates$value + max(col2_detect$centers$value)

    centers <-
      rbind(col1_detect$centers,
            col2_detect$centers,
            col3_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates,
            col2_detect$coordinates,
            col3_detect$coordinates)

    # Adaptation for objects of different sizes
    if (lowerlimit == "auto" &
        upperlimit == "auto") {
      # Calculate quartiles and IQR for col1
      q1_col1 <- quantile(col1_detect$centers$size, 0.25)
      q3_col1 <- quantile(col1_detect$centers$size, 0.75)
      iqr_col1 <- q3_col1 - q1_col1

      lower_col1 <- q1_col1 - 1.5 * iqr_col1
      upper_col1 <- q3_col1 + 1.5 * iqr_col1

      # Calculate quartiles and IQR for col2
      q1_col2 <- quantile(col2_detect$centers$size, 0.25)
      q3_col2 <- quantile(col2_detect$centers$size, 0.75)
      iqr_col2 <- q3_col2 - q1_col2

      lower_col2 <- q1_col2 - 1.5 * iqr_col2
      upper_col2 <- q3_col2 + 1.5 * iqr_col2

      # Calculate quartiles and IQR for col3
      q1_col3 <- quantile(col3_detect$centers$size, 0.25)
      q3_col3 <- quantile(col3_detect$centers$size, 0.75)
      iqr_col3 <- q3_col3 - q1_col3

      lower_col3 <- q1_col3 - 1.5 * iqr_col3
      upper_col3 <- q3_col3 + 1.5 * iqr_col3

      # Set global upper and lower limits
      upperlimit <-
        max(upper_col1, upper_col2, upper_col3)
      lowerlimit <-
        min(lower_col1, lower_col2, lower_col3)
    }
  }

  # Apply size filtering if enabled
  if (sizeFilter == TRUE) {
    printWithTimestamp("Starting size filtering")
    if (exists("path") == TRUE) {
      if (file.exists(file.path(path, "log_file.Rmd")) == TRUE) {
        logIt("Starting size filtering   ")
      }
    }
    res_sizeFilter <- sizeFilter(
      centers = centers,
      coordinates = coordinates,
      upperlimit = upperlimit,
      lowerlimit = lowerlimit
    )
  } else {
    res_sizeFilter <- list(centers = centers,
                           coordinates = coordinates)
  }

  # Apply proximity filtering if enabled
  if (proximityFilter == TRUE) {
    printWithTimestamp("Starting proximity filtering")
    if (exists("path") == TRUE) {
      if (file.exists(file.path(path, "log_file.Rmd")) == TRUE) {
        logIt("Starting proximity filtering   ")
      }
    }
    res_proximityFilter <- proximityFilter(
      centers = res_sizeFilter$centers,
      coordinates = coordinates,
      radius = radius
    )
  } else {
    res_proximityFilter <- res_sizeFilter
  }

  # Combine grayscale images if multiple images are provided
  if (!is.null(img2)) {
    printWithTimestamp("Combining images")
    if (dim(img1)[4] != 1) {
      img1 <- grayscale(img1)
    }
    if (dim(img2)[4] != 1) {
      img2 <- grayscale(img2)
    }
    combine <- add(list(img1, img2))
  } else {
    if (dim(img1)[4] != 1) {
      combine <- grayscale(img1)
    } else {
      combine <- img1
    }
  }

  # Combine grayscale images if a third image is provided
  if (!is.null(img2) & !is.null(img3)) {
    printWithTimestamp("Combining images")
    if (dim(img1)[4] != 1) {
      img1 <- grayscale(img1)
    }
    if (dim(img2)[4] != 1) {
      img2 <- grayscale(img2)
    }
    if (dim(img3)[4] != 1) {
      img3 <- grayscale(img3)
    }
    combine <- add(list(img1, img2, img3))
  }

  # Starting feature extraction
  printWithTimestamp("Starting feature extraction")
  if (exists("path") == TRUE) {
    if (file.exists(file.path(path, "log_file.Rmd")) == TRUE) {
      logIt("Starting feature extraction   ")
    }
  }

  # Extract quantitative information from the images
  res <- resultAnalytics(
    img = combine,
    coordinates = res_proximityFilter$coordinates,
    unfiltered = coordinates
  )

  # Add multi-color information to the results if multiple images are provided
  if (!is.null(img2) | !is.null(img3)) {
    printWithTimestamp("Adding color information to results")

    # Function to determine color of objects detected in the images
    witch <- function(detect,
                      res_proximityFilter) {
      detect_centers <- detect$centers
      filter_centers <- res_proximityFilter$centers
      col_witch <- vector("list", nrow(detect_centers))

      for (a in seq_len(nrow(detect$centers))) {
        pos <-
          which(
            filter_centers$mx == detect_centers$mx[a] &
              filter_centers$my == detect_centers$my[a]
          )

        value <- filter_centers$value[pos]
        # Error handling: Check if the same object is detected in multiple images
        if (length(value) > 1) {
          warning(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " The threshold 'alpha' needs to be adjusted, as the same object was detected in both images"
          )
        }
        if (length(value) == 0 |
            length(value %in% unlist(col_witch)) > 1) {
          next
        } else {
          col_witch[[a]] <- value
        }
      }

      # Remove NULL elements from the list
      col_witch <- col_witch[!sapply(col_witch, is.null)]

      out <- col_witch
    }

    # Determine the color of objects detected in the first image
    col1_witch <- witch(col1_detect, res_proximityFilter)
    res$summary$of_col1 <- length(unlist(col1_witch))

    # Determine the color of objects detected in the second image if provided
    if (!is.null(img2)) {
      col2_witch <- witch(col2_detect, res_proximityFilter)
      res$summary$of_col2 <- length(unlist(col2_witch))
    }

    # Determine the color of objects detected in the third image if provided
    if (!is.null(img3)) {
      col3_witch <- witch(col3_detect, res_proximityFilter)
      res$summary$of_col3 <- length(unlist(col3_witch))
    }

    res$detailed$color <- rep(NA, nrow(res$detailed))

    # Function to assign colors to the objects in the detailed results
    detailed <- function(witch, res, color) {
      witch_unlist <- unlist(witch)
      detailed_data <- res$detailed

      match_indices <-
        match(detailed_data$objectnumber, witch_unlist)
      valid_indices <- !is.na(match_indices)

      detailed_data$color[valid_indices] <- color

      res$detailed <- detailed_data

      out <- res
    }

    res <- detailed(col1_witch, res, color1)

    if (!is.null(img2)) {
      res <- detailed(col2_witch, res, color2)
    }
    if (!is.null(img3)) {
      res <- detailed(col3_witch, res, color3)
    }

    # Create a separate result for each color regarding amount, intensity, and
    # size
    DT <- data.table(res$detailed)
    res_detailed <-
      DT[, list(
        number = length(objectnumber),
        mean_size = mean(size),
        sd_size = sd(size),
        mean_intensity = mean(intensity),
        sd_intensity = sd(intensity)
      ), by = color]

    if (!is.null(img2)) {
      res$dual <- res_detailed
    } else {
      res$multi <- res_detailed
    }
  }

  # Final message indicating the completion of the analysis
  printWithTimestamp("Analysis was successfully completed")
  if (exists("path") == TRUE) {
    if (file.exists(file.path(path, "log_file.Rmd")) == TRUE) {
      logIt("Analysis was successfully completed   ")
    }
  }

  # Add unfiltered centers for visualization in scanDir if path exists
  if (exists("path") == TRUE) {
    unfiltered_centers <- col1_detect$centers
    res <-
      c(
        res,
        list(
          unfiltered_centers = unfiltered_centers,
          coordinates = res_proximityFilter$coordinates
        )
      )
  }

  # Output the final result
  out <- res
}
