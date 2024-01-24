# Function to print a message with a timestamp
print_with_timestamp <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, msg))
}

#' Image analysis pipeline
#'
#' This function serves as a pipeline that integrates tools for complete
#' start-to-finish image analysis. It enables the handling of images from
#' different channels, including the analysis of dual-color microbeads. This
#' approach simplifies the workflow, providing a straightforward method to
#' analyze complex image data.
#' @param img1 image (import by \code{\link[imager]{load.image}})
#' @param color1 name of color in img1
#' @param img2 image (import by \code{\link[imager]{load.image}})
#' @param color2 name of color in img2
#' @param img3 image (import by \code{\link[imager]{load.image}})
#' @param color3 name of color in img3
#' @param alpha threshold adjustment factor
#' @param sigma smoothing
#' @param sizeFilter applying sizeFilter function (default - TRUE)
#' @param upperlimit highest accepted object size (only needed if
#' sizeFilter = TRUE)
#' @param lowerlimit smallest accepted object size (when 'auto' both limits are
#' calculated by using the mean and the standard deviation)
#' @param proximityFilter applying proximityFilter function (default - TRUE)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels)
#' @param parallel if TRUE uses multiple cores (75 %) to process results
#' @returns list of 3 to 4 objects:
#' 1. summary of all the microbeads in the image
#' 2. detailed information about every single bead
#' 3. (optional) result for every individual color
#' 4. unfiltered coordinates of img1
#' @import data.table
#' @examples
#' result <- imgPipe(beads,
#'   alpha = 1, sigma = 2, upperlimit = 150,
#'   lowerlimit = 50
#'   )
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
                    alpha = 1,
                    sigma = 2,
                    sizeFilter = TRUE,
                    upperlimit = "auto",
                    lowerlimit = "auto",
                    proximityFilter = TRUE,
                    radius = "auto",
                    parallel = FALSE) {

  print_with_timestamp("Starting object detection")

  # binding for global variables
  img <- beadnumber <- intensity <- color <- NULL

  # object detection of every individual channel
  col1_detect <- objectDetection(img1, alpha = alpha, sigma = sigma)

  if (is.null(img2) != TRUE) {
    print_with_timestamp("Starting object detection II & checking dimensions")
    col2_detect <- objectDetection(img2, alpha = alpha, sigma = sigma)

    # error
    if (unique(dim(img1)[1:2] != dim(img2)[1:2])) {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " image1 and image2 must have the same dimensions (width & height)"
      )
    }
  }

  if (is.null(img3) != TRUE) {
    print_with_timestamp("Starting object detection III & checking dimensions")
    col3_detect <- objectDetection(img3, alpha = alpha, sigma = sigma)

    # error
    if (unique(dim(img1)[1:2] != dim(img2)[1:2]) & unique(dim(img1)[1:2] != dim(img3)[1:2])) {
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " all images must have the same dimensions (width & height)"
      )
    }
  }

  # assign input for next function when only one image is used
  if (is.null(img2) == TRUE & is.null(img3) == TRUE) {
    centers <- col1_detect$centers
    coordinates <- col1_detect$coordinates
    size <- col1_detect$size
  }

  # combine results obtained from different images in one data frame, therefore
  # all values from img2/3 are increased
  if (is.null(img2) != TRUE) {
    print_with_timestamp("Creating new dataframe for all objects")
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    centers <- rbind(col1_detect$centers, col2_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates, col2_detect$coordinates)
    size <- unlist(c(col1_detect$size, col2_detect$size))

    # adaptation for objects of different sizes
    if (lowerlimit == "auto" &
      upperlimit == "auto") {

      # calculating quartiles
      q1_col1 <- quantile(col1_detect$size, 0.25)
      q3_col1 <- quantile(col1_detect$size, 0.75)

      # Calculate IQR
      iqr_col1 <- q3_col1 - q1_col1

      lower_col1 <- q1_col1 - 1.5 * iqr_col1
      upper_col1 <- q3_col1 + 1.5 * iqr_col1

      # calculating quartiles
      q1_col2 <- quantile(col2_detect$size, 0.25)
      q3_col2 <- quantile(col2_detect$size, 0.75)

      # Calculate IQR
      iqr_col2 <- q3_col2 - q1_col2

      lower_col2 <- q1_col2 - 1.5 * iqr_col2
      upper_col2 <- q3_col2 + 1.5 * iqr_col2

      upperlimit <-
        max(upper_col1, upper_col2)
      lowerlimit <-
        min(lower_col1, lower_col2)
    }
  }

  if (is.null(img3) != TRUE) {
    print_with_timestamp("Creating new dataframe for all objects")
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    col3_detect$centers$value <-
      col3_detect$centers$value + max(col2_detect$centers$value)
    col3_detect$coordinates$value <-
      col3_detect$coordinates$value + max(col2_detect$centers$value)

    centers <- rbind(col1_detect$centers, col2_detect$centers, col3_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates, col2_detect$coordinates, col3_detect$coordinates)
    size <- unlist(c(col1_detect$size, col2_detect$size, col3_detect$size))

    # adaptation for objects of different sizes
    if (lowerlimit == "auto" &
        upperlimit == "auto") {
      # calculating quartiles
      q1_col1 <- quantile(col1_detect$size, 0.25)
      q3_col1 <- quantile(col1_detect$size, 0.75)

      # Calculate IQR
      iqr_col1 <- q3_col1 - q1_col1

      lower_col1 <- q1_col1 - 1.5 * iqr_col1
      upper_col1 <- q3_col1 + 1.5 * iqr_col1

      # calculating quartiles
      q1_col2 <- quantile(col2_detect$size, 0.25)
      q3_col2 <- quantile(col2_detect$size, 0.75)

      # Calculate IQR
      iqr_col2 <- q3_col2 - q1_col2

      lower_col2 <- q1_col2 - 1.5 * iqr_col2
      upper_col2 <- q3_col2 + 1.5 * iqr_col2

      # calculating quartiles
      q1_col3 <- quantile(col3_detect$size, 0.25)
      q3_col3 <- quantile(col3_detect$size, 0.75)

      # Calculate IQR
      iqr_col3 <- q3_col3 - q1_col3

      lower_col3 <- q1_col3 - 1.5 * iqr_col3
      upper_col3 <- q3_col3 + 1.5 * iqr_col3

      upperlimit <-
        max(upper_col1, upper_col2, upper_col3)
      lowerlimit <-
        min(lower_col1, lower_col2, lower_col3)
    }
  }


  # size filtering
  if (sizeFilter == TRUE) {
    print_with_timestamp("Starting size filtering")
    res_sizeFilter <- sizeFilter(
      centers = centers,
      coordinates = coordinates,
      upperlimit = upperlimit,
      lowerlimit = lowerlimit
    )
  } else {
    res_sizeFilter <- list(
      centers = centers,
      coordinates = coordinates,
      size = size
    )
  }

  # proximity filtering
  if (proximityFilter == TRUE) {
    print_with_timestamp("Starting proximity filtering")
    res_proximityFilter <- proximityFilter(
      centers = res_sizeFilter$centers,
      coordinates = coordinates,
      radius = radius
    )
  } else {
    res_proximityFilter <- res_sizeFilter
  }

  if (is.null(img2) != TRUE) {
    print_with_timestamp("Combining images")
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

  if (is.null(img2) != TRUE & is.null(img3) != TRUE) {
    print_with_timestamp("Combining images")
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

  print_with_timestamp("Starting feature extraction")
  # extract quantitative information from the images
  res <- resultAnalytics(
    unfiltered = coordinates,
    coordinates = res_proximityFilter$coordinates,
    size = res_proximityFilter$size,
    img = combine,
    parallel = parallel
  )

  # add multi-color info to result
  if (is.null(img2) != TRUE | is.null(img3) != TRUE) {
    print_with_timestamp("Adding color information to results")
    witch <- function(detect,
                      res_proximityFilter) {
      col_witch <- list()
      for (a in 1:nrow(detect$centers)) {
        pos <-
          which(
            res_proximityFilter$centers$mx == detect$centers$mx[a] &
              res_proximityFilter$centers$my == detect$centers$my[a]
          )

        value <- res_proximityFilter$centers$value[pos]
        # error
        if (length(value) > 1) {
          warning(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " The threshold 'alpha' needs to be adjusted, as the same object was detected in both images"
          )
        }
        if (length(which(unlist(col_witch) == value)) != 0 | length(value) == 0) {
          next
        } else {
          col_witch[a] <- value
        }
      }
      out <- col_witch
    }

    # how many objects have color one
    col1_witch <- witch(col1_detect, res_proximityFilter)
    res$summary$of_col1 <- length(unlist(col1_witch))

    # how many objects have color two
    if (is.null(img2) != TRUE) {
      col2_witch <- witch(col2_detect, res_proximityFilter)
      res$summary$of_col2 <- length(unlist(col2_witch))
    }

    # how many objects have color three
    if (is.null(img3) != TRUE) {
      col3_witch <- witch(col3_detect, res_proximityFilter)
      res$summary$of_col3 <- length(unlist(col3_witch))
    }


    res$detailed$color <- rep(NA, nrow(res$detailed))

    # which objects have which color
    # assign color to res$detailed
    detailed <- function(witch, res, color) {
      for (a in unlist(witch)) {
        b <- which(res$detailed$beadnumber == a)
        res$detailed$color[b] <- color
      }
      out <- res
    }

    res <- detailed(col1_witch, res, color1)

    if (is.null(img2) != TRUE) {
      res <- detailed(col2_witch, res, color2)
    }
    if (is.null(img3) != TRUE) {
      res <- detailed(col3_witch, res, color3)
    }

    # creating separate result for every color regarding amount, intensity and
    # size
    DT <- data.table(res$detailed)
    res_detailed <-
      DT[, list(
        number = length(beadnumber),
        mean_size = mean(size),
        mean_intensity = mean(intensity)
      ), by = color]

    if (is.null(img2) != TRUE) {
      res$dual <- res_detailed
    } else {
      res$multi <- res_detailed
    }
  }
  print_with_timestamp("Analysis was successfully completed")

  # adding unfiltered for visualization in scanDir
  if(exists("path") == TRUE){
      unfiltered_centers <- col1_detect$centers
  res <- c(res, list(unfiltered_centers = unfiltered_centers))
  }

  out <- res
}
