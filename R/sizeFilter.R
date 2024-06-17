#' Size-based exclusion
#'
#' Takes the size of the objects in an image and discards objects based
#' on a lower and an upper size limit.
#' (Input can be obtained by \code{\link[biopixR]{objectDetection}} function)
#' @param centers center coordinates of objects (value|mx|my|size data frame)
#' @param coordinates all coordinates of the objects (x|y|value data frame)
#' @param lowerlimit smallest accepted object size (numeric / 'auto' / 'interactive')
#' @param upperlimit highest accepted object size (numeric / 'auto' / 'interactive')
#' @returns list of 2 objects:
#' \itemize{
#'   \item Remaining centers after discarding according to size.
#'   \item Remaining coordinates after discarding according to size.
#' }
#' @details
#' The \code{\link[biopixR]{sizeFilter}} function is designed to filter
#' detected objects based on their size, either through automated detection or
#' user-defined limits. The automated detection of size limits uses the 1.5*IQR
#' method to identify and remove outliers. This approach is most effective when
#' dealing with a large number of objects, (typically more than 50), and when
#' the sizes of the objects are relatively uniform. For smaller samples or when
#' the sizes of the objects vary significantly, the automated detection may not
#' be as accurate, and manual limit setting is recommended.
#' @importFrom stats sd
#' @importFrom stats quantile
#' @examples
#' res_objectDetection <- objectDetection(
#'   beads,
#'   method = 'edge',
#'   alpha = 1,
#'   sigma = 0
#'   )
#' res_sizeFilter <- sizeFilter(
#'   centers = res_objectDetection$centers,
#'   coordinates = res_objectDetection$coordinates,
#'   lowerlimit = 50, upperlimit = 150
#'   )
#' changePixelColor(
#'   beads,
#'   res_sizeFilter$coordinates,
#'   color = "darkgreen",
#'   visualize = TRUE
#'   )
#' @export
sizeFilter <- function(centers,
                       coordinates,
                       lowerlimit = "auto",
                       upperlimit = "auto") {
  # Assign input arguments to local variables
  center_df <- centers
  xy_coords <- coordinates

  # Error handling: Both limits must be set to 'auto' or selected individually
  if (lowerlimit == "auto" & upperlimit != "auto") {
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }
  if (lowerlimit != "auto" & upperlimit == "auto") {
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Both limits must be set to 'auto' or selected individually"
    )
  }

  # Automated limit calculation
  if (lowerlimit == "auto" & upperlimit == "auto") {
    # Extract sizes from center_df
    cluster_size <- center_df$size

    # Error handling: Warn if the number of detected objects is less than 50
    if (nrow(center_df) < 50) {
      cluster_size |> plot(ylab = "size in px")
      warning(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Number of detected objects should be >50 for automated detection"
      )

      # Interactive input to handle low number of detected objects
      user_input <-
        readline(prompt = "Detected objects are less than 50. \n Would you like to adjust the thresholds and try again? (yes/no) ")
      if (tolower(user_input) == "no") {
        # Calculate quartiles
        q1 <- quantile(cluster_size, 0.25)
        q3 <- quantile(cluster_size, 0.75)

        # Calculate Interquartile Range (IQR)
        iqr <- q3 - q1

        # Identify non-outliers using the 1.5*IQR rule
        no_outliers <-
          which(cluster_size > (q1 - 1.5 * iqr) & cluster_size < (q3 + 1.5 * iqr))

        # Filter results to include only non-outliers
        res_centers <- center_df[no_outliers]

        # Extract remaining coordinates based on filtered centers
        res_xy_coords <-
          xy_coords[xy_coords$value %in% res_centers$value, ]

      } else if (tolower(user_input) == "yes") {
        # User chose to adjust thresholds, prompt for new limits

        adjustment_input <-
          readline(prompt = "Please enter new limits as c(lowerlimit, upperlimit): ")

        # Convert the user input string to numeric values
        adjustment <-
          as.numeric(unlist(strsplit(
            gsub("[c()]", "", adjustment_input), ","
          )))

        # Check if the input is valid
        if (length(adjustment) != 2 || any(is.na(adjustment))) {
          cat("Invalid input. Please try again.\n")
        }
        # Set new limits based on user input
        lowerlimit <- adjustment[1]
        upperlimit <- adjustment[2]

      } else {
        # User provided invalid input
        cat("Invalid input. Exiting function.\n")
        stop("Invalid input. Exiting function.")
      }
    }

    # Calculate quartiles
    q1 <- quantile(cluster_size, 0.25)
    q3 <- quantile(cluster_size, 0.75)

    # Calculate Interquartile Range (IQR)
    iqr <- q3 - q1

    # Identify non-outliers using the 1.5*IQR rule
    no_outliers <-
      which(cluster_size > (q1 - 1.5 * iqr) & cluster_size < (q3 + 1.5 * iqr))

    # Filter results to include only non-outliers
    res_centers <- center_df[no_outliers]

    # Extract remaining coordinates based on filtered centers
    res_xy_coords <-
      xy_coords[xy_coords$value %in% res_centers$value, ]
  }

  # Interactive limit calculation
  if (lowerlimit == "interactive" & upperlimit == "interactive") {
    # Extract sizes from center_df
    cluster_size <- center_df$size

    # Visualize size to facilitate limit selection
    cluster_size |> plot(ylab = "size in px")

    # Interactive user input; User provides limits
    cha_lowerlimit <-
      readline(prompt = "Please enter lowerlimit: ")
    cha_upperlimit <-
      readline(prompt = "Please enter upperlimit: ")

    # Convert input
    lowerlimit <- as.numeric(cha_lowerlimit)
    upperlimit <- as.numeric(cha_upperlimit)

    # Filter clusters based on size limits
    res_centers <-
      center_df[cluster_size > lowerlimit &
                  cluster_size < upperlimit]

    # Extract remaining coordinates based on filtered clusters
    res_xy_coords <-
      xy_coords[xy_coords$value %in% res_centers$value, ]
  }


  # Manual limit calculation
  if (lowerlimit != "auto" & upperlimit != "auto") {
    # Extract sizes from center_df
    cluster_size <- center_df$size

    # Filter clusters based on size limits
    res_centers <-
      center_df[cluster_size > lowerlimit &
                  cluster_size < upperlimit]

    # Extract remaining coordinates based on filtered clusters
    res_xy_coords <-
      xy_coords[xy_coords$value %in% res_centers$value, ]
  }

  # Return the filtered centers (includes size) and coordinates as a list
  out <- list(centers = res_centers,
              coordinates = res_xy_coords)
}
