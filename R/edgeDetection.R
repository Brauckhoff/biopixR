# Initialize variables
y <- x <- value <- NULL

# Function to initialize labeling of strong edge points in an image
fillInit <- function(strong) {
  # Label connected components in the strong image, multiplying by strong to
  # keep original labels
  lab <- label(strong, TRUE) * strong
  # Convert labeled matrix to a dataframe and filter out zero values
  lab_df <- as.data.frame(lab) |> subset(value > 0)
  # Convert filtered dataframe to a data.table for better data manipulation
  DT <- data.table(lab_df)
  # Group data by label value and extract the first x and y coordinates for each
  # group
  grouped_first <-
    DT[, list(x = x[1], y = y[1]), by = value]
}

# Function to process the image starting at strong edge points and attempt to
# fill gaps
rescueFill <- function(strong, weak) {
  # Copy strong matrix to v
  v <- strong
  # Set values in v to 0.9 where corresponding weak matrix values are 1
  v[weak == 1] <- .9
  # Get starting locations for filling using fillInit function
  loc <- fillInit(strong)
  # Convert loc to a data.table for processing
  df <- data.table(x = loc$x,
                   y = loc$y)
  # Store initial state in out
  out <- v
  # Define a limiter threshold based on 1% of the total number of pixels in the strong matrix
  limiter <- nrow(strong) * ncol(strong) * 0.01

  # Check if the number of starting locations exceeds the limiter threshold
  if (nrow(df) > limiter) {
    stop("High background detected. Please increase the threshold adjustment factor.")
  }

  # Stop function if no edges are detected to process (i.e., dataframe is empty)
  if (nrow(df) == 0) {
    stop("The parameters cannot be increased any further since no edges could be detected.")
  }

  # Loop through each row in dataframe to process each edge starting point
  for (r in seq_len(nrow(df))) {
    # Apply bucket fill algorithm to current location, updating out matrix
    out <-
      bucketfill(
        out,
        df$x[r],
        df$y[r],
        color = 1,
        sigma = .1,
        high_connexity = TRUE
      )
  }
  # Return processed image converted back to 'cimg' class, with values reset to
  # either 0 or 1
  as.cimg(out == 1)
}

# Function for non-maximum suppression to thin edges in gradient matrix
nonmax <- function(gr) {
  # Compute magnitude of gradient
  mag <- with(gr, sqrt(x ^ 2 + y ^ 2))
  # Normalize gradient components to unit vectors
  grs <- list(x = gr$x / mag, y = gr$y / mag)
  # Compute coordinates for backward and forward checking
  X <- Xc(gr$x)
  Y <- Yc(gr$y)
  # Interpolate magnitude values backward and forward
  val.bwd <- interp(mag, data.frame(x = as.vector(X - grs$x),
                                    y = as.vector(Y - grs$y)))
  val.fwd <- interp(mag, data.frame(x = as.vector(X + grs$x),
                                    y = as.vector(Y + grs$y)))

  # Suppress points where central magnitude is not the local maximum
  throw <- (mag < val.bwd) | (mag < val.fwd)
  mag[throw] <- 0
  # Return thinned magnitudes
  mag
}

# Function to automatically determine thresholds for edge detection using
# k-means clustering
guessKmeans <- function(x) {
  # Apply k-means clustering with centers initialized at min, mean, and max
  # values
  out <- kmeans(as.vector(x), centers = c(min(x), mean(x), max(x)))
  # Return thresholds based on maximum values in the first two clusters
  list(t1 = max(x[out$cluster == 1]), t2 = max(x[out$cluster == 2]))
}

#' Canny edge detector
#'
#' Adapted code from the 'imager' \code{\link[imager]{cannyEdges}} function
#' without the usage of 'dplyr' and 'purrr'. If the threshold parameters are
#' missing, they are determined automatically using a k-means heuristic. Use
#' the alpha parameter to adjust the automatic thresholds up or down. The
#' thresholds are returned as attributes. The edge detection is based on a
#' smoothed image gradient with a degree of smoothing set by the sigma
#' parameter.
#' @param img image (import by \code{\link[biopixR]{importImage}})
#' @param t1 threshold for weak edges (if missing, both thresholds are
#' determined automatically)
#' @param t2 threshold for strong edges
#' @param alpha threshold adjustment factor (default 1)
#' @param sigma smoothing (default 2)
#' @returns Object of class 'cimg', displaying detected edges.
#' @import imager
#' @import magick
#' @import data.table
#' @importFrom stats kmeans
#' @examples
#' edgeDetection(beads, alpha = 0.5, sigma = 0.5) |> plot()
#' @references https://CRAN.R-project.org/package=imager
#' @export
edgeDetection <- function(img,
                          t1,
                          t2,
                          alpha = 1,
                          sigma = 2) {
  # Check if the image has more than one color channel
  has.col <- spectrum(img) > 1
  # Convert to grayscale if the image is colored
  if (has.col) {
    warning("Running edge detector on luminance channel")
    img <- grayscale(img)
  }
  # Stop the function if the input is a video or multi-frame data
  if (depth(img) > 1) {
    stop("Videos not supported, run the function on single frames")
  }

  # Apply image smoothing and gradient computation, then refine edges using
  # nonmax suppression
  mag <- isoblur(img, sigma) |>
    imgradient("xy") |>
    nonmax()

  # If thresholds are not provided, use k-means to estimate them
  if (missing(t1)) {
    guess <- guessKmeans(mag)
    t2 <-
      alpha * guess$t2 # Adjust threshold based on alpha scaling factor
    t1 <- alpha * guess$t1
  }

  # Identify strong and weak edges based on the thresholds
  strong <- as.cimg(mag > t2)
  weak <- as.cimg(mag %inr% c(t1, t2))

  # Fill gaps in detected edges using the rescueFill function
  out <- rescueFill(strong, weak)

  # Attach calculated thresholds to the output for reference
  attr(out, "thresholds") <- c(t1, t2)

  # Additional processing to handle discontinuous edges
  out_magick <- cimg2magick(out)
  out_cimg <- mirror(out, axis = "x")

  # Morphological processing to detect line ends and diagonal ends
  mo1_lineends <- image_morphology(out_magick,
                                   "HitAndMiss", "LineEnds")
  mo2_diagonalends <- image_morphology(out_magick,
                                       "HitAndMiss", "LineEnds:2>")

  # Convert morphology results back to 'cimg' format
  lineends_cimg <- magick2cimg(mo1_lineends)
  diagonalends_cimg <- magick2cimg(mo2_diagonalends)

  # Extract end points from detected lines for further processing
  # Find coordinates of pixels marked as TRUE in 'lineends_cimg'
  end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)

  # Convert end points to a data frame
  end_points_df <- as.data.frame(end_points)
  colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

  # Define a limiter threshold based on 1% of the total number of pixels in the image
  limiter <- nrow(img) * ncol(img) * 0.01

  # If the number of end points is less than the limiter threshold, process diagonal edges
  if (nrow(end_points_df) < limiter) {
    # Find coordinates of pixels marked as TRUE in 'diagonalends_cimg'
    diagonal_edges <-
      which(diagonalends_cimg == TRUE, arr.ind = TRUE)

    # Convert diagonal edges to a data frame
    diagonal_edges_df <- as.data.frame(diagonal_edges)
    colnames(diagonal_edges_df) <- c("x", "y", "dim3", "dim4")

    # Label regions in the processed image for additional analysis
    lab <- label(out_cimg)
    df_lab <- as.data.frame(lab) |>
      subset(value > 0)

    # Collect data of labeled regions that correspond to edges
    alt_x <- list()
    alt_y <- list()
    alt_value <- list()
    for (g in seq_len(nrow(df_lab))) {
      # droplets_array <- as.array(droplets)
      if (out_cimg[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
        alt_x[g] <- df_lab$x[g]
        alt_y[g] <- df_lab$y[g]
        alt_value[g] <- df_lab$value[g]
      }
    }

    # Clean and prepare data for further interpolation
    clean_lab_df <- data.frame(
      x = unlist(alt_x),
      y = unlist(alt_y),
      value = unlist(alt_value)
    )

    # Use adaptive interpolation to connect discontinuous edges
    first_overlay <-
      adaptiveInterpolation(end_points_df,
                            diagonal_edges_df,
                            clean_lab_df,
                            lineends_cimg,
                            radius = 5)

    # Merge original edge image with interpolated results
    first_connect <-
      parmax(list(out_cimg, as.cimg(first_overlay$overlay)))

    # Final morphology processing to thin and skeletonize the connected edges
    connect_magick <- cimg2magick(first_connect)
    thresh_clean_magick <-
      image_morphology(connect_magick, "thinning", "skeleton")

    # Convert the final processed image back to 'cimg' format
    out_cimg <- magick2cimg(thresh_clean_magick)

  } else {
    stop("High background detected. Please increase the threshold adjustment factor.")
  }

  # Final output in pixset format
  out <- out_cimg
  as.pixset(out)
}
