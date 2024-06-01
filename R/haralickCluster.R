# Function to compute GLCM for a given image
computeGLCM <-
  function(image,
           distance = 1,                          # Specifies the pixel distance for computing relationships
           angles = c(0, pi / 4, pi / 2, 3 * pi / 4)) {  # Angles at which to compute pixel adjacency
    glcm <- matrix(0, nrow = 256, ncol = 256)     # Initialize GLCM with 256x256 matrix (for grayscale images)
    image <- round(image * 255)                   # Scale image pixel values from range 0-1 to 0-255

    for (angle in angles) {                       # Loop through each specified angle
      dx <- round(distance * cos(angle))          # Calculate horizontal offset based on angle and distance
      dy <- round(distance * sin(angle))          # Calculate vertical offset based on angle and distance

      for (i in 1:(nrow(image) - abs(dy))) {      # Iterate over image rows, adjusted for vertical offset
        for (j in 1:(ncol(image) - abs(dx))) {    # Iterate over image columns, adjusted for horizontal offset
          x <- i
          y <- j
          x_prime <- x + dx                       # Calculate new row index based on horizontal offset
          y_prime <- y + dy                       # Calculate new column index based on vertical offset

          # Ensure new indices are within image bounds
          if (x_prime >= 1 &&
              x_prime <= nrow(image) &&
              y_prime >= 1 && y_prime <= ncol(image)) {
            intensity1 <- image[x, y]             # Get intensity at the original pixel
            intensity2 <- image[x_prime, y_prime] # Get intensity at the offset pixel

            # Increment GLCM value based on intensities found
            glcm[intensity1 + 1, intensity2 + 1] <-
              glcm[intensity1 + 1, intensity2 + 1] + 1
          }
        }
      }
    }

    # Normalize the GLCM matrix by the total number of observations
    glcm <- glcm / sum(glcm)
    return(glcm)                                  # Return the normalized GLCM
  }

#' k-medoids clustering of images according to the Haralick features
#'
#' This function performs k-medoids clustering on images using Haralick
#' features, which describe texture. By evaluating contrast, correlation,
#' entropy, and homogeneity, it groups images into clusters with similar
#' textures. K-medoids is chosen for its outlier resilience, using actual
#' images as cluster centers. This approach simplifies texture-based image
#' analysis and classification.
#' @param path directory path to folder with images to be analyzed
#' @returns \code{data.frame} containing file names, md5sums and cluster number.
#' @import imager
#' @importFrom stats dist
#' @importFrom cluster pam silhouette
#' @importFrom tools md5sum
#' @references https://cran.r-project.org/package=radiomics
#' @examples
#' \donttest{
#'   path2dir <- system.file("images", package = 'biopixR')
#'   result <- haralickCluster(path2dir)
#'   print(result)
#' }
#' @export
haralickCluster <- function(path) {
  # Get full file paths and exclude directories
  file_paths <- list.files(path, full.names = TRUE)
  file_paths <- file_paths[!file.info(file_paths)$isdir]

  # Load images from the directory specified by the path
  cimg_list <- lapply(file_paths, importImage)

  # Function to calculate md5 hash of a file, used to check for identical images
  calculatemd5 <- function(file_path) {
    md5_hash <- md5sum(file_path)
    return(md5_hash)
  }

  # Apply md5 hash function to all files in the directory
  md5sums <- function(file_paths) {
    sapply(file_paths, calculatemd5)
  }

  # Compute md5 sums of the files
  md5_sums <- md5sums(file_paths)

  # Create a summary dataframe of files and their md5 hashes
  md5_result <- data.frame(file = file_paths,
                           md5_sum = md5_sums)
  rownames(md5_result) <- basename(file_paths)

  duplicate_indices <- duplicated(md5_result$md5_sum)

  # Identify duplicate files based on md5 hash
  if (length(unique(duplicate_indices)) > 1) {
    duplicate_entries <- md5_result[duplicate_indices,]
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Some files seem to have identical md5sums! Please remove: ",
      duplicate_entries$file
    )
  }

  # Function to compute Haralick features for a given GLCM
  computeHaralickFeatures <- function(glcm) {
    # Initialize vector for 8 features
    features <- numeric(8)

    # Angular Second Moment
    features[1] <- sum(glcm ^ 2)

    # Contrast
    contrast <- 0
    for (i in seq_len(nrow(glcm))) {
      for (j in seq_len(ncol(glcm))) {
        contrast <- contrast + (i - j) ^ 2 * glcm[i, j]
      }
    }
    features[2] <- contrast

    # Correlation
    numerator <- 0
    # Calculate marginal probabilities along the rows for p_x(i)
    p_x <- rowSums(glcm)
    mu_x <- sum(1:(nrow(glcm)) * p_x)
    sigma_x <- sqrt(sum((1:(nrow(
      glcm
    )) - mu_x) ^ 2 * p_x))

    # Calculate marginal probabilities along the columns for p_y(j)
    p_y <- colSums(glcm)
    mu_y <- sum(1:(ncol(glcm)) * p_y)
    sigma_y <- sqrt(sum((1:(ncol(
      glcm
    )) - mu_y) ^ 2 * p_y))

    for (i in seq_len(nrow(glcm))) {
      for (j in seq_len(ncol(glcm))) {
        numerator <- numerator + (i - mu_x) * (j - mu_y) * glcm[i, j]
      }
    }
    correlation <- numerator / (sigma_x * sigma_y)
    features[3] <- correlation

    # Sum of Squares: Variance
    mean <- sum(1:(nrow(glcm)) %*% glcm)
    variance <- 0
    for (i in seq_len(nrow(glcm))) {
      for (j in seq_len(ncol(glcm))) {
        variance <- variance + ((i - mean) ^ 2) * glcm[i, j]
      }
    }
    features[4] <- variance

    # Inverse Difference Moment
    IDM <- 0
    for (i in seq_len(nrow(glcm))) {
      for (j in seq_len(ncol(glcm))) {
        IDM <- IDM + (1 / (1 + (i - j) ^ 2)) * glcm[i, j]
      }
    }
    features[5] <- IDM

    # Sum Average
    # Number of gray levels
    Ng <- nrow(glcm)
    P_x_plus_y <- numeric(2 * Ng)
    # Calculate P_{x+y}
    for (i in 1:Ng) {
      for (j in 1:Ng) {
        P_x_plus_y[i + j] <- P_x_plus_y[i + j] + glcm[i, j]
      }
    }
    sum_average <- sum((2:(2 * Ng)) * P_x_plus_y[2:(2 * Ng)])
    features[6] <- sum_average

    # Sum Variance
    sum_variance <-
      sum(((2:(2 * Ng)) - sum_average) ^ 2 * P_x_plus_y[2:(2 * Ng)])
    features[7] <- sum_variance

    # Entropy
    entropy <- -sum(glcm * log(ifelse(glcm == 0, 1, glcm)))
    features[8] <- entropy

    return(features)
  }

  # Function to extract Haralick features from an image
  extractFeatures <- function(image) {
    gray_matrix <- as.matrix(image)
    glcm <- computeGLCM(gray_matrix)
    haralick_features <- computeHaralickFeatures(glcm)
    return(haralick_features)
  }

  # Function to check and convert to grayscale if necessary
  convert2Grayscale <- function(img) {
    if (spectrum(img) > 1) {  # Check if the image has more than one channel (color image)
      img <- grayscale(img)  # Convert to grayscale
    }
    img
  }

  # Apply the function to each cimg object in the list
  cimg_list <- lapply(cimg_list, convert2Grayscale)

  # Process each image to extract features
  features_list <- lapply(cimg_list, extractFeatures)

  # Combine features into a matrix
  feature_matrix <- do.call(rbind, features_list)

  # Standardize the feature matrix
  scaled_features <- scale(feature_matrix)

  silhouette_scores <- numeric(10)

  # Compute silhouette scores for k clusters
  for (k in (nrow(scaled_features) - 1)) {
    kmeans_result <- pam(scaled_features, k)
    sil_obj <-
      silhouette(kmeans_result$clustering, dist(scaled_features))
    silhouette_scores[k] <- mean(as.data.frame(sil_obj)$sil_width)
  }

  silhouette_scores <- numeric(9)

  for (k in 2:ifelse(nrow(scaled_features) < 10, nrow(scaled_features) -
                     1, 10)) {
    # Perform PAM clustering with k clusters
    kmeans_result <- pam(scaled_features, k)

    # Calculate silhouette object
    sil_obj <-
      silhouette(kmeans_result$clustering, dist(scaled_features))

    # Calculate and store the average silhouette width for this k
    silhouette_scores[k - 1] <-
      mean(as.data.frame(sil_obj)$sil_width)
  }

  # Identify the optimal number of clusters
  optimal_k <- which.max(silhouette_scores) + 1

  # Perform final clustering
  k <- optimal_k  # Number of clusters
  kmeans_result <- pam(scaled_features, k)

  # Return the final data including file path, md5 sum, and cluster assignment
  out <- cbind(md5_result, cluster = kmeans_result$cluster)
}
