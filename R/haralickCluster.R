# Function to compute GLCM for a given image
compute_glcm <-
  function(image,
           distance = 1,
           angles = c(0, pi / 4, pi / 2, 3 * pi / 4)) {
    glcm <- matrix(0, nrow = 256, ncol = 256)
    image <- round(image * 255)

    for (angle in angles) {
      dx <- round(distance * cos(angle))
      dy <- round(distance * sin(angle))

      for (i in 1:(nrow(image) - abs(dy))) {
        for (j in 1:(ncol(image) - abs(dx))) {
          x <- i
          y <- j
          x_prime <- x + dx
          y_prime <- y + dy

          # Check bounds
          if (x_prime >= 1 &&
              x_prime <= nrow(image) && y_prime >= 1 && y_prime <= ncol(image)) {
            intensity1 <- image[x, y]
            intensity2 <- image[x_prime, y_prime]

            glcm[intensity1 + 1, intensity2 + 1] <-
              glcm[intensity1 + 1, intensity2 + 1] + 1
          }
        }
      }
    }

    glcm <- glcm / sum(glcm)
    return(glcm)
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
#' @returns data frame containing file names, md5sums and cluster number
#' @import imager
#' @importFrom stats dist
#' @importFrom cluster pam silhouette
#' @references https://cran.r-project.org/package=radiomics
#' @examples
#' tempdir()
#' temp_dir <- tempdir()
#' file_path <- file.path(temp_dir, "beads.png")
#' save.image(beads, file_path)
#' file_path <- file.path(temp_dir, "droplet_beads.png")
#' save.image(grayscale(droplet_beads), file_path)
#' file_path <- file.path(temp_dir, "beads_large1.png")
#' save.image(beads_large1, file_path)
#' file_path <- file.path(temp_dir, "beads_large2.png")
#' save.image(grayscale(beads_large2), file_path)
#' result <- haralickCluster(temp_dir)
#' result
#' unlink(temp_dir, recursive = TRUE)
#' @export
haralickCluster <- function(path) {
  # Import directory
  cimg_list <- load.dir(path = path)

  # checking directory for identical images using md5 sums
  # calculating md5sum function
  calculatemd5 <- function(file_path) {
    md5_hash <- tools::md5sum(file_path)
    return(md5_hash)
  }

  # applying calculating function to all files in directory
  md5sums <- function(file_paths) {
    sapply(file_paths, calculatemd5)
  }

  file_paths <- list.files(path, full.names = TRUE)

  # ignore created directories
  file_paths <- file_paths[!file.info(file_paths)$isdir]

  correct_files <- basename(file_paths) %in% names(cimg_list)
  file_paths <- file_paths[correct_files]

  md5_sums <- md5sums(file_paths)

  # summary of file names and md5 sums
  md5_result <- data.frame(file = file_paths,
                           md5_sum = md5_sums)

  duplicate_indices <- duplicated(md5_result$md5_sum)

  # error if md5 sums appear more than once
  if (length(unique(duplicate_indices)) > 1) {
    duplicate_entries <- md5_result[duplicate_indices, ]
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Some files seem to have identical md5sums! Please remove: ",
      duplicate_entries$file
    )
  }

  # Function to compute Haralick features from a given GLCM
  compute_haralick_features <- function(glcm) {
    features <- numeric(8)

    # Angular Second Moment (checked)
    features[1] <- sum(glcm ^ 2)

    # Contrast (checked)
    contrast <- 0
    for (i in 1:nrow(glcm)) {
      for (j in 1:ncol(glcm)) {
        contrast <- contrast + (i - j) ^ 2 * glcm[i, j]
      }
    }
    features[2] <- contrast

    # Correlation (checked)
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

    for (i in 1:nrow(glcm)) {
      for (j in 1:ncol(glcm)) {
        numerator <- numerator + (i - mu_x) * (j - mu_y) * glcm[i, j]
      }
    }
    correlation <- numerator / (sigma_x * sigma_y)
    features[3] <- correlation

    # Sum of Squares: Variance (checked)
    mean <- sum(1:(nrow(glcm)) %*% glcm)
    variance <- 0
    for (i in 1:nrow(glcm)) {
      for (j in 1:ncol(glcm)) {
        variance <- variance + ((i - mean) ^ 2) * glcm[i, j]
      }
    }
    features[4] <- variance

    # Inverse Difference Moment (checked)
    IDM <- 0
    for (i in 1:nrow(glcm)) {
      for (j in 1:ncol(glcm)) {
        IDM <- IDM + (1 / (1 + (i - j) ^ 2)) * glcm[i, j]
      }
    }
    features[5] <- IDM

    # Sum Average (checked)
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

    # Sum Variance (checked)
    sum_variance <-
      sum(((2:(2 * Ng)) - sum_average) ^ 2 * P_x_plus_y[2:(2 * Ng)])
    features[7] <- sum_variance

    # Entropy (checked)
    # It takes into account cells with a probability of 0 by replacing these with
    # 1 before taking the logarithm, as log(1) = 0 and log(0) is undefined.
    # This adjustment doesn't affect the entropy calculation because
    # the contribution from these cells is 0.
    entropy <- -sum(glcm * log(ifelse(glcm == 0, 1, glcm)))
    features[8] <- entropy

    return(features)
  }

  # Function to extract features from an image using Haralick features
  extract_features <- function(image) {
    gray_matrix <- as.matrix(image)
    glcm <- compute_glcm(gray_matrix)
    haralick_features <- compute_haralick_features(glcm)
    return(haralick_features)
  }

  # Extract features from each image
  features_list <- lapply(cimg_list, extract_features)

  # Combine features into a matrix
  feature_matrix <- do.call(rbind, features_list)

  # Standardize the feature matrix
  scaled_features <- scale(feature_matrix)

  silhouette_scores <- numeric(10)

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

  optimal_k <- which.max(silhouette_scores) + 1

  k <- optimal_k  # Number of clusters
  kmeans_result <- pam(scaled_features, k)

  rownames(md5_result) <- NULL
  out <- cbind(md5_result, cluster = kmeans_result$cluster)
}
