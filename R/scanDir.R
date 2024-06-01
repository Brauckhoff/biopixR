# Declaring global variables to avoid R CMD check notes
globalVariables(c('registerDoParallel', 'getDoParWorkers', '%dopar%', 'foreach'))

#' Scan Directory for Image Analysis
#'
#' This function scans a specified directory, imports images, and performs various analyses
#' including object detection, size filtering, and proximity filtering. Optionally, it can
#' perform these tasks in parallel and log the process.
#' @param path directory path to folder with images to be analyzed
#' @param parallel processing multiple images at the same time (default - FALSE)
#' @param backend 'PSOCK' or 'FORK' (see \code{\link[parallel]{makeCluster}})
#' @param cores number of cores for parallel processing (numeric / 'auto') ('auto' uses 75% of the available cores)
#' @param method choose method for object detection ('edge' / 'threshold')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian')
#' (from \code{\link[biopixR]{objectDetection}})
#' @param sizeFilter applying \code{\link[biopixR]{sizeFilter}} function (default - FALSE)
#' @param upperlimit highest accepted object size (only needed if sizeFilter = TRUE)
#' @param lowerlimit smallest accepted object size (numeric / 'auto')
#' @param proximityFilter applying \code{\link[biopixR]{proximityFilter}} function (default - FALSE)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels) (only needed if proximityFilter = TRUE)
#' @param Rlog creates a log markdown document, summarizing the results (default - FALSE)
#' @returns \code{data.frame} summarizing each analyzed image, including details such as the number of objects, average size and intensity, estimated rejections, and coverage.
#' @details
#' The function scans a specified directory for image files, imports them,
#' and performs analysis using designated methods. The function is capable of
#' parallel processing, utilizing multiple cores to accelerate computation.
#' Additionally, it is able to log the results into an R Markdown file.
#' Duplicate images are identified through the use of MD5 sums. In addition a
#' variety of filtering options are available to refine the analysis. If
#' logging is enabled, the results can be saved and rendered into a report.
#' When `Rlog = TRUE`, an R Markdown file and a CSV file are generated in the
#' current directory. More detailed information on individual results,
#' can be accessed through saved RDS files.
#' @import parallel
#' @importFrom tools file_path_sans_ext
#' @importFrom utils file.edit write.csv
#' @seealso [imgPipe()], [objectDetection()], [sizeFilter()], [proximityFilter()], [resultAnalytics()]
#' @examples
#' \donttest{
#' if (interactive()) {
#'   path2dir <- system.file("images", package = 'biopixR')
#'   results <- scanDir(path2dir, alpha = 'interactive', sigma = 'interactive')
#'   print(results)
#'   }
#' }
#' @export
scanDir <- function(path,                    # Path to the directory to scan
                    parallel = FALSE,        # Whether to run in parallel
                    backend = 'PSOCK',       # Backend for parallel processing
                    cores = 'auto',          # Number of cores to use
                    method = 'edge',         # Method for image processing
                    alpha = 1,               # Alpha parameter for 'edge'
                    sigma = 2,               # Sigma parameter for 'edge'
                    sizeFilter = FALSE,      # Whether to apply size filter
                    upperlimit = 'auto',     # Upper limit for size filter
                    lowerlimit = 'auto',     # Lower limit for size filter
                    proximityFilter = FALSE, # Whether to apply proximity filter
                    radius = 'auto',         # Radius for proximity filter
                    Rlog = FALSE) {          # Whether to log the process to a file
  # Function to print a message with a timestamp
  printWithTimestamp <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste(timestamp, msg))
  }

  # Function to log a message with a timestamp to the log file
  logIt <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(timestamp, msg, "\n",
        file = new_script_path,
        append = TRUE)
  }

  # Create log file if Rlog is TRUE
  if (Rlog == TRUE) {
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
    } else {
      # Handle missing rmarkdown package
      logIt(
        "Error: Please install the Package 'rmarkdown' for log file creation \n (install.package('rmarkdown')  "
      )
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Please install the Package 'rmarkdown' for log file creation \n (install.package('rmarkdown')"
      )
    }
    # Create path of log_file
    desired_location <- path
    log_file <- "log_file.Rmd"
    new_script_path <- file.path(desired_location, log_file)

    # Create .rmd log file at input path
    file.edit(new_script_path)

    # Writing initial content to the log file (do not use styler on this part!!!)
    cat(
"---
title: 'scanDir log file'
output: pdf_document
--- \n",
      file = new_script_path,
      append = TRUE
    )

    cat(
"# Analysis of directory",
path,
"\n",
        file = new_script_path,
        append = TRUE)

    # Creating folder for log files containing results
    log_path <- file.path(desired_location, "log_files")
    dir.create(log_path)
    log_path <- file.path(desired_location, "log_files/")
  }

  # Printing steps to console and log file
  if (Rlog == TRUE) {
    logIt("Importing images from directory...  ")
  }
  printWithTimestamp("Importing images from directory...")

  # Import images from the specified directory
  image_files <- list.files(path = path, full.names = TRUE)
  image_files <- image_files[!file.info(image_files)$isdir]   # Exclude directories
  image_files <-
    image_files[grep("\\.Rmd$", image_files, invert = TRUE)]  # Exclude log files

  cimg_list <- lapply(image_files, importImage)   # Import images

  # Log and print checking md5sums step
  if (Rlog == TRUE) {
    logIt("Checking md5sums...  ")
  }
  printWithTimestamp("Checking md5sums...")

  # Function to calculate md5sum of a file
  calculatemd5 <- function(file_path) {
    md5_hash <- tools::md5sum(file_path)
    return(md5_hash)
  }

  # Calculate md5sums for all image files
  md5sums <- function(file_paths) {
    sapply(file_paths, calculatemd5)
  }

  md5_sums <- md5sums(image_files)

  # Create a data frame of file names and md5sums
  md5_result <- data.frame(file = image_files,
                           md5_sum = md5_sums)
  duplicate_indices <- duplicated(md5_result$md5_sum)

  # Error handling for duplicate md5sums
  if (length(unique(duplicate_indices)) > 1) {
    duplicate_entries <- md5_result[duplicate_indices, ]
    if (Rlog == TRUE) {
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "Error: Some files seem to have identical md5sums! \n Please remove:",
        duplicate_entries$file,
        "  \n",
        file = new_script_path,
        append = TRUE
      )
    }
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Some files seem to have identical md5sums! Please remove: ",
      duplicate_entries$file
    )
  }

  # Save input parameters
  alpha_i <- alpha
  sigma_i <- sigma

  # Parallel processing section
  if (parallel == TRUE) {
    if (requireNamespace(c("doParallel", "foreach"), quietly = TRUE)) {
      # Log and print creating parallel backend step
      if (Rlog == TRUE) {
        logIt("Creating parallel backend...  ")
      }
      printWithTimestamp("Creating parallel backend...")

      # Detect and set number of cores
      if (cores == 'auto') {
        n_cores <- round(parallel::detectCores() * 0.75)
        my_cluster <- parallel::makeCluster(n_cores,
                                            type = backend)
        doParallel::registerDoParallel(cl = my_cluster)
      } else {
        if (!is.numeric(cores)) {
          if (Rlog == TRUE) {
            logIt("Error: Number of cores needs to be numeric.  ")
          }
          stop(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               " Number of cores needs to be numeric.")
        }
        n_cores <- cores
        my_cluster <- makeCluster(n_cores,
                                  type = backend,
                                  outfile = "")
        registerDoParallel(cl = my_cluster)
      }

      # Log and print information about cores used
      if (Rlog == TRUE) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Parallel backend successfully created.",
          getDoParWorkers(),
          "workers recruited.  \n",
          file = new_script_path,
          append = TRUE
        )
      }
      message(
        paste(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Parallel backend successfully created.",
          getDoParWorkers(),
          "workers recruited."
        )
      )

      # Log and print starting image analysis step
      if (Rlog == TRUE) {
        logIt("Starting image analysis...  ")
      }
      printWithTimestamp("Starting image analysis...")

      # Analyze images in parallel
      md5_result <-
        foreach(
          i = 1:length(cimg_list),
          .combine = rbind,
          .verbose = TRUE,
          .packages = c("GPareto", "imager")
        ) %dopar% {
          if (Rlog == TRUE) {
            cat("\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              "Currently analyzing:",
              md5_result$file[i],
              "  \n",
              file = new_script_path,
              append = TRUE
            )
          }

          message(paste(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "Currently analyzing:",
            md5_result$file[i]
          ))

          img <- cimg_list[[i]]

          # Convert image to grayscale if not already
          if (dim(img)[4] != 1) {
            img <- grayscale(img)
          }

          #devtools::load_all()

          # Analyze the image with a timeout
          timeout <- 3600
          res <- tryCatch({
            setTimeLimit(elapsed = timeout, transient = FALSE)
            res <- imgPipe(
              img1 = img,
              method = method,
              alpha = alpha_i,
              sigma = sigma_i,
              sizeFilter = sizeFilter,
              lowerlimit = lowerlimit,
              upperlimit = upperlimit,
              proximityFilter = proximityFilter,
              radius = radius
            )
            res
          }, error = function(e) {
            res <- NULL
          }, finally = {
            setTimeLimit(elapsed = Inf)
          })

          # Handle failed analysis
          if (is.null(res)) {
            res <- data.frame(matrix(NA, nrow = 1, ncol = 7))
            res <- list(summary = res)
            if (Rlog == TRUE) {
              cat(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                "Failed to analyze:",
                md5_result$file[i],
                "  \n",
                file = new_script_path,
                append = TRUE
              )
            }
          }

          file_names <- tools::file_path_sans_ext(basename(image_files))

          # Save data and image to log files
          if (Rlog == TRUE) {
            export <- list(data = res,
                           image = cimg_list[i])
            saveRDS(export, file = paste0(log_path, "log_", file_names[i], ".RDS"))
          }

          # Combine results
          md5_result_row <- cbind(md5_result[i, ], res$summary)
          rownames(md5_result_row) <- file_names[i]
          md5_result_row
        }

      # Stop the parallel cluster
      parallel::stopCluster(cl = my_cluster)
    } else {
      # Handle missing doParallel package
      if (Rlog == TRUE) {
        logIt(
          "Error: Please install the Package 'doParallel' & 'foreach' for parallel processing \n (install.package(c('doparallel', 'foreach'))  "
        )
      }
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Please install the Package 'doParallel' & 'foreach' for parallel processing \n (install.package(c('doparallel', 'foreach'))"
      )
    }
  }

  # Sequential processing section
  if (parallel == FALSE) {
    for (i in seq_along(cimg_list)) {
      if (Rlog == TRUE) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Currently analyzing:",
          md5_result$file[i],
          "  \n",
          file = new_script_path,
          append = TRUE
        )
      }
      message(paste(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "Currently analyzing:",
        md5_result$file[i]
      ))

      img <- cimg_list[[i]]

      # Convert image to grayscale if not already
      if (dim(img)[4] != 1) {
        img <- grayscale(img)
      }

      # Analyze the image with a timeout
      timeout <- 3600
      res <- tryCatch({
        setTimeLimit(elapsed = timeout, transient = FALSE)
        res <- imgPipe(
          img1 = img,
          method = method,
          alpha = alpha_i,
          sigma = sigma_i,
          sizeFilter = sizeFilter,
          lowerlimit = lowerlimit,
          upperlimit = upperlimit,
          proximityFilter = proximityFilter,
          radius = radius
        )
        res
      }, error = function(e) {
        res <- NULL
      }, finally = {
        setTimeLimit(elapsed = Inf)
      })

      # Handle failed analysis
      if (is.null(res)) {
        if (Rlog == TRUE) {
          cat(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "Failed to analyze:",
            md5_result$file[i],
            "  \n",
            file = new_script_path,
            append = TRUE
          )
        }
      }

      file_names <- tools::file_path_sans_ext(basename(image_files))

      # Save data and image to log files
      if (Rlog == TRUE) {
        export <- list(data = res,
                       image = cimg_list[i])
        saveRDS(export, file = paste0(log_path, "log_", file_names[i], ".RDS"))
      }

      # Combine results
      md5_result[i, c(colnames(res$summary))] <- res$summary
      rownames(md5_result) <- file_names
    }
  }

  # Render log file if Rlog is TRUE
  if (Rlog == TRUE) {
    # Save md5result
    saveRDS(md5_result, file = paste0(log_path, "result.RDS"))

        cat(
"```{r, echo=FALSE}
directory_path <- getwd()
target_file_name <- 'result.RDS'
all_files <- list.files(directory_path, recursive = TRUE, full.names = TRUE)
matching_files <- all_files[grep(target_file_name, all_files, ignore.case = TRUE)]

md5result <- readRDS(matching_files)
name <- basename(md5result$file)
md5result <- data.frame(name = name,
                        amount = md5result$number_of_objects,
                        size = md5result$mean_size,
                        intensity = md5result$mean_intensity,
                        rejected = md5result$estimated_rejected,
                        coverage = md5result$coverage)
md5result
```  \n",
      file = new_script_path,
      append = TRUE)


    cat(
"```{r, echo=FALSE}
#library(biopixR)
target_directory_name <- 'log_files'
all_directories <- list.dirs(directory_path, recursive = TRUE, full.names = TRUE)
matching_directories <- all_directories[grep(target_directory_name, all_directories, ignore.case = TRUE)]

file_names <- list.files(directory_path, full.names = TRUE)
file_names <- file_names[!file.info(file_names)$isdir]
file_names <-
  file_names[grep('\\\\.Rmd$', file_names, invert = TRUE)]
file_names <- tools::file_path_sans_ext(basename(file_names))

for (i in seq_len(nrow(md5result))) {
file_path <- file.path(matching_directories, paste0('log_', file_names[i], '.RDS'))
import <- readRDS(file_path)
image <- import$image[[1]]
data <- import$data
name_i <- name[i]

plot(image, main = name_i)
with(data$unfiltered_centers, points(data$unfiltered_centers$mx, data$unfiltered_centers$my, col = 'darkred'))
with(data$detailed, points(data$detailed$x, data$detailed$y, col = 'darkgreen'))
#changePixelColor(image, data$coordinates, color = factor(data$coordinates$value), visualize = TRUE)
}
```  \n",
      file = new_script_path,
      append = TRUE)

    rmarkdown::render(new_script_path)
    # Write the data frame to a CSV file
    write.csv(md5_result,
              file = paste0(path, "/result.csv"),
              row.names = FALSE)
  }

  out <- md5_result
}
