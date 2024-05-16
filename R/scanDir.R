#' Directory Analysis
#'
#'
#' @param path directory path to folder with images to be analyzed
#' @param format ('jpg' / 'png' / 'bmp') uses \code{\link[imager]{load.dir}} or 'tiff' uses \code{\link[magick]{image_read}}
#' @param parallel processing multiple images at the same time (TRUE | FALSE)
#' @param backend 'PSOCK' or 'FORK' (see \code{\link[parallel]{makeCluster}})
#' @param cores number of cores for parallel processing (numeric / 'auto') ('auto' uses 75% of the available cores)
#' @param alpha threshold adjustment factor (numeric / 'static' / 'interactive' / 'gaussian') (from \code{\link[biopixR]{objectDetection}})
#' @param sigma smoothing (numeric / 'static' / 'interactive' / 'gaussian') (from \code{\link[biopixR]{objectDetection}})
#' @param sizeFilter applying \code{\link[biopixR]{sizeFilter}} function (default - TRUE)
#' @param upperlimit highest accepted object size (only needed if sizeFilter = TRUE)
#' @param lowerlimit smallest accepted object size (when 'auto' both limits are
#' calculated by using the IQR)
#' @param proximityFilter applying \code{\link[biopixR]{proximityFilter}} function (default - TRUE)
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels) (only needed if proximityFilter = TRUE)
#' @param Rlog creates a log markdown document, summarizing the results (TRUE | FALSE)
#' @returns
#'
#' @details
#'
#' @import
#' @import
#' @importFrom rmarkdown render
#' @seealso [imgPipe()]
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tempdir()
#'   temp_dir <- tempdir()
#'   file_path <- file.path(temp_dir, "beads.png")
#'   save.image(beads, file_path)
#'   file_path <- file.path(temp_dir, "droplet_beads.png")
#'   save.image(grayscale(droplet_beads), file_path)
#'   file_path <- file.path(temp_dir, "beads_large1.png")
#'   save.image(beads_large1, file_path)
#'   file_path <- file.path(temp_dir, "beads_large2.png")
#'   save.image(grayscale(beads_large2), file_path)
#'   scanDir(temp_dir, alpha = 'interactive', sigma = 'interactive')
#'   unlink(temp_dir, recursive = TRUE)
#' }
#' }
#' @export
scanDir <- function(path,
                    format = 'jpg',
                    parallel = TRUE,
                    backend = 'PSOCK',
                    cores = 'auto',
                    alpha = 1,
                    sigma = 2,
                    sizeFilter = TRUE,
                    upperlimit = 'auto',
                    lowerlimit = 'auto',
                    proximityFilter = TRUE,
                    radius = 'auto',
                    Rlog = TRUE) {
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

  # Creating log
  if (Rlog == TRUE) {
    # Create path of log_file
    desired_location <- path
    log_file <- "log_file.Rmd"
    new_script_path <- file.path(desired_location, log_file)

    # Create .rmd log file at input path
    file.edit(new_script_path)

    # Writing into the log.rmd (do not use styler on this part!!!)
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

    # Create directory for log files
    dir.create(log_path)

    log_path <- file.path(desired_location, "log_files/")

    #optimize:
    #if (file.exists(new_directory)) {
    #  print("Directory created successfully.")
    #} else {
    #  print("Failed to create directory.")
    #}
  }

  # Printing steps to console and log file
  if (Rlog == TRUE) {
    logIt("Importing images from directory...  ")
  }
  printWithTimestamp("Importing images from directory...")

  # Import images via imager for jpg/png/bmp formats
  if (format == 'jpg' | format == 'png' | format == 'bmp') {
    cimg_list <- load.dir(path = path)
  }

  # Import images via magick for tiff format
  if (format == 'tif') {
    image_files <-
      list.files(path = path,
                 pattern = "\\.tif$",
                 full.names = TRUE)
    image_list <- lapply(image_files, image_read)
    cimg_list <- lapply(image_list, magick2cimg)
  }

  if (Rlog == TRUE) {
    logIt("Checking md5sums...  ")
  }
  printWithTimestamp("Checking md5sums...")

  # Checking directory for identical images using md5 sums
  # Function to calculate md5sum
  calculatemd5 <- function(file_path) {
    md5_hash <- tools::md5sum(file_path)
    return(md5_hash)
  }

  # Applying md5sum calculation function to all files in directory
  md5sums <- function(file_paths) {
    sapply(file_paths, calculatemd5)
  }

  file_paths <- list.files(path, full.names = TRUE)

  # Ignore created directories and log file
  file_paths <- file_paths[!file.info(file_paths)$isdir]
  file_paths <-
    file_paths[grep("\\.Rmd$", file_paths, invert = TRUE)]

  md5_sums <- md5sums(file_paths)

  # Summary of file names and md5 sums
  md5_result <- data.frame(file = file_paths,
                           md5_sum = md5_sums)

  duplicate_indices <- duplicated(md5_result$md5_sum)

  # Error if md5 sums appear more than once
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

  # With parallel processing
  if (parallel == TRUE) {
    if (requireNamespace("doParallel", quietly = TRUE)) {
      # Creating parallel backend
      if (Rlog == TRUE) {
        logIt("Creating parallel backend...  ")
      }
      printWithTimestamp("Creating parallel backend...")

      if (cores == 'auto') {
        n_cores <- round(detectCores() * 0.75)

        my_cluster <- makeCluster(n_cores,
                                  type = backend)

        doParallel::registerDoParallel(cl = my_cluster)

      }

      if (cores != 'auto') {
        if (is.numeric(cores) != TRUE) {
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

      # Giving information about the cores used
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

      if (Rlog == TRUE) {
        logIt("Starting image analysis...  ")
      }
      printWithTimestamp("Starting image analysis...")
      # Starting analysis with foreach loop
      md5_result <-
        foreach(
          i = 1:length(cimg_list),
          .combine = rbind,
          .verbose = TRUE,
          .packages = "biopixR"
        ) %dopar% {
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

          # Actual function to be processed
          res <- biopixR::imgPipe(
            img1 = img,
            alpha = alpha,
            sigma = sigma,
            sizeFilter = sizeFilter,
            lowerlimit = lowerlimit,
            upperlimit = upperlimit,
            proximityFilter = proximityFilter,
            radius = radius,
            parallel = FALSE
          )

          # Save produced data into log_files
          if (Rlog == TRUE) {
            export <- list(data = res,
                           image = cimg_list[i])
            saveRDS(export, file = paste0(log_path, "log_", i, ".RDS"))
          }

          # Combining results from the loop with the file information
          md5_result_row <- cbind(md5_result[i, ], res$summary)
          md5_result_row
        }

      stopCluster(cl = my_cluster)
    } else {
      # doParallel is necessary for parallel processing
      if (Rlog == TRUE) {
        logIt(
          "Error: Please install the Package 'doParallel' for parallel processing \n (install.package('doparallel')  "
        )
      }
      stop(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Please install the Package 'doParallel' for parallel processing \n (install.package('doparallel')"
      )
    }
  }

  # Without parallel processing
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

      # Actual function to be processed
      img <- cimg_list[[i]]

      # Convert image to grayscale if not already
      if (dim(img)[4] != 1) {
        img <- grayscale(img)
      }

      res <- biopixR::imgPipe(
        img1 = img,
        alpha = alpha,
        sigma = sigma,
        sizeFilter = sizeFilter,
        lowerlimit = lowerlimit,
        upperlimit = upperlimit,
        proximityFilter = proximityFilter,
        radius = radius
      )

      # Save produced data into log_files
      if (Rlog == TRUE) {
        export <- list(data = res,
                       image = cimg_list[i])
        saveRDS(export, file = paste0(log_path, "log_", i, ".RDS"))
      }

      md5_result[i, c(colnames(res$summary))] <- res$summary
    }
  }

  # Render log file
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
library(biopixR)
target_directory_name <- 'log_files'
all_directories <- list.dirs(directory_path, recursive = TRUE, full.names = TRUE)
matching_directories <- all_directories[grep(target_directory_name, all_directories, ignore.case = TRUE)]

for (i in 1:nrow(md5result)) {
file_path <- file.path(matching_directories, paste0('log_', i, '.RDS'))
import <- readRDS(file_path)
image <- import$image[[1]]
data <- import$data
name_i <- name[i]

plot(image, main = name_i)
with(data$unfiltered_centers, points(data$unfiltered_centers$mx, data$unfiltered_centers$my, col = 'darkred'))
with(data$detailed, points(data$detailed$x, data$detailed$y, col = 'darkgreen'))
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
