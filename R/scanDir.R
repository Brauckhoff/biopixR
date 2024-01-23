
print_with_timestamp <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, msg))
}

logIt <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(timestamp, msg, "\n", file = new_script_path,
      append = TRUE)
}

dirScan <- function(path,
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
                    logIt = TRUE
                    ) {

  # notes:
  # to do: creating final visualization in log file
  # format = jpg/png/bmp/tiff
  # backend = 'PSOCK' or 'FORK'
  # cores auto using 75% of present cores or number of cores to be used
  # Function to print a message with a timestamp

  # creating log
  if (logIt == TRUE) {
    # create path of log_file
    desired_location <- path
    log_file <- "log_file.Rmd"
    new_script_path <- file.path(desired_location, log_file)

    # create .rmd log file at input path
    file.edit(new_script_path)

    cat(
"---
title: 'scanDir log file'
output:
  pdf_document:
    latex_engine: xelatex
--- \n \begin{flushleft} \n",
file = new_script_path,
append = TRUE)

    cat(
"# Analysis of directory", path, "\n",
file = new_script_path,
append = TRUE)

  }


  if (logIt == TRUE) {
    logIt("Importing images from directory...  ")
  }
  print_with_timestamp("Importing images from directory...")

  # import via imager for jpg/png/bmp
  if (format == 'jpg' | format == 'png' | format == 'bmp') {
    cimg_list <- load.dir(path = path)
  }

  # import via magick for other formats
  if (format == 'tif') {
    image_files <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
    image_list <- lapply(image_files, image_read)
    cimg_list <- lapply(image_list, magick2cimg)
  }


  if (logIt == TRUE) {
    logIt("Checking md5sums...  ")
  }
  print_with_timestamp("Checking md5sums...")

  # checking directory for identical images using md5 sums
  # calculating function
  calculatemd5 <- function(file_path) {
    md5_hash <- md5sum(file_path)
    return(md5_hash)
  }

  # applying calculating function to all files in directory
  md5sums <- function(file_paths) {
    sapply(file_paths, calculatemd5)
  }

  file_paths <- list.files(path, full.names = TRUE)
  md5_sums <- md5sums(file_paths)

  # summary of file names and md5 sums
  md5_result <- data.frame(file = file_paths,
                           md5_sum = md5_sums)

  duplicate_indices <- duplicated(md5_result$md5_sum)

  # error if md5 sums appear more than onces
  if (length(unique(duplicate_indices)) > 1) {
    duplicate_entries <- md5_result[duplicate_indices, ]
    if (logIt == TRUE) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "Error: Some files seem to have identical md5sums! \n Please remove:",
          duplicate_entries$file, "  \n",
        file = new_script_path,
        append = TRUE)
    }
    stop(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Some files seem to have identical md5sums! Please remove: ",
      duplicate_entries$file
    )
  }

  # with parallel processing
  if (parallel == TRUE) {
    if (requireNamespace("doParallel", quietly = TRUE)) {
      # creating parallel backend
      if (logIt == TRUE) {
        logIt("Creating parallel backend...  ")
      }
      print_with_timestamp("Creating parallel backend...")

      if (cores == 'auto') {
        n_cores <- round(detectCores() * 0.75)

        my_cluster <- makeCluster(n_cores,
                                  type = backend)

        registerDoParallel(cl = my_cluster)

      }

      if (cores != 'auto') {
        n_cores <- cores

        my_cluster <- makeCluster(n_cores,
                                  type = backend,
                                  outfile="")

        registerDoParallel(cl = my_cluster)
      }


      if (logIt == TRUE) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Parallel backend successfully created.",
          getDoParWorkers(),
          "workers recruted.  \n",
          file = new_script_path,
          append = TRUE
        )
      }
      message(
        paste(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Parallel backend successfully created.",
          getDoParWorkers(),
          "workers recruted."
        )
      )

      if (logIt == TRUE) {
        logIt("Starting image analysis...  ")
      }
      print_with_timestamp("Starting image analysis...")
      # actual function to be processed
      md5_result <- foreach(i = 1:length(cimg_list), .combine = rbind, .verbose = TRUE) %dopar% {
        img <- cimg_list[[i]]

        if (logIt == TRUE) {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "Currently analyzing:",
            md5_result$file[i], "  \n",
            file = new_script_path,
            append = TRUE)
        }

        message(
          paste(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "Currently analyzing:",
            md5_result$file[i]
          )
        )

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

        if (logIt == TRUE) {
          cat(
            "```{r}
plot(cimg_list[i])
with(res$unfiltered_centers, points(res$unfiltered_centers$mx, res$unfiltered_centers$my, col = 'darkred'))
with(res$detailed, points(res$detailed$x, res$detailed$y, col = 'darkgreen'))
```",
            file = new_script_path,
            append = TRUE)
        }

        # Assuming md5_result is a pre-allocated matrix
        md5_result_row <- cbind(md5_result[i, ], res$summary)
        md5_result_row
      }

      stopCluster(cl = my_cluster)
    } else {
      if (logIt == TRUE) {
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

  # withou parallel processing
  if (parallel == FALSE) {

    for (i in seq_along(cimg_list)) {

      if (logIt == TRUE) {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "Currently analyzing:",
            md5_result$file[i], "  \n",
            file = new_script_path,
            append = TRUE)
      }

      message(
        paste(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Currently analyzing:",
          md5_result$file[i]
        )
      )

      img <- cimg_list[[i]]
      res <- biopixR::imgPipe(img1 = img,
                     alpha = alpha,
                     sigma = sigma,
                     sizeFilter = sizeFilter,
                     lowerlimit = lowerlimit,
                     upperlimit = upperlimit,
                     proximityFilter = proximityFilter,
                     radius = radius,
                     parallel = FALSE)

      if (logIt == TRUE) {
        cat(
"```{r}
plot(cimg_list[i])
with(res$unfiltered_centers, points(res$unfiltered_centers$mx, res$unfiltered_centers$my, col = 'darkred'))
with(res$detailed, points(res$detailed$x, res$detailed$y, col = 'darkgreen'))
```",
            file = new_script_path,
            append = TRUE)
      }

      md5_result[i, c(colnames(res$summary))] <- res$summary
    }

  }

  if (logIt == TRUE) {
    rmarkdown::render(new_script_path)
  }

}
