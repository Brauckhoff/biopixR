

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

  # function to print a message with a timestamp
  print_with_timestamp <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste(timestamp, msg))
  }

  # another function to print with timestamp (but directly into log file)
  logIt <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(timestamp, msg, "\n",
        file = new_script_path,
        append = TRUE)
  }


  # format = jpg/png/bmp/tiff
  # backend = 'PSOCK' or 'FORK'
  # cores auto using 75% of present cores or number of cores to be used

  # creating log
  if (Rlog == TRUE) {
    # create path of log_file
    desired_location <- path
    log_file <- "log_file.Rmd"
    new_script_path <- file.path(desired_location, log_file)

    # create .rmd log file at input path
    file.edit(new_script_path)

    # writing into the log.rmg (do not use styler on this part!!!)
    cat(
"---
title: 'scanDir log file'
output:
  pdf_document:
    latex_engine: xelatex
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

    # creating folder for log files containing results
    log_path <- file.path(desired_location, "log_files")

    # create directory for log files
    dir.create(log_path)

    log_path <- file.path(desired_location, "log_files/")

    #optimize:
    #if (file.exists(new_directory)) {
    #  print("Directory created successfully.")
    #} else {
    #  print("Failed to create directory.")
    #}


  }

  # printing steps to console and log file
  if (Rlog == TRUE) {
    logIt("Importing images from directory...  ")
  }
  print_with_timestamp("Importing images from directory...")

  # import via imager for jpg/png/bmp
  if (format == 'jpg' | format == 'png' | format == 'bmp') {
    cimg_list <- load.dir(path = path)
  }

  # import via magick for tif
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
  print_with_timestamp("Checking md5sums...")

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

  md5_sums <- md5sums(file_paths)

  # summary of file names and md5 sums
  md5_result <- data.frame(file = file_paths,
                           md5_sum = md5_sums)

  duplicate_indices <- duplicated(md5_result$md5_sum)

  # error if md5 sums appear more than once
  if (length(unique(duplicate_indices)) > 1) {
    duplicate_entries <- md5_result[duplicate_indices, ]
    if (Rlog == TRUE) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "Error: Some files seem to have identical md5sums! \n Please remove:",
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
      if (Rlog == TRUE) {
        logIt("Creating parallel backend...  ")
      }
      print_with_timestamp("Creating parallel backend...")

      if (cores == 'auto') {
        n_cores <- round(detectCores() * 0.75)

        my_cluster <- makeCluster(n_cores,
                                  type = backend)

        doParallel::registerDoParallel(cl = my_cluster)

      }

      if (cores != 'auto') {
        if(is.numeric(cores) != TRUE) {
          if (Rlog == TRUE) {
            logIt("Error: Number of cores needs to be numeric.  ")
          }
          stop(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               " Number of cores needs to be numeric.")
        }
        n_cores <- cores

        my_cluster <- makeCluster(n_cores,
                                  type = backend,
                                  outfile="")

        registerDoParallel(cl = my_cluster)
      }

      # giving information about the cores used
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
      print_with_timestamp("Starting image analysis...")
      # starting with analysis foreach loop
      md5_result <-
        foreach(
          i = 1:length(cimg_list),
          .combine = rbind,
          .verbose = TRUE
        ) %dopar% {

        img <- cimg_list[[i]]

        if (Rlog == TRUE) {
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

        # actual function to be processed
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

        # plot visualization into log file
        if (Rlog == TRUE) {
          # save produced data into log_files
          export <- list(
            data = res,
            image = cimg_list[i]
          )
          saveRDS(export, file = paste0(log_path, "log_", i, ".RDS"))
        }

        # combining results from the loop with the file information
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

  # without parallel processing
  if (parallel == FALSE) {
    for (i in seq_along(cimg_list)) {

      if (Rlog == TRUE) {
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

      # actual function to be processed
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

      # plot visualization into log file
      if (Rlog == TRUE) {
        # save produced data into log_files
        export <- list(
          data = res,
          image = cimg_list[i]
        )
        saveRDS(export, file = paste0(log_path, "log_", i, ".RDS"))
      }

      md5_result[i, c(colnames(res$summary))] <- res$summary
    }

  }
  # render log file
  if (Rlog == TRUE) {
    # save md5result
    saveRDS(md5_result, file = paste0(log_path, "result.RDS"))

        cat(
"```{r, echo=FALSE}
directory_path <- getwd()
target_file_name <- 'result.RDS'
all_files <- list.files(directory_path, recursive = TRUE, full.names = TRUE)
matching_files <- all_files[grep(target_file_name, all_files, ignore.case = TRUE)]

md5result <- readRDS(matching_files)
print(md5result)
```  \n",
      file = new_script_path,
      append = TRUE)


    cat(
"```{r, echo=FALSE}
target_directory_name <- 'log_files'
all_directories <- list.dirs(directory_path, recursive = TRUE, full.names = TRUE)
matching_directories <- all_directories[grep(target_directory_name, all_directories, ignore.case = TRUE)]

for (i in 1:nrow(md5result)) {
file_path <- file.path(matching_directories, paste0('log_', i, '.RDS'))
import <- readRDS(file_path)
image <- import$image
data <- import$data

plot(image)
with(data$unfiltered_centers, points(data$unfiltered_centers$mx, data$unfiltered_centers$my, col = 'darkred'))
with(data$detailed, points(data$detailed$x, data$detailed$y, col = 'darkgreen'))
}
```  \n",
      file = new_script_path,
      append = TRUE)



    rmarkdown::render(new_script_path)
  }
  out <- md5_result
}
