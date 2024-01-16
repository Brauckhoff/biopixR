imgPipe <- function(img1 = img,
                       color1 = 'color1',
                       img2 = NULL,
                       color2 = 'color2',
                       img3 = NULL,
                       color3 = 'color3',
                       alpha = 1,
                       sigma = 2,
                       sizeFilter = TRUE,
                       upperlimit = 'auto',
                       lowerlimit = 'auto',
                       proximityFilter = TRUE,
                       radius = 'auto') {

  # object detection of every individual channel
  col1_detect <- objectDetection(img1, alpha = alpha, sigma = sigma)

  if (is.null(img2) != TRUE) {
    col2_detect <- objectDetection(img2, alpha = alpha, sigma = sigma)
  }

  if (is.null(img3) != TRUE) {
    col3_detect <- objectDetection(img3, alpha = alpha, sigma = sigma)
  }

  # object detection of the combined image
  if(is.null(img2) == TRUE & is.null(img3) == TRUE) {
    centers <- col1_detect$centers
    coordinates <- col1_detect$coordinates
  }

  # combine results obtained from differnet images
  if (is.null(img2) != TRUE) {
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    centers <- rbind(col1_detect$centers, col2_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates, col2_detect$coordinates)

    # adaptation for objects of different sizes
    if (lowerlimit == 'auto' &
        upperlimit == 'auto') {
      upperlimit <-
        max(mean(col1_detect$size), mean(col2_detect$size)) + max(sd(col1_detect$size), sd(col2_detect$size))
      lowerlimit <-
        min(mean(col1_detect$size), mean(col2_detect$size)) - max(sd(col1_detect$size), sd(col2_detect$size))
    }
  }

  if (is.null(img3) != TRUE) {
    col2_detect$centers$value <-
      col2_detect$centers$value + max(col1_detect$centers$value)
    col2_detect$coordinates$value <-
      col2_detect$coordinates$value + max(col1_detect$centers$value)

    centers <- rbind(col1_detect$centers, col2_detect$centers)
    coordinates <-
      rbind(col1_detect$coordinates, col2_detect$coordinates)

    # adaptation for objects of different sizes
    if (lowerlimit == 'auto' &
        upperlimit == 'auto')
      {
        upperlimit <-
          max(mean(col1_detect$size),
              mean(col2_detect$size),
              mean(col3_detect$size)) + max(sd(col1_detect$size),
                                            sd(col2_detect$size),
                                            sd(col3_detect$size))
      lowerlimit <-
        min(mean(col1_detect$size),
            mean(col2_detect$size),
            mean(col3_detect$size)) - max(sd(col1_detect$size),
                                          sd(col2_detect$size),
                                          sd(col3_detect$size))
    }
  }

  # size filtering
  if (sizeFilter == TRUE) {
    res_sizeFilter <- sizeFilter(centers = centers,
                                 coordinates = coordinates,
                                 upperlimit = upperlimit,
                                 lowerlimit = lowerlimit)
  } else {
    res_sizeFilter <- list(centers = centers,
                           coordinates = coordinates)
  }

  # proximity filtering
  if (proximityFilter == TRUE) {
    res_proximityFilter <- proximityFilter(centers = res_sizeFilter$centers,
                                           coordinates = coordinates,
                                           radius = radius)
  } else {
    res_proximityFilter <- res_sizeFilter
  }

  if (is.null(img2) != TRUE) {
    if (dim(img1)[4] != 1) {
      img1 <- grayscale(img1)
    }
    if (dim(img2)[4] != 1) {
      img2 <- grayscale(img2)
    }
    combine <- add(list(img1, img2))
  } else {
    combine <- img1
  }

  if (is.null(img2) != TRUE & is.null(img3) != TRUE) {
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

  # as img input is just for dimensions no further adaptation is needed
  res <- resultAnalytics(unfiltered = coordinates,
                         coordinates = res_proximityFilter$coordinates,
                         size = res_proximityFilter$size,
                         img = combine)

  # add multi-color info to result
  if (is.null(img2) != TRUE | is.null(img3) != TRUE) {

    witch <- function(detect,
                      res_proximityFilter) {
      col_witch <- list()
      for (a in 1:nrow(detect$coordinates)) {
        pos <-
          which(
            res_proximityFilter$coordinates$x == detect$coordinates$x[a]
            &
              res_proximityFilter$coordinates$y == detect$coordinates$y[a]
          )

        value <- res_proximityFilter$coordinates$value[pos]
        if(length(which(unlist(col_witch) == value)) != 0 | length(value) == 0) {next} else {
          col_witch[a] <- value
        }

      }
      out <- col_witch
    }

    # how many objects have color one
    col1_witch <- witch(col1_detect, res_proximityFilter)
    res$summary$of_col1 <- length(unlist(col1_witch))

    # how many objects have color two
    if(is.null(img2) != TRUE) {
      col2_witch <- witch(col2_detect, res_proximityFilter)
      res$summary$of_col2 <- length(unlist(col2_witch))
    }

    # how many objects have color three
    if(is.null(img3) != TRUE) {
      col3_witch <- witch(col3_detect, res_proximityFilter)
      res$summary$of_col3 <- length(unlist(col3_witch))
    }


    res$detailed$color <- rep(NA, nrow(res$detailed))

    # which objects have which color
    detailed <- function(green_witch, res, color) {
      for (a in unlist(green_witch)) {
        b <- which(res$detailed$beadnumber == a)
        res$detailed$color[b] <- color
      }
      out <- res
    }

    res <- detailed(col1_witch, res, color1)

    if(is.null(img2) != TRUE) {
      res <- detailed(col2_witch, res, color2)
    }
    if(is.null(img3) != TRUE) {
      res <- detailed(col3_witch, res, color3)
    }

    DT <- data.table(res$detailed)
    res_detailed <-
      DT[, list(
        number = length(beadnumber),
        mean_size = mean(size),
        mean_intensity = mean(intensity)
      ), by = color]

    if(is.null(img2) != TRUE) {
      res$dual <- res_detailed
    } else {
      res$multi <- res_detailed
    }

  }
  out <- res
}
