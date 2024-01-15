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

  # combine collected images
  if (is.null(img2) != TRUE | is.null(img3) != TRUE) {
    col1_mat <- matrix(0, nrow = nrow(img1), ncol = ncol(img1))

    # create first layer
    for (i in 1:nrow(col1_detect$coordinates)) {
      col1_mat[col1_detect$coordinates$x[i], col1_detect$coordinates$y[i]] <- 1
    }
  }


  if (is.null(img2) != TRUE) {
    col2_mat <- matrix(0, nrow = nrow(img2), ncol = ncol(img2))

    # create second
    for (i in 1:nrow(col2_detect$coordinates)) {
      col2_mat[col2_detect$coordinates$x[i], col2_detect$coordinates$y[i]] <- 1
    }
  }


  if (is.null(img3) != TRUE) {
    col3_mat <- matrix(0, nrow = nrow(img3), ncol = ncol(img3))

    # third layer
    for (i in 1:nrow(col3_detect$coordinates)) {
      col3_mat[col3_detect$coordinates$x[i], col3_detect$coordinates$y[i]] <- 1
    }
  }


  # combine layers and delete overlapping objects
  if (is.null(img2) != TRUE) {
    combine <- add(list(as.cimg(col1_mat), as.cimg(col2_mat)))
    overlap <- which(combine == 2)
    combine[overlap] <- 0
  }

  if (is.null(img2) != TRUE & is.null(img3) != TRUE) {
    combine <- add(list(as.cimg(col1_mat), as.cimg(col2_mat), as.cimg(col3_mat)))
    overlap <- which(combine >= 2)
    combine[overlap] <- 0
  }

  # object detection of the combined image
  if(length(which(combine == 1)) == 0) {
    res_objectDetection <- col1_detect
  } else {
    res_objectDetection <- objectDetection(combine, alpha = alpha, sigma = sigma)
  }

  # size filtering
  if (sizeFilter == TRUE) {
    res_sizeFilter <- sizeFilter(centers = res_objectDetection$centers,
                                 coordinates = res_objectDetection$coordinates,
                                 upperlimit = upperlimit,
                                 lowerlimit = lowerlimit)
  } else {
    res_sizeFilter <- res_objectDetection
  }

  # proximity filtering
  if (proximityFilter == TRUE) {
    res_proximityFilter <- proximityFilter(centers = res_sizeFilter$centers,
                                           coordinates = res_objectDetection$coordinates,
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
    dual_color <- add(list(img1, img2))
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
    multi_color <- add(list(img1, img2, img3))
  }

  # as img input is just for dimensions no further adaptation is needed
  res <- resultAnalytics(unfiltered = res_objectDetection$coordinates,
                         coordinates = res_proximityFilter$coordinates,
                         size = res_proximityFilter$size,
                         img = dual_color)

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
    res$summary$davon_col1 <- length(unlist(green_witch))

    # how many objects have color two
    if(is.null(img2) != TRUE) {
      col2_witch <- witch(col2_detect, res_proximityFilter)
      res$summary$davon_col2 <- length(unlist(col2_witch))
    }

    # how many objects have color three
    if(is.null(img3) != TRUE) {
      col3_witch <- witch(col3_detect, res_proximityFilter)
      res$summary$davon_col3 <- length(unlist(col3_witch))
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
  }
  out <- res
  out
}
