y <- x <- value <- NULL
fillInit <- function(strong) {
  lab <- label(strong, TRUE) * strong
  lab_df <- as.data.frame(lab) |> subset(value > 0)
  DT <- data.table(lab_df)
  grouped_first <-
    DT[, list(x = x[1], y = y[1]), by = value]
}

# starts a fill at each successive location, and accumulates the results
rescueFill <- function(strong, weak) {
  v <- strong
  v[weak == 1] <- .9
  loc <- fillInit(strong)
  df <- data.table(
    x = loc$x,
    y = loc$y
  )
  out <- v

  # error
  if (nrow(df) == 0) {
    stop("The parameters cannot be increased any further since no edges could be detected.")
  }

  for (r in 1:nrow(df)) {
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
  as.cimg(out == 1)
}

nonmax <- function(gr) {
  mag <- with(gr, sqrt(x^2 + y^2))
  grs <- list(x = gr$x / mag, y = gr$y / mag)
  X <- Xc(gr$x)
  Y <- Yc(gr$y)
  val.bwd <- interp(mag, data.frame(
    x = as.vector(X - grs$x),
    y = as.vector(Y - grs$y)
  ))
  val.fwd <- interp(mag, data.frame(
    x = as.vector(X + grs$x),
    y = as.vector(Y + grs$y)
  ))

  throw <- (mag < val.bwd) | (mag < val.fwd)
  mag[throw] <- 0
  mag
}

guess.kmeans <- function(x) {
  out <- kmeans(as.vector(x), centers = c(min(x), mean(x), max(x)))
  list(t1 = max(x[out$cluster == 1]), t2 = max(x[out$cluster == 2]))
}

#' Canny edge detector
#'
#' Adapted code from the imager \code{\link[imager]{cannyEdges}}) function
#' without the usage of dplyr and purrr. If the threshold parameters are
#' missing, they are determined automatically using a k-means heuristic. Use
#' the alpha parameter to adjust the automatic thresholds up or down. The
#' thresholds are returned as attributes. The edge detection is based on a
#' smoothed image gradient with a degree of smoothing set by the sigma
#' parameter.
#' @param img input image
#' @param t1 threshold for weak edges (if missing, both thresholds are
#' determined automatically)
#' @param t2 threshold for strong edges
#' @param alpha threshold adjustment factor (default 1)
#' @param sigma smoothing
#' @returns Object of class 'cimg', displaying detected edges.
#' @importFrom stats kmeans
#' @examples
#' edgeDetection(beads) |> plot()
#' @references https://CRAN.R-project.org/package=imager
#' @export
edgeDetection <- function(img,
                          t1,
                          t2,
                          alpha = 1,
                          sigma = 2) {
  has.col <- spectrum(img) > 1
  if (has.col) {
    warning("Running edge detector on luminance channel")
    img <- grayscale(img)
  }
  if (depth(img) > 1) {
    stop("Videos not supported, run the function on single frames")
  }
  mag <- isoblur(img, sigma) |>
    imgradient("xy") |>
    nonmax()
  if (missing(t1)) {
    guess <- guess.kmeans(mag)
    t2 <- alpha * guess$t2
    t1 <- alpha * guess$t1
  }
  strong <- as.cimg(mag > t2)
  weak <- as.cimg(mag %inr% c(t1, t2))
  out <- rescueFill(strong, weak)

  if (has.col) {
    out <- add.colour(out)
  }
  attr(out, "thresholds") <- c(t1, t2)

  # fill discontinous edges

  out_magick <- cimg2magick(out)
  out_cimg <- mirror(out, axis = "x")
  mo1_lineends <- image_morphology(
    out_magick,
    "HitAndMiss", "LineEnds"
  )
  mo2_diagonalends <- image_morphology(
    out_magick,
    "HitAndMiss", "LineEnds:2>"
  )
  lineends_cimg <- magick2cimg(mo1_lineends)
  diagonalends_cimg <- magick2cimg(mo2_diagonalends)

  end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
  end_points_df <- as.data.frame(end_points)
  colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

  diagonal_edges <-
    which(diagonalends_cimg == TRUE, arr.ind = TRUE)
  diagonal_edges_df <- as.data.frame(diagonal_edges)
  colnames(diagonal_edges_df) <- c("x", "y", "dim3", "dim4")

  lab <- label(out_cimg)
  df_lab <- as.data.frame(lab) |>
    subset(value > 0)

  alt_x <- list()
  alt_y <- list()
  alt_value <- list()
  for (g in 1:nrow(df_lab)) {
    # droplets_array <- as.array(droplets)
    if (out_cimg[df_lab$x[g], df_lab$y[g], 1, 1] == 1) {
      alt_x[g] <- df_lab$x[g]
      alt_y[g] <- df_lab$y[g]
      alt_value[g] <- df_lab$value[g]
    }
  }

  clean_lab_df <- data.frame(x = unlist(alt_x),
                             y = unlist(alt_y),
                             value = unlist(alt_value))

  first_overlay <-
    adaptiveInterpolation(end_points_df,
                          diagonal_edges_df,
                          clean_lab_df,
                          lineends_cimg,
                          radius = 5
    )

  first_connect <-
    parmax(list(out_cimg, as.cimg(first_overlay$overlay)))

  connect_magick <- cimg2magick(first_connect)
  thresh_clean_magick <-
    image_morphology(connect_magick, "thinning", "skeleton")

  out_cimg <- magick2cimg(thresh_clean_magick)

  out <- out_cimg

  as.pixset(out)
}
