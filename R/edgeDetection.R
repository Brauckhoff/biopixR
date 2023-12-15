fillInit <- function(strong) {
  lab <- label(strong, TRUE) * strong
  first <- as.data.frame(lab) |> subset(value > 0)
  DT <- data.table(first)
  grouped_first <-
    DT[, .(x = x[1], y = y[1]), by = value]
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
  as.pixset(out)
}
