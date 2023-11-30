#' Proximity based exclusion
#'
#' Detects objects that are within a certain range of each other.
#' The range is calculated in regards to the center of the objects.
#' @param res_objectDetection list with center coordinates of objects (needs to
#' be a data.frame named centers which contains three columns:
#' 1 - mxx: x coordinates;
#' 2 - myy: y coordinates;
#' 3 - value: cluster number)
#' or list obtained by the objectDetection function
#' @param radius distance from one center in which no other centers
#' are allowed (in pixels)
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled objects
#' 3. center coordinates of discarded objects
#' 4. original image
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' proximityFilter(res_objectDetection, radius = 10)
#'
#' # without usage of objectDetection
#' mat <- matrix(0, 8, 8)
#' mat[3, 5] <- 1
#' mat[5, 2] <- 1
#' mat[6, 7] <- 1
#' mat[7, 3] <- 1
#' sim_img <- as.cimg(mat)
#' centers <- data.frame(
#' mxx = c(3, 5, 6, 7),
#' myy = c(5, 2, 7, 3),
#' value = c(1:4)
#' )
#' objects <- list(centers = centers)
#' res <- proximityFilter(objects, radius = 3)
#' na.omit(res$discard)
#' @export
proximityFilter <- function(res_objectDetection, radius = 10) {
  # assign imports
  grouped_lab_img <- res_objectDetection$centers

  # looking at every center of an labeled object
  distanced_excl_list <- list()
  for (i in seq_along(grouped_lab_img$value)) {
    x <- grouped_lab_img[i, ]$mxx
    y <- grouped_lab_img[i, ]$myy
    for (d in grouped_lab_img$mxx) {
      if (x == d) {
        next
      } else {

        # first: check the surrounding pixels from the current center, if there
        # is another center -> rectangle over hole y axis, with width of
        # 2*radius
        if (x - radius < d & x + radius > d) {
          clus <- which(grouped_lab_img$mxx == d)
          y_clus <- grouped_lab_img[clus, ]$myy

          # second (if first true): check if the found x coordinates are in a
          # range of 'radius' pixels from the current center regarding the y
          # coordinates -> creates second rectangle over hole x axis, with
          # width of 2*radius
          for (v in y_clus) {
            if (y - radius < v & y + radius > v) {

              # third: only if a coordinate is in both rectangles (the radius^2
              # around the current center) it is viewed as too close and
              # therefore discarded
              too_close <- which(grouped_lab_img$myy == v &
                grouped_lab_img$mxx == d)
              distanced_excl_list[too_close] <- c(too_close)

            }
          }
        }
      }
    }
  }

  clean_distanced_excl <- unlist(distanced_excl_list)

  # create data frame with the center coordinates of the discarded clusters
  distance_discard_df <- data.frame(
    mx = rep(NA, nrow(grouped_lab_img)),
    my = rep(NA, nrow(grouped_lab_img))
  )

  for (a in clean_distanced_excl) {
    x <- grouped_lab_img[a, ]$mxx
    y <- grouped_lab_img[a, ]$myy
    distance_discard_df[a, 1] <- x
    distance_discard_df[a, 2] <- y
  }

  out <- list(
    centers = grouped_lab_img,
    coordinates = res_objectDetection$coordinates,
    discard = distance_discard_df,
    image = res_objectDetection$image
  )
  out
}
