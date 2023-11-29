#' Distance based exclusion
#'
#' Detects beads that are too close to each other
#' @param res_objectDetection list obtained by the objectDetection function
#' @param radius distance in pixels from one center in which no other centers
#' are allowed
#' @returns list of 4 objects:
#' 1. data frame of labeled region with the central coordinates
#' 2. all coordinates that are in labeled regions
#' 3. center coordinates and cluster number of discarded regions
#' 4. original image
#' @examples
#' res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)
#' proximityFilter(res_objectDetection, radius = 10)
#' @export
proximityFilter <- function(res_objectDetection, radius = 10) {
  # section two: distance based exclusion
  grouped_lab_img <- res_objectDetection$centers
  df_lab_img <- res_objectDetection$coordinates

  # looking at every center of an labeled object
  # first: check the surrounding pixels from the current center, if there is
  # another center -> rectangle over hole y
  # axis, with width of 2*radius
  # second (if first true): check if the found x coordinates are in a range of
  # 'radius' pixels from the current center regarding the y coordinates ->
  # creates second rectangle over hole x axis, with width of 2*radius
  # third: only if a coordinate is in both rectangles (the radius^2 around the
  # current center) it is viewed as too close and therefore discarded
  distanced_excl_list <- list()
  for (i in seq_along(grouped_lab_img$value)) {
    x <- grouped_lab_img[i, ]$mxx
    y <- grouped_lab_img[i, ]$myy
    for (d in grouped_lab_img$mxx) {
      if (x == d) {
        next
      } else {
        if (x - radius < d & x + radius > d) {
          clus <- which(grouped_lab_img$mxx == d)
          # print(g)
          y_clus <- grouped_lab_img[clus, ]$myy
          # print(h)
          for (v in y_clus) {
            if (y - radius < v & y + radius > v) {
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
    coordinates = df_lab_img,
    discard = distance_discard_df,
    image = res_objectDetection$image
  )
  out
}
