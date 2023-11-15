# section two: distance based exclusion

# looking at every center of an labeled object
# first: check the surrounding x pixels from the current center, if there is
# another center (looking only at x coordinates) -> rectangle over hole y axis,
# with width of 2x
# second (if first true): check if the found x coordinates are in a range of x 
# pixels from the current center regarding the y coordinates -> creates second
# rectangle over hole x axis, with width of 2x
# third: only if a coordinate is in both rectangles (the x^2 around the current 
# center) it is viewed as too close and therefore discarded
distanceR <- function(groubped_lab_img, radius = 10) {
  distanced_excl_list <- list()
  for(i in seq_along(grouped_lab_img$value)) {
    x <- grouped_lab_img[i, ]$mxx
    y <- grouped_lab_img[i, ]$myy
    for(d in grouped_lab_img$mxx) {
      if(x == d) {
        next
      } else {
        if(x - radius < d & x + radius > d) {
          clus <- which(grouped_lab_img$mxx == d)
          #print(g)
          y_clus <- grouped_lab_img[clus, ]$myy
          #print(h)
          for(v in y_clus) {
            if(y - radius < v & y + radius > v) {
              too_close <- which(grouped_lab_img$myy == v & grouped_lab_img$mxx == d)
              distanced_excl_list[too_close] <- c(too_close)
            }
          }
        }
      }
    }
  }
  
  clean_distanced_excl <- unlist(distanced_excl_list)
  
  distance_discard_df <- data.frame(mx = rep(NA, length(clean_distanced_excl)), 
                                    my = rep(NA, length(clean_distanced_excl)))
  
  for (a in clean_distanced_excl) {
    x <- grouped_lab_img[a, ]$mxx
    y <- grouped_lab_img[a, ]$myy
    distance_discard_df[a, 1] <- x
    distance_discard_df[a, 2] <- y
  }
  distance_discard_df
}
