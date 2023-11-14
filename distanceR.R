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
resX <- lapply(seq_along(grouped_lab_img$value), function(i) {
  x <- grouped_lab_img[i, ]$mxx
  y <- grouped_lab_img[i, ]$myy
  lapply(as.list(grouped_lab_img$mxx), function(d) {
    if(x != d) {
      if(x - 10 < d & x + 10 > d) {
        g <- which(grouped_lab_img$mxx == d)
        h <- grouped_lab_img[g, ]$myy
        lapply(as.list(h), function(v) {
          if(y - 10 < v & y + 10 > v) {
            j <- which(grouped_lab_img$myy == v & grouped_lab_img$mxx == d)
            j
          }
        })
      }
    }
  })
})

distanced_list <- sort(unique(unlist(resX)))

distanceR <- function(, radius = 10)
