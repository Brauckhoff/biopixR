# first section: detect all beads

# edge detection with default: alpha = 0.75, sigma = 0.1
# fill detected edges and label areas
# create data frame without background
# summarize cluster and calculate center -> display & check
edgeR<- function(image, alpha = 0.75, sigma = 0.1) {
  img <- image
  edge_img <- cannyEdges(img, alpha = alpha, sigma = sigma)
  filled_img <- EBImage::fillHull(edge_img)
  labeled_img <- label(filled_img)
  df_lab_img <- as.data.frame(labeled_img) %>% 
    subset(value > 0)
  grouped_lab_img <- dplyr::group_by(df_lab_img, value) %>% 
    dplyr::summarise(mxx = mean(x), myy = mean(y))
}