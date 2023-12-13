# biopixR - Package for analysis of bioimage image data

## Introduction
In the rapidly evolving landscape of scientific research and technology, the field of image analysis and processing has become indispensable, providing unprecedented insight into complex phenomena. This project aims to utilize image processing techniques, using the R programming language, to analyze and characterize bead microparticles.Microparticles find extensive utility in various scientific domains, including medical diagnostics, environmental monitoring, and materials science.

The rise of high-resolution imaging technologies has led to an exponential increase in the volume and complexity of image data, requiring sophisticated computational methods for efficient analysis. In this context, R is a standout programming language that excels in statistical computing and graphics. Leveraging the rich ecosystem of R packages, this project aims to develop an automated image analysis pipeline for bead microparticles. 

**The primary objectives of this project include**:
 - *Image Preprocessing*: Implementing techniques to fill and reconnect discontinuous lines and edges. Discard coagulated duplets, multiplets, and beads that are too close together as they may excite each other and produce a false positive signal.
 - *Segmentation and Feature Extraction*: Applying algorithms to accurately identify and segment sperical objects (e.g, microparticles). Extracting relevant features such as quantity, size, and intensity for comprehensive characterization. 
 - *Visualization*: Implementing interactive tools with tcltk for the selection of thresholds and smoothing factors. Generating images to control the appropriate functioning of algorithms, for example the reconnection of lines.
 - *Automatization*: Combining algorithms for high-throughput analysis of image data.

This project aims to meet the immediate need for effective bead microparticle analysis and contribute to the broader field of image processing methodologies in the R programming environment. By providing a comprehensive and adaptable framework, the work empowers researchers and practitioners to extract meaningful insights from image data, thus enhancing our understanding of bead microparticles and their diverse applications.


## Example I

The objective of this task is to extract important data from an image of beads. The first step involves identifying individual beads and acquiring their coordinates using the `objectDetection` function, utilizing edge detection and labeling for thorough analysis and identification of distinct edges. This will enable precise coordinate extraction and provide a basis for further analysis using the biopixR package.

```{r}
library(biopixR)

res_objectDetection <- objectDetection(beads, alpha = 0.75, sigma = 0.1)

plot(beads)
with(res_objectDetection$centers, 
     points(res_objectDetection$centers$mxx,
            res_objectDetection$centers$myy,
            col = factor(res_objectDetection$centers$value),
            pch = 19))
```
![1 1](https://github.com/Brauckhoff/biopixR/assets/121032772/ce106352-af73-47ba-836b-911b96a937fa)


During examination, precise identification and marking of each individual bead were achieved, aligning with our intended objective. The `objectDetection` functionality successfully detects each bead using a singular center point and varying colors for differentiation. (Any issues with clotted beads, referred to as doublets or multiplets, are currently disregarded.) Using an alternative visualization method that utilizes the internal `changePixelColor` function can provide a more comprehensive view of the results (based on https://CRAN.R-project.org/package=countcolors).

```{r}
changePixelColor(beads, res_objectDetection$coordinates, color = "purple")
```
![2 1](https://github.com/Brauckhoff/biopixR/assets/121032772/be945b1d-527b-4d0e-8453-315e3c7a7ebd)


For precise analysis, the next step entails eliminating doublets and multiplets using the `proximityFilter`. This process reviews each center, scanning within a specified radius for additional centers. In the event of another center within this range, both are discarded owing to proximity. Notably, this technique proves particularly effective in managing multiplets, with users granted the ability to modify the radius for examination.

```{r}
res_proximityFilter <- proximityFilter(res_objectDetection, radius = 10)

plot(beads)
with(res_proximityFilter$remaining.centers, 
     points(res_proximityFilter$remaining.centers$mxx,
            res_proximityFilter$remaining.centers$myy,
            col = "darkgreen",
            pch = 19))
with(res_proximityFilter$discard,
     points(res_proximityFilter$discard$mx,
            res_proximityFilter$discard$my,
            col = "darkred",
            pch = 19))
```
![3 1](https://github.com/Brauckhoff/biopixR/assets/121032772/4b3e91c9-ea07-4ebd-9709-b71d72e133d0)


When the size filter is applied to the remainder of the beads, both centers and coordinates are taken into account. This algorithm calculates the number of pixels in each labeled object, allowing users to establish limits for rejecting duplicates. It also provides an option for establishing a lower limit.

```{r}
res_sizeFilter <- sizeFilter(res_proximityFilter,
                             lowerlimit = 0,
                             upperlimit = 150)

changePixelColor(beads, res_sizeFilter$remaining.coordinates.s, color = "darkgreen")
```
![4 1](https://github.com/Brauckhoff/biopixR/assets/121032772/e4c93b06-dbf4-4924-94be-fafd311b3918)


In conclusion, obtaining meaningful information from the filtered dataset is essential. The main findings encompass the number of objects that remained and were discarded, object sizes analyzed via the `sizeFilter`, signal intensity, and area density. The `ResultAnalytics` function of biopixR extracts and calculates the described parameters. It requires input of the data frame with remaining coordinates, cluster size, and the original image obtained through the `sizeFilter` function.

```{r}
result <- ResultAnalytics(res_sizeFilter)
result$Summary
```
Number_of_Beads | Mean_Size | Mean_intensity | Bead_density | Estimated_rejected | mean_distance 
--- | --- | --- | --- | --- | ---
8 | 94.8 | 0.59 | 0.0469 | 6 | 67.1


For more detailed information about the features and capabilities of the package feel free to consult our [vignettes](https://github.com/Brauckhoff/biopixR/blob/main/vignettes/biopixR.Rmd).


## Example II

The objective of this example is to address discontinuous edges by filling gaps in lines. To demonstrate the versatility of the package, an algorithm is used to fill gaps in these discontinuous edges, providing valuable insights into the distribution of droplets and microbeads.

```{r}
threshold(droplets, "13%") |> plot()
```
![5 1](https://github.com/Brauckhoff/biopixR/assets/121032772/95102d49-9f78-4f86-884c-13c6160ec75d)




```{r}
thresh_cimg <- as.cimg(thresh)
thresh_magick <- cimg2magick(thresh_cimg)
neg_thresh <- image_negate(thresh_magick)
neg_thresh_cimg <- magick2cimg(neg_thresh)
neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")
neg_thresh_m |> plot()
```
![5 3](https://github.com/Brauckhoff/biopixR/assets/121032772/f993ac0a-b682-4d8c-a3bd-6c5458edab01)


```{r}
# same orientation for 'cimg' and 'magick-image'
thresh_clean_m <- mirror(neg_thresh_m, axis = "x")
thresh_clean_magick <- cimg2magick(thresh_clean_m)

# getting coordinates of all line ends
mo1_lineends <- image_morphology(thresh_clean_magick,
                                 "HitAndMiss", "LineEnds")

# transform extracted coordinates into data frame
lineends_cimg <- magick2cimg(mo1_lineends)

end_points <- which(lineends_cimg == TRUE, arr.ind = TRUE)
end_points_df <- as.data.frame(end_points)
colnames(end_points_df) <- c("x", "y", "dim3", "dim4")

# highlighted line ends
changePixelColor(neg_thresh_m, end_points_df, color = "green")
```
![6 2](https://github.com/Brauckhoff/biopixR/assets/121032772/7e8be967-7ada-4f1e-a59d-6139623bc16b)


```{r}
closed_gaps <- fillLineGaps(droplets,
                            droplet_beads,
                            threshold = "13%",
                            alpha = 1,
                            sigma = 0.1,
                            radius = 5,
                            iterations = 3,
                            visualize = TRUE)

closed_gaps |> plot()
```
![7 1](https://github.com/Brauckhoff/biopixR/assets/121032772/f691ca38-af52-4941-abb3-c98ec7c26ae2)




![8 3](https://github.com/Brauckhoff/biopixR/assets/121032772/4714dcfa-0d69-48d9-8267-771d55c24c08)




partitions | empty_partitions | bead_partitions | single_bead | multiple_beads  
--- | --- | --- | --- | --- 
125 | 121 | 4 | 3 | 1 


For more detailed information about the features and capabilities of the package feel free to consult our [vignettes](https://github.com/Brauckhoff/biopixR/blob/main/vignettes/biopixR.Rmd).


## Installation

```{r}
install.package("devtools")
devtools::install_github("Brauckhoff/biopixR")
```


# Related Research

- Reimann, R., Zeng, B., Jakopec, M., Burdukiewicz, M., Petrick, I., Schierack, P., Rödiger, S., 2020. Classification of dead and living microalgae Chlorella vulgaris by bioimage informatics and machine learning. Algal Research 48, 101908. https://doi.org/10.1016/j.algal.2020.101908
- Schneider, J., Weiss, R., Ruhe, M., Jung, T., Roggenbuck, D., Stohwasser, R., Schierack, P., Rödiger, S., 2019. Open source bioimage informatics tools for the analysis of DNA damage and associated biomarkers. Journal of Laboratory and Precision Medicine 4, 1–27. https://doi.org/10.21037/jlpm.2019.04.05
- Weiss, R., Karimijafarbigloo, S., Roggenbuck, D., Rödiger, S., 2022. Applications of Neural Networks in Biomedical Data Analysis. Biomedicines 10, 1469. https://doi.org/10.3390/biomedicines10071469
- Rödiger, S., Böhm, A., Schimke, I., 2013. Surface Melting Curve Analysis with R. The R Journal 5, 37–53. https://doi.org/10.32614/RJ-2013-024
- Rödiger, S., Liebsch, C., Schmidt, C., Lehmann, W., Resch-Genger, U., Schedler, U., Schierack, P., 2014. Nucleic acid detection based on the use of microbeads: a review. Microchim Acta 181, 1151–1168. https://doi.org/10.1007/s00604-014-1243-4
- Rödiger, S., Schierack, P., Böhm, A., Nitschke, J., Berger, I., Frömmel, U., Schmidt, C., Ruhland, M., Schimke, I., Roggenbuck, D., Lehmann, W., Schröder, C., 2013. A highly versatile microscope imaging technology platform for the multiplex real-time detection of biomolecules and autoimmune antibodies. Adv Biochem Eng Biotechnol 133, 35–74. https://doi.org/10.1007/10_2011_132

