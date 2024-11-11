  <!-- badges: start -->
  [![CRAN Version](https://www.r-pkg.org/badges/version/biopixR)](https://cran.r-project.org/package=biopixR)
  [![R-CMD-check](https://github.com/Brauckhoff/biopixR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/Brauckhoff/biopixR/actions/workflows/R-CMD-check.yml)
  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12744223.svg)](https://doi.org/10.5281/zenodo.12744223)
  [![status](https://joss.theoj.org/papers/80bda20fb774fd758a14b5ff02aaed68/status.svg)](https://joss.theoj.org/papers/10.21105/joss.07074)
  <!-- badges: end -->

![logo2 (another copy)](https://github.com/Brauckhoff/biopixR/assets/121032772/4a057464-1212-47d8-9af3-391f10179c4b)


# biopixR - Package for analysis of bioimage image data

## Introduction
In the rapidly evolving landscape of scientific research and technology, the field of image analysis and processing has become indispensable, providing unprecedented insight into complex phenomena. This project aims to utilize image processing techniques, using the [`R` statistical programming language](https://www.r-project.org/), to analyze and characterize bead microparticles. Microparticles find extensive utility in various scientific domains, including medical diagnostics, environmental monitoring, and materials science.

The rise of high-resolution imaging technologies has led to an exponential increase in the volume and complexity of image data, requiring sophisticated computational methods for efficient analysis. In this context, `R` is a standout programming language that excels in statistical computing and graphics. Leveraging the rich ecosystem of `R` packages, this project aims to develop an automated image analysis pipeline for bead microparticles. 

**The primary objectives of this project include**:
 - *Image Preprocessing*: Implementing techniques to fill and reconnect discontinuous lines and edges. Discard coagulated duplets, multiplets, and beads that are too close together as they may excite each other and produce a false positive signal.
 - *Segmentation and Feature Extraction*: Applying algorithms to accurately identify and segment sperical objects (e.g, microparticles). Extracting relevant features such as quantity, size, and intensity for comprehensive characterization. 
 - *Visualization*: Implementing interactive tools with [Tcl/Tk](https://www.tcl-lang.org/) via the `tcltk` package for the selection of thresholds and smoothing factors. Generating images to control the appropriate functioning of algorithms, for example the reconnection of lines.
 - *Automatization*: Combining algorithms for medium-throughput analysis of image data.

This project aims to meet the immediate need for effective bead microparticle analysis and contribute to the broader field of image processing methodologies in the `R` programming environment. By providing a comprehensive and adaptable framework, the work empowers researchers and practitioners to extract meaningful insights from image data, thus enhancing our understanding of bead microparticles and their diverse applications.


## Installation

Now available on CRAN so try:

```{r}
install.packages("biopixR")
```

or try the latest version of `biopixR`:

```{r}
install.packages("devtools")
devtools::install_github("Brauckhoff/biopixR")
```

The `biopixR` package has been tested across multiple platforms and is expected to work on all operating systems. However, we've received reports of users encountering issues when loading the package. For macOS users, please ensure that X11 is installed on your system, as its absence can prevent the `imager` dependency from loading, which in turn affects the package. If X11 is not installed, you can resolve this by downloading it from the official [XQuartz](https://www.xquartz.org/) website or by running the following command with the **Homebrew Cask** extension: `brew install --cask xquartz`.

All other dependencies listed in the `DESCRIPTION` file, including `imager`, `magick`, `tcltk`, `data.table`, and `cluster`, will be installed automatically with the command mentioned above. The package works with `R` version 4.2.0 or higher.


## Example I

The objective of this task is to extract important data from an image of microbeads. The first step involves identifying individual microbeads and acquiring their coordinates using the `objectDetection` function, utilizing edge detection and labeling for thorough analysis and identification of distinct edges. This will enable precise coordinate extraction and provide a basis for further analysis using the biopixR package.

```{r}
library(biopixR)

res_objectDetection <-
  objectDetection(beads, alpha = 0.75, sigma = 0.1)

plot(beads)
with(
  res_objectDetection$centers,
  points(
    res_objectDetection$centers$mx,
    res_objectDetection$centers$my,
    col = factor(res_objectDetection$centers$value),
    pch = 19
  )
)
```
![1](https://github.com/Brauckhoff/biopixR/assets/121032772/ecfe2f87-9463-4384-a111-99779146d054)


During examination, precise identification and marking of each individual bead were achieved, aligning with our intended objective. The `objectDetection` functionality successfully detects each bead using a singular center point and varying colors for differentiation. Using an alternative visualization method that utilizes the internal `changePixelColor` function can provide a more comprehensive view of the results (based on https://CRAN.R-project.org/package=countcolors).

```{r}
changePixelColor(
  beads,
  res_objectDetection$coordinates,
  color = "purple",
  visualize = TRUE
)
```
![2](https://github.com/Brauckhoff/biopixR/assets/121032772/0464421a-e4f7-410e-a3c0-3fae860527db)


For precise analysis, the next step entails eliminating doublets and multiplets using the `sizeFilter`. The filter is applied using the previously obtained coordinates and centers, as well as predetermined upper and lower limits. This algorithm calculates the number of pixels in each labeled object, allowing users to establish limits for rejecting doublets and multiplets. If the number of detected objects is higher, the algorithm can automatically calculate limits using the IQR of the size distribution.

```{r}
res_sizeFilter <- sizeFilter(
  centers = res_objectDetection$centers,
  coordinates = res_objectDetection$coordinates,
  lowerlimit = 50,
  upperlimit = 150
)

changePixelColor(
  beads,
  res_sizeFilter$coordinates,
  color = "darkgreen",
  visualize = TRUE
)
```
![3](https://github.com/Brauckhoff/biopixR/assets/121032772/5cf5c5a3-a8cf-47fc-b3f8-6ae367fb7abb)


This function examines each collected center and scans a specified radius for positive pixels. If another positive pixel from a different object is detected within this range, both are rejected due to their proximity. The radius can be manually selected or determined automatically.

```{r}
res_proximityFilter <-
  proximityFilter(
    centers = res_sizeFilter$centers,
    coordinates = res_objectDetection$coordinates,
    radius = 'auto'
  )

changePixelColor(
  beads,
  res_proximityFilter$coordinates,
  color = "darkgreen",
  visualize = TRUE
)
text(
  res_proximityFilter$centers$mx,
  res_proximityFilter$centers$my,
  res_proximityFilter$centers$value,
  col = "grey"
)
```
![4](https://github.com/Brauckhoff/biopixR/assets/121032772/9ff1a69e-acf2-4e27-88de-8004aa2ef6fa)


In conclusion, obtaining meaningful information from the filtered dataset is essential. The main findings encompass the number of objects that remained and were discarded, object size, signal intensity, and area density. The `resultAnalytics` function of `biopixR` extracts and calculates the described parameters. The function requires input parameters in the form of coordinates, object size, and the original image. The coordinates can be obtained through any of the three previously mentioned functions, allowing for flexibility in selectively applying or omitting specific filters.

```{r}
result <-
  resultAnalytics(
    img = beads,
    coordinates = res_proximityFilter$coordinates,
    unfiltered = res_objectDetection$coordinates
  )

result$detailed
```
beadnumber | size | intensity | x | y | relative_distance 
--- | --- | --- | --- | --- | ---
3 | 83 | 0.644 | 9.18 | 37.8 | 72.2
4 | 84 | 0.670 | 53.11 | 39.4 | 55.6 
5 | 84 | 0.637 | 108.50 | 43.7 | 80.6 
7 | 73 | 0.647 | 35.05 | 97.8 | 60.3 
8 | 85 | 0.576 | 58.36 | 101.1 | 60.4 

```{r}
result$summary
```
number_of_beads | mean_size | mean_intensity | bead_density | estimated_rejected | mean_distance 
--- | --- | --- | --- | --- | ---
5 | 82 | 0.63 | 0.08 | 10 | 65.8


For more detailed information about the features and capabilities of the package feel free to consult our [vignettes](https://CRAN.R-project.org/package=biopixR).


## Example II

The objective of this example is to address discontinuous edges by filling gaps in lines. To demonstrate the versatility of the package, an algorithm is used to fill gaps in these discontinuous edges, providing valuable insights into the distribution of droplets and microbeads. To demonstrate the algorithm's functionality, a simplified overview of the preprocessing process is presented, focusing on thresholding and detecting line endings.

```{r}
thresh <- threshold(droplets, "13%") |> plot()
thresh_cimg <- as.cimg(thresh)
thresh_magick <- cimg2magick(thresh_cimg)
neg_thresh <- image_negate(thresh_magick)
neg_thresh_cimg <- magick2cimg(neg_thresh)
neg_thresh_m <- mirror(neg_thresh_cimg, axis = "x")
neg_thresh_m |> plot()
```
![5 3](https://github.com/Brauckhoff/biopixR/assets/121032772/f993ac0a-b682-4d8c-a3bd-6c5458edab01)


**Detection of line ends**:

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
changePixelColor(neg_thresh_m,
                 end_points_df,
                 color = "green",
                 visualize = TRUE)
```
![6 2](https://github.com/Brauckhoff/biopixR/assets/121032772/7e8be967-7ada-4f1e-a59d-6139623bc16b)


The `fillLineGaps` function from the package is utilized to address the challenge of discontinuous edges. After applying a threshold, the function identifies line endpoints and connects them to the nearest neighboring edge, ensuring more continuous partition boundaries. Additionally, the `objectDetection` function is incorporated to exclude specific objects, such as microbeads, to prevent unwanted connections. The resulting output displays the line ends reconnected and microbeads removed.

```{r}
closed_gaps <- fillLineGaps(droplets,
                            droplet_beads,
                            threshold = "13%",
                            alpha = 1,
                            sigma = 0.1,
                            radius = 5,
                            iterations = 3,
                            visualize = FALSE)

closed_gaps |> plot()
```
![7 1](https://github.com/Brauckhoff/biopixR/assets/121032772/f691ca38-af52-4941-abb3-c98ec7c26ae2)


**Animation displaying the closing of gaps with the fillLineGaps function**:

![8 3](https://github.com/Brauckhoff/biopixR/assets/121032772/4714dcfa-0d69-48d9-8267-771d55c24c08)


After closing the gaps in the lines, important information is extracted and displayed as shown below. The process of feature extraction is described in detail in our [vignettes](https://CRAN.R-project.org/package=biopixR).

partitions | empty_partitions | bead_partitions | single_bead | multiple_beads  
--- | --- | --- | --- | --- 
125 | 121 | 4 | 3 | 1 


For more detailed information about the features and capabilities of the package feel free to consult our [vignettes](https://CRAN.R-project.org/package=biopixR).


## Code citation

The following functions are inherited functionality from other packages and were enhanced or adapted by the package authors:
- `edgeDetection` based on `cannyEdges` from the imager package (https://cran.r-project.org/package=imager)
- `interactive_objectDetection` based on `interactive_blur` from the magickGUI package (https://cran.r-project.org/package=magickGUI)
- `changePixelColor` based on `changePixelColor` from the countcolors package (https://cran.r-project.org/package=countcolors)
- `haralickCluster` partially based on `GLCMFeatures` from the radiomics package (https://cran.r-project.org/package=radiomics)


# Related Research

## Most recent (publications using `biopixR`)

- Geithe, C., Zeng, B., Schmidt, C., Dinter, F., Roggenbuck, D., Lehmann, W., Dame, G., Schierack, P., Hanack, K., Rödiger, S., 2024. A multiplex microchamber diffusion assay for the antibody-based detection of microRNAs on randomly ordered microbeads. Biosensors and Bioelectronics: X 18, 100484. [https://doi.org/10.1016/j.biosx.2024.100484](https://doi.org/10.1016/j.biosx.2024.100484)

## Previous (providing research data and methods for `biopixR`)

- Weiss, R., Karimijafarbigloo, S., Roggenbuck, D., Rödiger, S., 2022. Applications of Neural Networks in Biomedical Data Analysis. Biomedicines 10, 1469. [https://doi.org/10.3390/biomedicines10071469](https://doi.org/10.3390/biomedicines10071469)
- Reimann, R., Zeng, B., Jakopec, M., Burdukiewicz, M., Petrick, I., Schierack, P., Rödiger, S., 2020. Classification of dead and living microalgae Chlorella vulgaris by bioimage informatics and machine learning. Algal Research 48, 101908. [https://doi.org/10.1016/j.algal.2020.101908](https://doi.org/10.1016/j.biosx.2024.100484)
- Schneider, J., Weiss, R., Ruhe, M., Jung, T., Roggenbuck, D., Stohwasser, R., Schierack, P., Rödiger, S., 2019. Open source bioimage informatics tools for the analysis of DNA damage and associated biomarkers. Journal of Laboratory and Precision Medicine 4, 1–27. [https://doi.org/10.21037/jlpm.2019.04.05](https://doi.org/10.21037/jlpm.2019.04.05)
- Rödiger, S., Liebsch, C., Schmidt, C., Lehmann, W., Resch-Genger, U., Schedler, U., Schierack, P., 2014. Nucleic acid detection based on the use of microbeads: a review. Microchim Acta 181, 1151–1168. [https://doi.org/10.1007/s00604-014-1243-4](https://doi.org/10.32614/RJ-2013-024)
- Rödiger, S., Böhm, A., Schimke, I., 2013. Surface Melting Curve Analysis with R. The R Journal 5, 37–53. [https://doi.org/10.32614/RJ-2013-024](https://doi.org/10.32614/RJ-2013-024)
- Rödiger, S., Schierack, P., Böhm, A., Nitschke, J., Berger, I., Frömmel, U., Schmidt, C., Ruhland, M., Schimke, I., Roggenbuck, D., Lehmann, W., Schröder, C., 2013. A highly versatile microscope imaging technology platform for the multiplex real-time detection of biomolecules and autoimmune antibodies. Adv Biochem Eng Biotechnol 133, 35–74. [https://doi.org/10.1007/10_2011_132](https://doi.org/10.1007/10_2011_132)

