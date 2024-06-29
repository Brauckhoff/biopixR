---
title: '`biopixR`: Extracting Insights from Biological Images'
tags:
- R
- bioimages
- bioinformatics
- microparticles
- batch processing
- reproducible research
date: "24 June 2024"
output:
  pdf_document: default
  html_document:
    df_print: paged
authors:
- name: Tim Brauckhoff
  orcid: "0009-0002-0142-7017"
  affiliation: 1
- name: Coline Kieffer
  affiliation: 2
- name: Stefan Rödiger^[Corresponding author]
  orcid: "0000-0002-1441-6512"
  affiliation: 1, 3
bibliography: paper.bib
affiliations:
- name: BTU Cottbus–Senftenberg, Faculty Environment and Natural Sciences, Senftenberg,
    Germany    \linebreak
  index: 1
- name: The City of Paris Industrial Physics and Chemistry Higher Educational Institution,
    Paris, France    \linebreak
  index: 2
- name: BTU Cottbus–Senftenberg, Faculty of Health Brandenburg, Senftenberg, Germany    \linebreak
  index: 3
---

# Summary

`biopixR` is an `R` package designed specifically for the analysis of bioimage
data. The package comprises 15 functions that cater to a variety of tasks,
including image import, segmentation, feature extraction, quantification,
clustering, parameter optimization, and a distinctive algorithm for line gap
mitigation. Among these is an optimized edge detection algorithm based on the
Canny edge detection algorithm. Together with a noise-resilient threshold
method, these two algorithm form the foundation of feature extraction within the
`biopixR` package. Both edge detection and thresholding can be parameterized via
an interactive Tcl/Tk user interface. For circular objects, the threshold
adjustment factor and the smoothing factor for edge detection can be
automatically calculated using Gaussian process regression models (Kriging
models). Thereby making an automated features extraction, which includes
segmentation and labeling, available in `R`. Furthermore, all available methods
for feature extraction and filtering can be accessed within a single pipeline
function, not found in other packages. The package also includes a function for
clustering objects within an image based on their shape features using
Self-Organizing Maps (SOM).

Designed for medium-throughput analysis, the package is able to analyze whole
directories, utilize multiple cores for parallel processing, and generate
detailed log files to track the analytical process. Furthermore, the `biopixR`
package offers distinctive datasets of microbead images and microbeads in
water-oil emulsions. The fundamental functionality of `biopixR` was recently
employed in the study by @geithe_multiplex_2024 to perform quality control on
microbeads. As the `biopixR` package provides access to a range of fundamental
data, including object size, quantity, shape, and intensity, it is a valuable
tool for researchers in the field. The `biopixR` package leverages capabilities 
from the `imager` [@imager] and `magick` [@magick] packagesto perform its own 
tasks related to biological image processing or analysis.

# Statement of need

Imaging plays a pivotal role in data acquisition within biological laboratories
and the broader life sciences. It offers crucial insights into cells
[@Roediger_2018; @schneider_open_2019], biomarkers [@Vafajoo_2018],
stress responses [@Korkmaz_2018], and gene expression [@Brenner_2000],
which are vital for diagnostics and disease prediction. Microbead technology,
known for its rapid and flexible data collection, relies heavily on imaging
techniques like microscopy [@Roediger_2014; @Ding_2020]. The challenge lies
in the fast and accurate extraction of valuable information from complex
biological images, particularly in the quantification of objects within these
images.

The quantification of biological image data is a fundamental aspect of deriving
insights into biological processes. This is exemplified by microbead-assays, which
offer extensive multiplexing abilities, high-throughput capacity, reduced
analysis time, and minimal sample requirements [@Roediger_2014]. Methods for
detecting microbeads include flow cytometry, microfluidics, and image-based
techniques [@Choi_2019]. A user-friendly tool integrated into `R` could
enhance analysis by combining statistical analysis and visualization techniques.
The advancement of smart device imaging and chip development is facilitating the
utilization of microbeads in point-of-care testing (POCT) and disease diagnosis
[@Dinter_2019; @Zhang_2019]. This progress underscores the necessity for the
development of efficient methods for extracting biological information from
images, thereby eliminating the necessity for the use of complex laboratory
equipment. Software that extracts attributes such as fluorescence intensity,
size and shape allow for microbead encoding and differentiation between
populations, thereby enhancing multiplexing capabilities [@Zhang_2019].

Microbead-based emulsion Polymerase Chain Reaction (ePCR) assays, which are
typically analyzed using fluorescence-activated cell sorting (FACS)
[@Fraser_2015], could potentially benefit from the use of imaging techniques.
As this software is capable of analyzing microbead-based ePCR by preprocessing
brightfield droplet images with the provided gap-filling algorithm. The
aforementioned applications extend to the assessment of wastewater for the
detection of microplastics [@Ding_2020], the real-time localization in
microbead-based drug delivery systems [@Bannerman_2016], and other fields of
life science, such as cell biology.

Consequently, the `biopixR` package for `R` is a fundamental tool. The automated
evaluation process enables medium-throughput analysis directly from images,
simplifying analytical procedures and expanding experimental possibilities.

# Software engineering

`biopixR` (1.0.0, 
[LGLP-3.0 license](https://www.gnu.org/licenses/lgpl-3.0.en.html)) is an `R` [@R_Core_Team]
package (S3 class system). To ensure code quality, we employed various methodologies, 
including [Continuous Integration](https://github.com/Brauckhoff/biopixR/blob/main/.github/workflows/R-CMD-check.yml)
(CI), unit testing [@testthat], adherence to naming conventions
[@Rasmus_2012], and the application of style guidelines [@Wickham_2019]. The choice of `R` as a programming language for image
processing is based on its open-source framework, advanced packages such as
`imager` [@imager] and `magick` [@magick], and its strong support for
reproducible research [@Xie_2019]. The importance of ensuring software 
reproducibility is widely recognized, not only for our own work but also 
for the broader scientific community  [@gentleman_statistical_2007 ;@rodiger_r_2015]. One measure to achieve this is by minimizing dependencies on other packages 
or libraries and single archives whenever possible. Therefore, `biopixR` depends on `R` ($\ge$ 4.2.0), `imager`, `magick` and `tcltk`, imports `data.table` and `cluster`
and suggests `knitr`, `rmarkdown`, `doParallel`, `kohonen`, `imagerExtra`, `GPareto` and `foreach` exclusively from the Comprehensive R Archive Network (CRAN).
These features collectively provide a
sophisticated environment for image analysis and editing, with the added
advantage of community-driven improvements [@Chambers_2008]. `R`'s
integration of analysis with documentation ensures methodological precision and
transparency in scientific research, making it a preferred choice for complex
image processing tasks. The `biopixR` package underwent quality control through
unit testing using the `testthat` package [@testthat], ensuring its
reliability.

# Installation

The `biopixR` package can be installed from CRAN or GitHub, providing users with
stable and developmental versions respectively.

To install the stable version of `biopixR` from CRAN, execute the following
command in `R`:

```
install.packages("biopixR")
```

This command will download and install the latest stable release of the package,
ensuring compatibility and reliability.

For users interested in the latest features and ongoing development, the
developmental version of `biopixR` is available on GitHub. To install this
version, it is first necessary to install the `devtools` package if it is not
already present in your R environment:

```
install.packages("devtools")
devtools::install_github("Brauckhoff/biopixR")
```
This command enables the installation of the development build from the GitHub
repository. This provides access to the most recent features and updates that
may not yet be available in the CRAN release.

## Functions

![Dependency graph of the functions present in the `biopixR` package. Showing the levels of complexity by showing the descendants and ancestors of the `imgPipe()` function. The figure was created using the `foodwebr` package from @foodwebr (package version 0.1.1, RStudio 2023.09.0+463, R 4.3.2 on Linux, Ubuntu 22.04.3 LTS). \label{fig:Dependency}](fig_2.png)

The `biopixR` package is intended for analyzing bioimage data in `R`, with a
specific focus on the analysis and characterization of microbeads. The package
provides tools for image preprocessing, segmentation, feature extraction,
filtering, and visualization. It supports automation for medium-throughput
analysis, utilizing algorithms to identify spherical objects, extract their
features, and implement interactive tools for threshold and smoothing factor
selection. Furthermore, it offers features for removing clumped or closely
positioned particles to prevent inaccurate results, with the goal of improving
the analysis of microparticles in diverse scientific disciplines.

The `shapeFeatures()` function is capable of extracting shape-related
information from detected objects and grouping them using the SOM algorithm
[@kohonen]. The central function of `biopixR` is `imgPipe()`, which performs
object detection and applies individual filters. This pipeline integrates all
the fundamental procedures for comprehensive analysis into a single function.
The user is required to provide the input image and select the appropriate
detection methods to gain insights into the specific objectives present
in the image. Furthermore, the user has the option to customize their workflow
with individual filters. In addition, the function is capable of processing
multiple color channels, such as the analysis of dual-color microbeads. The
integration of this function within the dependency network and its interaction
with other functions is illustrated in \autoref{fig:Dependency}. This function
serves as a comprehensive pipeline for image analysis, offering a variety of
selectable functions:

* `importImage()`, joins import functions of the `imager` [@imager] and `magick` [@magick] packages.
* `edgeDetection()`, a combination of a Canny edge detector and gap filling [@imager],
* `objectDetection()`, detects objects in an image by edge detection or thresholding,
* `sizeFilter()`, eliminates objects that exceed or fall below a certain size threshold,
* `proximityFilter()`, filters objects that are in proximity,
* `resultAnalytics()`, summarizes the extracted features in a clear and concise manner.

The `biopixR` package includes functions for analyzing entire directories,
allowing for medium-throughput analysis. Making feature extraction and image
clustering easily accessible:

* `haralickCluster()`, extracts Haralick features and clusters information using PAM (Partitioning Around Medoids) [@Haralick_1973; @radiomics; @cluster],
* `scanDir()`, utilizing the pipeline for whole directory analysis.

The `fillLineGaps()` algorithm, along with helper functions:

* `interpolatePixels()`, calculates the coordinates required to connect two given points,
* `adaptiveInterpolation()`, searches a given radius surrounding a line end for contours and connects them,

addresses the issue of discontinuous edges by iteratively scanning for line ends
within the image and reconnecting them to adjacent contours. Other approaches
for dealing with discontinuous contours are not found in other `R` packages.

Examples demonstrating the use of `biopixR` for image analysis tasks can be found
in the package's vignette [@biopixR].

## Graphical User Interface:

The `biopixR` package has broader applicability beyond microbead detection. The `biopixR`
package is adaptable and can be utilized in any research field where the
identification of distinct objects in images can be achieved through the use of
a Canny edge detector or thresholding. This encompasses research areas such as
foci detection, microplastics, and plant seeds. The automation methods employed
in `biopixR` are predicated on the assumption of circular objects, rendering it
particularly well-suited for the detection, quantification, and extraction of
useful information from circular objects within images. Another integrated tool
is an interactive function that assists the user in selecting the optimal input
for their analysis. The function `interactive_objectDetection()` initiates a
graphical user interface (GUI) (\autoref{fig:RKWard}) that utilizes the Tcl/Tk 
framework [@tcltk], enabling users to adjust the threshold and smoothing settings 
of the image.

![Graphical User Interface for interactive parameter selection. The function `interactive_objectDetection()` provides a simple interface with sliders to adjust threshold, smoothing, and scale. It highlights object contours in purple and centers in green for easy visualization. A) In this example, the GUI was used in RKWard (0.7.5z+0.7.6+devel3, Linux, TUXEDO OS 2, [@rodiger_rkward_2012]). With a few commands, an image can be imported and analyzed. B) The `plot()` function displays the false-color image as a preview. In this figure, cells with DNA damage (similar to @Roediger_2018) are visible. C) Loading the `biopixR` package in the `R` console shows additional information such as loaded libraries and the number of CPU threads (n = 20, parallel processing). D) The rendering process is displayed on the console, including timestamps and the current state. \label{fig:RKWard}](fig_1.png)

# Conclusion

In summary, `biopixR` represents a significant advancement in image analysis capabilities for `R` users. By providing both automation methods and interactive tools, the package empowers researchers to extract valuable insights from images with ease. Its adaptability across various research fields makes it a valuable tool for researchers seeking efficient solutions. Leveraging the power of `R` and its extensive library of packages, users can seamlessly integrate `biopixR` into their existing workflows, streamlining data analysis and visualization tasks. Moreover, the package's minimal dependencies ensure long-term stability and maintainability over time, making it an attractive choice for researchers seeking reproducible software.

# Acknowledgments

The study was funded in part by the project Rubin: NeuroMiR (03RU1U051A, federal
ministry of education and research, Germany).

\pagebreak

# References

