---
title: 'biopixR: Extracting Insights from Biological Images'
tags:
  - R
  - bioimages
  - bioinformatics
  - microparticles
  - high-throughput
  - reproducible research
date: "24 April 2024"
affiliations:
  - name: BTU Cottbus–Senftenberg, Faculty Environment and Natural Sciences, Senftenberg, Germany
    index: 1
  - name: The City of Paris Industrial Physics and Chemistry Higher Educational Institution, Paris, France
    index: 2
  - name: BTU Cottbus–Senftenberg, Faculty of Health Brandenburg, Senftenberg, Germany
    index: 3
authors:
  - name: Tim Brauckhoff
    orcid: 0009-0002-0142-7017
    affiliation: 1
  - name: Coline Kieffer
    affiliation: 2
  - name: Stefan Rödiger^[Corresponding author]
    orcid: 0000-0002-1441-6512
    affiliation: 1, 3
bibliography: paper.bib
---

# Summary

beads 

`biopixR` is an `R` package [@R_Core_Team] ...



# Statement of need

explain the research applications of the software in the context of related work

detection is often done by using microscopy, advances towards imageing on smart devices in order to make microbeads mor accesible for POCT [@Zhang_2019] software for automated evaluation is needed also precenting the possiblty for high troughput analysis directly from images without flow cytometry
dtection of other factors than fluorescent intensity can be used for bead encoding like size and shape [@Zhang_2019]. disease diagnosis

improvemnets in this field could be made by making beads detectable through imaging in the field of microbead based drug delivery making real-time detection and localization possible [@Bannerman_2016]

requires software for image analysis, microboead technology for quantification of multiple biomolecules and biomarkers relevant for medical diagnostic applications [@Vafajoo_2018]

potential of microbeads: high degree of multiplexing, high throughput applicability, reduced time of analysis and smaller sample consumption [@Roediger_2014]

application gene expression analysis [@Brenner_2000]

analysis through image analysis; other fields like wastewater evalutation [@Ding_2020]

# Software engineering

`biopixR` (0.2.4, [LGLP-3.0 license](https://www.gnu.org/licenses/lgpl-3.0.en.html)) is an `R` package (S3 class systen). 
The choice of `R` as a programming language for image processing is based on its open-source framework, advanced packages such as `imager` [@imager] 
and `magick` [@magick], and its strong support for reproducible research [@Xie_2019]. These features collectively provide a sophisticated environment for 
image analysis and editing, with the added advantage of community-driven improvements [@Chambers_2008]. `R`'s integration of analysis with documentation ensures 
methodological precision and transparency in scientific research, making it a preferred choice for complex image processing tasks. The `biopixR` package underwent 
quality control through unit testing using the `testthat` package [@testthat], ensuring its reliability.

## Functions

The `biopixR` package in R is intended for analyzing bioimage data, with a specific focus on the analysis and characterization of bead microparticles. 
The package provides tools for image preprocessing, segmentation, feature extraction, and visualization. It supports automation for high-throughput analysis, 
utilizing algorithms to identify spherical objects, extract their features, and implement interactive tools for threshold and smoothing factor selection. Furthermore, 
it offers features for removing clumped or closely positioned particles to prevent inaccurate results, 
with the goal of improving the analysis of microparticles in diverse scientific disciplines.

In addition to the `shapeFeatures()` function, capable of extracting shape-related information from detected objects and grouping them using the 
SOM (Self-Organizing Map) algorithm [@kohonen], there's also the `imgPipe()` function. This latter function serves as a comprehensive pipeline for image analysis, 
offering a variety of selectable functions:

* `edgeDetection()`, a combination of a Canny edge detector and gap filling [@imager],
* `objectDetection()`, detects objects in an image by identifying their coordinates,
* `sizeFilter()`, eliminates objects that exceed or fall below a certain size threshold,
* `proximityFilter()`, filters objects that are in close proximity to each other,
* `resultAnalytics()`, summarizes the extracted features in a clear and concise manner.

The `biopixR` package includes functions for analyzing entire directories, allowing for high-throughput analysis. 
Making feature extraction and image clustering easily accessible:

* `haralickCluster()`, extracts Haralick features and cluster using PAM (Partitioning Around Medoids) [@Haralick_1973; @radiomics; @cluster],
* `scanDir()`, utilizing the pipline for whole directory analysis.

The `fillLineGaps()` algorthim, along with helper functions:

* `interpolatePixels()`, calculates the coordinates required to connect two given points,
* `adaptiveInterpolation()`, searches a given radius surrounding a line end for contours and connects them,

addresses the issue of discontinuous edges by iteratively scanning for line ends within the image and reconnecting them to adjacent contours.

Examples demonstrating the use of @biopixR for image analysis tasks can be found in the package's vignette.

## Graphical User Interface:

The function `interactive_objectDetection()` initiates a graphical user interface (GUI) that utilizes the Tcl/Tk framework [@tcltk], 
enabling users to adjust the threshold and smoothing settings of the image (\autoref{fig:fig_1}).

![Graphical user interface for interactive parameter selection. A) The function `interactive_objectDetection()` provides a user-friendly interface with sliders to adjust threshold, smoothing, and scale. It highlights object contours in purple and centers in green for easy visualization. In this example the GUI was used in `RStudio` (2023.09.0+463, Linux, Ubuntu 22.04.3 LTS, [@RStudio]). B) The rendering process is displayed on the console, including timestamps and the current state.\label{fig:fig_1}](fig_1.png)

# Current status and outlook

The `biopixR` package was released on CRAN in March 2024. To ensure code quality, we employed various methodologies, including 
[Continuous Integration](https://github.com/Brauckhoff/biopixR/blob/main/.github/workflows/R-CMD-check.yml) (CI), 
unit testing [@testthat], adherence to naming conventions [@Rasmus_2012], and the application of style guidelines [@Wickham_2019]. 
Although the package is relatively new, we are working to expand its features and evaluate its applicability using empirical research data from diverse sources. 
In addition, future developments will involve expanding its capabilities to identify DNA damage, particularly in the form of foci.

# Acknowledgments



# Funding



# References

