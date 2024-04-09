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

explain the software functionality and domain of use to a non-specialist reader

`biopixR` is a `R` @R_Core_Team

# Statement of need

explain the research applications of the software in the context of related work

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

...:

* `edgeDetection()`, [@imager],
* `objectDetection()`, ,
* `sizeFilter()`, ,
* `proximityFilter()`, ,
* `resultAnalytics()`, ,
* `imgPipe()`, ,
* `shapeFeatures()`, .

...:

* `haralickCluster()`, [@radiomics],
* `scanDir()`, .

...:

* `changePixelColor()`, [@countcolors],
* `interpolatePixels()`, ,
* `adaptiveInterpolation()`, ,
* `fillLineGaps()`, .

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

