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

`biopixR` (0.2.4, [LGLP-3.0 license](https://www.gnu.org/licenses/lgpl-3.0.en.html)) is an `R` package ... . Unit testing utilizing the `testthat` package [@testthat] was used 
for quality control of the `biopixR` package.

## Functions

The `biopixR` package in R is intended for analyzing bioimage data, with a specific focus on the analysis and characterization of bead microparticles. 
The package provides tools for image preprocessing, segmentation, feature extraction, and visualization. It supports automation for high-throughput analysis, 
utilizing algorithms to identify spherical objects, extract their features, and implement interactive tools for threshold and smoothing factor selection. Furthermore, 
it offers features for removing clumped or closely positioned particles to prevent inaccurate results, 
with the goal of improving the analysis of microparticles in diverse scientific disciplines.



...:

* `edgeDetection()`, ,
* `objectDetection()`, ,
* `sizeFilter()`, ,
* `proximityFilter()`, ,
* `resultAnalytics()`, ,
* `imgPipe()`, .
* `scanDir()` ???

* `haralickCluster()` ???
* `shapeFeatures()` ???

...:

* `changePixelColor()`, ,
* `interpolatePixels()`, ,
* `adaptiveInterpolation()`, ,
* `fillLineGaps()`, .

Examples demonstrating the use of @biopixR for image analysis tasks can be found in the package's vignette.

## Graphical User Interface:

`interactive_objectDetection()` @tcltk

Image
In this example the GUI was used in RStudio (2023.09.0+463, Linux, Ubuntu 22.04.3 LTS) (to do: cite RStudio)

# Conclusion



# Acknowledgments



# Funding



# References

250-1000 words
