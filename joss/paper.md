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

Imaging is crucial for data acquisition in biological laboratories and the
broader field of life sciences. It provides essential insights into cells
[@Roediger_2018; @schneider_open_2019], biomarkers [@Vafajoo_2018], stress responses
[@Korkmaz_2018], and gene expression [@Brenner_2000], which are necessary
for diagnostics and predicting disease outcomes. Microbead technology is a
highly promising field for gathering complex information in a straightforward,
rapid, and flexible manner [@Roediger_2014]. These assays that use microbeads
rely on visualization methods, such as microscopy [@Ding_2020]. Therefore, it
is necessary to have user-friendly image processing software to effectively
analyze the data.

`biopixR` is an `R` package [@R_Core_Team] that utilizes the `imager`
[@imager] and `magick` [@magick] packages to perform various image
processing tasks, such as preprocessing, object counting, feature extraction,
and filtering. It is designed for medium-throughput analysis and generates log
files to track the analytical process in detail. Core functionality of `biopixR` 
was used in a recent study [@geithe_multiplex_2024] to perform quality control 
on microbeads. `biopixR` can extract various
types of information, including *object size*, *quantity*, *shape* and
*intensity*. The package includes an algorithm that fills gaps between lines and
reconnects loose ends, making complex images accessible for later analysis.

# Statement of need

Extracting valuable information from complex biological images in a quick,
reliable, and straightforward manner presents a significant challenge. A
critical aspect of this process is the quantification of objects within the
images.

Quantification is crucial for deriving biological insights from experiments, as
demonstrated in bead-assays. This method is highly promising due to its
extensive multiplexing abilities, high-throughput capacity, shortened analysis
time, and minimal sample requirements [@Roediger_2014]. Detection methods for
microbeads include flow cytometry, microfluidics, and image-based techniques
[@Choi_2019]. A user-friendly tool integrated into R could be highly
beneficial for comprehensive analysis, particularly when combined with
statistical analysis and visualization techniques. Advancements in smart device
imaging and chip development for bead-assays are improving the use of microbeads
in point-of-care testing (POCT) and disease diagnosis [@Dinter_2019;
@Zhang_2019]. This progress underscores the need for direct extraction of
biological information from images, eliminating the dependence on complex
laboratory equipment. Additionally, software capable of extracting attributes
like fluorescence intensity, size, and shape enables the encoding of beads
[@Zhang_2019]. This capability makes it possible to differentiate between
populations, thus significantly enhancing the scope for multiplexing.

text1 | text2 | text3 
--- | --- | ---
A | B | C
D | E | F
G | H | I

**to do: add image**

Bead-based ePCR assays, like other bead-based assays, are often analyzed using
fluorescence activated cell sorting (FACS) [@Fraser_2015]. However, this
software could enable the analysis of bead-based ePCR through imaging
techniques, as droplets can be imaged and altered to appear distinct by closing
gaps between their contours. The package could be applied in various domains,
including wastewater assessment, to investigate the presence of microplastics in
water [@Ding_2020]. It could also be used in microbead-based drug delivery
systems to facilitate real-time detection and localization [@Bannerman_2016].

Therefore, the `biopixR` package for `R` is needed, as it automates the
evaluation process and enables high-throughput analysis directly from images,
simplifying analytical procedures and opening up new experimental possibilities.

# Software engineering

`biopixR` (0.2.4, 
[LGLP-3.0license](https://www.gnu.org/licenses/lgpl-3.0.en.html)) is an `R` 
package (S3 class systen). The choice of `R` as a programming language for image
processing is based on its open-source framework, advanced packages such as
`imager` [@imager] and `magick` [@magick], and its strong support for
reproducible research [@Xie_2019]. These features collectively provide a
sophisticated environment for image analysis and editing, with the added
advantage of community-driven improvements [@Chambers_2008]. `R`'s
integration of analysis with documentation ensures methodological precision and
transparency in scientific research, making it a preferred choice for complex
image processing tasks. The `biopixR` package underwent quality control through
unit testing using the `testthat` package [@testthat], ensuring its
reliability.

## Functions

The `biopixR` package in R is intended for analyzing bioimage data, with a
specific focus on the analysis and characterization of bead microparticles. The
package provides tools for image preprocessing, segmentation, feature
extraction, and visualization. It supports automation for high-throughput
analysis, utilizing algorithms to identify spherical objects, extract their
features, and implement interactive tools for threshold and smoothing factor
selection. Furthermore, it offers features for removing clumped or closely
positioned particles to prevent inaccurate results, with the goal of improving
the analysis of microparticles in diverse scientific disciplines.

In addition to the `shapeFeatures()` function, capable of extracting
shape-related information from detected objects and grouping them using the SOM
(Self-Organizing Map) algorithm [@kohonen], there's also the `imgPipe()`
function. This latter function serves as a comprehensive pipeline for image
analysis, offering a variety of selectable functions:

* `edgeDetection()`, a combination of a Canny edge detector and gap filling [@imager],
* `objectDetection()`, detects objects in an image by identifying their coordinates,
* `sizeFilter()`, eliminates objects that exceed or fall below a certain size threshold,
* `proximityFilter()`, filters objects that are in close proximity to each other,
* `resultAnalytics()`, summarizes the extracted features in a clear and concise manner.

The `biopixR` package includes functions for analyzing entire directories,
allowing for high-throughput analysis. Making feature extraction and image
clustering easily accessible:

* `haralickCluster()`, extracts Haralick features and cluster using PAM (Partitioning Around Medoids) [@Haralick_1973; @radiomics; @cluster],
* `scanDir()`, utilizing the pipline for whole directory analysis (under development).

The `fillLineGaps()` algorthim, along with helper functions:

* `interpolatePixels()`, calculates the coordinates required to connect two given points,
* `adaptiveInterpolation()`, searches a given radius surrounding a line end for contours and connects them,

addresses the issue of discontinuous edges by iteratively scanning for line ends
within the image and reconnecting them to adjacent contours.

Examples demonstrating the use of @biopixR for image analysis tasks can be found
in the package's vignette.

## Graphical User Interface:

The function `interactive_objectDetection()` initiates a graphical user
interface (GUI) that utilizes the Tcl/Tk framework [@tcltk], enabling users to
adjust the threshold and smoothing settings of the image
(\\autoref{fig:fig\_1}).

![Graphical user interface for interactive parameter selection. A) The function `interactive_objectDetection()` provides a user-friendly interface with sliders to adjust threshold, smoothing, and scale. It highlights object contours in purple and centers in green for easy visualization. In this example the GUI was used in `RStudio` (2023.09.0+463, Linux, Ubuntu 22.04.3 LTS, [@RStudio]). B) The rendering process is displayed on the console, including timestamps and the current state.\label{fig:fig_1}](fig_1.png)

# Current status and outlook

The `biopixR` package was released on 
CRAN in March 2024. To ensure code quality, we employed various methodologies, 
including [Continuous Integration](https://github.com/Brauckhoff/biopixR/blob/main/.github/workflows/R-CMD-check.yml)
(CI), unit testing [@testthat], adherence to naming conventions
[@Rasmus_2012], and the application of style guidelines [@Wickham_2019].
Although the package is relatively new, we are working to expand its features
and evaluate its applicability using empirical research data from diverse
sources. In addition, future developments will involve expanding its
capabilities to identify DNA damage, particularly in the form of foci.

# Acknowledgments



# Funding



# References

