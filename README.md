# beadR - Package for analysis of bioimage image data

## Introduction
In the rapidly evolving landscape of scientific research and technology, the field of image analysis and processing has become indispensable, providing unprecedented insight into complex phenomena. This project aims to utilize advanced image processing techniques, using the R programming language, to analyze and characterize bead microparticles. Bead microparticles find extensive utility in various scientific domains, including medical diagnostics, environmental monitoring, and materials science.

The rise of high-resolution imaging technologies has led to an exponential increase in the volume and complexity of image data, requiring sophisticated computational methods for efficient analysis. In this context, R is a standout programming language that excels in statistical computing and graphics. Leveraging the rich ecosystem of R packages, this project aims to develop an automated image analysis pipeline for bead microparticles. 

**The primary objectives of this project include**:
 - *Image Preprocessing*: Implementing techniques to fill and reconnect discontinuous lines and edges. Discard coagulated duplets, multiplets, and beads that are too close together as they may excite each other and produce a false positive signal.
 - *Segmentation and Feature Extraction*: Applying algorithms to accurately identify and segment sperical objects (e.g, microparticles). Extracting relevant features such as quantity, size, and intensity for comprehensive characterization. 
 - *Visualization*: Implementing interactive tools with tcltk for the selection of thresholds and smoothing factors. Generating images to control the appropriate functioning of algorithms, for example the reconnection of lines.
 - *Automatization*: Combining algorithms for high-throughput analysis of image data.

This project aims to meet the immediate need for effective bead microparticle analysis and contribute to the broader field of image processing methodologies in the R programming environment. By providing a comprehensive and adaptable framework, the work empowers researchers and practitioners to extract meaningful insights from image data, thus enhancing our understanding of bead microparticles and their diverse applications.


## Examples

*in progress*


## Installation
```{r}
install.package("devtools")
devtools::install_github("Brauckhoff/beadR")
```

# Related Research

Reimann, R., Zeng, B., Jakopec, M., Burdukiewicz, M., Petrick, I., Schierack, P., Rödiger, S., 2020. Classification of dead and living microalgae Chlorella vulgaris by bioimage informatics and machine learning. Algal Research 48, 101908. https://doi.org/10.1016/j.algal.2020.101908
Schneider, J., Weiss, R., Ruhe, M., Jung, T., Roggenbuck, D., Stohwasser, R., Schierack, P., Rödiger, S., 2019. Open source bioimage informatics tools for the analysis of DNA damage and associated biomarkers. Journal of Laboratory and Precision Medicine 4, 1–27. https://doi.org/10.21037/jlpm.2019.04.05
Weiss, R., Karimijafarbigloo, S., Roggenbuck, D., Rödiger, S., 2022. Applications of Neural Networks in Biomedical Data Analysis. Biomedicines 10, 1469. https://doi.org/10.3390/biomedicines10071469
Rödiger, S., Böhm, A., Schimke, I., 2013. Surface Melting Curve Analysis with R. The R Journal 5, 37–53. https://doi.org/10.32614/RJ-2013-024
Rödiger, S., Liebsch, C., Schmidt, C., Lehmann, W., Resch-Genger, U., Schedler, U., Schierack, P., 2014. Nucleic acid detection based on the use of microbeads: a review. Microchim Acta 181, 1151–1168. https://doi.org/10.1007/s00604-014-1243-4
Rödiger, S., Schierack, P., Böhm, A., Nitschke, J., Berger, I., Frömmel, U., Schmidt, C., Ruhland, M., Schimke, I., Roggenbuck, D., Lehmann, W., Schröder, C., 2013. A highly versatile microscope imaging technology platform for the multiplex real-time detection of biomolecules and autoimmune antibodies. Adv Biochem Eng Biotechnol 133, 35–74. https://doi.org/10.1007/10_2011_132

