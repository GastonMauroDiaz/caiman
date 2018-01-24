
<!-- README.md is generated from README.Rmd. Please edit that file -->
caiman
======

CAnopy IMage ANalysis
---------------------

### Description

Caiman uses the functionalities of the packages *raster* and *EBImage* to provide algorithms specially developed to process photographs of the plant canopy.

Canopy structure can be estimated using gap fraction (GF) data, which can be directly measured with hemispherical photography. However, GF data accuracy is affected by sunlit canopy, multiple scattering, vignetting, blooming, and chromatic aberration. The caiman package implement the algorithm presented in [IEEE Geoscience and Remote Sensing Letters](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=7103294), whose aim is to reduce errors in the extraction of Gap Fraction data. The target errors are those that are produced by sunlit canopy, multiple scattering, vignetting, blooming, and chromatic aberration.

On the other hand, we develop *extractSkyBrightness* and *modelBasedThresholding* to obtain unbiased estimation of GF with high angle resolution in carefully acquired hemispherical photographs. This development are currently being reviewed as part of a submitted scientific paper. The documentation for these methods are under development.

### How to use caiman

------------------------------------------------------------------------

The main function of *caiman* package is *doOBIA*. Use it to get binarized images whit the aim of extract gap fraction and estimate forest structure or understory light transmission. After binarization, you will need to continue the proccess with specific software as [CIMES](http://jmnw.free.fr/).

This document shows you how to use a simple script for processing all the hemispherical photographs of an input directory with doOBIA (batch processing).

If you are a frequently user of R, probably you do not need to read this vignette.

#### Script for batch processing

I assume that you take upward looking hemispherical photographs, whit the optical axis aligned with the zenith (leveled). Also, I assume that the equipment gives you a circular picture inside the rectangular frame (i.e., is not a full-frame equipment) in JPEG format.

In the root of your *c* disk create a folder named *folder\_in* and copy your photographs there. Also, create a folder named *folder\_out*.

Run this code:

    path_in <- "c:/folder_in" # Please edit it (Do not use / at the end of the string)
    path_out <- "c:/folder_out"

    listofpics <- list.files(path_in, pattern = ".jpg", ignore.case = TRUE)

    for (i in unique(listofpics)) {
      # see the help of loadPhoto with ?loadPhoto. 
      # Maybe you need to use the arguments upperLeft, width and height.
      x <- loadPhoto(paste0(path_in, "/", i))
      fisheye(x) <- newFishEye(TRUE, TRUE, FALSE)
      x <- normalize(x, 0, 255)
      # Here, I assume that your lens has perfect polor projection.
      z <- makeZimage(ncol(x), lensPolyCoef())
      m <- doMask(z)
      bin <- autoThr(enhanceHP(x, m, sharpen = FALSE))
      # This takes a while but you can see the progres
      seg <- doPolarQtree(x, z, scaleParameter = 0.2)
      name <- strsplit(i, "\\.")[[1]][1]
      out <- doOBIA(x, bin, z, seg, zlim = asAngle(c(20, 80)))
      writeRaster(out * 255, paste0(path_out, "/", name, ".TIF"), datatype = "INT1U", overwrite = TRUE)
    }

Wait… The result will be written in *c:/folder\_out* whit the same name of the original photo.

### How to install caiman

------------------------------------------------------------------------

You can install *caiman* package in your system using *devtools* package from CRAN. Make sure you have *devtools* installed, if not run `install.packages("devtools")` in the console. Next, run `devtools::install_github("GastonMauroDiaz/caiman")`.

Probably, you do not have installed the dependent packages *EBImage* and *exif*. The former is a package in the BioConductor repository, follow the link to get full instruction about [how to install BioConductor packages](https://www.bioconductor.org/install/#install-bioconductor-packages) or run:

    source("https://bioconductor.org/biocLite.R")
    biocLite()
    biocLite(“EBImage”)

To install the *exif* package run `devtools::install_github("Ironholds/exif")`.
