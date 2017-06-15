.automatic_selection_of_samples <- function(blueValues, index, sampleSize) {

  blueBrightness <- tapply(blueValues, index, mean)
  rows <- as.numeric(names(blueBrightness))
  rows <- rows[order(blueBrightness)]

  if (length(rows) < (sampleSize * 5)) {
     stop("You need to decrease the scaleParameter used to produce the argument seg.")
  }


  center <- round(length(rows) / 2)
  rows.mix <- rows[seq(from = center  - sampleSize / 2,
    to = center  + sampleSize / 2, by = 1)]
  rows.sky <- rows[seq(from = length(rows) - sampleSize,
    to = length(rows), by = 1)]
  rows.test <- rows[is.na(match(rows, c(rows.mix, rows.sky)))]

  return(list(mix=rows.mix, sky=rows.sky, test=rows.test))
}

.sample_based_multiTexture_classification <-
  function(segmentation, blue, red, samples, k) {

    features <- cbind(
      EBImage::computeFeatures.haralick(segmentation, blue),
      EBImage::computeFeatures.haralick(segmentation, red)
    )
    train <- rbind(features[samples$sky, ], features[samples$mix, ])
    cl <- c(rep("sky", length(samples$sky)), rep("mix", length(samples$mix)))
    test <- features[samples$test, ]
    pred.knn <- class::knn(train, test, cl, k=k)
    return(list(pred.knn = pred.knn, cl = cl))
  }


#' @title Do an Object-based image analysis to classify plant.
#'
#' @description Do an Object-based image analysis with the aim of classify
#'   plants in full-color-upward-looking hemispherical photographs.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param z \code{\linkS4class{ZenithImage}}.
#' @param seg \code{\linkS4class{PolarSegmentation}}.
#' @param g1 \code{\linkS4class{PolarSegmentation}}. The default option is a
#'   PolarSegmentation created by calling \code{makePolarGrid(z)}. To save time in
#'   batch processing of photos token with the same camera, you can compute
#'   \code{makePolarGrid(z)} only once and provide the result through this argument.
#' @param sampleSize integer. Default is \code{50}, see Details.
#' @param k integer. Default is \code{1} nearest neighbor, see Details.
#' @param zlim \code{\linkS4class{Angle}}. Defaults are \code{30} to \code{60} degrees of
#'   zenith angle, see Details.
#' @param calibration logical. Default is \code{FALSE}, see Details.
#'
#' @details This algorithm uses color transformations, fuzzy logic, and
#'   object-based image analysis (OBIA). Internally, this algorithm makes a call
#'   to \code{\link{enhanceHemiPhoto}} which result is binarized calling
#'   \code{\link{autoThr}}. The class \emph{Gap-candidate} is assigned to pixels
#'   above the threshold and the class \emph{Plant} to the rest of the
#'   unclassified pixels. Next, the algorithm uses this result and \code{g1} to
#'   isolate hemisphere segments with \code{1} degree of resolution that are not
#'   fully cover by \emph{Plant} (i.e., Gap Fraction > 0), which are classified
#'   as \emph{Mix-OR-Gap}. Next, the algorithm get a binary mask from this
#'   result and intersect it with the argument \code{seg}. At this point, the
#'   algorithms achieve the identification of all segments of \code{seg} that
#'   could have some gaps at pixel level (i.e., \emph{Mix-OR-Gap}). Next, the
#'   algorithm classified all this segments in \emph{Gap} or \emph{Mix} in a two
#'   stage process: (1) automatic selection of samples and (2) sample-based
#'   classification. The argument \code{sampleSize} controls the sample size for
#'   both targeted classes. The algorithm uses the brightness of the blue
#'   channel to select the samples. It assumes that brighter objects belong to
#'   \emph{Gap} and objects with middle brightness belong to \emph{Mix}. The
#'   argument \code{k} is for the knn used in the second stage of the
#'   sample-based classification. Processing continues on Mix-segments in order
#'   to unmix them at pixel level (see references for more details). The
#'   arguments \code{mnZ} and \code{mxZ} can be used to delimitate the range of
#'   zenith angle in which the aforementioned process is computed. In the rest
#'   of the image the result will be the same as using \code{\link{autoThr}} to
#'   binarize the result of calling \code{\link{enhanceHemiPhoto}}.
#'
#'   If calibrate is set as \code{TRUE}, the process stops just after the
#'   sample-based classification described in the previous paragraph and returns
#'   a classification at object level of \emph{Plan}, \emph{Mix} and \emph{Gap}.
#'   This kind of output can be used to calibrate \code{sampleSize} and
#'   \code{k}.
#'
#' @return A \code{\linkS4class{BinImage}} by default. If \code{calibrate} is
#'   set to \code{TRUE}, a \code{\linkS4class{RasterLayer}}.
#'
#' @references Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction
#' Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett.
#' 12, 1784-1789.
#'
#' @seealso \code{\link{loadPhoto}}, \code{\link{doPolarQtree}}, \code{\link{makeZimage}}.
#'
#' @example /inst/examples/doOBIAexample.R
#'
setGeneric("doOBIA",
  function(x, z, seg = doPolarQtree(x, z, scaleParameter = 0.2),
    g1 = makePolarGrid(z), sampleSize = 50, k = 1, zlim = asAngle(c(30, 60)),
       calibration = FALSE) standardGeneric("doOBIA"))
#' @export doOBIA

#' @rdname doOBIA
setMethod("doOBIA",
  signature(x = "CanopyPhoto"),
  function (x, z, seg, g1, sampleSize, k, zlim, calibration) {

    msn <- "This algorithm was designed to process upward looking hemispherical photographs."
    if (!fisheye(x)@is) {
      warning(msn)
    } else {
      if (!fisheye(x)@up) warning(msn)
      # if (fisheye(x)@fullframe) warning(msn)
    }
stop("this need work")
    isFullframe <-

    if (x@fisheye@fullframe) {
      bin <- autoThr(enhanceHemiPhoto(x))
      x <- expandFullframe(x)
    } else {
      bin <- autoThr(enhanceHemiPhoto(x))
    }

    stopifnot(compareRaster(x, z))
    stopifnot(compareRaster(x, seg))
    stopifnot(compareRaster(x, g1))

    stopifnot(is(zlim) == "Angle")

    if (!zlim@degrees) zlim <- switchUnit(zlim)

    stopifnot(length(zlim@values) == 2)

    mnZ <- zlim@values[1]
    mxZ <- zlim@values[2]

    stopifnot(mnZ < mxZ)

    stopifnot(max(getMax(x)) <= 1)
    stopifnot(min(getMin(x)) >= 0)

    g1[z > mxZ | z < mnZ]  <- NA

    ## step 3

    toclass <- presetThr(getFeatureImage(bin, g1, mean), 0)
    rm(g1)

    ## step 5

    toclass <- seg * toclass
    rm(seg)
    toclass[z > mxZ | z < mnZ]  <- NA
    rm(z)
    toclass[toclass == 0]  <- NA

    samples <- .automatic_selection_of_samples(
      values(raster::subset(x, "Blue")),
      values(toclass),
      sampleSize
    )

    toclass[is.na(toclass)] <- 0

    mtClass <- .sample_based_multiTexture_classification(
      as.matrix(toclass),
      as.matrix(raster::subset(x, "Blue")),
      as.matrix(raster::subset(x, "Red")),
      samples,
      k
    )

    df <- data.frame(
      c(samples$sky, samples$mix, samples$test),
      c(as.factor(mtClass$cl), mtClass$pred.knn)
    )
    df <- df[!duplicated(df[, 1]), ]

    rhara <- subs(toclass, df)

    rhara[rhara > 2] <- 1
    if (calibration) {
      rhara[is.na(rhara)] <- 0
      rhara[z > mxZ | z < mnZ]  <- NA
      rhara[is.na(z)] <- NA
      rtitle(rhara) <- "Sample-based classification at object level"
      return(rhara)
    } else {
      ## step 6 and 7
      rL <- raster(sRGB2LAB(x), 1)
      rm(x)
      toclass[!(bin == 1 & rhara == 1 & rL <= 95)] <- NA
      toclass[toclass == 0] <- NA

      ## step 8
      ## Feature calculation
      toclass <- as(toclass, "PolarSegmentation")
      rL <- rL / getFeatureImage(rL, toclass, mean)
      rL[is.infinite(rL)] <- NA
      ## Regional thresholding with standarized Lightness feature
      rbin <- autoThr(rL)
      rm(rL)
      rbin[rbin == 1] <- NA
      bin <- cover(rbin, bin)
      rm(rbin)
      rtitle(bin) <- "OBIA output"
      bin@data@names <- "Plant"
      bin <- as(bin, "BinImage")
      return(bin)
    }
  }
)
