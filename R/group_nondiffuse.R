#### colorfulness ####

#' @title Quantify the colorfulness of an image.
#'
#' @description Quantify the colorfulness of a sRGB image using a bidimensional
#'   space form by the green/red and the blue/yellow axes of the CIE
#'   \emph{L*a*b*} space, in other words, the lightness is excluded. In this
#'   bidimensional space, the algorithm creates a square with sides of 200
#'   and centered in 0. Next, it compute how much area of this square is covered
#'   by the image pixels. The index is the percentage of cover.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}. Is the image that you want to know
#'   how colorful is.
#' @param mask \linkS4class{BinImage}. The default value \code{NULL} means that
#'   all the pixels will be taking into account in the computations. If you
#'   provide a \linkS4class{BinImage}, it must have the same extent and
#'   resolution of \code{x}. All pixels from the image covered by pixels of the
#'   mask with value \code{1} will be taking into account in the computations.
#' @param plot logical. By default it is \code{FALSE}, if \code{TRUE} a plot
#'   will be printed. See Details.
#' @param returnRaster logical. By default it is \code{FALSE}, if \code{TRUE}
#'   this method returns the raster that you can see when plot is \code{TRUE}.
#'   See Details.
#'
#' @details When you request a plot with argument \code{plot = TRUE}, the
#'   colorfulness is returned but also a plot is sent to the active graphics
#'   Device. The plot shows the color of the image represented in a
#'   bidimensional space made by the axis \emph{a} and \emph{b} of the CIE
#'   \emph{L*a*b* space}.
#'
#' @return numeric or \code{\linkS4class{Raster}}.
#'
#' @seealso \code{\link{normalize}}, \code{\link{outOfDR}}.
#'
#' @example /inst/examples/colorfulnessExample.R
#'
setGeneric("colorfulness",
           function(x, mask = NULL, plot = FALSE, returnRaster = FALSE)
             standardGeneric("colorfulness"))
#' @export colorfulness

#' @rdname colorfulness
setMethod("colorfulness",
          signature(x = "CanopyPhoto"),
          function (x, mask, plot, returnRaster)
          {

            stopifnot(is.logical(plot))
            stopifnot(all(getMax(x) <= 1))
            stopifnot(all(getMin(x) >= 0))


            if (!is.null(mask)) {
              stopifnot(compareRaster(x, mask) == TRUE)
              stopifnot(any(is(mask) == "BinImage"))
              x[mask == 0] <- 0
            }

            #x <- sRGB2LAB(x)

            fun <- function(x) {
              # colorspace::hex(colorspace::LAB(x[, 1], x[, 2], x[, 3]))
              colorspace::hex(colorspace::sRGB(x[, 1], x[, 2], x[, 3]))
            }

            bs <- raster::blockSize(x)
            hexs <- c()
            for (i in 1:bs$n) {
              hexs <- c(hexs, levels(as.factor(fun(x[bs$row[i]:bs$nrow[i], ]))))
            }
            rm(x)
            hexs <- levels(as.factor(hexs))
            rgb <- colorspace::hex2RGB(hexs)
            lab <- as(rgb, "LAB")
            lab <- colorspace::coords(lab)
            rgb <- colorspace::coords(rgb)
            lab <- sp::SpatialPointsDataFrame(lab[,2:3], data = data.frame(rgb))
            rm(rgb)

            r <- raster(nrows = 200, ncols = 200,
                        xmn = -100, xmx = 100,
                        ymn = -100, ymx = 100
            )

            r <- rasterize(lab, r)
            rm(lab)

            r <- raster::subset(r, 2:4)

            if (plot) {
              plot(
                raster::extent(r),
                col = 0,
                main = "",
                xlab="a", ylab="b"
              )
              plotRGB(r*255, add = TRUE)
            }

            if (!returnRaster) {
              r <- raster::subset(r, 1)
              r <- is.na(r)
              f <-  raster::freq(r, value = 0) / length(r)
              f * 100
            } else {
              r * 255
            }

          }
)



#### membership2color ####

.gaussian2d <- function(x, y, targetA, targetB, sigma) {
  stats::dnorm(x, targetA, sigma) * stats::dnorm(y, targetB, sigma)
}

.getGaussian2dParameters <- function(targetColor, sigma) {
  if (class(targetColor) != "LAB") targetColor <- as(targetColor, "LAB")

  ma <- colorspace::coords(targetColor)
  targetA <- ma[, 2]
  targetB <- ma[, 3]

  if (is.null(sigma)) sigma <- unname(sqrt(targetA ^ 2 + targetB ^ 2))

  x <- c(targetA, targetB, sigma)
  names(x) <- c("targetA", "targetB", "sigma")
  return(x)
}

#' @title Compute the membership value to a color.
#'
#' @description This algorithm models the degree of membership to a color with
#'   two Gaussian membership functions and the layer \emph{A} and \emph{B} of the \emph{CIE L*a*b*}
#'   color space. The lightness information is omitted.
#'
#' @param x \linkS4class{color} or \code{\linkS4class{RasterStack}} or \code{\linkS4class{RasterBrick}}.
#' @param targetColor \linkS4class{color}.
#' @param sigma numeric. Default \code{NULL} means the algorithms will estimate it.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return Numeric or \code{\linkS4class{RasterLayer}}.
#'
#' @seealso \code{\link{normalize}}, \code{\link{enhanceHP}}.
#'
#' @example /inst/examples/enhanceHPExample.R
#'
setGeneric("membership2color", function(x, ...) standardGeneric("membership2color"))
#' @export membership2color

#' @rdname membership2color
setMethod("membership2color",
          signature(x = "color"),
          function(x, targetColor, sigma = NULL, ...) {

            error_msn <- "'targetColor' must be a subclass of the virtual class named 'color' from 'colorspace' package"
            if (!isS4(targetColor)) stop(error_msn)
            if (attr(class(targetColor), "package") != "colorspace") stop(error_msn)

            if (class(x) != "LAB") x <- as(x, "LAB")

            p <- .getGaussian2dParameters(targetColor, sigma)

            max.z <- .gaussian2d(p[1], p[2], p[1], p[2], p[3])
            x <- colorspace::coords(x)
            m <- .gaussian2d(x[, 2], x[, 3], p[1], p[2], p[3]) / max.z
            m <- unname(m)

            attr(m, "sigma") <- p[3]
            attr(m, "targetColor") <- targetColor

            return(m)
          }
)

#' @rdname membership2color
setMethod("membership2color",
          signature(x = "RasterStackBrick"),
          function (x, targetColor, sigma = NULL, ...)
          {
            #m <- raster::raster(raster::subset(x, 1))
            stopifnot(all(getMax(x) <= 1))
            stopifnot(all(getMin(x) >= 0))

            foo <- function(x, ...) {
              x <- colorspace::sRGB(x)
              return(membership2color(x, targetColor, sigma))
            }
            fun <- .makeF8single(foo, ...)

            out <- fun(x, sigma = sigma, ...)

            tC <- colorspace::coords(targetColor)[1,]

            rtitle(out) <-
              paste0("Membership to color sRGB(", tC[1], ", ", tC[2], ", ", tC[3], ")")

            return(out)
          }
)

#### fuzzyLightness ####
#' @title Compute a weighted membership value to lightness.
#'
#' @description This algorithm uses a threshold value as the location parameter
#'   of a logistic membership function whose scale parameter depends on a
#'   variable. This dependence can be explained as follows: if the variable is
#'   equal to \code{1}, then the membership function is as a threshold function
#'   because the scale parameter is \code{0}; lowering the variable increases
#'   the scale parameter, thus blurring the threshold because it decreases the
#'   steepness of the curve.
#'
#' @param x numeric. The lightness value.
#' @param m numeric lying between \code{0} and \code{1}, same length as
#'   \code{x}. It is the scale parameter of the logistic membership function.
#' @param thr numeric of length \code{1}. Location parameter of the logistic
#'   membership function.
#' @param fuzziness numeric of length \code{1}.
#'
#' @return \code{numeric.}
#'
#' @seealso  \code{\link[stats]{plogis}}.
#'
#' @example /inst/examples/fuzzyLightnessExample.R
#'
#' @references Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction
#' Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett.
#' 12, 1784-1789.
#'
setGeneric("fuzzyLightness", function(x, m, thr, fuzziness) standardGeneric("fuzzyLightness"))
#' @export fuzzyLightness

#' @rdname fuzzyLightness
setMethod("fuzzyLightness",
          signature(x = "numeric"),
          function (x, m, thr, fuzziness)
          {
            stopifnot(length(thr) == 1)
            stopifnot(length(fuzziness) == 1)
            stopifnot(length(x) == length(m))
            stats::plogis(x, thr, fuzziness * (1 - m))
          }
)

#### enhanceHP ####
.relativeBrightness <- function(x, wR, wB) {
  ((x[, 1] * wR + x[, 3] * wB) / 2) * x[, 4] + x[, 3] * (1 - x[, 4])
}

#' @title Enhance upward looking hemispherical photographs.
#'
#' @description This algorithm uses the color perceptual attributes to enhance
#'   the contrast between the sky and plants through fuzzy classification. Color
#'   has three different perceptual attributes: hue, lightness, and chroma. The
#'   algorithm was developed using the following premise: the color of the sky
#'   is different from the color of plants. It performs the next classification
#'   rules, here expressed in natural language: clear sky is blue and clouds
#'   decrease its chroma; if clouds are highly dense, then the sky is
#'   achromatic, and, in such cases, it can be light or dark; everything that
#'   does not match this description is not sky. These linguistic rules were
#'   translated to math language by means of fuzzy logic.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param mask \code{\linkS4class{BinImage}}.
#' @param wR numeric. Weight of red layer. See details.
#' @param wB numeric. Weight of blue layer. See details.
#' @param sharpen logical. The default is TRUE, see details.
#' @param skyBlue \linkS4class{color}. See details.
#' @param thr numeric. By default, the algorithm will try to estimate it.
#' @param fuzziness numeric. By default, the algorithm will try to estimate it.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @details This is a pixelwise algorithm that evaluates if pixels are sky blue
#'   colored. High score means high membership to Sky Blue. When a pixel are
#'   achromatic, then it uses pixel brightness. The algorithm internally uses
#'   \code{\link{membership2color}} and \code{\link{fuzzyLightness}}. The
#'   argument skyBlue is the \code{targetColor} of the former function, which
#'   output is the argument \code{m} of the latter function. To evaluate the
#'   brightness of an achromatic pixel, the algorithm uses \strong{Relative
#'   Brightness} (see references).
#'
#'   Argument \code{mask} can be used to affect the estimation of two arguments
#'   of \code{\link{fuzzyLightness}}. Affected arguments are \code{thr} and
#'   \code{fuzziness}. The function \code{\link{autoThr}} is used to estimate
#'   \code{thr}. To compute \code{fuzziness}, the algorithm takes the maximum
#'   and the minimum values of the Relative Brightness and calculate its mean.
#'
#'   If sharpen is set as \code{TRUE}, a sharpen filter is applied to
#'   the raster with the membership values. This kernel is used:
#'   \code{matrix(c(rep(-1, 3), -1, 12, -1, rep(-1, 3)), ncol = 3)}.
#'
#'
#' @references Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction
#' Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett.
#' 12, 1784-1789.
#'
#' @return numeric or \linkS4class{RasterLayer}.
#'
#' @seealso \code{\link{membership2color}}, \code{\link{fuzzyLightness}}.
#'
#' @example /inst/examples/enhanceHPExample.R
#'
setGeneric("enhanceHP", function(x, ...)
  standardGeneric("enhanceHP"))
#' @export enhanceHP

#' @export enhanceHemiPhoto
#' @rdname enhanceHP
enhanceHemiPhoto <- function(x, ...) print("use enhanceHP")

#' @describeIn enhanceHP The output is a numeric vector.
setMethod("enhanceHP",
          signature(x = "matrix"),
          function (x, thr = NULL, fuzziness = NULL, wR = 0.5, wB = 1.5,
                    skyBlue = colorspace::sRGB(matrix(normalize(c(135, 206, 235), 0, 255),
                                                      ncol = 3)), ...)
          {

            stopifnot(max(x, na.rm = TRUE) <= 1)
            stopifnot(min(x, na.rm = TRUE) >= 0)
            if (ncol(x) != 4) stop("x must be a matrix with 4 columns.")

            mask <- x[, 4]
            x <- x[, 1:3]

            mSkyBlue <- membership2color(colorspace::sRGB(x), skyBlue)
            mSkyBlue <- as.numeric(mSkyBlue)
            y <- sRGB2LAB(x)

            reBr <- .relativeBrightness(cbind(x, mSkyBlue), wR, wB)

            if(length(reBr) > 1) {
              if(is.null(thr)) thr <- autoThr(reBr[as.logical(mask)])
              if(is.null(fuzziness)) fuzziness <- (max(reBr) - min(reBr)) / 2
            } else {
              stop("Please provide thr and fuzziness.")
            }

            mLight <- fuzzyLightness(reBr, mSkyBlue, thr, fuzziness)

            out <- mSkyBlue * mLight

            return(out)
          }
)

#' @describeIn enhanceHP The output is a \linkS4class{RasterLayer}.
setMethod("enhanceHP",
          signature(x = "CanopyPhoto"),
          function (x, mask = NULL, wR = 0.5, wB = 1.5, sharpen = FALSE,
                    skyBlue = colorspace::sRGB(matrix(normalize(c(135, 206, 235), 0, 255),
                                                      ncol = 3)), ...)
          {

            stopifnot(all(getMax(x) <= 1))
            stopifnot(all(getMin(x) >= 0))


            if (is.null(mask)) {
              mask <- raster(x)
              mask[] <- 1
            }

            x <- as(x, "RasterBrick")

            if (!compareRaster(x, mask, stopiffalse = FALSE))
              stop("x should match pixel by pixel whit mask.")

            x <- stack(x, mask)

            reBr <- .relativeBrightness(values(x), wR, wB)

            thr <-  autoThr(reBr[as.logical(values(mask))])
            fuzziness <- (max(reBr[as.logical(values(mask))]) -
                            min(reBr[as.logical(values(mask))])) / 2

            foo <- function(x, ...) enhanceHP(x, thr=thr, fuzziness=fuzziness)

            fun <- .makeF8single(foo, ...)

            out <- fun(x, ...)

            if (sharpen) {
              out <- focal(out,
                           w = matrix(c(rep(-1, 3), -1, 12, -1, rep(-1, 3)), ncol = 3),
                           pad = TRUE, padValue = 0)
              out <- normalize(out, getMin(out), getMax(out))
            }

            rtitle(out) <- "Enhanced hemispherical photograph"
            return(out)
          }
)


#### doOBIA ####
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


#' @title Do an Object-based image analysis to classify gaps.
#'
#' @description Do an Object-based image analysis with the aim of classify
#'   gaps in full-color-upward-looking hemispherical photographs.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param bin \code{\linkS4class{BinImage}}. The standard is a call to
#'   \code{\link{enhanceHP}} followed by a call to \code{\link{autoThr}}
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
#' @details This algorithm uses object-based image analysis (OBIA). The class
#'   \emph{Gap-candidate} is assigned to pixels that are white in \code{bin} and
#'   the class \emph{Plant} to the rest of the pixels. Next, the algorithm uses
#'   this result and \code{g1} to isolate hemisphere segments with \code{1}
#'   degree of resolution that are not fully cover by \emph{Plant} (i.e., Gap
#'   Fraction > 0), which are classified as \emph{Mix-OR-Gap}. Next, the
#'   algorithm get a binary mask from this result and intersect it with the
#'   argument \code{seg}. At this point, the algorithms achieve the
#'   identification of all segments of \code{seg} that could have some gaps at
#'   pixel level (i.e., \emph{Mix-OR-Gap}). Next, the algorithm classified all
#'   this segments in \emph{Gap} or \emph{Mix} in a two stage process: (1)
#'   automatic selection of samples and (2) sample-based classification. The
#'   argument \code{sampleSize} controls the sample size for both targeted
#'   classes. The algorithm uses the brightness of the blue channel to select
#'   the samples. It assumes that brighter objects belong to \emph{Gap} and
#'   objects with middle brightness belong to \emph{Mix}. The argument \code{k}
#'   is for the knn used in the second stage of the sample-based classification.
#'   Processing continues on Mix-segments in order to unmix them at pixel level
#'   (see references for more details). The arguments \code{mnZ} and \code{mxZ}
#'   can be used to delimitate the range of zenith angle in which the
#'   aforementioned process is computed. In the rest of the image the result
#'   will be the same as \code{bin}.
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
  function(x, bin, z, seg = doPolarQtree(x, z, scaleParameter = 0.2),
    g1 = makePolarGrid(z), sampleSize = 50, k = 1, zlim = asAngle(c(30, 60)),
       calibration = FALSE) standardGeneric("doOBIA"))
#' @export doOBIA

#' @rdname doOBIA
setMethod("doOBIA",
  signature(x = "CanopyPhoto"),
  function (x, bin, z, seg, g1, sampleSize, k, zlim, calibration) {

    msn <- "This algorithm was designed to process upward looking hemispherical photographs."
    if (!fisheye(x)@is) {
      warning(msn)
    } else {
      if (!fisheye(x)@up) warning(msn)
      # if (fisheye(x)@fullframe) warning(msn)
    }

    # if (x@fisheye@fullframe) {
    #   bin <- autoThr(enhanceHP(x, z = z))
    #   x <- expandFullframe(x, z)
    # } else {
    #   bin <- autoThr(enhanceHP(x))
    # }

    stopifnot(class(bin) == "BinImage")

    stopifnot(compareRaster(x, bin))
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


#### adaptive_binarization ####

#' @title Adaptive binarization
#'
#' @description todo
#'
#' @param cp \code{\linkS4class{CanopyPhoto}}. todo
#' @param m \linkS4class{BinImage}. todo
#' @param thr4only_catching_plants numerical.
#'
#' @details todo
#'
#' @return numeric or \code{\linkS4class{Raster}}.
#'
#' @seealso \code{\link{autoThr}}, \code{\link{normalize}}.
#'
#'
setGeneric("adaptive_binarization",
           function(cp, m = NULL, thr4only_catching_plants = 0.2)
             standardGeneric("adaptive_binarization"))
#' @export adaptive_binarization

#' @rdname adaptive_binarization
setMethod("adaptive_binarization",
          signature(cp = "CanopyPhoto"),
          function(cp, m, thr4only_catching_plants) {

            is_any_plant_here <- function(x, thr = 0.2) {
              x <- raster::aggregate(x, 2, mean)
              any(x[] < thr)
            }


            cover_percentage <- function(x) {
              #This is the cover percentage from the point of view of the
              #sensor. Therefore, it is ok to not correct lens distortion.
              total <- ncell(x) - freq(is.na(x), value = 1)
              (freq(x, value = 1) / total) * 100
            }

            x <- cp

            if (!is.null(m)) {x[!m] <- NA}

            if (is_any_plant_here(x$Blue, thr4only_catching_plants)) {

              # create a broad mask to roughly know how much unobscored sky
              # is seen in the picture
              foo <- raster::aggregate(x$Blue, 4)
              foo <- foo < thr4only_catching_plants
              w <- round(sum(foo[!is.na(foo)]) / 27857.11) # empirical parameter
              if (round(w/2) == w/2) w <- w + 1
              if (w < 3) w <- 3
              foo <- focal(foo, matrix(1, w, w), pad = TRUE)
              m <- foo != 0
              m <- disaggregate(m, 4, "")
              m[is.na(m)] <- 0
              m <- resample(m, x$Blue, "ngb")
              compareRaster(m, x$Blue)



              if (cover_percentage(m) < 25) {
                # When the photo is taken with auto-exposure under an open canopy, the
                # auto-exposure system adjusted the exposure to produce a middle grey
                # image (considering the full picture). Because a high percentage of the
                # sky was visible in the scene, auto-exposure produced a middle grey sky
                # (considering only value information). A middle gray sky can be easily
                # classified as plant because the algorithm is expecting a light sky.
                # There are two very different scenarios: (1) blue skies, and (2) overcast
                # skies. Broken clouds skies, of course, fall into the middle.  To known
                # were a given photo falls, this function uses colorfulness(). The idea is
                # that blue skies produce colorfulness picture and the reverse is true
                # too.


                if (colorfulness(x) > 1) {
                  # If we are here, the photo was taken under the open sky on a day with a
                  # blue sky. This kind of photo has sunlit canopy elements, so it is
                  # better to use the color information because there is a lot of info in
                  # that domain. Therefore, I use only the color (hue) information. First,
                  # I extract the data as a matrix so I can access the fuzyness parameters
                  # (I need to modify the code of enhanceHP to make this easier)

                  ma <- x[]

                  ma <- cbind(ma, 1) # a fake mask is added
                  ma <- enhanceHP(ma, 0.5, 1)
                  xe <- x$Red
                  xe[] <- ma
                  plot(xe)
                  xe[!m] <- NA
                  foo <- autoThr(xe)
                  xe[] <- ma
                  x <- presetThr(xe, foo@threshold/2)


                } else {
                  # If we are here, the photo was taken under the open sky on an overcast
                  # day. In this scenario, the diffuse light is high but is very likely
                  # that the canopy element will be darker than the sky. However, there
                  # are a lot of sky pixels, so autoThr() produce a wrong thr. The
                  # solution is to use the rough mask. On the other hand, enhanceHP is not
                  # needed because there is no color info here.

                  foo <- x$Blue
                  foo[!m] <- NA
                  plot(foo)
                  foo <- autoThr(foo)
                  x <- presetThr(x$Blue, foo@threshold/2)

                }


              } else{
                # This is when a lot of the canopy is seen in the photo.
                # Generally, it works fine.
                x <- enhanceHP(x, mask = m)
                x <- autoThr(x)
              }

            } else {
              x[] <- 1
              x <- presetThr(x$Blue, 0.5)
              x@threshold <- NA
            }

            return(x)

          }
)


