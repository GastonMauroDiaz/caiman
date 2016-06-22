#####################
## ** presetThr ** ##
#####################

#' @title Binarize images with a preset threshold.
#'
#' @description Given a single layer image and a global threshold value, it returns a binarized image. All pixels above the threshold are replaced with \code{1} and the rest with \code{0}.
#'
#' @param x \code{\linkS4class{RasterLayer}}.
#' @param thr numeric. Threshold value. It should be between minimum and maximum layer value.
#' @inheritParams autoThr
#'
#' @return \code{\linkS4class{BinImage}}.
#'
#' @seealso \code{\link{autoThr}}.
#'
#' @example /inst/examples/presetThrExample.R
#'
setGeneric("presetThr", function(x, ...) standardGeneric("presetThr"))

#' @rdname presetThr
setMethod("presetThr",
  signature(x = "RasterLayer"),
  function (x, thr)
  {
    tmp <- values(x)
    if (thr < min(tmp, na.rm = TRUE)) stop("thr must be greater than or equal to minimum layer value")
    if (thr >= max(tmp, na.rm = TRUE)) stop("thr must be lower than maximum layer value")
    x[x > thr] <- NA
    x <- is.na(x)
    x <- as(x, "BinImage")
    x@threshold <- thr
    return(x)
  }
)
#' @export presetThr

###################
## ** autoThr ** ##
###################

#' @title Binarize images with an automatic threshold.
#'
#' @description Given a single layer image, a global threshold value is automatically obtained and a binarized image is returned. All pixels above the auto-threshold are replaced with \code{1} and the rest with \code{0}. On the other hand, only a threshold value is returned for numeric vector input.
#'
#' @param x numeric, \code{\linkS4class{CanopyPhoto}} or \code{\linkS4class{RasterLayer}}.
#' @param subset numeric or character. Should indicate the channel to process (represented as integer or by their name).
#' @param ... Additional arguments.
#'
#' @details Threshold value is obtained with the Ridler and Calvard (1978) method. The implementation is based on \href{http://fiji.sc/Auto_Threshold#IsoData}{the IsoData method of Auto Threshold ImageJ plugin by Gabriel Landini}.
#'
#' @references
#' Ridler, T., Calvard, S., 1978. Picture thresholding using an iterative selection method. IEEE Trans. Syst. Man Cybern. 8, 260-263.
#'
#' @return Numeric or \code{\linkS4class{BinImage}}.
#'
#' @seealso \code{\link{presetThr}}.
#'
#' @example /inst/examples/autoThrExample.R
#'
setGeneric("autoThr", function(x, ...) standardGeneric("autoThr"))
#' @export autoThr

#' @describeIn autoThr Require a numeric vector of length greater than one and standard deviation greater than zero. This method returns the threshold value.
setMethod("autoThr",
  signature(x = "numeric"),
  function (x)
  {
    if (length(x) <= 1) stop("length(x) must be greater than one.")
    if (stats::sd(x, na.rm = TRUE) == 0) stop("sd(x) must be greater than zero.")
    thr <- mean(x, na.rm = TRUE)
    thr.back <- 0
    while (thr != thr.back) {
      thr.back <- thr
      x0 <- x[x <= thr]
      x1 <- x[x > thr]
      thr <- (mean(x0, na.rm = TRUE) + mean(x1, na.rm = TRUE)) / 2
    }
    return(thr)
  }
)

#' @describeIn autoThr The argument \code{subset} will be passed to \code{\link[raster]{subset}} for selecting which channel of the \code{\linkS4class{CanopyPhoto}} will be processed. It computes the threshold of selected layer and return a \code{\linkS4class{BinImage}}.
setMethod("autoThr",
  signature(x = "CanopyPhoto"),
  function (x, subset = 3)
  {
    x <- raster::subset(x, subset)
    od <- x@file@name
    thr <- autoThr(values(x))
    x <- presetThr(x, thr)
    x@originalData <- od
    x@processedLayer <- subset
    validObject(x)
    return(x)
  }
)

#' @describeIn autoThr Compute threshold and return a \code{\linkS4class{BinImage}}.
setMethod("autoThr",
  signature(x = "RasterLayer"),
  function (x)
  {
    thr <- autoThr(values(x))
    x <- presetThr(x, thr)
    return(x)
  }
)

#####################
## ** normalize ** ##
#####################

normalize <- function(x, mn, mx, ...) (x - mn) / (mx - mn)

#' @title Normalize data in the range \code{0} to \code{1}.
#'
#' @description Normalize data lying between \code{mn} and \code{mx} in the range \code{0} to \code{1}. Data greater than mx get values greater than \code{1}, proportionally. Conversely, data less than \code{mn} get values less than \code{0}.
#'
#' @param x \linkS4class{CanopyPhoto} or \code{\linkS4class{Raster}}.
#' @param mn numeric. Minimum expected value.
#' @param mx numeric. Maximum expected value.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return \linkS4class{CanopyPhoto} or \linkS4class{Raster}
#'
#' @example /inst/examples/normalizeExample.R
#'
setGeneric("normalize", normalize)
#' @export normalize

#' @describeIn normalize Result is a \linkS4class{Raster} of the same type that \code{x}.
setMethod("normalize",
  signature(x = "Raster"),
  function(x, mn, mx, ...) {
    fun <- .makeF8multi(function(x,...) normalize(x, mn, mx), ...)
    return(fun(x, ...))
  }
)

#' @rdname normalize
setMethod("normalize",
  signature(x = "RasterLayer"),
  function(x, mn, mx, ...) {
    fun <- .makeF8single(function(x,...) normalize(x, mn, mx), ...)
    return(fun(x, ...))
  }
)

#' @describeIn normalize Result is a \linkS4class{CanopyPhoto}.
setMethod("normalize",
  signature(x = "CanopyPhoto"),
  function(x, mn, mx, ...) {
    out <- normalize(as(x, "RasterBrick"), mn, mx, ...)
    out <- as(out, "CanopyPhoto")
    out <- cloneSlots(x, out)
    return(out)
  }
)

####################
## ** sRGB2LAB ** ##
####################

#' @title Convert sRGB to LAB.
#'
#' @description Wrapper function of \code{\link[colorspace]{sRGB}} and \code{\link[colorspace]{LAB}} that convert colors from sRGB to LAB.
#'
#' @param x numeric, matrix or \code{\linkS4class{CanopyPhoto}}. Values must lying between \code{0} and {1}.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return Matrix, or \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{normalize}}.
#'
#' @example /inst/examples/sRGB2LABexample.R
#'
setGeneric("sRGB2LAB", function(x, ...) standardGeneric("sRGB2LAB"))
#' @export sRGB2LAB

#' @describeIn sRGB2LAB Each row should represent a color. Argument \code{x} must have three columns.
setMethod("sRGB2LAB",
  signature(x = "matrix"),
  function (x)
  {
    stopifnot(max(x, na.rm = TRUE) <= 1)
    stopifnot(min(x, na.rm = TRUE) >= 0)
    if (ncol(x) != 3) stop("x must be a matrix with three columns")
    z <- colorspace::sRGB(x[, 1], x[, 2], x[, 3])
    x <- as(z, "LAB")
    x <- colorspace::coords(x)
    return(x)
  }
)

#' @describeIn sRGB2LAB Convert a single color from sRGB to LAB. Argument \code{x} must be of length three. The output is a matrix.
setMethod("sRGB2LAB",
  signature(x = "numeric"),
  function (x)
  {
    if (length(x) != 3) stop("x must be a numeric vector of length three")
    x <- matrix(x, nrow = 1)
    x <- sRGB2LAB(x)
    return(x)
  }
)

#' @describeIn sRGB2LAB The output is a \code{\linkS4class{CanopyPhoto}} with layer names L, A and B.
setMethod("sRGB2LAB",
  signature(x = "CanopyPhoto"),
  function (x, ...)
  {
    stopifnot(names(x) == c("Blue", "Red", "Green"))
    from <- x
    stopifnot(max(getMax(x)) <= 1)
    stopifnot(min(getMin(x)) >= 0)
    fun <- .makeF8multi(function(x, ...) sRGB2LAB(x), ...)
    x <- fun(x, ...)
    x <- as(x, "CanopyPhoto")
    cloneSlots(from, x)
    names(x) <- c("L", "A", "B")
    return(x)
  }
)

############################
## ** membership2color ** ##
############################

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

#' @title Compute the membership value of a color to a color.
#'
#' @description This algorithm models the degree of membership to a color with two Gaussian membership functions and the layer A and B of the CIE \emph{L*a*b*} color space. The lightness information is omitted.
#'
#' @param x \linkS4class{color} or \code{\linkS4class{RasterStack}} or \code{\linkS4class{RasterBrick}}.
#' @param targetColor \linkS4class{color}.
#' @param sigma numeric. Default \code{NULL} means the algorithms will estimate it.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return Numeric or \code{\linkS4class{RasterLayer}}.
#'
#' @seealso \code{\link{normalize}}, \code{\link{enhanceHemiPhoto}}.
#'
#' @example /inst/examples/enhanceHemiPhotoExample.R
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

##########################
## ** fuzzyLightness ** ##
##########################

#' @title Compute a weighted membership value to lightness.
#'
#' @description This algorithm uses a threshold value as the location parameter of a logistic membership function whose scale parameter depend on a variable. This dependence can be explained as follows: if the variable is equal to \code{1}, then the membership function is as a threshold function because the scale parameter is zero; lowering the variable increases the scale parameter, thus blurring the threshold because it decreases the steepness of the curve.
#'
#' @param x numeric. The lightness value.
#' @param m numeric lying between \code{0} and {1}. It must have the same lenght as x. It is the scale parameter of the logistic membership function.
#' @param thr numeric of length one. Location parameter of the logistic membership function.
#' @param fuzzyness numeric of length one.
#'
#' @return Numeric.
#'
#' @seealso  \code{\link[stats]{plogis}}.
#'
#' @example /inst/examples/fuzzyLightnessExample.R
#'
#' @references
#' Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett. 12, 1784-1789.
#'
setGeneric("fuzzyLightness", function(x, m, thr, fuzzyness) standardGeneric("fuzzyLightness"))
#' @export fuzzyLightness

#' @rdname fuzzyLightness
setMethod("fuzzyLightness",
  signature(x = "numeric"),
  function (x, m, thr, fuzzyness)
  {
    stopifnot(length(thr) == 1)
    stopifnot(length(fuzzyness) == 1)
    stopifnot(length(x) == length(m))
    stats::plogis(x, thr, fuzzyness * (1 - m))
  }
)

############################
## ** enhanceHemiphoto ** ##
############################

.relativeBrightness <- function(x, wR, wB) {
  ((x[, 1] * wR + x[, 3] * wB) / 2) * x[, 4] + x[, 3] * (1 - x[, 4])
}

#' @title Enhance upward looking hemispherical photographs.
#'
#' @description This algorithm uses the color perceptual attributes to enhance the contrast between the sky and plants through fuzzy classification. Color has three different perceptual attributes: hue, lightness, and chroma. The algorithm was developed using this premise: the color of the sky is different from the color of plants. It performs the next classification rules, here expressed in natural language: clear sky is blue and clouds decrease its chroma; if clouds are highly dense, then the sky is achromatic, and, in such cases, it can be light or dark; everything that does not match this description is not sky. These linguistic rules were translated to math language by means of fuzzy logic.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param mask \code{\linkS4class{BinImage}}.
#' @param wR numeric. Weight of red layer. See details.
#' @param wB numeric. Weight of blue layer. See details.
#' @param sharpen logical. The default is TRUE, see details.
#' @param skyBlue \linkS4class{color}. See details.
#' @param thr numeric. By default, the algorithm will try to estimate it.
#' @param fuzzyness numeric. By default, the algorithm will try to estimate it.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @details The greatest contrast on gray levels between the sky and the plants should be observed in the blue channel because sky scattering is relatively high in the higher frequencies of light, and so the blue channel ensures the brightest sky pixels. Nevertheless, in the blue channel, the interface between the sky and the foliage pixels can be as bright as sky pixels. These pixels look dark cyan colored instead of green. The color distortion observed should be linked with: color formation through color filter array and chromatic aberration. Thus, we propose to use an original feature that we denominate Relative Brightness instead of one-channel brightness, which use weighting parameters for red and blue channels (see references).
#'
#' If sharpen is set to TRUE (default), a sharpen filter is applied to the raster with the membership values. The kernel used is \code{matrix(c(rep(-1, 3), -1, 12, -1, rep(-1, 3)), ncol = 3)}.
#'
#' @references
#' Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett. 12, 1784-1789.
#'
#' @return Numeric or \linkS4class{RasterLayer}.
#'
#' @seealso \code{\link{membership2color}}, \code{\link{fuzzyLightness}}.
#'
#' @example /inst/examples/enhanceHemiPhotoExample.R
#'
setGeneric("enhanceHemiPhoto", function(x, ...) standardGeneric("enhanceHemiPhoto"))
#' @export enhanceHemiPhoto

#' @describeIn enhanceHemiPhoto The output is a numeric vector.
setMethod("enhanceHemiPhoto",
  signature(x = "matrix"),
  function (x, thr = NULL, fuzzyness = NULL, wR = 0.5, wB = 1.5,
    skyBlue = colorspace::sRGB(matrix(normalize(c(135, 206, 235), 0, 255), ncol = 3)), ...)
  {

    stopifnot(max(x, na.rm = TRUE) <= 1)
    stopifnot(min(x, na.rm = TRUE) >= 0)
    if (ncol(x) != 4) stop("x must be a matrix with 4 columns")

    mask <- x[, 4]
    x <- x[, 1:3]

    mSkyBlue <- membership2color(colorspace::sRGB(x), skyBlue)
    mSkyBlue <- as.numeric(mSkyBlue)
    y <- sRGB2LAB(x)

    reBr <- .relativeBrightness(cbind(x, mSkyBlue), wR, wB)

    if(length(reBr) > 1) {
      if(is.null(thr)) thr <- autoThr(reBr[as.logical(mask)])
      if(is.null(fuzzyness)) fuzzyness <- (max(reBr) - min(reBr)) / 2
    } else {
      stop("Please provide thr and fuzzyness.")
    }

    mLight <- fuzzyLightness(reBr, mSkyBlue, thr, fuzzyness)

    out <- mSkyBlue * mLight

    return(out)
  }
)

#' @describeIn enhanceHemiPhoto The output is a \linkS4class{RasterLayer}.
setMethod("enhanceHemiPhoto",
  signature(x = "CanopyPhoto"),
  function (x, mask = !is.na(makeRimage(ncol(x))), wR = 0.5, wB = 1.5,
    sharpen = TRUE, skyBlue = colorspace::sRGB(matrix(normalize(c(135, 206, 235), 0, 255),
                                                                ncol = 3)), ...)
  {
    stopifnot(all(getMax(x) <= 1))
    stopifnot(all(getMin(x) >= 0))

    if (class(mask)[[1]] == "RasterLayer") {
      mask <- as(mask, "BinImage")
      validObject(mask)
    } else {
      if (class(mask)[[1]] != "BinImage")
        stop("Invalid object class for argument mask.")
    }

    x <- as(x, "RasterBrick")
    x <- stack(x, mask)

    reBr <- .relativeBrightness(values(x), wR, wB)

    thr <-  autoThr(reBr[as.logical(values(mask))])
    fuzzyness <- (max(reBr[as.logical(values(mask))]) -
                                        min(reBr[as.logical(values(mask))])) / 2

    foo <- function(x, ...) enhanceHemiPhoto(x, thr=thr, fuzzyness=fuzzyness)

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

###################
## ** outOfDR ** ##
###################

#' @title Get the percentages of pixels out of the dynamic range.
#'
#' @description Get the percentages of pixels out of the dynamic range, i.e., the under and overexposure pixels of an image.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param mask \linkS4class{BinImage}. The default of NULL means that all the pixels will be taking into account in the computations. If you provide a \code{BinImage}, it must have the same extent and resolution of \code{x}. All pixels from the image covered by pixels of the \code{mask} with value \code{1} will be taking into account in the computations.
#' @param returnImages logical. The default is FALSE.
#'
#' @details This algorithm classifies the pixels of \code{x} that are in the limits of the dynamic range. These pixels are often called under or overexpose because is very probably that in such a case the energy that reach the sensor fall outside the range. If the energy is under the dynamic range, then the pixel adopt value 0 for the three channel. If the energy is over the dynamic range, then the pixel adopts the maximum value for the three channel. The maximum value depends of the data type used, for example, 8-bit image has 255 and 16-bit has 65536.
#'
#' @return By default, it returns a vector of length tree with percentages for all the pixels outside the dynamic range, only the underexposure and only the overexposure. If you set \code{returnImages = TRUE}, it return a \code{\linkS4class{RasterStack}} that has two binary layers in which \code{1} means under or overexposure, respectively.
#'
#' @seealso \code{link{normalize}}, \code{link{colorfulIndex}}.
#'
#' @example /inst/examples/outOfDRexample.R
#'
setGeneric("outOfDR", function(x, mask = NULL, returnImages = FALSE)
                                                    standardGeneric("outOfDR"))
#' @export outOfDR

#' @rdname outOfDR
setMethod("outOfDR",
  signature(x = "CanopyPhoto"),
  function (x, mask, returnImages)
  {
    stopifnot(is.logical(returnImages))
    stopifnot(all(getMax(x) <= 1))
    stopifnot(all(getMin(x) >= 0))
    stopifnot(compareRaster(x, mask) == TRUE)
    stopifnot(any(is(mask) == "BinImage"))

    fun <- function(x, value, mask){

      x <- x == value
      x[x == 0] <- NA
      x <- raster::subset(x, 1) * raster::subset(x, 2) * raster::subset(x, 3)

      if(!is.null(mask)){
        x[mask == 0] <- NA
      }

      tmp <-  raster::freq(x, value = 1)

      if(is.null(mask)){
        f <- tmp / ncell(x)
      } else {
        f <- tmp / (ncell(x) - raster::freq(mask, value = 0))
      }

      list(f * 100, x)
    }

    u <- fun(x, 0, mask)
    o <- fun(x, 1, mask)

    iuo <- c(u[[1]] + o[[1]], u[[1]], o[[1]])

    names(iuo) <- c("Total", "Underexposure", "Overexposure")

    if(returnImages) {
      ruo <- stack(!is.na(u[[2]]), !is.na(o[[2]]))
      names(ruo) <- c("Underexposure", "Overexposure")
      return(ruo)
    } else {
      return(iuo)
    }
  }
)

#########################
## ** colorfulIndex ** ##
#########################

#' @title Quantify the colorful of an image.
#'
#' @description Quantify the colorful of a sRGB image using a bidimensional space form by green/red and blue/yellow axes of the CIE \emph{L*a*b*} space, in other words, the lightness is excluded.  In that bidimensional space, this algorithm creates a square surface between -100 and 100 of both axes and see how much area of this square is covered by the image pixels. The index is the percentage of that covered area.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}. Is the image that you want to know how colorful is.
#' @param mask \linkS4class{BinImage}. The default of NULL means that all the pixels will be taking into account in the computations. If you provide a \code{BinImage}, it must have the same extent and resolution of \code{x}. All pixels from the image covered by pixels of the \code{mask} with value \code{1} will be taking into account in the computations.
#' @param plot logical. The default is \code{FALSE}, if it is \code{TRUE} a plot will be printed. See Details.
#' @param returnRaster logical. The default is \code{FALSE}, if it is \code{TRUE} the raster that you can see when \code{plot} is \code{TRUE} is returned. See Details.
#'
#' @details When you request a plot with argument \code{plot = TRUE}, the colorful index is returned but also a plot is send to the active graphics Device. The plot shows the color of the image represented in a bidimensional space former by axis \emph{a} and \emph{b} of the CIE \emph{L*a*b* space}.
#'
#' @return Numeric or \code{\linkS4class{Raster}}.
#'
#' @seealso \code{link{normalize}}, \code{link{outOfDR}}.
#'
#' @example /inst/examples/colorfulIndexExample.R
#'
setGeneric("colorfulIndex",
  function(x, mask = NULL, plot = FALSE, returnRaster = FALSE)
    standardGeneric("colorfulIndex"))
#' @export colorfulIndex

#' @rdname colorfulIndex
setMethod("colorfulIndex",
  signature(x = "CanopyPhoto"),
  function (x, mask, plot, returnRaster)
  {

    stopifnot(is.logical(plot))
    stopifnot(all(getMax(x) <= 1))
    stopifnot(all(getMin(x) >= 0))
    stopifnot(compareRaster(x, mask) == TRUE)
    stopifnot(any(is(mask) == "BinImage"))

    if (!is.null(mask)) {
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
