#### getFeatureImage ####
#' @title Get an image that stores object features.
#'
#' @description Given a single layer data, a segmentation and a function, this
#'   algorithm returns \code{RasterLayer} like segmentation but with its segment-IDs
#'   replaced with the segment-features, which are the return of processing all
#'   the pixels that belong to a segment with the aforementioned function.
#'
#' @inheritParams extractFeatures
#'
#' @return \code{\linkS4class{RasterLayer}}.
#'
#' @seealso \code{\link{extractFeatures}}.
#'
#' @example /inst/examples/getFeatureImageExample.R
#'
setGeneric("getFeatureImage", function(x, ...) standardGeneric("getFeatureImage"))
#' @export getFeatureImage

#' @rdname getFeatureImage
setMethod("getFeatureImage",
  signature(x = "RasterLayer"),
  function(x, segmentation, fun) {
    stopifnot(class(segmentation)[[1]] == "PolarSegmentation")
    stopifnot(any(class(fun) == "function", class(fun) == "standardGeneric"))

    feature <- tapply(values(x), values(segmentation), fun)
    id <- as.numeric(names(feature))
    df <- data.frame(id, feature)
    featureImage <- subs(segmentation, df)
    return(featureImage)
  }
)

#### extractFeatures ####
#' @title Extract object features.
#' @description Given a single layer data, a segmentation and a function, this
#'   algorithm returns a numeric vector of length equal to the number of
#'   segments in the segmentation. Each value of the vector is the return
#'   obtained if all pixels that belong to a segment are processed with the
#'   aforementioned function.
#'
#' @param x \code{\linkS4class{RasterLayer}}. The image that you want to extract
#'   feature from.
#' @param segmentation \linkS4class{PolarSegmentation}. The segmentation of
#'   \code{x}.
#' @param fun \code{function} that take a vector as input and return a one-length
#'   numeric or logical (e.g. mean).
#' @param ... Additional arguments (none implemented).
#'
#' @return \code{numeric.}
#'
#' @seealso \code{\link{getFeatureImage}}.
#'
#' @example /inst/examples/getFeatureImageExample.R
#'
setGeneric("extractFeatures", function(x, ...) standardGeneric("extractFeatures"))
#' @export extractFeatures

#' @rdname extractFeatures
setMethod("extractFeatures",
  signature(x = "RasterLayer"),
  function(x, segmentation, fun) {

    stopifnot(class(segmentation)[[1]] == "PolarSegmentation")
    stopifnot(any(class(fun) == "function", class(fun) == "standardGeneric"))

    fe <- tapply(values(x), values(segmentation), fun)
    ids <- names(fe)
    fe <- as.numeric(fe)
    names(fe) <- ids
    return(fe)
  }
)


#### regional_thr ####
#' @title Regional Thresholding
#' @description todo
#'
#' @param r \code{\linkS4class{RasterLayer}}.
#' @param segmentation \linkS4class{PolarSegmentation}. The segmentation of
#'   \code{x}.
#' @param method \code{character}.
#' @param ... Additional arguments (none implemented).
#'
#'
setGeneric("regional_thr", function(r, segmentation, method = "autoThr") standardGeneric("regional_thr"))
#' @export regional_thr

#' @rdname regional_thr
setMethod("regional_thr",
          signature(r = "RasterLayer"),
          function(r, segmentation, method) {
            .binarize_per_segment <- function(segment_id) {
              indices <- segmentation == segment_id
              r[indices] <<- r[indices] > autoThr(r[indices])
            }
            Map(.binarize_per_segment, unique(segmentation))
            as(r, "BinImage")
            r
          }
)
