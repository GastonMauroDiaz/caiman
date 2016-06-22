########################
## ** lenspolyCoef ** ##
########################

#' @title Generate a LensPolyCoef.
#'
#' @description Helper function to generate a \code{\linkS4class{LensPolyCoef}} needed to make a \code{\linkS4class{ZenithImage}}.
#'
#' @param x numeric or missing. Coefficient of the polynomial function. First number correspond with the term powered to one, second with the term powered to two and so on. To ingest the coefficient to the first and third term, you need to input \code{c(a, 0, c)}, being \code{a} and \code{c} the coefficient. Missing return a perfect equidistant model.
#'
#' @details A projection method is required to visually represent the reality in a flat surface. A natural looking picture can be made with a central perspective projection model restricted to the cone of vision of an average person (looking with one eye), which has more or less 50 degrees. This is achieved using cameras with focal length of about 50 mm. For more detail see a good perspective drawing book.
#'
#' In fisheye lens the focal length is about 7-8 mm. The path of all the light rays that go through the camera diaphragm can be barely imagined as a very wide cone, so the term field of view is used instead of cone of vision. The standard fisheye lens has 180 degrees field of view, which represents a complete hemisphere.
#'
#' There is a variety of models that can be used to mathematically project a 180 degrees field of view into a plane, such as: orthographic, equisolid angle, equidistant, stereographic and the lather mentioned central perspective. This models relate zenith angle with relative radius.
#'
#' The \strong{zenith} is an imaginary point directly above a location. The \strong{location} is represented as a point in the tridimensional space. The straight line that contain the location point and the zenith is a perfect vertical. The angle between this vertical line and any other line that pass though the location point is the \strong{zenith angle}.
#'
#' In looking upward leveled hemispherical photography, the zenith is the center of a circle which perimeter is the horizon. This is true only if the lens field of view is 180 degrees. The \strong{relative radius} is the radius of concentric circles expressed as a fraction of the radius that belong to the circle that has the horizon as perimeter.
#'
#' The equidistant model, also called polar, is the most widely used as a standard reference. Real lens can approximate the projection models but they always have some kind of distortion. In the equidistant model the relation between zenith angle and relative radius is modeled with a straight line. In caiman, a polynomial curve is used to model lens distortion. This kind of model is used by the software HemiView and Hemisphere, both are proprietary software currently available in the market.
#'
#' @references
#' Inoue, A., Yamamoto, K., Mizoue, N., Kawahara, Y., 2004. Calibrating view angle and lens distortion of the Nikon fish-eye converter FC-E8. J. For. Res. 9, 177-181. DOI: 10.1007/s10310-003-0073-8
#'
#' Kannala, J., Brandt, S.S., 2006. A generic camera model and calibration method for conventional, wide-angle, and fish-eye lenses. IEEE Trans. Pattern Anal. Mach. Intell. 28, 1335-1340. DOI: 10.1109/TPAMI.2006.153
#'
#'Roberson, S., Bertling, T., 2013. How to Draw: drawing and sketching objects and environments from your imagination. Design Studio Press.
#'
#'Schneider, D., Schwalbe, E., Maas, H.-G., 2009. Validation of geometric models for fisheye lenses. ISPRS J. Photogramm. Remote Sens. 64, 259-266. DOI: 10.1016/j.isprsjprs.2009.01.001
#'
#' @return \code{\linkS4class{LensPolyCoef}}.
#'
#' @example /inst/examples/lensPolyCoefExample.R
#'
setGeneric("lensPolyCoef", function(x) standardGeneric("lensPolyCoef"))
#' @export lensPolyCoef

#' @rdname lensPolyCoef
setMethod("lensPolyCoef",
  signature(x = "numeric"),
  function (x)
  {
    new("LensPolyCoef", coef = x)
  }
)

#' @rdname lensPolyCoef
setMethod("lensPolyCoef",
  signature(x = "missing"),
  function (x)
  {
    new("LensPolyCoef")
  }
)

#################
## ** calcR ** ##
#################

#' @title Calculate relative radius.
#'
#' @description Given a zenith angle, calculate \strong{relative radius} (R) taken into account the lens distortion. For definition of relative radius see details of \code{\link{lensPolyCoef}}.
#'
#' @aliases calcR
#'
#' @param x \code{\linkS4class{Angle}}.
#' @param y \code{\linkS4class{LensPolyCoef}}.
#'
#' @return Numeric.
#'
#' @seealso \code{\link{makeRimage}}, \code{\link{makeZimage}}
#'
#' @example /inst/examples/calcRexample.R
#'
setGeneric("calcR", function(x, y) standardGeneric("calcR"))
#' @export calcR

#' @rdname calcR
setMethod("calcR",
  signature(x = "Angle", y = "LensPolyCoef"),
  function (x, y)
  {
    if (x@degrees) {x <- switchUnit(x)}
    if (max(x@values, na.rm = TRUE) > (pi / 2))
    {
      stop("x must be less than 90 degrees because it represent zenith angle.")
    }

    temp <- cbind(y@coef, seq(1, length(y@coef)))
    for (i in 1:length(y@coef)) {
      if (i == 1) {
        ma <- temp[i, 1] * x@values^temp[i, 2]
      } else {
        ma <- rbind(ma, temp[i, 1] * x@values^temp[i, 2])
      }
    }

    if (length(y@coef) == 1) {
      relative_radius <- ma
    } else {
      relative_radius <- apply(ma, 2, sum)
    }
    return(relative_radius)

  }
)

######################
## ** makeRimage ** ##
######################

#' @title Generate a RelativeRadiusImage.
#'
#' @description Helper function to generate a \code{\linkS4class{RelativeRadiusImage}}. Compute relative radius for each pixel that is needed to represent a circular hemispherical image with a 180 degrees field of view and a given diameter, expressed in pixels. For definition of relative radius see details of \code{\link{lensPolyCoef}}.
#'
#' @param x numeric. Diameter of the circle in pixels expressed as one-length even integer.
#'
#' @details Most digital cameras take photos with an aspect ratio of 3:4, which means a rectangular frame. If all the circle area is in the picture frame, hundreds of pixels will have no-data. RelativeRadiusImage has 1:1 aspect ratio to minimize no-data pixels, so the circle is inscribed in a square. The size of the square equals the diameter of the circle. To represent the zenith as a point between pixels, the diameter of the circle must be even. In a high resolution image this restriction of snapping the zenith between pixels does not affect accuracy because half-pixel is less than the uncertainty in localizing the circle in the frame.
#' @return \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @seealso code{\link{makeZimage}}.
#'
#' @example /inst/examples/makeRimageExample.R
#'
setGeneric("makeRimage", function(x) standardGeneric("makeRimage"))
#' @export makeRimage

#' @rdname makeRimage
setMethod("makeRimage",
  signature(x = "numeric"),
  function (x)
  {
    x <- as.integer(x)
    # Make esqueleton
    r <- new("RelativeRadiusImage", diameter = x)
    r@title <- "Relative Radius"
    r@ncols <- x
    r@nrows <- x
    extent(r) <- c(0, x, 0, x)
    values(r) <- 1

    ## Calculation of Relative Radius -R- raster
    zenithPoint <- x / 2
    dis <- distanceFromPoints(r, matrix(c(zenithPoint, zenithPoint), ncol = 2))
    dis[dis > zenithPoint] <- NA
    dis <- dis / zenithPoint #  relative radius raster

    values(r) <- values(dis)

    return(r)
  }
)

######################
## ** makeZimage ** ##
######################

#' @title Generate a ZenithImage.
#'
#' @description Helper function to generate a \code{\linkS4class{ZenithImage}}. Compute zenith angle for each pixel that is needed to represent a circular hemispherical image with 180 degrees field of view. For definition of zenith angle see details of \code{\link{lensPolyCoef}}.
#'
#' @param x numeric or \code{\linkS4class{RelativeRadiusImage}}.
#' @param y \code{linkS4class{LensPolyCoef}}.
#'
#' @details If the \code{x} argument of a call to makeZimage is numeric, the function internally calls to makeRimage.
#'
#' @return \code{\linkS4class{ZenithImage}}.
#'
#' @seealso \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @example /inst/examples/makeZimageExample.R
#'
setGeneric("makeZimage", function(x, y) standardGeneric("makeZimage"))
#' @export makeZimage

#' @describeIn makeZimage It is the most frequent use. You only need to provide the diameter in pixels of the circle that has data in the hemispherical photographs that you want to process.
setMethod("makeZimage",
  signature(x = "RelativeRadiusImage", y = "LensPolyCoef"),
  function (x, y)
  {
    ## Estimation of Zenith angle based on Relative radius inversion
    ## by Lookup Table -LUT-.

    ## Calculation of the LUT
    r <- seq(0, 90,  length.out = nrow(x) + 1)
    R <- calcR(x = asAngle(r, degrees = TRUE), y = y) #  relative radius -R-.

    ## Perform the inversion
    rcl <- matrix(c(c(0, R[-length(R)]), R, r), ncol = 3)

    ## fix rcl
    if (!all(rcl[, 2] >= rcl[, 1])) {
      index <- !(rcl[, 2] >= rcl[, 1])
      problemsAreHere <- round(mean(rcl[index, 3]), 1)
      wathProblems <- max(rcl[index, 1] - rcl[index, 2])

      tolerance <- 0.0001
      if (wathProblems < tolerance) {
        rcl[index, 2] <- rcl[index, 1]
        warning(
          paste0("Relative radius around ", problemsAreHere, " degrees of zenith angle were rectified adding or subtracting around ", wathProblems, ". Tolerance is ", tolerance, ".")
        )
      } else {
        stop(
          paste0("Relative radius around ", problemsAreHere, " degrees of zenith angle has unexpected values.")
        )
      }
    }

    zenithImage <- reclassify(x, rcl)
    #zenithImage <- 180 * zenithImage / pi

    z <- new("ZenithImage")
    z@diameter <- x@diameter
    z@lens <- y

    z@title <- "Zenith angle"
    z@ncols <- z@diameter
    z@nrows <- z@diameter
    extent(z) <- c(0, z@diameter, 0, z@diameter)
    values(z) <- values(zenithImage)

    return(z)

  }
)

#' @describeIn makeZimage In certain processing chain, maybe it could help to decrease processing time. For example, in processing same resolution photo but taken with different lens.
#'
setMethod("makeZimage",
  signature(x = "numeric", y = "LensPolyCoef"),
  function (x, y)
  {
    makeZimage(makeRimage(x), y)
  }
)

######################
## ** makeAimage ** ##
######################

#' @title Generate an AzimuthImage.
#'
#' @description Helper function to generate an \code{\linkS4class{AzimuthImage}}. Compute Azimuth angle for each pixel that is needed to represent a circular hemispherical image with 180 degrees field of view. North is oriented up and West is oriented left.
#'
#' @param x \code{\linkS4class{ZenithImage}}.
#'
#' @details Azimuth angle is the angle between the North direction and the line of sight projected to the leveled plane. The angle increase in clockwise direction at the field but not in the hemispherical image because it is flipped in the East-West direction.
#'
#' @return \code{\linkS4class{AzimuthImage}}.
#'
#' @seealso code{\link{makeZimage}}.
#'
#' @example /inst/examples/makeAimageExample.R
#'
setGeneric("makeAimage", function(x) standardGeneric("makeAimage"))
#' @export makeAimage

#' @rdname makeAimage
setMethod("makeAimage",
  signature(x = "ZenithImage"),
  function (x)
  {
    mask <- is.na(x)

    xy <- xyFromCell(x, seq(length = ncell(x)))
    v <- values(x)
    sph <- pracma::cart2sph(
      matrix(c(xy[, 1] - x@diameter / 2, xy[, 2] - x@diameter / 2, values(x)), ncol = 3)
    )

    a <- new("AzimuthImage")
    a@diameter <- x@diameter
    a@title <- "Azimuth angle"
    a@ncols <- x@diameter
    a@nrows <- x@diameter
    extent(a) <- extent(x)

    values(a) <- sph[, 1] * 180 / pi
    values(a) <- values(abs(t(a) - 180)) # to orient North up and West left
    a[is.na(x)] <- NA

    return(a)
  }
)
