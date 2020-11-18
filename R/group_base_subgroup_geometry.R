#### lensPolyCoef####
#' @title Generate a LensPolyCoef.
#'
#' @description Helper function to generate a \code{\linkS4class{LensPolyCoef}}
#'   needed to make a \code{\linkS4class{ZenithImage}}.
#'
#' @param x numeric or missing. Coefficient of the polynomial function. First
#'   number corresponds to the term powered to \code{1}, the second with the
#'   term powered to \code{2} and so on. To input the coefficient to the first
#'   and third term, you need to input \code{c(a, 0, c)}, being \code{a} and
#'   \code{c} the coefficient. Missing returns a perfect equidistant model.
#'
#' @details A projection method is required to visually represent the reality in
#'   a flat surface. A natural looking picture can be made with a central
#'   perspective projection model restricted to the cone of vision of an average
#'   person (looking with one eye), which has more or less 50 degrees. This is
#'   achieved using cameras with focal length of about 50 mm. For more detail
#'   see a good perspective drawing book. In fisheye lens the focal length is
#'   about 7-8 mm. The path of all the light rays that go through the camera
#'   diaphragm can be barely imagined as a very wide cone, so the term field of
#'   view is used instead of cone of vision. The standard fisheye lens has 180
#'   degrees field of view, which represents a complete hemisphere. There is a
#'   variety of models that can be used to mathematically project a 180 degrees
#'   field of view into a plane, such as: orthographic, equisolid angle,
#'   equidistant, stereographic and the previously mentioned central
#'   perspective. This models relate zenith angle with relative radius. The
#'   zenith is an imaginary point directly above a location. The location is
#'   represented as a point in the tridimensional space. The straight line that
#'   contains the location point and the zenith is a perfect vertical. The angle
#'   between this vertical line and any other line that pass though the location
#'   point is the zenith angle.
#'
#'   In looking upward leveled hemispherical photography, the zenith is the
#'   center of a circle which perimeter is the horizon. This is true only if the
#'   lens field of view is 180 degrees. The relative radius is the radius of
#'   concentric circles expressed as a fraction of the radius that belong to the
#'   circle that has the horizon as perimeter. The equidistant model, also
#'   called polar, is the most widely used as a standard reference. Real lens
#'   can approximate the projection models but they always have some kind of
#'   distortion. In the equidistant model the relation between zenith angle and
#'   relative radius is modeled with a straight line. In \code{caiman}, a
#'   polynomial curve is used to model lens distortion. This kind of model is
#'   used by the software HemiView and Hemisphere, both are proprietary software
#'   currently available in the market. A third-order polynomial is sufficient
#'   in most cases (Frazer et al., 2001).
#'
#' @references
#' Frazer, G. W., Fournier, R. A., Trofymow, J. A., & Hall, R. J. (2001). A
#' comparison of digital and film fisheye photography for analysisof forest
#' canopy structure and gap light transmission. Agricultural and Forest
#' Meteorology, 109, 249-263.
#'
#' Inoue, A., Yamamoto, K., Mizoue, N., Kawahara, Y., 2004. Calibrating view
#' angle and lens distortion of the Nikon fish-eye converter FC-E8. J. For. Res.
#' 9, 177-181. DOI: 10.1007/s10310-003-0073-8
#'
#' Kannala, J., Brandt, S.S., 2006. A generic camera model and calibration
#' method for conventional, wide-angle, and fish-eye lenses. IEEE Trans. Pattern
#' Anal. Mach. Intell. 28, 1335-1340. DOI: 10.1109/TPAMI.2006.153
#'
#' Roberson, S., Bertling, T., 2013. How to Draw: drawing and sketching objects
#' and environments from your imagination. Design Studio Press.
#'
#' Schneider, D., Schwalbe, E., Maas, H.-G., 2009. Validation of geometric
#' models for fisheye lenses. ISPRS J. Photogramm. Remote Sens. 64, 259-266.
#' DOI: 10.1016/j.isprsjprs.2009.01.001
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

#### calcR ####
#' @title Calculate relative radius.
#'
#' @description Given a zenith angle, it calculates the \strong{relative radius}
#'   (R) taken into account the lens distortion. For a definition of relative
#'   radius see details of \code{\link{lensPolyCoef}}.
#'
#' @aliases calcR
#'
#' @param x \code{\linkS4class{Angle}}.
#' @param y \code{\linkS4class{LensPolyCoef}}.
#'
#' @return numeric.
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

#### makeRimage ####
#' @title Generate a RelativeRadiusImage.
#'
#' @description Helper function to generate a
#'   \code{\linkS4class{RelativeRadiusImage}}. Compute relative radius for each
#'   pixes needed to represent a circular hemispherical image with a \code{180}
#'   degrees field of view and a given diameter, expressed in pixels. For
#'   definition of relative radius see details of \code{\link{lensPolyCoef}}.
#'
#' @param x numeric. Diameter of the circle in pixels expressed as one-length
#'   even integer.
#' @param y \code{\linkS4class{LensPolyCoef}}.
#'
#' @details Most digital cameras take photos with an aspect ratio of 3:4, which
#'   means a rectangular frame. If all the circle area is in the picture frame,
#'   hundreds of pixels will have no-data. \code{RelativeRadiusImage} has 1:1 aspect
#'   ratio to minimize no-data pixels, so the circle is inscribed in a square.
#'   The size of the square equals the diameter of the circle. To represent the
#'   zenith as a point between pixels, the diameter of the circle must be even.
#'   In a high resolution image this restriction of snapping the zenith between
#'   pixels does not affect accuracy because half-pixel is less than the
#'   uncertainty in localizing the circle in the frame.
#'
#' @return \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @seealso \code{\link{makeZimage}}.
#'
#' @example /inst/examples/makeRimageExample.R
#'
setGeneric("makeRimage", function(x, y) standardGeneric("makeRimage"))
#' @export makeRimage

#' @describeIn makeRimage Assuming a polar projection.
#'
setMethod("makeRimage",
  signature(x = "numeric", y = "missing"),
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

#' @describeIn makeRimage Used to specified a projection different from the polar.
#'
setMethod("makeRimage",
          signature(x = "numeric", y = "LensPolyCoef"),
          function (x, y)
          {
            r <- makeRimage(x)
            foo <- r * calcR(asAngle(90), y)
            values(r) <- values(foo)
            return(r)
          }
)
#### makeZimage ####
#' @title Generate a ZenithImage.
#'
#' @description Helper function to generate a \code{\linkS4class{ZenithImage}}. Compute zenith angle
#'   for each pixel needed to represent a circular hemispherical image with \code{180}
#'   degrees field of view. For definition of zenith angle see details of
#'   \code{\link{lensPolyCoef}}.
#'
#' @param x numeric or \code{\linkS4class{RelativeRadiusImage}}.
#' @param y \code{\linkS4class{LensPolyCoef}}.
#'
#' @details If the \code{x} argument of a call to \code{makeZimage} is numeric,
#'   the function internally calls to \code{\link{makeRimage}}.
#'
#' @return \code{\linkS4class{ZenithImage}}.
#'
#' @seealso \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @example /inst/examples/makeZimageExample.R
#'
setGeneric("makeZimage", function(x, y) standardGeneric("makeZimage"))
#' @export makeZimage

#' @describeIn makeZimage In certains processing chain, maybe it could help to decrease
#'   processing time. For example, in processing photos with same resolution but
#'   taken with different lens.
#'
setMethod("makeZimage",
  signature(x = "RelativeRadiusImage", y = "LensPolyCoef"),
  function (x, y)
  {
    ## Estimation of Zenith angle based on Relative radius inversion
    ## by Lookup Table -LUT-.

    ## Calculation of the LUT
    r <- seq(0, 90,  length.out = nrow(x) + 1)
    R <- calcR(x = asAngle(r), y = y) #  relative radius -R-.

    ## Perform the inversion
    rcl <- matrix(c(c(0, R[-length(R)]), R, r), ncol = 3)

    ## fix rcl
    if (!all(rcl[, 2] >= rcl[, 1])) {
      index <- !(rcl[, 2] >= rcl[, 1])
      problemsAreHere <- round(mean(rcl[index, 3]), 1)
      whatProblems <- max(rcl[index, 1] - rcl[index, 2])

      tolerance <- 0.0001
      if (whatProblems < tolerance) {
        rcl[index, 2] <- rcl[index, 1]
        warning(
          paste0("Relative radius around ", problemsAreHere,
                 " degrees of zenith angle were rectified adding or subtracting around ",
                 whatProblems, ". Tolerance is ", tolerance, ".")
        )
      } else {
        stop(
          paste0("Relative radius around ", problemsAreHere,
                 " degrees of zenith angle has unexpected values.")
        )
      }
    }

    # fix for issue found with RAMI dataset. Jun, 2019
    # calcR(asAngle(90), lensPolyCoef(c(0.67657, 0.04487, -0.04760))) < 1
    rcl[nrow(rcl),2] <- 1

    zenithImage <- reclassify(x, rcl)

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

#' @describeIn makeZimage It is the most frequent use. You only need to provide
#'   the diameter in pixels of the circle that has data in the circular hemispherical
#'   photographs you want to process.
#'
setMethod("makeZimage",
  signature(x = "numeric", y = "LensPolyCoef"),
  function (x, y)
  {
    makeZimage(makeRimage(x), y)
  }
)

#### makeAimage ####
#' @title Generate an AzimuthImage.
#'
#' @description Helper function to generate an
#'   \code{\linkS4class{AzimuthImage}}. Compute Azimuth angle for each pixels
#'   that are needed to represent a circular hemispherical image with \code{180}
#'   degrees field of view. North is oriented up and West is oriented left.
#'
#' @param x \code{\linkS4class{ZenithImage}}.
#'
#' @details Azimuth angle is the angle between the North direction and the line
#'   of sight projected to the leveled plane. The angle increases in clockwise
#'   direction at the field but not in the hemispherical image because it is
#'   flipped in the East-West direction.
#'
#' @return \code{\linkS4class{AzimuthImage}}.
#'
#' @seealso \code{\link{makeZimage}}.
#'
#' @example /inst/examples/makeAimageExample.R
#'
setGeneric("makeAimage", function(x)
            standardGeneric("makeAimage"))
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




#### calcOpticalCenter ####
#' todo
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#'
#' @return numeric. todo
#' @examples /inst/examples/calcOpticalCenterExample.R
setGeneric("calcOpticalCenter",
           function(x, y) standardGeneric("calcOpticalCenter"))
#' @export calcOpticalCenter

#' @rdname calcOpticalCenter
setMethod("calcOpticalCenter",
          signature(x = "data.frame"),
          function (x)
          {
            # each tracked hole have two columns
            stopifnot(ncol(x)/2 == round(ncol(x)/2))

            nHoles <- ncol(x) / 2

            index <- seq(1, ncol(x), 2)

            circle <- list()
            for (i in 1:nHoles) {
              theIndex <- index[i]
              theData <- x[, c(theIndex, theIndex+1)]
              theData <- theData[!is.na(theData[,1]),]
              theData <- as.matrix(theData)
              foo <- conicfit::CircleFitByKasa(theData)
              circle[[i]] <- foo
            }

            circle <- unlist(circle)
            circle <- matrix(circle, ncol = 3, byrow = TRUE)
            colnames(circle) <- c("x", "y", "radius")

            upperLeft <- cbind(circle[,"x"] - circle[,"radius"],
                           circle[,"y"] - circle[,"radius"])
            side <- circle[,"radius"] * 2

            circle <- cbind(circle, upperLeft, side)
            colnames(circle) <- cbind("x", "y", "radius",
                                      "upperLeftX", "upperLeftY", "side")
            row.names(circle) <- NULL

            circle
          }
)

#### calcDiameter ####
#' Calculate diameter corresponding to a circular fisheye photography.
#'
#' @param x \code{\linkS4class{LensPolyCoef}}.
#' @param pix todo
#' @param angle todo
#'
#' @return numeric.
#' @example /inst/examples/calcDiameterExample.R
setGeneric("calcDiameter", function(x, pix, angle) standardGeneric("calcDiameter"))
#' @export calcDiameter

#' @rdname calcDiameter
setMethod("calcDiameter",
          signature(x = "LensPolyCoef"),
          function (x, pix, angle)
          {

            stopifnot(length(pix) == length(angle@values))

            Rfor90 <- calcR(asAngle(90), x)
            RforMyAngle <- calcR(angle, x)

            fun <- function(pix, RforMyAngle) {
              Rfor90 * pix / RforMyAngle * 2
            }

            if(length(pix) == 1) {
              diameter <- round(fun(pix, RforMyAngle))

            } else {

              diameters <- unlist(Map(fun, pix, RforMyAngle))
              diameter <- round(median(diameters))
              attr(diameter, "IQR") <- IQR(diameters)
            }

            if (diameter/2 != round(diameter/2)) {
              diameter <- diameter + 1
            }

            return(diameter)

          }
)


#### expandFullframe ####
#' @title Expand full frame hemispherical photographs
#'
#' @description Expand a full frame hemispherical photograph to get the
#'   equivalent of a circular hemispherical photograph. Added
#'   pixels will be \code{0}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param z \code{\linkS4class{ZenithImage}}.

#' @return \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{doMask}}.
#'
#' @example /inst/examples/doMaskExample.R
#'
setGeneric("expandFullframe",
           function (x, z, zenith_colrow)
             standardGeneric("expandFullframe"))
#' @export expandFullframe

#' @rdname expandFullframe
setMethod("expandFullframe",
          signature(x = "CanopyPhoto", z = "ZenithImage"),
          function (x, z, zenith_colrow) {

            zenith_xy <- zenith_colrow
            zenith_xy[2] <- nrow(x) - zenith_colrow[2]

            # locate the center of x over the center of z
            center <- ncol(z) / 2
            xmn <- center - (ncol(x) / 2)
            xmx <- center + (ncol(x) / 2)
            ymn <- center - (nrow(x) / 2)
            ymx <- center + (nrow(x) / 2)
            e <- extent(xmn, xmx, ymn, ymx)
            extent(x) <- e

            # shift the center of x according with zenith_xy
            delta_x <- zenith_xy[1] - ncol(x) / 2
            delta_y <- zenith_xy[2] - nrow(x) / 2
            xmn <- xmin(x) + delta_x
            xmx <- xmax(x) + delta_x
            ymn <- ymin(x) + delta_y
            ymx <- ymax(x) + delta_y
            e <- extent(xmn, xmx, ymn, ymx)
            extent(x) <- e


            foo <- extend(x, z, value = NA)

            foo <- as(foo, "CanopyPhoto")
            foo <- cloneSlots(x, foo)
            e <- extent(0, ncol(foo), 0, nrow(foo))
            extent(foo) <- e
            foo

          }
)

##### rotAzim ####
#' @title Rotate Azimuth
#'
#' @description Rotate an \code{\linkS4class{AzimuthImage}} or a
#' similar \code{\linkS4class{RasterLayer}}.
#'
#' @param x \code{\linkS4class{RasterLayer}}.
#' @return cwRotAngle \code{\linkS4class{Angle}}.
#'
#' @export rotAzim
#'
#' @examples /inst/examples/rotAzimExample.R
setGeneric("rotAzim",
           function(x, cwRotAngle)
             standardGeneric("rotAzim"))

#' @rdname rotAzim
setMethod("rotAzim",
          signature(x = "RasterLayer"),
          function (x, cwRotAngle) {

            if (!cwRotAngle@degrees) {
              cwRotAngle <- switchUnit(cwRotAngle)
            }
            cwRotAngle <- cwRotAngle@values

            m <- is.na(x)

            if (cwRotAngle != 0) {
              foo <- as.matrix(x)
              foo <- imager::as.cimg(foo)
              foo <- imager::rotate_xy(foo, -cwRotAngle,
                                       ncol(x) / 2, nrow(x) / 2, interpolation = 0)
              values(x) <- as.matrix(foo)
            }

            x[m] <- NA
            x

          }
)

#' @rdname rotAzim
setMethod("rotAzim",
          signature(x = "PolarSegmentation"),
          function (x, cwRotAngle) {

            x <- as(x, "RasterLayer")
            x <- rotAzim(x, cwRotAngle)
            as(x, "PolarSegmentation")

          }
)

#' @rdname rotAzim
setMethod("rotAzim",
          signature(x = "BinImage"),
          function (x, cwRotAngle) {

            x <- as(x, "RasterLayer")
            x <- rotAzim(x, cwRotAngle)
            as(x, "BinImage")

          }
)

#' @rdname rotAzim
setMethod("rotAzim",
          signature(x = "AzimuthImage"),
          function (x, cwRotAngle) {

            x <- as(x, "RasterLayer")
            x <- rotAzim(x, cwRotAngle)
            x[x == 0] <- 0.001
            as(x, "AzimuthImage")

          }
)

##### fisheye2pano ####

#' @title todo
#'
#' @description todo
#'
#' @param cp todo
#'
#' @export fisheye2pano
#'
#' @examples #todo
setGeneric("fisheye2pano",
           function(x, g, m = NULL, fill_na = TRUE, fun = mean)
             standardGeneric("fisheye2pano"))

#' @rdname fisheye2pano
setMethod("fisheye2pano",
          signature(x = "CanopyPhoto"),
          function (x, g, m, fill_na, fun)
          {

            red <- fisheye2pano(x$Red, g, m, fill_na, fun)
            green <- fisheye2pano(x$Green, g, m, fill_na, fun)
            blue <- fisheye2pano(x$Blue, g, m, fill_na, fun)

            pano <- stack(red, green, blue)
            names(pano) <- c("Red", "Green", "Blue")

            pano

          }
)

#' @rdname fisheye2pano
setMethod("fisheye2pano",
          signature(x = "RasterLayer"),
          function (x, g, m, fill_na, fun)
          {

            if(!is.null(m)) x[!m] <- NA

            blue <- extractFeatures(x, g, fun)

            decode_label <- function(label) {
              sector_ID <- trunc(label / 1000)
              rings_ID <- label - sector_ID * 1000
              #browser()
              #data.frame(sector_ID, rings_ID = max(rings_ID) - rings_ID)
              data.frame(sector_ID, rings_ID)
            }

            xy <- decode_label(as.numeric(names(blue)))

            r <- matrix(NA, ncol = max(xy$sector_ID), nrow = max(xy$rings_ID))
            r <- raster(r)
            extent(r) <- extent(0, ncol(r), 0, nrow(r))

            cells <- cellFromXY(r, as.matrix(xy)-0.5)
            r[cells] <- blue
            # .labels <- r
            # .labels[cells] <- as.numeric(names(blue))

            if(!is.null(m)) r <- trim(r)

            if (fill_na) {
              fun <- function(x) {
                x <- focal(x, matrix(1, 5, 5), mean, NAonly = TRUE, na.rm = TRUE)
                no_of_loops <- 0
                unlock <- TRUE
                while (unlock) {
                  no_of_loops <- no_of_loops + 1
                  x <- focal(x, matrix(1, 5, 5), mean, NAonly = TRUE, na.rm = TRUE)
                  unlock <- any(is.na(x)[])
                  if (unlock) unlock <- no_of_loops < 100
                }
                x
              }

              r <- fun(r)
            }

            # list(r, .labels)
            r

          }
)

##### fisheye2pano_hr ####

#' @title todo
#'
#' @description todo
#'
#' @param cp todo
#'
#' @export fisheye2pano_hr
#'
#' @examples #todo
setGeneric("fisheye2pano_hr",
           function(hcp, z, a)
             standardGeneric("fisheye2pano_hr"))

#' @rdname fisheye2pano_hr
setMethod("fisheye2pano_hr",
          signature(hcp = "CanopyPhoto"),
          function (hcp, z, a)
          {
            m <- doMask(z)
            ds <- data.frame(a = a[m],
                             z = 90-z[m],
                             red = hcp$Red[m],
                             green = hcp$Green[m],
                             blue = hcp$Blue[m])

            pixels_per_degree_z <- nrow(hcp)/180 # average
            pixels_per_degree_a <- nrow(hcp)*pi/360 #maximum, at zenith it tents to 0
            pixels_per_degree <- mean(pixels_per_degree_a, pixels_per_degree_z)

            r <- matrix(NA, nrow = 90 * pixels_per_degree, ncol = 360 * pixels_per_degree)
            r <- raster(r)

            red <- SpatialPointsDataFrame(ds[,1:2], data.frame(ds$red))
            green <- SpatialPointsDataFrame(ds[,1:2], data.frame(ds$green))
            blue <- SpatialPointsDataFrame(ds[,1:2], data.frame(ds$blue))

            red <- rasterize(red, r)
            green <- rasterize(green, r)
            blue <- rasterize(blue, r)

            fun <- function(x) {
              x <- focal(x, matrix(1, 5, 5), mean, NAonly = TRUE, na.rm = TRUE)
              no_of_loops <- 0
              lock <- TRUE
              while (lock) {
                no_of_loops <- no_of_loops + 1
                x <- focal(x, matrix(1, 5, 5), mean, NAonly = TRUE, na.rm = TRUE)
                lock <- any(is.na(x)[])
                lock <- no_of_loops > 100
              }
              x
            }

            .red <- fun(red$ds.red)
            .green <- fun(green$ds.green)
            .blue <- fun(blue$ds.blue)

            photo <- stack(.red, .green, .blue)

            extent(photo) <- extent(0, ncol(r), 0, nrow(r))
            photo
          }
)

##### calibrate_lens ####

#' @title todo
#'
#' @description todo
#'
#' @param cvs todo
#'
#' @export calibrate_lens
#'
#' @examples #todo
setGeneric("calibrate_lens",
           function(csv)
             standardGeneric("calibrate_lens"))

#' @rdname calibrate_lens
setMethod("calibrate_lens",
          signature(csv = "data.frame"),
          function (csv)
          {
            degree <- 3

            csv <- cbind(csv$X, csv$Y)

            #center in (0,0)
            csv[,1] <- csv[,1] - csv[1,1]
            csv[,2] <- csv[,2] - csv[1,2]

            maxFOV_px <- csv[nrow(csv),]
            csv <- csv[-nrow(csv),]

            theta <- seq(0,90,5) * pi/180
            theta <- theta[1:nrow(csv)]

            #convert
            csv <- cart2pol(csv)
            maxFOV_px <- cart2pol(maxFOV_px)
            maxFOV_px <- maxFOV_px[2]

            pix <- csv[,2]

            #fit to get FOV
            fit <- lm(theta ~ poly(pix, degree, raw = TRUE) -1)
            maxFOV <- predict(fit, data.frame(pix = maxFOV_px)) * 180/pi

            # fit to get the radius of the horizon
            fit <- lm(pix ~ poly(theta, degree, raw = TRUE) -1)
            pix90 <- predict(fit, data.frame(theta = pi/2))

            #relative radius
            R <- pix/pix90

            # fit to get the coefficients
            fit <- lm(R ~ poly(theta, degree, raw = TRUE) -1)

            coefficients(fit)
            lens <- lensPolyCoef()
            lens@coef <- unname(coefficients(fit))
            lens
          }
)
