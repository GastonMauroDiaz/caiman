##### fisheye2pano_dl ####
# 14/4/2020

#' @title todo
#'
#' @description todo
#'
#' @param cp todo
#'
#' @export fisheye2pano_dl
#'
#' @examples #todo
setGeneric("fisheye2pano_dl",
           function(x, g, m = NULL, fill_na = TRUE, fun = mean)
           standardGeneric("fisheye2pano_dl"))

#' @rdname fisheye2pano_dl
setMethod("fisheye2pano_dl",
          signature(x = "CanopyPhoto"),
          function (x, g, m, fill_na, fun)
          {

            red <- fisheye2pano_dl(x$Red, g, m, fill_na, fun)
            green <- fisheye2pano_dl(x$Green, g, m, fill_na, fun)
            blue <- fisheye2pano_dl(x$Blue, g, m, fill_na, fun)
            pano <- stack(red, green, blue)
            names(pano) <- c("Red", "Green", "Blue")
            pano

          }
)

#' @rdname fisheye2pano_dl
setMethod("fisheye2pano_dl",
          signature(x = "RasterLayer"),
          function (x, g, m, fill_na, fun)
          {

            if(!is.null(m)) x[!m] <- NA

            blue <- extractFeatures(x, g, fun)

            decode_label <- function(label) {
              sector_ID <- trunc(label / 1000)
              rings_ID <- label - sector_ID * 1000
              data.frame(sector_ID, rings_ID = max(rings_ID) - rings_ID)
            }

            xy <- decode_label(as.numeric(names(blue)))

            r <- matrix(NA, ncol = max(xy$sector_ID), nrow = max(xy$rings_ID))
            r <- raster(r)
            extent(r) <- extent(0, ncol(r), 0, nrow(r))

            cells <- cellFromXY(r, as.matrix(xy))
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
