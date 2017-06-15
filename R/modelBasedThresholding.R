#' @title todo
#'
#' @description todo
#'
#' @param x todo
#' @param ... todo
#' @param z todo
#' @param path2photos todo
#' @param path4write todo
#' @param fun todo
#' @param model todo
#' @param mn todo
#' @param mx todo
#'
#' @return todo
#' @export modelBasedThresholding
#'
#' @examples #todo
setGeneric("modelBasedThresholding",
           function(x, ...) standardGeneric("modelBasedThresholding"))

setMethod("modelBasedThresholding",
          signature(x = "data.frame"),
          function (x, z, path2photos, path4write, fun, model = NULL, mn, mx, ...) {

              foo <- substr(path2photos, nchar(path2photos), nchar(path2photos))
              if (any(foo == c(letters, LETTERS))) stop("Use the slash path at the end of x.")

              foo <- substr(path2photos, nchar(path2photos), nchar(path4write))
              if (any(foo == c(letters, LETTERS))) stop("Use the slash path at the end of x.")

              stopifnot(names(x) == c("photo", "er2osAutoE", "aperture"))


              photos <- paste0(path2photos, x$photo)

              for (i in 1:length(photos)) {

                print(paste(i, "of", length(photos), ":", as.character(x$photo[i])))
                cp <- loadPhoto(photos[i], ...)

                foo <- function(z) fun(z, x$er2osAutoE[i], aperture(cp))

                thr <- calc(z, foo)


                bright <- calc(normalize(cp, mn, mx), mean)


                ## estimate the color of the sky
                ### find pure sky pixels
                cost <- function(x, y) {
                  cost_z <- function(x) 1 - (IQR(x) / IQR(z[], na.rm = TRUE)) # se premia mayor rango de Z
                  cost_w <- function(x) (x - 1)^2 # se penaliza que w se aleje de 1
                  mean(c(cost_z(x), cost_w(y)))
                }
                getCost <- function(w) {
                  pureSky <- bright > thr * w
                  cost(z[pureSky], w)
                }
                #ws <- data.frame(matrix(seq(0.5, 1.5, 0.1), nrow = 1))
                ws <- as.list(seq(0.5, 1.5, 0.1))
                w <- ws[which.min(lapply(ws, getCost))]

                pureSky <- bright > thr * as.numeric(w)

                ### calculate thr_blue
                blue <- normalize(subset(cp, "Blue"), mn, mx)
                model <- lm(blue[pureSky] ~ thr[pureSky])

                gray2blue <- function(x) coef(model)[1] + coef(model)[2] * x

                thr_blue <- calc(thr, gray2blue)

                songFun <- function(x) {
                  thr <- (216.542 / (1 + exp((-x + 1.422) / 1.160))) # esta es original del paper
                  thr/255
                }

                blueValue <- gray2blue(fun(0, x$er2osAutoE[i], x$aperture[i]))

                w <- songFun(x$er2osAutoE[i]) / blueValue
                bin <- bright > thr_blue * w

                # delta <- blueValue - songFun(x$er2osAutoE[i])
                # bin <- bright > thr_blue - delta

                file <- as.character(x$photo[i])
                substr(file, nchar(file) - 2, nchar(file)) <- "TIF"
                writeRaster(bin * 255, paste0(path4write, file), "GTiff", datatype = "INT1U", overwrite = TRUE)
              }

            }
          )
