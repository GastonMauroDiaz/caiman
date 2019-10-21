.makeF8single <- function(a, ...) { # single layer output

  function(x, filename = "", ...) {
    # code from raster-package vignette
    # "Writing functions for large raster files"
    # function referred as f8
    out <- raster(x)
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
      filename <- rasterTmpFile()
    }
    if (filename != '') {
      out <- writeStart(out, filename, ...)
      todisk <- TRUE
    } else {
      vv <- matrix(ncol=nrow(out), nrow=ncol(out))
      todisk <- FALSE
    }

    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)

    if (todisk) {
      for (i in 1:bs$n) {
        v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        v <- a(v, ...) # new code
        out <- writeValues(out, v, bs$row[i])
        pbStep(pb, i)
      }
      out <- writeStop(out)
    } else {
      for (i in 1:bs$n) {
        v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        v <- a(v, ...) # new code
        cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
        vv[,cols] <- matrix(v, nrow=out@ncols)
        pbStep(pb, i)
      }
      out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
  }
}

.makeF8multi <- function(a, ...) { # multi layer output

  function(x, filename = "", ...) {
    # based on code from raster-package vignette
    # "Writing functions for large raster files"
    # function referred as f8
    out <- brick(x) #
    big <- ! canProcessInMemory(out, 3)
    filename <- trim(filename)
    if (big & filename == '') {
      filename <- rasterTmpFile()
    }
    if (filename != '') {
      out <- writeStart(out, filename, ...)
      todisk <- TRUE
    } else {
      vv <- array(dim = c(ncol(out), nrow(out), nlayers(out)))
      todisk <- FALSE
    }

    bs <- blockSize(x)
    pb <- pbCreate(bs$n, ...)

    if (todisk) {
      for (i in 1:bs$n) {
        v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        v <- a(v, ...)
        out <- writeValues(out, v, bs$row[i])
        pbStep(pb, i)
      }
      out <- writeStop(out)
    } else {
      for (i in 1:bs$n) {
        v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        v <- a(v, ...)
        cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
        vv[,cols,] <- array(v, dim=c(bs$nrows[i],nrow=out@ncols, nlayers(x)))
        pbStep(pb, i)
      }
      out <- setValues(out, as.vector(vv))
    }
    pbClose(pb)
    return(out)
  }
}
