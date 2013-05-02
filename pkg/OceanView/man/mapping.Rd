\name{Map and extract data}
\alias{map}
\alias{map.matrix}
\alias{map.array}
\alias{extract}
\alias{extract.matrix}
\alias{extract.array}
\alias{transect}
\alias{mapsigma}
\alias{mapsigma.matrix}
\alias{mapsigma.array}
\alias{transectsigma}

\title{
   Functions for remapping and extracting from 2-D or 3D data
}
\description{
  S3 functions \code{map} map a variable ( \code{var}) from a \code{matrix} or array 
  with x, y (and z) coordinates 
  to a matrix or array with coordinates given by xto, yto (and zto).
  \code{x, y, z, xto, yto} and \code{zto} are all vectors.
  The functions extrapolate to all combinations of xto, yto and zto. 
  Simple 2-D linear interpolation is used. 
  Result is a matrix or array.

  S3-functions \code{extract} map a variable (\code{var}) from a matrix with (x, y) coordinates
   or from an array with (x, y, z) coordinates to the xy 
   coordinate \emph{pair} \code{xyto} or xyz coordinate \emph{triplets} \code{xyzto}
   by linear interpolation. Result is a vector.

  \code{transect} takes a cross section across an array (\code{var}). 
  Result is a matrix.

  \code{mapsigma} maps a matrix \code{var} containing values defined at (x, sigma) coordinates 
  to (x, depth) coordinates.
  The depths corresponding to the sigma values in \code{var} are in input matrix sigma.
  The result is a matrix which will contain \code{NA}s where the depth-coordinates
  extend beyond the sigma values.

}
\usage{
  map        (var, ...)
  
  \method{map}{matrix}(var, x, y, xto = NULL, yto = NULL, 
          resfac = 1, ...)

  \method{map}{array}(var, x, y, z, xto = NULL, yto = NULL, zto = NULL, 
          resfac = 1, ...)

  extract    (var, ...)

  \method{extract}{matrix}(var, x, y, xyto, ...)

  \method{extract}{array}(var, x, y, z, xyzto, ...)

  transect(var, x, y, z, to, margin = "xy", ...)

  mapsigma   (var, ...)
  
  \method{mapsigma}{matrix}(var = NULL, sigma, x = NULL,  
    depth = NULL, numdepth = NULL, xto = NULL, resfac = 1, ...)
    
  \method{mapsigma}{array}(var = NULL, sigma, x = NULL, y = NULL, 
    depth = NULL, numdepth = NULL, xto = NULL, yto = NULL, 
    resfac = 1, ...)

  transectsigma(var = NULL, sigma, x, y, to, depth = NULL, 
                numdepth = NULL, resfac = 1, ...)

}

\arguments{
  \item{var }{Matrix or array with values to be mapped to lower or higher resolution (\code{map}), 
    whose values have to be extracted (\code{extract}, \code{transect}),
    or which has to be mapped to the depth coordinates (\code{mapsigma}).
    For \code{transect} and \code{transectsigma}, \code{var} has to be an array.
    }
  \item{x }{Vector with original x-coordinates of the matrix or array \code{var} to be mapped.
    Length should be = first dimension of \code{var}.  
    }
  \item{y }{Vector with original y-coordinates of the matrix or array \code{var} to be mapped. 
    Length should be = second dimension of \code{var}.  
    }
  \item{z }{Vector with original z-coordinates of the array \code{var} to be mapped. 
    Length should be = third dimension of \code{var}.  
    }
  \item{xto }{Vector with x-coordinates to which \code{var} should be mapped.
    The elements in \code{xto} should be embraced by the elements in \code{x}
    (it is not allowed to extrapolate outside of the region).
    If \code{NULL} then the range of \code{x} is covered, with the same number of points.
    }
  \item{yto }{Vector with y-coordinates to which \code{var} should be mapped. 
    The elements in \code{yto} should be embraced by the elements in \code{y}
    (it is not allowed to extrapolate outside of the region).
    If \code{NULL} then the range of \code{y} is covered, with the same number of points.
    }
  \item{zto }{Vector with z-coordinates to which \code{var} should be mapped.
    The elements in \code{zto} should be embraced by the elements in \code{z}
    (it is not allowed to extrapolate outside of the region).
    If \code{NULL} then the range of \code{z} is covered, with the same number of points.
    }
    
  \item{xyto }{Two-columned matrix, with first and second column specifying the 
    x- respectively y-coordinates to which the matrix \code{var} should be mapped. 
    The elements should be embraced by the elements in \code{x} (first column)
    and \code{y} (second column)
    (it is not allowed to extrapolate outside of the region).
    }
  \item{xyzto }{Three-columned matrix, specifying the x-, y- and z-coordinates 
    to which the array \code{var} should be mapped. 
    The elements should be embraced by the elements in \code{x}, \code{y} 
    and \code{z}
    (it is not allowed to extrapolate outside of the region).
    }
  \item{to }{Two-columned matrix, specifying the values along the \code{margin} coordinates
    of the transect to be taken on the array \code{var}. 
    The elements should be embraced by the elements in \code{x}, \code{y} 
    and \code{z}
    (it is not allowed to extrapolate outside of the region).
    }
  \item{margin }{String with the names of the coordinates in the matrix \code{to}, 
    and along which the transect is to be taken on the array \code{var}. 
    One of \code{"xy", "xz", "yz"}. If \code{"xy"}, then the first and second 
    column in input \code{to} represent \code{x} and \code{y} values respectively,
    and the transect will select all \code{z} values corresponding with these
    inputs.
    }
  \item{sigma }{The sigma coordinates, a matrix or array with the same dimension 
    as \code{var}. The sigma coordinates should 
    refer to the second or third dimension in \code{var} for a \code{matrix}
    and \code{array} respectively.
    }
  \item{depth }{The depth (often referred to as 'z') coordinates to which matrix 
    \code{var} has to be mapped. 
    If \code{NULL} then \code{seq(min(sigma), max(sigma), length.out = numdepth)}. 
    }
  \item{numdepth }{Only used when \code{depth= NULL}, the length of the \code{depth} vector to 
    which the matrix \code{var} has to be mapped. 
    If \code{NULL} then the length will be equal to \code{ncol(var)} (if \code{var} is a matrix),
    or \code{dim(var)[3]} in case \code{var} is an array. 
    }
  \item{resfac }{Resolution factor, one value or a vector of two or three numbers, 
    for the x, y- and z- values respectively. 
    A value > 1 will increase the resolution. For instance, if \code{resfac} 
    equals \code{3} then for each adjacent pair of x- and y- values, 
    \code{var} will be interpolated to two intermediary points. 
    This uses simple linear interpolation. 
    If \code{resfac} is one number then the resolution will be increased similarly 
    in x, y- and z-direction. 
    \code{resfac} is overruled if \code{xto}, \code{yto} or \code{zto} is specified.
    }
  \item{... }{any other arguments.}  
}
\details{
  S3-function \code{map} can be used to increase or decrease 
  the resolution of a matrix or array \code{var}, or to zoom in on a certain area. 
  It returns an object of the same class as \code{var} (i.e. a matrix or array).

  S3-function \code{transect} takes a line transect or slice from a matrix and array respectively
  It returns an object with one dimension less than \code{var}
    
  S3-function \code{extract} returns a vector with one value
  corresponding to each row in \code{xyto} or \code{xyzto}.
    
  \code{mapsigma} should be used to make images from data that are in sigma 
  coordinates
}

\value{
  \code{map.matrix}: 
    \itemize{
     \item{var }{The higher or lower resolution matrix 
          with dimension = c(length(xto), length(yto)).
          }
     \item{x }{The x coordinates, corresponding to first dimension of \code{var}
          (input argument \code{xto}).
          }
     \item{y }{The y coordinates, corresponding to second dimension of \code{var}
          (input argument \code{yto}).
          }
    }

  \code{map.array}: 
    \itemize{
     \item{var }{The higher or lower resolution array, 
          with dimension = c(length(xto), length(yto), length(zto)).
          }
     \item{x }{The x coordinates, corresponding to first dimension of \code{var}
          (input argument \code{xto}).
          }
     \item{y }{The y coordinates, corresponding to second dimension of \code{var}
          (input argument \code{yto}).
          }
     \item{z }{The z coordinates, corresponding to third dimension of \code{var}
          (input argument \code{zto}).
          }
    }

  \code{extract.matrix}:
    \itemize{
     \item{var }{The higher or lower resolution object, 
          with dimension = c(nrow(xyto), dim(var)[3]).
          }
     \item{xy }{The pairs of (x,y) coordinates
          (input argument \code{xyto}).
          }
    }
  
  \code{extract.array}:
    \itemize{
     \item{var }{The higher or lower resolution object, 
          with dimension = c(nrow(xyzto), dim(var)[3]).
          }
     \item{xyz }{The triplets of (x,y,z) coordinates
          (input argument \code{xyzto}).
          }
    }

  \code{mapsigma}: 
    \itemize{
     \item{var }{A matrix with columns in depth-coordinates.
          }
     \item{x }{The 'x'-coordinates referring to the rows of \code{var}.
          }
     \item{depth }{The depth-coordinates, also known as 'z'-coordinates, 
          referring to the columns of \code{var}.
          }
    }
}
\seealso{
  \link{Sylt3D} for other examples of mapping.

}

\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Use map to add more detail to a slice3D plot
## =======================================================================

 par(mfrow = c(1, 1))
 x <- y <- z <- seq(-4, 4, by = 0.5)
 M <- mesh(x, y, z)

 R <- with (M, sqrt(x^2 + y^2 + z^2))
 p <- sin(2*R) /(R+1e-3)

 slice3D(x, y, z, ys = seq(-4, 4, by = 2), theta = 85, 
   colvar = p, pch = ".", clim = range(p))

 xto <- yto <- zto <- seq(-1.2, 1.2, 0.2)
 Res <- map (p, x, y, z, xto, yto, zto)

# expand grid for scatterplot
 Mt  <- mesh(Res$x, Res$y, Res$z)

 scatter3D(x = Mt$x, y = Mt$y, z = Mt$z, colvar = Res$var, 
   pch = ".", add = TRUE, cex = 3, clim = range(p))

# same in rgl:
 \dontrun{
   plotrgl()
 }

# extract specific values from 3-D data
 xyzto <- matrix(nrow = 2, data = c(1, 1, 1, 2, 2, 2), byrow = TRUE)
 extract(var = p, x, y, z, xyzto = xyzto)

# a transect
 to <- cbind(seq(-4, 4, length.out = 20), seq(-4, 4, length.out = 20))
 image2D( transect(p, x, y, z, to = to)$var)

## =======================================================================
## change the resolution of a 2-D image
## =======================================================================

 par(mfrow = c(2, 2))
 nr <- nrow(volcano)
 nc <- ncol(volcano)

 x  <- 1 : nr
 y  <- 1 : nc
 image2D(x = x, y = y, volcano, main = "original")

# increasing the resolution
 x2 <- seq(from = 1, to = nr, by = 0.25)
 y2 <- seq(from = 1, to = nc, by = 0.25)

 VOLC1 <- map(volcano, x = x, y = y, xto = x2, yto = y2)$var
 image2D(x = x2, y = y2, z = VOLC1, main = "high resolution")

# low resolution
 xb <- seq(from = 1, to = nr, by = 2)
 yb <- seq(from = 1, to = nc, by = 3)
 VOLC2 <- map(volcano, x, y, xb, yb)$var
 image2D(VOLC2, main = "low resolution")

# zooming in high resolution
 xc <- seq(10, 40, 0.1)
 yc <- seq(10, 40, 0.1)

 VOLC3 <- map(volcano,x, y, xc, yc)$var
 image2D(VOLC3, main = "zoom")

# Get one value or a grid of values
 map(volcano, x, y, xto = 2.5,     yto = 5)
 map(volcano, x, y, xto = c(2, 5), yto = c(5, 10))

# Specific values
 extract(volcano, x, y, xyto = cbind(c(2, 5), c(5, 10)))
 
## =======================================================================
## take a cross section or transect of volcano
## =======================================================================
 
 par(mfrow = c(2, 1)) 
 image2D(volcano, x = 1:nr, y = 1:nc)
 xyto <- cbind(seq(from = 1,  to = nr, length.out = 20),
               seq(from = 20, to = nc, length.out = 20))
 points(xyto[,1], xyto[,2], pch = 16)

 (Crossection <- extract (volcano, x = 1:nr, y = 1:nc,
                             xyto = xyto))

 scatter2D(xyto[, 1], Crossection$var, colvar = Crossection$var, 
   type = "b", cex = 2, pch = 16)

## =======================================================================
##  mapsigma: changing from sigma coordinates into depth-coordinates
## =======================================================================

 par(mfrow = c(2, 2))
 var <- t(matrix (nrow = 10, ncol = 10, data = 1:10))
 image2D(var, ylab = "sigma", main = "values in sigma coordinates",
       clab = "var")

 # The depth at each 'column' 
 Depth <- approx(x = 1:5, y = c(10, 4, 5, 6, 4), 
                 xout = seq(1,5, length.out = 10))$y

# Sigma coordinates
 sigma <- t(matrix(nrow = 10, ncol = 10, data = Depth, byrow = TRUE) * 
                seq(from = 0, to = 1, length = 10))
 matplot(sigma, type = "l", main = "sigma coordinates", 
         xlab = "sigma", ylab = "depth", ylim = c(10, 0))

# Mapping to the default depth coordinates
 varz <- mapsigma(var = var, sigma = sigma)
 image2D(varz$var, y = varz$depth, NAcol = "black", ylim = c(10, 0), 
       clab = "var", ylab = "depth", 
       main = "depth-coord, low resolution")

# Mapping at higher resolution of depth coordinates
 varz <- mapsigma(var, sigma = sigma, resfac = 10)
 image2D(varz$var, y = varz$depth, NAcol = "black", ylim = c(10, 0), 
       clab = "var", ylab = "depth", 
       main = "depth-coord, high resolution")

# restore plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

