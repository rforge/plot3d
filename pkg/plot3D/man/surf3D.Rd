\name{3-D surfaces}
\alias{surf3D}
\alias{spheresurf3D}
\title{
 Functions for plotting 3 dimensional shapes
}
\description{
  \code{surf3D} plots a surface in 3-d with a color variable.

  \code{spheresurf3D} plots a colored image on a sphere.
}
\usage{
surf3D (x, y, z, colvar = z, ..., phi = 40, theta = 40,
        col = jet.col(100), NAcol = "white", border = NA, 
        facets = TRUE, colkey = list(side = 4), 
        panel.first = NULL,
        clim = NULL, clab = NULL, bty = "n", 
        lighting = FALSE, inttype = 1, 
        add = FALSE, plot = TRUE)

spheresurf3D (colvar = matrix(nrow = 50, ncol = 50, data = 1:50, byrow = TRUE), 
        ..., phi = 0, theta = 0,
        col = jet.col(100), NAcol = "white", border = NA, 
        facets = TRUE, contour = FALSE, 
        colkey = list(side = 4), resfac = 1,
        panel.first = NULL, clim = NULL, clab = NULL, bty = "n", 
        lighting = FALSE, inttype = 1, full = FALSE,
        add = FALSE, plot = TRUE) 
}

\arguments{
  \item{x, y, z }{Matrices with x, y and z-values that define the surfaces to
    be colored. They should be of the same dimension
    as \code{colvar}.
    }
  \item{colvar }{The variable used for coloring. If a matrix, it should be of the same 
    dimension as \code{x, y, z}.
    Values of \code{NULL}, \code{NA}, or \code{FALSE}
    will toggle off coloration according to \code{colvar}. This gives good results
    only if \code{border} is given a color or a shade is used.
    }
  \item{theta, phi }{the angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. see \link{persp}.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "grey". 
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
  \item{border }{The color of the lines drawn around the surface facets.
    The default, \code{NA}, will disable the drawing of borders.
    }
  \item{facets }{If \code{TRUE}, then \code{col} denotes the color of the surface facets. 
    If \code{FALSE}, then the surface facets are colored ``white'' and the \code{border} 
    (if \code{NA}) will be colored as specified by \code{col}. 
    If \code{NA} then the facets will be transparent. 
    It is usually faster to draw with \code{facets = FALSE}.
    }
  \item{contour }{If \code{TRUE}, then a \link{contour} plot will be 
    added to the image plot, unless \code{x, y} are a matrix. 
    Also allowed is to pass a \code{list} with arguments for the 
    \link{contour} function. 
    }
  \item{colkey }{A logical, or a \code{list} with parameters for the color key (legend). 
    Parameters should be one of 
    \code{side, length, width, dist, shift, col.clab, cex.clab},
    and the axis parameters \code{at, labels, tick, line, pos, outer, font, lty, lwd,
    lwd.ticks, col.box, col.axis, col.ticks, hadj, padj, cex.axis, mgp, tck, tcl, las}.
    See \link{colkey}.
    The default is to draw the color key on side = 4, i.e. in the right margin.
    If \code{colkey} = \code{NULL} or \code{FALSE}, no color key legend will be added.
    } 
  \item{resfac }{Resolution factor, one value or a vector of two numbers, for 
    the x and y- values respectively. A value > 1 will increase the 
    resolution. For instance, if \code{resfac} equals \code{3} then for each 
    adjacent pair of x- and y-values, z will be interpolated to two intermediary points. 
    This uses simple linear interpolation. If \code{resfac} is one number then
    the resolution will be increased similarly in x and y-direction.  
    }
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See last example of \link{persp3D}.
    } 
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE}, 
    the label to be written on top of the color key. 
    The label will be written at the same level as the main title.
    To lower it, \code{clab} can be made a vector, with the first values empty 
    strings. 
    } 
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable, used
    for the color key.  Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{bty }{The type of the box, the default is to draw no box.
    See \link{perspbox}.
    }
  \item{lighting }{If \code{TRUE}, the facets will be illuminated, and colors may
    appear more bright.  See examples in \link{jet.col}.
    } 
  \item{inttype }{The interpolation type to create the polygons, either 
    interpolating the \code{colvar} (\code{inttype = 1}) or the \code{x, y, z}
    values (\code{inttype = 2}) - see details of \link{persp3D}.
    } 
  \item{full }{Logical. If \code{TRUE}, the full sphere will be drawn, including the 
    invisible part. If \code{FALSE} only the visible half will be drawn (faster). 
    } 
  \item{add }{Logical. If \code{TRUE}, then the surfaces will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
     otherwise the viewing transformation matrix is returned (as invisible). 
  } 
  \item{\dots}{Additional arguments passed to the plotting methods. 
    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype, shade, ltheta, lphi}.      
    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.
    The arguments after \dots must be matched exactly.
    }
}

\value{
 Returns the viewing transformation matrix,
  See \link{trans3D}
}
\seealso{
  \link{persp} for the function on which this implementation is based.

  \link{jet.col}, \link{plotdev} for other examples of \code{surf3D}.

  \link{plotdev} for zooming, rescaling, rotating a plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\details{
Function \code{spheresurf3D} is a projection on a sphere with radius 1.
This means that the x- y- and z- axes range from [-1, 1].
}
\examples{
# save plotting parameters
 pm   <- par("mfrow")
 pmar <- par("mar")

 par(mar = c(1, 1, 1, 1))

## =======================================================================
## A three-dimensional shape 
## (ala http://docs.enthought.com/mayavi/mayavi/mlab.html)
## =======================================================================

 par(mfrow = c(2, 2))
# create grid matrices
 X       <- seq(0, pi, length.out = 100)
 Y       <- seq(0, 2*pi, length.out = 100)
 M       <- mesh(X, Y)
 phi     <- M$x
 theta   <- M$y

# x, y and z grids
 r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
 x <- r * sin(phi) * cos(theta)
 y <- r * cos(phi)
 z <- r * sin(phi) * sin(theta)

# full colored image
 surf3D(x, y, z, colvar = y, colkey = FALSE, shade = 0.5,
        box = FALSE, theta = 60)

# same, but just facets
 surf3D(x, y, z, colvar = y, colkey = FALSE, box = FALSE, 
        theta = 60, facets = FALSE)

# with colors and border, AND increasing the size
# (by reducing the x- y and z- ranges
 surf3D(x, y, z, colvar = y, colkey = FALSE, box = FALSE, 
        theta = 60, border = "black", xlim = range(x)*0.8, 
        ylim = range(y)*0.8, zlim = range(z)*0.8)

# Now with one color and shading
 surf3D(x, y, z, box = FALSE,
        theta = 60, col = "lightblue", shade = 0.9)

\dontrun{ # rotation
  for (angle in seq(0, 360, by = 10))
    plotdev(theta = angle)

}

## =======================================================================
## Several other shapes 
## http://xahlee.info/surface/gallery.html
## =======================================================================

 par(mfrow = c(2, 2)) 
 # Shape 1
 M  <- mesh(seq(0,  6*pi, length.out = 100), 
            seq(pi/3, pi, length.out = 100))
 u  <- M$x ; v <- M$y

 x <- u/2 * sin(v) * cos(u)
 y <- u/2 * sin(v) * sin(u)
 z <- u/2 * cos(v)

 surf3D(x, y, z, colvar = z, colkey = FALSE, box = FALSE, phi = 50)

# Shape 2: add border
 M  <- mesh(seq(0, 2*pi, length.out = 100), 
            seq(0, 2*pi, length.out = 100))
 u  <- M$x ; v  <- M$y

 x  <- sin(u)
 y  <- sin(v)
 z  <- sin(u + v)

# with default box type that has only back panels (bty)
 surf3D(x, y, z, colvar = z, border = "black", 
        colkey = FALSE)

# shape 3: uses same mesh, other perspective (d >1)
 x <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*cos(v)
 y <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*sin(v)
 z <- sin(v/2)*sin(u) + cos(v/2)*sin(2*u)

 surf3D(x, y, z, colvar = z, colkey = FALSE, d = 2, facets = FALSE)

# shape 4: more complex colvar
 M  <- mesh(seq(-13.2, 13.2, length.out = 50), 
            seq(-37.4, 37.4, length.out = 50))
 u  <- M$x   ; v <- M$y

 b <- 0.4; r <- 1 - b^2; w <- sqrt(r)
 D <- b*((w*cosh(b*u))^2 + (b*sin(w*v))^2)
 x <- -u + (2*r*cosh(b*u)*sinh(b*u)) / D
 y <- (2*w*cosh(b*u)*(-(w*cos(v)*cos(w*v)) - sin(v)*sin(w*v))) / D
 z <- (2*w*cosh(b*u)*(-(w*sin(v)*cos(w*v)) + cos(v)*sin(w*v))) / D

 surf3D(x, y, z, colvar = sqrt(x + 8.3), colkey = FALSE, 
        theta = 10, border = "black", box = FALSE)
 box()

## =======================================================================
## A sphere, with box type with grid lines
## =======================================================================

 par(mar = c(2, 2, 2, 2))
 par(mfrow = c(1, 1))
 M  <- mesh(seq(0, 2*pi, length.out = 100), 
            seq(0,   pi, length.out = 100))
 u  <- M$x ; v  <- M$y

 x <- cos(u)*sin(v)
 y <- sin(u)*sin(v)
 z <- cos(v)

 colvar <- sin(u*6) * sin(v*6)

 surf3D(y, x, z, colvar = colvar, phi = 0, bty = "b2", 
        lighting = TRUE, ltheta = 40)

## =======================================================================
## Function spheresurf3D
## =======================================================================

 par(mfrow = c(2, 2))
 spheresurf3D()
 
# true ranges are [-1, 1]; set limits to [-0.8, 0.8] to make larger plots
 lim <- c(-0.8, 0.8)
 spheresurf3D(colkey = FALSE, xlim = lim, ylim = lim, zlim = lim)

 spheresurf3D(bty = "b", ticktype = "detailed", phi = 50)
 spheresurf3D(colvar = matrix(nrow = 30, data = runif(900)))
 
## =======================================================================
## Images on a sphere
## =======================================================================

 par(mfrow = c(1, 1), mar = c(1, 1, 1, 3))

 AA <- Hypsometry$z; AA[AA<=0] <- NA
 
 lim <- c(-0.8, 0.8)

# log transformation of color variable
 spheresurf3D(AA, NAcol = "black", theta = 90, phi = 30, box = FALSE,
   xlim = lim, ylim = lim, zlim = lim, log = "c")

# restore plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\keyword{ hplot }

