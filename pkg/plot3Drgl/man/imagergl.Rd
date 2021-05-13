\name{image2Drgl}
\alias{image2Drgl}
\alias{contour2Drgl}
\alias{scatter2Drgl}
\alias{lines2Drgl}
\alias{points2Drgl}
\alias{text2Drgl}
\alias{arrows2Drgl}
\alias{segments2Drgl}
\alias{rect2Drgl}
\title{
 2-D image, contour, scatterplots, segments and arrows in rgl.
}
\description{
  \code{image2Drgl} plots an image in rgl. 
  
  \code{contour2Drgl} creates a contourplot in rgl. 

  \code{scatter2Drgl} creates a scatterplot (lineplot, points, ...) in rgl. 

  \code{points2Drgl} is shorthand for \code{scatter2Drgl(..., type = "p")}

  \code{lines2Drgl} is shorthand for \code{scatter2Drgl(..., type = "l")}


  \code{arrows2Drgl} and \code{segments3D} plot arrows and segments in rgl. 
  
  \code{text2Drgl} plots labels in rgl.
  
  These functions were implemented for their side effect that rgl plots can be
  zoomed, translocated, rectangular selections taken.
}
\usage{
image2Drgl (z, x = seq(0, 1, length.out = nrow(z)), 
          y = seq(0, 1, length.out = ncol(z)), ...,
          col = NULL, NAcol = "white", breaks = NULL, border = NA,
          facets = TRUE, colkey = NULL, contour = FALSE,
          smooth = FALSE, clim = NULL, clab = NULL, shade = NA,
          inttype = 1, dz = 0, add = FALSE)

contour2Drgl (z, x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), ...,
            col = NULL, colkey = NULL, clim = NULL, clab = NULL, dz = 0.1,
            add = FALSE)

scatter2Drgl (x, y, colvar = NULL, ...,
          col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
          clim = NULL, clab = NULL, CI = NULL, dz = 0.1, add = FALSE)

text2Drgl (x, y, labels, colvar = NULL, ...,
          col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
          clim = NULL, clab = NULL, dz = 0.1, add = FALSE)

arrows2Drgl (x0, y0, x1, y1, colvar = NULL, ...,
           col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
           clim = NULL, clab = NULL, type = "simple", dz = 0.1, add = FALSE)

segments2Drgl (x0, y0, x1, y1, colvar = NULL, ...,
           col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
           clim = NULL, clab = NULL, dz = 0.1, add = FALSE)

rect2Drgl (x0, y0, x1, y1, colvar = NULL, ...,
           col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
           clim = NULL, clab = NULL, dz = 0.1, add = FALSE)

lines2Drgl(x, y, ...)

points2Drgl(x, y, ...)

}

\arguments{
  \item{x, y }{Vectors with the x- and y- values. 
    }
  \item{z }{The variable used for coloring the image plot, or containing the 
    values to be plotted for the contour plot. 
    }
  \item{x0, y0 }{coordinates of points \emph{from} which to draw the arrows.  
    }
  \item{x1, y1 }{coordinates of points \emph{to} which to draw the arrows. 
    At least one must be supplied. 
    }
  \item{colvar }{The variable used for coloring the scatter plot or the arrows. 
    If \code{NULL}, then \code{col} will be used as such.
    }
  \item{labels }{The text to be written. A vector of length equal to length of
    x, y.
    }
  \item{col }{Color palette to be used for the \code{z} or \code{colvar} variable.
    If \code{colvar} is \code{NULL}, then the colors are used as such.
    }
  \item{NAcol }{Color to be used for \code{NA} values; default is ``white''. 
    }
  \item{breaks }{a set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
    }
  \item{border }{The color of the lines drawn around the surface facets.
    The default, \code{NA}, will disable the drawing of borders.
    }
  \item{facets }{If \code{TRUE}, then \code{col} denotes the color of the surface facets.
    If \code{FALSE}, then the surface facets are colored ``white'' and the \code{border}
    will be colored as specified by \code{col}. If \code{NA} then the facets will be
    transparent.
    }
  \item{shade }{the degree of shading of the surface facets.
    Values of shade close to one yield shading similar to a point light
    source model and values close to zero produce no shading.
    Creates the illusion of perspective.
    See \link{persp}.
    }
  \item{contour }{If \code{TRUE}, then a \link{contour} plot will be
    added to the image plot. 
    Also allowed is to pass a \code{list} with arguments for the 
    \link{contour2D} function. 
    }
  \item{smooth }{Logical, specifying whether Gouraud shading (smooth) 
    or flat shading should be used. (if \code{TRUE} then function 
    \link{cutrgl} will not work).
    }  
  \item{colkey }{A logical, NULL (default), or a list with parameters for the
    color key (legend). Not all arguments from the original \link{colkey} function
    from \code{plot3D} are supported. For instance, color keys will always be
    put on the 4th margin.
    }
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable used. 
    Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{clab }{Only if \code{colkey = TRUE}, the label to be written on top of the
    color key. The label will be written at the same level as the main title.
    to lower it, \code{clab} can be made a vector, with the first values empty
    strings.
    }
  \item{inttype }{The interpolation type to create the polygons, either
    interpolating the \code{z} (\code{inttype = 1} or the \code{x, y}
    values (\code{inttype = 2}) - see \link{persp3D}. 
    } 
  \item{CI }{A \code{list} with parameters and values for the confidence 
    intervals or \code{NULL}.
    If a list it should contain at least the item \code{x} or \code{y}. 
    Other parameters should be one of (with defaults):
    \code{alen = 0.01, lty = par("lty"), lwd = par("lwd"), col = NULL},
    to set the length of the arrow head, the line type and width, and the color.
    If \code{col} is \code{NULL}, then the colors of the scatter points are used.
    }     
  \item{type }{The type of the arrow head, one of \code{"simple"} 
   (the default, which uses R-function \link{arrows}) or
    \code{"triangle"}.
    }
  \item{dz }{The 'layer depth', The z-position is defined as 1 + dz. 
    }
  \item{add }{Logical. If \code{TRUE}, then the image, contour or points will be 
    added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{\dots}{additional arguments passed to the plotting methods. 
    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype, shade, ltheta, lphi}.      
    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.
    
    Also the arguments \code{lty, lwd} can be specified.
    
    The arguments after \dots must be matched exactly.
    }
}
\details{
  The first step in 2D rgl plotting consists in calling a 3-D function from package \code{plot3D}
  with argument \code{plot} set to \code{FALSE}.

  \code{image2Drgl} and \code{contour2Drgl} call the \link{image3D} and \link{contour3D}
  functions of R-package \code{plot3D}, with \code{colvar} equal to \code{z}. 
  Functions \code{scatter2Drgl} and \code{arrows2Drgl} call \link{scatter3D} and \link{arrows3D}.
  
  The z value argument to the 3-D functions is set equal to \code{1 + dz}; 
  For \code{contour3D}, \code{scatter3D} and \code{arrows3D}, it is by default equal to \code{1.1},
  while for \code{image3D} it is \code{1}.
  This way, contours, points, segments and arrows will be drawn on top of the image. 

  The next step is to create a 3-D rgl plot, by calling \link{plotrgl}.   
  After that, the viewing arguments are set equal to 
  \code{view3d(phi = 0, fov = 0)}, i.e. the plot is viewed at from the top.
  
  The actions of the mouse on the plots is to zoom (left, middle), and to move it (right key).
}
\seealso{
  \link{image3D}, \link{contour3D}, \link{scatter3D}, \link{segments3D}, 
  \link{text3D} and \link{arrows3D} 
   on which the functions are based.

  \link{image2D}, \link{contour2D}, \link{scatter2D},  \link{segments2D}, 
   \link{text2D}, \link{arrows2D} 
  for \code{plot3D}'s functions, to plot in ordinary R graphics.

  \link{cutrgl} for cutting a rectangular region from the 2D plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>
}
\examples{

## =======================================================================
## image and points
## =======================================================================
 image2Drgl(z = volcano, contour = TRUE, main = "volcano")
 scatter2Drgl(x = seq(0, 1, by = 0.2), y = seq(0, 1, by = 0.2), 
   cex = 3, add = TRUE)

\dontrun{
  cutrgl()    # select a rectangle
  uncutrgl()
}

## =======================================================================
## scatter points, and lines
## =======================================================================

 scatter2Drgl(cars[,1], cars[,2], xlab = "speed", ylab = "dist")
\dontrun{
  cutrgl()
}
 lc <- lowess(cars)
 scatter2Drgl(lc$x, lc$y, type = "l", add = TRUE, lwd = 4)
\dontrun{
  cutrgl()
  uncutrgl()
}

## =======================================================================
## confidence intervals
## =======================================================================
 x  <- sort(rnorm(10)) 
 y  <- runif(10)
 cv <- sqrt(x^2 + y^2)

 CI <- list(lwd = 2)
 CI$x <- matrix (nrow = length(x), ncol = 2, data = rep(0.125, 2*length(x)))
 scatter2D(x, y, colvar = cv, pch = 16, cex = 2, CI = CI)

 scatter2Drgl(x, y, colvar = cv, cex = 2, CI = CI)
 
## =======================================================================
## arrows
## =======================================================================

 arrows2Drgl(x0 = 100*runif(30), y0 = runif(30), x1 = 100*runif(30), 
    y1 = runif(30), length = 0.1*runif(30), col = 1:30, angle = 15:45, 
    type = c("simple", "triangle"), lwd = 2)

 x0 <- 1:30
 x1 <- 2:31
 arrows2Drgl(x0 = x0, y0 = sin(x0), x1 = x1, y1 = sin(x1), 
   colvar = x1, lwd = 2)
}
\keyword{ hplot }

                                                   