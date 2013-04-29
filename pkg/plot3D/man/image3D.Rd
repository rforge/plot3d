\name{images in 3D frame}
\alias{image3D}
\title{
 Images in 3-D plots.
}
\description{
  \code{image3D} adds an \link{Image} in a 3-D plot. 
}
\usage{
image3D (x = NULL, y = NULL, z = NULL, ..., colvar = NULL, 
      phi = 40, theta = 40, col = jet.col(100), 
      NAcol = "white", border = NA, facets = TRUE,
      colkey = list(side = 4), panel.first = NULL,
      clim = NULL, clab = NULL, bty = "b",
      inttype = 1, add = FALSE, plot = TRUE)
}

\arguments{
  \item{x, y, z }{Matrix (2-D), vector, or one value containing the values 
    where the \link{Image} is to be plotted. 
    At least one of them should be one number, as this will determine where the 
    Image is to be plotted.
    If two are vectors vectors, the first vector should be of length equal to 
    \code{nrow(colvar)} and the second should be of length equal to 
    \code{ncol(colvar)}. 
    }
  \item{colvar }{The variable used for coloring. 
    }
  \item{col }{Color palette to be used for the \code{colvar} variable.
    }
  \item{NAcol }{Color to be used for \code{NA} values of \code{colvar}; default is ``white''. 
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
  \item{clab }{Only if \code{colkey = TRUE}, the label to be written on top of the
    color key. The label will be written at the same level as the main title.
    to lower it, \code{clab} can be made a vector, with the first values empty 
    strings. 
    } 
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable, used
    for the color key. Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{theta, phi }{The angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. 
    see \link{persp}.
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
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up (and if applicable, images or contours drawn) but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See example of \link{persp3D} and last example of \link{voxel3D}.
    } 
  \item{bty }{The type of the box, the default only drawing background panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{inttype }{The interpolation type to create the polygons, either 
    interpolating the \code{colvar} (\code{inttype = 1} or the \code{x, y, z}
    values (\code{inttype = 2}) - see \link{persp3D}.
    } 
  \item{add }{Logical. If \code{TRUE}, then the Image will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise the viewing transformation matrix is returned (as invisible). 
    } 
  \item{\dots}{additional arguments passed to the plotting methods. 
    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype, shade, ltheta, lphi}.      
    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.
    
    Also the arguments \code{lty, lwd} can be specified (when \code{border} is not \code{NA}).
    
    The arguments after \dots must be matched exactly.
    }
}
\details{
  \code{image3D} calls the \link{surf3D} function. The x, y, and z values are 
  expanded as a matrix. 
}
\value{
  Returns the viewing transformation matrix.
  See \link{trans3D}.
}
\seealso{
  \link{surf3D} for the function on which \code{image3D} is based.

  \link{Image} for \code{plot3D}s 2-D image function.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>
}
\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## images in x, y, z plane
## =======================================================================

 par(mfrow = c(2, 2))

# images in x, y, z plane
 image3D(y = seq(0, 1, 0.1), z = seq(0, 1, 0.1), x = 0.5, 
   col = "blue", xlim = c(0,1))
 image3D(x = seq(0, 1, 0.1), z = seq(0, 1, 0.1), y = 0.5, 
   add = TRUE, col = "red")
 image3D(x = seq(0, 1, 0.1), y = seq(0, 1, 0.1), z = 0.5, 
   add = TRUE, col = "green")
 colkey(col = c("green", "red", "blue"), clim = c(0.5, 3.5), 
   at = 1:3, labels = c("z", "y", "x"), add = TRUE)
#
 image3D(z = 100, colvar = volcano, zlim = c(0, 150),
   clab = c("height", "m"))
  
#
 image3D( x = 0.5, colvar = volcano, xlim = c(0, 1), 
   ylim = c(0, 1), zlim = c(0, 1))

 image3D( y = 0.5, colvar = volcano, add = TRUE)

#
 image3D( z = 1, colvar = volcano, 
   x = seq(0, 1, length.out = nrow(volcano)),
   y = seq(0, 1, length.out = ncol(volcano)), 
   xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 2))
 image3D(y = 2, colvar = volcano, add = TRUE, 
    x = seq(0, 1, length.out = nrow(volcano)),
    z = seq(1, 2, length.out = ncol(volcano)))
 image3D(x = 2, colvar = NULL, col = "orange", add = TRUE, 
    y = seq(0, 1, length.out = nrow(volcano)),
    z = seq(1, 2, length.out = ncol(volcano)))

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

                                                   