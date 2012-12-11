\name{3-D volume visualisation}
\alias{slice3D}
\alias{slicecont3D}
\alias{isosurf3D}
\alias{voxel3D}
\alias{createisosurf}
\alias{createvoxel}
\title{
 Functions for plotting 3-D volumetric data.
}
\description{
  
  \code{slice3D} plots a 3-d dataset with a color variable as slices or on surfaces.

  \code{slicecont3D} plots a 3-d dataset with a color variable as contours on slices.
  
  \code{isosurf3D} creates and plots isosurfaces from a 3-d dataset.

  \code{voxel3D} creates and plots isosurfaces as scatterpoints.

  Functions \code{createisosurf, createvoxel} create the isosurfaces (traingulations)
  and voxels (x, y, z) points from volumetric data.
}
\usage{
slice3D (x, y, z, colvar, ..., phi = 40, theta = 40,
         xs = min(x), ys = max(y), zs = min(z),
         col = jet.col(100), NAcol = "white", 
         border = NA, facets = TRUE, colkey = list(side = 4), 
         panel.first = NULL, clim = NULL, 
         clab = NULL, bty = "b", lighting = FALSE, 
         add = FALSE, plot = TRUE) 

slicecont3D (x, y, z, colvar, ..., phi = 40, theta = 40,
         xs = NULL, ys = NULL, zs = NULL, level = NULL,
         col = jet.col(100), NAcol = "white", 
         border = NA, facets = TRUE, 
         colkey = list(side = 4), panel.first = NULL,
         clim = NULL, clab = NULL, bty = "b", 
         dDepth = 0, add = FALSE, plot = TRUE) 

isosurf3D (x, y, z, colvar, ..., phi = 40, theta = 40, 
         level = mean(colvar), col = "grey", border = NA, 
         facets = TRUE, colkey = list(side = 4), 
         panel.first = NULL, clab = NULL, bty = "b", 
         ltheta = -135, lphi = 0, shade = 0.5, 
         lighting = FALSE, add = FALSE, plot = TRUE) 

createisosurf (x, y, z, colvar, level = mean(colvar))

voxel3D (x, y, z, colvar, ..., phi = 40, theta = 40, 
         level = mean(colvar), eps = 0.01, operator = "=", 
         col = jet.col(100), panel.first = NULL, 
         bty = "b", add = FALSE, plot = TRUE) 

createvoxel (x, y, z, colvar, level = mean(colvar), eps = 0.01,
             operator = "=")
}

\arguments{
  \item{x, y, z }{Vectors with x, y and z-values. 
    They should be of length equal to the first, second and 
    third dimension of \code{colvar} respectively.
    }
  \item{colvar }{The variable used for coloring. 
    It should be an array of dimension equal to 
    \code{c(length(x), length(y), length(z))}.
    It must be present.
    }
  \item{theta, phi }{the angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. see \link{persp}.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
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
  \item{colkey }{A logical, or a \code{list} with parameters for the color key (legend). 
    Parameters should be one of 
    \code{side, length, width, dist, shift, col.clab, cex.clab},
    and the axis parameters \code{at, labels, tick, line, pos, outer, font, lty, lwd,
    lwd.ticks, col.box, col.axis, col.ticks, hadj, padj, cex.axis, mgp, tck, tcl, las}.
    See \link{colkey}.
    The default is to draw the color key on side = 4, i.e. in the right margin.
    If \code{colkey} = \code{NULL} or \code{FALSE}, no color key legend will be added.
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
    for the color key. Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{xs, ys, zs }{Vectors or matrices.
    Vectors specify the positions in x, y or z where the slices (planes) are to be drawn.
    The values of \code{colvar} will be projected on these slices.
    Matrices specify a surface on which the \code{colvar} will be projected.
    } 
  \item{level }{The level(s) at which the \code{contour} will be generated or the
    isosurfaces generated. 
    
    There can be more than one level, but for \code{slicecont3D} too many will give a crowded view, and one is often best.
    For \code{isosurf3D}, the use of multiple values may need transparent colors (and it will be slow).
    For \code{voxel3D}, \code{level} should either be one number (if \code{operator}
    equals \code{'=', '<', '>'}) or two numbers (for \code{operator = '<>'}).
    } 
  \item{ltheta, lphi }{if finite values are specified for \code{ltheta} and 
    \code{lphi}, the surface is shaded as though it was being illuminated from 
    the direction specified by azimuth \code{ltheta} and colatitude \code{lphi}.
    See \link{persp}.
    }
  \item{shade }{the shade at a surface facet is computed as ((1+d)/2)^shade, 
    where d is the dot product of a unit vector normal to the facet and 
    a unit vector in the direction of a light source. 
    Values of shade close to one yield shading similar to a point light 
    source model and values close to zero produce no shading. 
    Values in the range 0.5 to 0.75 provide an approximation to daylight illumination.
    See \link{persp}.
    } 
  \item{lighting }{If \code{TRUE}, the facets will be illuminated, and colors may
    appear more bright.  See examples in \link{jet.col}.
    } 
  \item{bty }{The type of the box, the default only draws background panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{eps }{The voxel precision, only used when \code{operator = "="}.
    A point is selected if it closer than \code{eps*diff(range(colvar))} 
    to the required level.
    }
  \item{operator }{One of '=', '<', '>', '<>' for selection of points equal (within
    precision), larger or smaller than the required level or to be within an 
    interval.
    }
  \item{dDepth }{When a contour is added on an image, the image polygons may
    hide some contour segments. To avoid that, the viewing depth of the segments
    can be artificially decreased with the factor \code{dDepth} times the 
    \link{persp} argument \code{expand} (usually = 1), to make them 
    appear in front of the polygons. Too large values may create visible artifacts.
    See \link{contour3D}.
    }   
  \item{add }{Logical. If \code{TRUE}, then the slices, voxels or surfaces will be added to the current plot.
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

    For all functions, the arguments \code{lty, lwd} can be specified.
    
    The arguments after \dots must be matched exactly.
    }
}

\note{
  The \code{isosurf3D} function implements the Marching Cubes algorithm.
  It is quite slow, and should not be used for large values of x, y, z.

  A different (better?) implementation can be found in package \code{misc3d} - where it 
  is called \code{contour3d}.
}

\value{
  Returns the viewing transformation matrix,
  See \link{trans3D}
  
  Function \code{createisosurf} returns a three-columned matrix (x, y, z) with 
  triangles. A triangle is defined by three consecutive rows.
  
  Function \code{createvoxel} returns a list with the elements \code{x, y, z} 
  defining the points that are at a distance of less than 
  \code{eps*diff(range(colvar))} from the required \code{level}.
  Can be plotted with \link{scatter3D}.
}
\seealso{
  \link{persp}, for the function on which this implementation is based.

  \link{Oxsat} for another example of \code{slice3D}.

  \link{plotdev} for zooming, rescaling, rotating a plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par("mfrow")
 pmar <- par("mar")

## =======================================================================
## Simple slice3D examples
## =======================================================================

 par(mfrow = c(2, 2))
 x <- y <- z <- seq(-1, 1, by = 0.1)
 grid   <- mesh(x, y, z)
 colvar <- with(grid, x*exp(-x^2 - y^2 - z^2))

# default is just the panels
 slice3D  (x, y, z, colvar = colvar, theta = 60)

# contour slices
 slicecont3D (x, y, z, ys = seq(-1, 1, by = 0.5), colvar = colvar, 
           theta = 60, border = "black")
          
 slice3D  (x, y, z, xs = c(-1, -0.5, 0.5), ys = c(-1, 0, 1), 
           zs = c(-1, 0), colvar = colvar, 
           theta = 60, phi = 40)

## =======================================================================
## coloring on a surface
## =======================================================================

 XY <- mesh(x, y)
 ZZ <- XY$x*XY$y
 slice3D  (x, y, z, xs = XY$x, ys = XY$y, zs = ZZ, colvar = colvar, 
           lighting =  TRUE, lphi = 90, ltheta = 0)

## =======================================================================
## Specifying transparent colors
## =======================================================================

 par(mfrow = c(1, 1))
 x <- y <- z <- seq(-4, 4, by = 0.2)
 M <- mesh(x, y, z)

 R <- with (M, sqrt(x^2 + y^2 + z^2))
 p <- sin(2*R) /(R+1e-3)

\dontrun{
# This is very slow - alpha = 0.5 makes it transparent

 slice3D(x, y, z, colvar = p, col = jet.col(alpha = 0.5), 
         xs = 0, ys = c(-4, 0, 4), zs = NULL, d = 2) 
}

 slice3D(x, y, z, colvar = p, d = 2, theta = 60, border = "black",
         xs = c(-4, 0), ys = c(-4, 0, 4), zs = c(-4, 0))

## =======================================================================
## A section along a transect
## =======================================================================

 data(Oxsat)
 Ox <- Oxsat$val[,  Oxsat$lat > - 5 & Oxsat$lat < 5, ]
 slice3D(x = Oxsat$lon, z = -Oxsat$depth, y = 1:5, colvar = Ox, 
         ys = 1:5, zs = NULL, NAcol = "black", 
         expand = 0.4, theta = 45, phi = 45)

## =======================================================================
## isosurf3D example - rather slow
## =======================================================================

 par(mfrow = c(2, 2), mar  = c(2, 2, 2, 2))
 x <- y <- z <- seq(-2, 2, length.out = 15)
 xyz <- mesh(x, y, z)
 F <- with(xyz, log(x^2 + y^2 + z^2 + 
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

# use shading for level = 1 - show triangulation with border
 isosurf3D(x, y, z, F, level = 1, shade = 0.5, 
           col = "yellow", border = "orange")

# lighting for level - 2
 isosurf3D(x, y, z, F, level = 2, lighting = TRUE,
           lphi = 0, ltheta = 0, col = "blue", shade = NA)  
 
# three levels, transparency must be added afterwards with plotdev 
 isosurf3D(x, y, z, F, level = seq(0, 4, by = 2), 
   col = c("red", "blue", "yellow"), 
   shade = NA, plot = FALSE, clab = "F")  
 plotdev(lighting = TRUE, alpha = 0.2, theta = 0)

# use of creatisosurf
\dontrun{
 iso <- createisosurf(x, y, z, F, level = 2)
 head(iso)
}

## =======================================================================
## voxel3D example
## =======================================================================

# fast but needs high resolution grid
 x <- y <- z <- seq(-2, 2, length.out = 100)
 xyz <- mesh(x, y, z)
 F <- with(xyz, log(x^2 + y^2 + z^2 + 
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

 voxel3D(x, y, z, F, level = 4)


## =======================================================================
## rotation 
## =======================================================================

\dontrun{ # using createvoxel
 vox <- createvoxel(x, y, z, F, level = 4)
 scatter3D(vox$x, vox$y, vox$z, colvar = vox$y, 
   bty = "b", colkey = FALSE)
}

 plotdev(theta = 0)
 plotdev(theta = 45, phi = 0)
 plotdev(theta = 90, phi = 10)
 plotdev(theta = 135, phi = 10)

## =======================================================================
## voxel3D to show hypox sites
## =======================================================================

 par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
 Hypox <- createvoxel(Oxsat$lon, Oxsat$lat, Oxsat$depth[1:19], 
                      Oxsat$val[,,1:19], level = 40, operator = "<")

 panel <- function(pmat) {  # an image at the bottom
   Nx <- length(Oxsat$lon)
   Ny <- length(Oxsat$lat)
   M <- mesh(Oxsat$lon, Oxsat$lat) 
   xy <- trans3D(pmat = pmat, x = as.vector(M$x), y = as.vector(M$y), 
        z = rep(-1000, length.out = Nx*Ny)) 
   x <- matrix(nrow = Nx, ncol = Ny, data = xy$x)
   y <- matrix(nrow = Nx, ncol = Ny, data = xy$y)
   Bat <- Oxsat$val[,,1]; Bat[!is.na(Bat)] <- 1
   Image(x = x, y = y, z = Bat, NAcol = "black", col = "grey",
         add = TRUE, colkey = FALSE)
 }
   
 scatter3D(Hypox$x, Hypox$y, -Hypox$z, colvar = Hypox$cv, 
           panel.first = panel, pch = ".", bty = "b", 
           theta = 30, phi = 20, ticktype = "detailed",
           zlim = c(-1000,0), xlim = range(Oxsat$lon), 
           ylim = range(Oxsat$lat) )
           
# restore plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\references{
Lorensen, W.E. and Cline, H.E.,
Marching Cubes: a high resolution 3D surface reconstruction algorithm,
Computer Graphics, Vol. 21, No. 4, pp 163-169 (Proc. of SIGGRAPH), 1987.

\url{http://paulbourke.net/geometry/polygonise/}

}

\keyword{ hplot }

