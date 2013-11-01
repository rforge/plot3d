\name{persp3Drgl}
\alias{persp3Drgl}
\alias{ribbon3Drgl}
\alias{hist3Drgl}
\alias{scatter3Drgl}
\alias{points3Drgl}
\alias{lines3Drgl}
\alias{slice3Drgl}
\alias{slicecont3Drgl}
\alias{isosurf3Drgl}
\alias{voxel3Drgl}
\alias{surf3Drgl}
\alias{spheresurf3Drgl}
\alias{segments3Drgl}
\alias{image3Drgl}
\alias{contour3Drgl}
\alias{box3Drgl}
\alias{border3Drgl}
\alias{rect3Drgl}
\alias{text3Drgl}
\title{
 3-D plotting functions using rgl.
}
\description{
  Functions \code{persp3Drgl}, \code{ribbon3Drgl}, \code{hist3Drgl} produce perspective plots
  using rgl; they are similar to functions \link{persp3D}, \link{ribbon3D}, \link{hist3D} 
  from package \code{plot3D}.
  
  Functions \code{scatter3Drgl}, \code{points3Drgl}, \code{lines3D}, \code{segments3Drgl} produce scatter plots and line plots
  using rgl; they are similar to functions \link{scatter3D}, \link{points3D}, \link{lines3D}, \link{segments3D} from package \code{plot3D}.
  
  Functions \code{slice3Drgl}, \code{points3Drgl}, \code{isosurf3Drgl}, \code{voxel3Drgl} can 
  visualise volumetric (3D) data using rgl; 
  they are similar to functions \link{slice3D}, \link{slicecont3D}, \link{isosurf3D}, \link{voxel3D} from package \code{plot3D}.
  
  Functions \code{surf3Drgl}, \code{spheresurf3Drgl} produce surface plots
  using rgl; they are similar to functions \link{surf3D}, \link{spheresurf3D} from package \code{plot3D}.
  
  Functions \code{box3Drgl}, \code{border3Drgl}, \code{rect3Drgl}, \code{text3Drgl} produce boxes, rectangles, texts to 3D plots
  using rgl; they are similar to functions \link{box3D}, \link{border3D}, \link{rect3D}, \link{text3Drgl} from package \code{plot3D}.
  
  These functions were implemented for their sideeffect that rgl plots can be 
  zoomed, translocated, rectangular selections taken.
}
\usage{
persp3Drgl(...) 
ribbon3Drgl(...)
hist3Drgl(...)
scatter3Drgl(...)
points3Drgl(...)
lines3Drgl(...)
slice3Drgl(...)
slicecont3Drgl(...)
isosurf3Drgl(...)
voxel3Drgl(...) 
surf3Drgl(...)
spheresurf3Drgl(...)
segments3Drgl(...)
image3Drgl(...)
contour3Drgl(...)
box3Drgl(...)
border3Drgl(...)
rect3Drgl(...)
text3Drgl(...)
}

\arguments{
  \item{\dots }{arguments passed to the plotting methods of package \code{plot3D},
    or to the \link{plotrgl} method. 
    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype, shade, ltheta, lphi}.      
    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.
    }
}
\details{
  The first step in 3D rgl plotting consists in calling the corresponding 3-D function from package \code{plot3D}
  with argument \code{plot} set to \code{FALSE}.

  The next step is to create a 3-D rgl plot, by calling \link{plotrgl}.   
  
  The actions of the mouse on the plots is to zoom (left, middle), and to move it (right key).
}
\seealso{
  \link{plotdev} to plot first in ordinary R graphics and then in rgl

  for \link{plotrgl} to plot first in ordinary R graphics and then in rgl

  \link{cutrgl} for cutting a rectangular region from the rgl plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>
}
\examples{

## =======================================================================
## perspective plots
## =======================================================================
 persp3Drgl(z = volcano, contour = list(side = "zmax"))

# ribbon, in x--direction
 V <- volcano[seq(1, nrow(volcano), by = 5), 
              seq(1, ncol(volcano), by = 5)]  # lower resolution
 ribbon3Drgl(z = V, ticktype = "detailed")
 hist3Drgl(z = V, col = "grey", border = "black", lighting = TRUE)
 
\dontrun{
  cutrgl()    # select a rectangle
  uncutrgl()
}

## =======================================================================
## scatter points
## =======================================================================

 with(quakes, scatter3Drgl(x = long, y = lat, z = -depth, 
   colvar = mag, cex = 3))

 plotdev()   # plots same on oridinary device...
 
## =======================================================================
## 3D surface
## =======================================================================

 M  <- mesh(seq(0, 2*pi, length.out = 50), 
            seq(0, 2*pi, length.out = 50))
 u  <- M$x ; v  <- M$y

 x  <- sin(u)
 y  <- sin(v)
 z  <- sin(u + v)

# alpha makes colors transparent
 surf3Drgl(x, y, z, colvar = z, border = "black", smooth  = TRUE, 
   alpha = 0.2)

## =======================================================================
## volumetric data
## =======================================================================
 x <- y <- z <- seq(-4, 4, by = 0.2)
 M <- mesh(x, y, z)

 R <- with (M, sqrt(x^2 + y^2 + z^2))
 p <- sin(2*R) /(R+1e-3)

 slice3Drgl(x, y, z, colvar = p, col = jet.col(alpha = 0.5), 
         xs = 0, ys = c(-4, 0, 4), zs = NULL, d = 2) 

}
\keyword{ hplot }

                                                   