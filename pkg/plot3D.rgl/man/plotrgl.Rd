\name{Plot in rgl window}
\alias{plotrgl}
\title{
  Plot 3D graphs in rgl window. 
}
\description{
  \code{plotrgl} plots objects created with functions from package \code{plot3D} 
  in an rgl window. 
}
\usage{
plotrgl (lighting = FALSE, new = TRUE, add = FALSE, smooth = FALSE, ...) 
}

\arguments{
  \item{lighting }{Logical, when \code{TRUE} will add light.
    Default is with lighting toggled off; this is similar to shading
    }
  \item{new }{Logical, when \code{TRUE} will open a new window. 
    When \code{FALSE} will start a new plot in the same window.
    Is overruled (to \code{FALSE}) if \code{add} is \code{TRUE}.
    }
  \item{add }{Logical, when \code{TRUE} will add to the current plot.
    }
  \item{smooth }{Logical, specifying whether Gouraud shading (smooth) 
    or flat shading should be used. See \link{material3d} from R-package \code{rgl}.
    (note: if \code{TRUE} then \link{cutrgl} will not work). 
    This only affects images.
    }  
  \item{... }{Any argument to the \code{rgl} functions, e.g arguments from
    \link{par3d}, \link{open3d} or \link{material3d}.
    }
}

\seealso{
  \link{material3d}, \link{par3d} for \code{rgl} arguments that can be passed to 
  the function.

  A similar function, \link{plotdev}, from package \code{plot3D} plots the
  3D graphs to the current device (other than a rgl-device). 

  Any function of package \code{plot3D}: see e.g. help files of
  \link{persp3D}, \link{scatter3D}, \link{arrows3D}, \link{slice3D}, \link{surf3D}.
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\note{
 Arrows are best reproduced with argument \code{type} from the \link{arrows3D} 
 function (package \code{plot3D}) set equal to \code{"cone"}, although this does
 not always work well (there is probably a flaw in how it is implemented).
 Another option is to use \code{type = "triangle"}, which simply maps the arrows 
 on the xy-plane, ignoring the z-axis. 
 In this case, only a view from above (phi = 0) will produce symmetric arrowheads
 (when \code{scale = TRUE} and \code{expand = 1}).

 The translation of \link{scatter3D} ignores the \code{pch} argument but displays
 all symbols as squares.  

 The color key is not (cannot be) plotted in rgl.
 
 In \code{rgl}, both \code{lty} and \code{lwd} have to be one number. 
 For \code{lwd}, this has been overruled, i.e. line widths can be a vector. 
 It is still not possible to use different line types in one type of object. 
}

\examples{
# save plotting parameters
 pm <- par("mfrow")
 pmar <- par("mar")

## =======================================================================
## Composite image and contour in 3D
## =======================================================================
# plot it to traditional window: 
 persp3D(z = volcano, contour = list(side = "zmax"))

 plotrgl(new = TRUE) # new window
 plotrgl(new = FALSE, lighting = TRUE) 
 plotrgl(new = FALSE, lighting = TRUE, smooth = TRUE)
  
## =======================================================================
## hist3D
## =======================================================================

 VV   <- volcano[seq(1, 87, 10), seq(1, 61, 10)]

# transparent colors
 hist3D(z = VV, scale = FALSE, expand = 0.01,
   col = jet.col(100, alpha = 0.3),  border = "black")
 plotrgl(alpha = 0.5)

## =======================================================================
## scatters with fitted surface and droplines (see ?scatter3D)
## =======================================================================

 par (mfrow = c(1, 1))
 with (mtcars, {

  # linear regression
   fit <- lm(mpg ~ wt + disp)

  # predict values on regular xy grid
   wt.pred <- seq(1.5, 5.5, length.out = 30)
   disp.pred <- seq(71, 472, length.out = 30)
   xy <- expand.grid(wt = wt.pred, 
                     disp = disp.pred)

   mpg.pred <- matrix (nrow = 30, ncol = 30, 
      data = predict(fit, newdata = data.frame(xy), 
      interval = "prediction"))

# fitted points for droplines to surface
   fitpoints <- predict(fit) 

   scatter3D(z = mpg, x = wt, y = disp, pch = 18, cex = 2, 
      theta = 20, phi = 20, ticktype = "detailed",
      xlab = "wt", ylab = "disp", zlab = "mpg",  
      surf = list(x = wt.pred, y = disp.pred, z = mpg.pred, 
                  facets = NA, fit = fitpoints),
      main = "mtcars")
  
 })

 plotrgl()
 
## =======================================================================
## scatter3D with text
## =======================================================================
 
 with(USArrests, text3D(Murder, Assault, Rape,
    colvar = UrbanPop, col = gg.col(100), theta = 60, phi = 20,
    xlab = "Murder", ylab = "Assault", zlab = "Rape",
    main = "USA arrests",
    labels = rownames(USArrests), cex = 0.8,
    bty = "g", ticktype = "detailed", d = 2,
    clab = c("Urban","Pop"), adj = 0.5, font = 2))

 with(USArrests, scatter3D(Murder, Assault, Rape - 1,
    colvar = UrbanPop, col = gg.col(100),
    type = "h", pch = ".", add = TRUE))

 plotrgl()

## =======================================================================
## slices from 3D full data set
## =======================================================================

 par(mfrow = c(1, 1))
 x <- y <- z <- seq(-4, 4, by = 0.1)
 M <- mesh(x, y, z)

 R <- with (M, sqrt(x^2 + y^2 + z^2))
 p <- sin(2*R) /(R+1e-3)

 slice3D(x, y, z, colvar = p, d = 2, theta = 60,
         xs = c(-4, 0), ys = c(-4, 0, 4), zs = c(-4, 0))
 plotrgl(smooth =  TRUE)
 
## =======================================================================
## 3D surfaces
## =======================================================================

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

# no shading or lighting in oridinary device
 surf3D(x, y, z, box = FALSE, theta = 60, col = "lightblue", plot = FALSE)

# add lighting afterwards in R graphics
 plotdev(lighting = TRUE, lphi = 0) 

# add light and smooth in rgl
 plotrgl(lighting = TRUE, smooth = TRUE)
 
## =======================================================================
## spheresurf3D
## =======================================================================

 AA <- Hypsometry$z
 lim <- c(-0.8, 0.8)

# log transformation of color variable; full = TRUE to plot both halves
 spheresurf3D(AA, theta = 90, phi = 30, box = FALSE,
    full = TRUE, xlim = lim, ylim = lim, zlim = lim)

# change the way the left mouse reacts
 plotrgl(mouseMode = c("zAxis", "zoom", "fov"))

## =======================================================================
## Arrows - has a flaw
## =======================================================================

 z <- seq(0, 2*pi, length.out = 100) 
 x <- cos(z)
 y <- sin(z)

 z0 <- z[seq(1, by = 10, length.out = 10)]
 z1 <- z[seq(9, by = 10, length.out = 10)]

# cone arrow heads
 arrows3D(x0 = 10*cos(z0), y0 = sin(z0), z0 = z0,
          x1 = 10*cos(z1), y1 = sin(z1), z1 = z1, 
          type = "cone", length = 0.4, lwd = 4,
          angle = 20, col = 1:10, plot = FALSE)

 plotrgl(lighting = TRUE)

## =======================================================================
## 2D plot
## =======================================================================

 image2D(z = volcano)
 plotrgl()
 
# reset plotting parameters 
 par(mfrow = pm)
 par(mar = pmar)

}
\keyword{ hplot }

