\name{Plot plot3D objects}
\alias{plotrgl}
\title{
  Plots 3D objects. 
}
\description{
  \code{plotrgl} plots objects created with functions from package \code{plot3D} to rgl. 

}
\usage{
plotrgl (new = TRUE, lighting = FALSE, ...) 
}

\arguments{
  \item{new }{Logical, when \code{TRUE} will open a new window. 
  When \code{FALSE} will start a new plot in the same window.
    }
  \item{lighting }{Logical, when \code{TRUE} will add light.
  Default is toggled off ; this is similar to shading
    }
  \item{... }{Any argument to the \code{rgl} functions, e.g arguments from
    \link{open3d} or \link{material3d}.
    }
    
}

\seealso{
  A similar function, \link{plotdev}, from package \code{plot3D} plots the
  3D graphs to the current device (other than a rgl-device). 

  Any function of package \code{plot3D}: see help files of
  \link{persp3D}, \link{scatter3D}, \link{arrows3D}, \link{slice3D}, \link{surf3D}.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}
\note{
 The \link{arrows3D} function is not well translated to rgl: arrow heads are
 written as "^" 

 The translation of \link{scatter3D} ignores the \code{pch} argument but displays
 all symbols as spheres.  

 The color key is not plotted in rgl.
}
\examples{
# save plotting parameters
 pm <- par("mfrow")
 pmar <- par("mar")

## =======================================================================
## Composite images and contours in 3D
## =======================================================================
# plot it to traditional window: 
 persp3D(z = volcano, zlim = c(90, 300), col = "white",
         shade = 0.1, d = 2)
 contour3D(z = volcano, colvar = volcano, lwd = 2,
         add = TRUE, nlevels = 20, clab = c("height", "m"))
 contour3D(z = 300, colvar = volcano, lwd = 2, col = "grey",
         add = TRUE, nlevels = 20, clab = c("height", "m"))

 plotrgl(new = TRUE) # new window
 
## ========================================================================
## Two spheres
## ========================================================================

 par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

 M  <- mesh(seq(0, 2*pi, length.out = 30),
            seq(0,   pi, length.out = 30))
 u  <- M$x ; v  <- M$y

 x <- cos(u)*sin(v)
 y <- sin(u)*sin(v)
 z <- cos(v)

 surf3D(x = 2*x, y = 2*y, z = 2*z, colvar = NULL,
        facets = NA, col = "blue", lwd = 5)

 surf3D(x, y, z, colvar = NULL,
        col = "red", add = TRUE)

 plotrgl(lighting = TRUE)

# similar for current window, rotated:
 plotdev(lighting = TRUE, phi = 20)
  
## ========================================================================
## A perspective plot and a plane
## ========================================================================

 par(mar = pmar)

# The volcano
 x <- 1:nrow(volcano)
 y <- 1:ncol(volcano)

# create and plot the perspective box
 perspbox(x, y, z = volcano, theta = 10, phi = 20,
          box = FALSE, scale = FALSE, expand = 0.3,
          colkey = TRUE)

# add volcano
 persp3D (x, y, z = volcano, colvar = volcano, add = TRUE,
          clim = range(volcano), shade = 0.1, plot = FALSE)

# add a plane (image) at z = 170; jetcolored, transparant: only border
 image3D(x, y, z = 170, add = TRUE, clim = range(volcano),
         colvar = volcano, facets = NA, plot = FALSE)

# add a contour (image) at z = 170; jetcolored,
 contour3D(x, y, z = 170, add = TRUE, clim = range(volcano),
           colvar = volcano, facets = NA, plot = FALSE)

# plot it as regular plot
 plotdev()

# plot on rgl device
 plotrgl()

## =======================================================================
## slices from the oxygen saturation data
## =======================================================================

 par(mfrow = c(1, 1))
 ii <- which (Oxsat$lon > -90 & Oxsat$lon < 90)
 jj <- which (Oxsat$lat > 0 & Oxsat$lat < 90)

 xs <- c(Oxsat$lon[ii[1]], Oxsat$lon[ii[length(ii)]])  # E , W boundary
 ys <- c(Oxsat$lat[jj[1]], Oxsat$lat[jj[length(jj)]])  # S , Nboundary

 slice3D(colvar = Oxsat$val[ii,jj,], x = Oxsat$lon[ii],
        y = Oxsat$lat[jj], z = -Oxsat$depth,
        NAcol = "black", xs = xs, ys = ys, zs = 0,
        theta = 35, phi = 50, colkey = list(length = 0.5),
        expand = 0.5, ticktype = "detailed",
        clab = "\%", main = "Oxygen saturation",
        xlab = "longitude", ylab = "latitude", zlab = "depth")
        
 plotrgl()

## =======================================================================
## Ribbon, persp
## =======================================================================

 ribbon3D(z = volcano, contour = TRUE, zlim= c(-100, 200),
          image = TRUE)
 plotrgl()

 persp3D(z = volcano, contour = list(side = c("zmin", "z", "350")),
       zlim= c(-100, 400), phi = 20, image = list(side = 350))
 plotrgl()
 
## =======================================================================
## hist3D
## =======================================================================

 VV   <- volcano[seq(1, 87, 10), seq(1, 61, 10)]

# transparent colors
 hist3D(z = VV, scale = FALSE, expand = 0.01,
   col = jet.col(100, alpha = 0.3),  border = "black")
 plotrgl(alpha = 0.5)

## =======================================================================
## hist3D for a 1-D data set
## =======================================================================

 par(mfrow = c(2, 1))
 x <- rchisq(1000, df = 4)
 hs <- hist(x, breaks = 15)

 hist3D(x = hs$mids, y = 1, z = matrix(ncol = 1, data = hs$density),
   bty = "g", ylim = c(0., 2.0), scale = FALSE, expand = 20,
   border = "black", col = "white", space = 0.1,
   theta = 20, phi = 20, main = "3-D perspective")
  
  plotrgl(lighting = TRUE)

## =======================================================================
## scatters
## =======================================================================

 par (mfrow = c(1, 1))
 z <- seq(0, 10, 0.2)
 x <- cos(z)
 y <- sin(z)*z

# greyish background for the boxtype (bty = "g")
 scatter3D(x, y, z, phi = 0, bty = "g",
           pch = 20, cex = 2, ticktype = "detailed")
# add another point
 scatter3D(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
           pch = 18, cex = 3, col = "black")

# add text
 text3D(x = cos(1:10), y = (sin(1:10)*(1:10) - 1),
        z = 1:10, colkey = FALSE, add = TRUE,
        labels = LETTERS[1:10], col = c("black", "red"))

 plotrgl()
 
## =======================================================================
## Scatter with a surface
## =======================================================================

 par(mfrow = c(1, 1))

# surface = volcano
 M <- mesh(1:nrow(volcano), 1:ncol(volcano))

# 100 points above volcano 
 N  <- 100
 xs <- runif(N) * 87
 ys <- runif(N) * 61
 zs <- runif(N)*50 + 154

# we must specify colvar here, as the default is as from surf3D
 scatter3D(xs, ys, zs, ticktype = "detailed", pch = 16,
   xlim = c(1, 87), ylim = c(1,61), zlim = c(94, 215),
   surf = list(x = M$x, y = M$y, z = volcano,
       col = jet.col(100), colvar = volcano))

 plotrgl(lighting = TRUE)

## =======================================================================
## scatter with droplines till the fitted surface
## =======================================================================

 M <- mesh(seq(0, 2*pi, length = 30), (1:30)/100)
 z <- with (M, sin(x) + y)

# points 'sampled'
 N <- 30
 xs <- runif(N) * 2*pi
 ys <- runif(N) * 0.3

 zs <- sin(xs) + ys + rnorm(N)*0.3

 disttoz <- sin(xs) + ys - zs
 CIz = matrix(nrow = length(xs), data = c(-disttoz, disttoz))
 CIz[CIz > 0] <- 0

# no arrow head (alen=0)
 CI <- list(z = CIz, lwd = 4, alen = 0, col = "black")

# transparent surface; border colored according to z (default colvar)
 scatter3D(xs, ys, zs, ticktype = "detailed", pch = 16,
   xlim = c(0, 2*pi), ylim = c(0, 0.3), zlim = c(-1.7, 1.7),
   CI = CI, theta = 20, phi = 30, cex = 2, bty = "g",
   surf = list(x = M$x, y = M$y, z = z,
       facets = NA, shade = 0.5)
   )

 plotrgl()
 
## =======================================================================
## quakes scatter
## =======================================================================
 
 with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag,
      pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude",
      zlab = "depth, km", clab = c("Richter","Magnitude"),
      main = "Earthquakes off Fiji", ticktype = "detailed",
      type = "h", theta = 10, d = 2,
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
      )
 plotrgl()
 
## =======================================================================
## hist3D with text
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
 plotrgl()
 
## =======================================================================
## voxel3D
## =======================================================================

 x <- y <- z <- seq(-2, 2, length.out = 50)
 xyz <- mesh(x, y, z)
 F <- with(xyz, log(x^2 + y^2 + z^2 +
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

 voxel3D(x, y, z, F, level = 1)
 plotrgl()

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

# full colored image
 surf3D(x, y, z, colvar = y, colkey = FALSE, shade = 0.5,
        box = FALSE, theta = 60)

 plotrgl()
 
 surf3D(x, y, z, box = FALSE,
        theta = 60, col = "lightblue")
 plotrgl(lighting = TRUE)
 
## =======================================================================
## spheresurf3D
## =======================================================================

 AA <- Hypsometry$z
 lim <- c(-0.8, 0.8)

# log transformation of color variable; full = TRUE to plot both halves
 spheresurf3D(AA, theta = 90, phi = 30, box = FALSE,
    full = TRUE, xlim = lim, ylim = lim, zlim = lim)

 plotrgl()

## =======================================================================
## several polygons, separated by NAs
## =======================================================================

# only polygons up to 4 corners supported
 x <- runif(39) 
 y <- runif(39)
 z <- runif(39)
 ii <- seq(4, 36, by  = 4)
 x[ii] <- y[ii] <- z[ii] <- NA 

# transparent colors (alpha)
 polygon3D(x, y, z, border = "black", lwd = 3,
   col = gg.col(length(ii) + 1, alpha = 0.8))

 plotrgl(alpha = 0.8)
 
# reset plotting parameters
 par(mfrow = pm)
 par(mar = pmar)

}
\keyword{ hplot }

