 require(plot3D.rgl)
 
 arrows2D(x0 = runif(10), y0 = runif(10), 
           x1 = runif(10), y1 = runif(10), colvar = 1:10, 
          code = 3, main = "arrows2D, segments2D")
 plotdev()
 plotrgl()
 plotrgl(xlim = c(0.2, 0.8))
 plotdev(xlim = c(0.2, 0.8))

#document this
require(plot3D)
x <- 1:4
y <- 2:6

z <- matrix(nr = 4, nc = 5, data = 1:20)
col <- matrix(nr = 3, nc = 4, data = grey(seq(0, 1, length=12)))

persp3D(z = z, col = col, ticktype = "detailed")

############
clim = range(volcano)
persp3D(z = volcano, zlim = c(100, 600), clim = clim, box = FALSE, plot = TRUE)

persp3D(z = volcano + 200, clim = clim, colvar = volcano, plot = FALSE,
       add = TRUE, colkey = FALSE)

 plist <- persp3D(z = volcano + 400, clim = clim, colvar = volcano,
       add = TRUE, colkey = FALSE, plot = TRUE)

require(OceanView)
   par(mfrow = c(1, 1))
  Speed <- sqrt(Syltsurf$u[,,2]^2 + Syltsurf$v[,,2]^2)
  with (Syltsurf,
     quiver2D(x = x, y = y, u = u[,,2], v = v[,,2], col = gg.col(100),
       xlim = c(5, 20), ylim = c(10, 25), by = 2, 
       colvar = Speed, clab = c("speed", "m/s"), 
       main = paste(formatC(time[1]), " hr"), scale = 1.5, 
       image = list(z = depth, x = x, y = y, col = "white",   # background
         NAcol = "darkblue"),
       contour = list(z = depth, x = x, y = y, col = "black", # depth 
         lwd = 2)
       )
    )
plotrgl()

# merge 2D and 3D graphics...
