require(plot3D)

scatter2D(c(0,1), c(0,1))
abline(h = seq(0,1,len=20))
V <- volcano
V[40:50, 30:40] <- NA
image2D(z = V, NAcol = "transparent", shade = 0.2, add = TRUE)


require(plot3Drgl)
 par(mfrow = c(1, 1), mar = c(1, 1, 1, 3))

 AA <- Hypsometry$z; AA[AA<=0] <- NA
 
 lim <- c(-0.8, 0.8)

# log transformation of color variable
 spheresurf3D(AA, NAcol = "black", theta = 90, phi = 30, box = FALSE,
   contour = TRUE)#, xlim = lim, ylim = lim, zlim = lim, log = "c")



require(plot3D)
X <- rev(1:nrow(volcano))
Y <- 1:ncol(volcano)
persp3D(z=volcano, x= X, y = Y, xlim = c(-1,100), ticktype = "detailed")

# OK
 
 image2D(z = Oxsat$val[ , , 1], x = Oxsat$lon, y = Oxsat$lat,
       main = "surface oxygen saturation (%) for 2005", shade = 0.1) 
       