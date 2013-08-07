require(plot3Drgl)


require(plot3D)
X <- rev(1:nrow(volcano))
Y <- 1:ncol(volcano)
persp3D(z=volcano, x= X, y = Y, xlim = c(-1,100), ticktype = "detailed")

# OK
 
 image2D(z = Oxsat$val[ , , 1], x = Oxsat$lon, y = Oxsat$lat,
       main = "surface oxygen saturation (%) for 2005", shade = 0.1) 
       