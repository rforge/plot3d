# merge 2D and 3D graphics...

#??contour3D along x, y
 require(plot3D.rgl)
persp3D(z = volcano, flighting = TRUE, curtain = TRUE, 
 lwd = 2, lphi = 45, ltheta = 45, phi = 0)
 
 plotrgl(smooth = TRUE)      # forgets curtains